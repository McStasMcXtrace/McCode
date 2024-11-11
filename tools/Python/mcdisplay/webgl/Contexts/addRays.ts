import { Ray, Group, RayData } from "../model/Rays";
import * as THREE from "three";
import { Lut } from "three/examples/jsm/Addons.js";
import { Component } from "../model/Component";

// Constants
export const SCATTERPOINTS = "scatterpoints";
export const RAY = "ray";

// LUT Configuration
const LUT = new Lut("cooltowarm", 512);

export function setRaysInvisible(parentnode) {
  for (let i = 0; i < parentnode.children.length; i++) {
    if (parentnode.children[i].name.includes(RAY))
      parentnode.children[i].visible = false;
  }
}

export function setRaysVisible(parentnode) {
  for (let i = 0; i < parentnode.children.length; i++) {
    if (parentnode.children[i].name.includes(RAY))
      parentnode.children[i].visible = true;
  }
}

export function setRayVisibility(
  parentnode,
  index: number,
  visibility: boolean
) {
  for (let i = 0; i < parentnode.children.length; i++) {
    if (parentnode.children[i].name === RAY + index) {
      parentnode.children[i].visible = visibility;
    }
  }
}

export function setScatterPointsInvisible(parentnode) {
  for (let i = 0; i < parentnode.children.length; i++) {
    if (parentnode.children[i].name.includes(RAY)) {
      for (let j = 0; j < parentnode.children[i].children.length; j++) {
        if (parentnode.children[i].children[j].name === SCATTERPOINTS)
          parentnode.children[i].children[j].visible = false;
      }
    }
  }
}

export function setScatterPointsVisible(parentnode) {
  for (let i = 0; i < parentnode.children.length; i++) {
    if (parentnode.children[i].name.includes(RAY)) {
      for (let j = 0; j < parentnode.children[i].children.length; j++) {
        if (parentnode.children[i].children[j].name === SCATTERPOINTS)
          parentnode.children[i].children[j].visible = true;
      }
    }
  }
}

export function addRays(parentNode, rayData: RayData, components: Component[]) {
  LUT.setMin(rayData.vmin);
  LUT.setMax(rayData.vmax);
  const rays = rayData.rays;
  rays.forEach((ray, index) => {
    const rayGroup = createRayGroup(ray, components);
    rayGroup.name = RAY + index;
    parentNode.add(rayGroup);
  });
}

function createRayGroup(ray: Ray, components: Component[]): THREE.Group {
  const rayGroup = new THREE.Group();
  rayGroup.name = RAY;
  let vertices: THREE.Vector3[] = [];
  ray.groups.forEach((group) => {
    let compVertices: THREE.Vector3[] = [];
    let component = components.find((c) => c.name === group.compname);
    compVertices = processGroupEvents(
      group,
      component as Component,
      compVertices
    );
    vertices = vertices.concat(compVertices);
  });
  const color = LUT.getColor(ray.speed);
  const line = createLine(vertices, color);
  rayGroup.add(line);

  const scatterPoints = createScatterPoints(vertices, color);
  rayGroup.add(scatterPoints);

  rayGroup.visible = false;
  return rayGroup;
}

function processGroupEvents(
  group: Group,
  component: Component,
  vertices: THREE.Vector3[]
): THREE.Vector3[] {
  group.events.forEach((event) => {
    vertices.push(
      new THREE.Vector3(event.args[0], event.args[1], event.args[2])
    );
  });

  if (component) {
    const matrix4 = new THREE.Matrix4(
      component.m4[0],
      component.m4[1],
      component.m4[2],
      component.m4[3],
      component.m4[4],
      component.m4[5],
      component.m4[6],
      component.m4[7],
      component.m4[8],
      component.m4[9],
      component.m4[10],
      component.m4[11],
      component.m4[12],
      component.m4[13],
      component.m4[14],
      component.m4[15]
    );
    vertices = applyTransformation(vertices, matrix4);
  }

  return vertices;
}

function applyTransformation(
  vertices: THREE.Vector3[],
  matrix: THREE.Matrix4
): THREE.Vector3[] {
  let geometry = new THREE.BufferGeometry();
  let float32Array = new Float32Array(vertices.length * 3);
  for (let i = 0; i < vertices.length; i++) {
    float32Array[i * 3] = vertices[i].x;
    float32Array[i * 3 + 1] = vertices[i].y;
    float32Array[i * 3 + 2] = vertices[i].z;
  }
  geometry.setAttribute("position", new THREE.BufferAttribute(float32Array, 3));

  geometry.applyMatrix4(matrix);

  let transformedVertices: THREE.Vector3[] = [];
  let position = geometry.attributes.position;
  for (let i = 0; i < position.count; i++) {
    transformedVertices.push(
      new THREE.Vector3(position.getX(i), position.getY(i), position.getZ(i))
    );
  }

  return transformedVertices;
}

function createLine(vertices: THREE.Vector3[], color: THREE.Color): THREE.Line {
  const geometry = new THREE.BufferGeometry().setFromPoints(vertices);
  const material = new THREE.LineBasicMaterial({ color });
  return new THREE.Line(geometry, material);
}

function createScatterPoints(
  vertices: THREE.Vector3[],
  color: THREE.Color
): THREE.Group {
  const scatterPoints = new THREE.Group();
  scatterPoints.name = SCATTERPOINTS;

  vertices.forEach((vertex) => {
    const geometry = new THREE.SphereGeometry(0.004, 4, 2);
    const material = new THREE.MeshBasicMaterial({ color });
    const sphere = new THREE.Mesh(geometry, material);
    sphere.position.copy(vertex);
    scatterPoints.add(sphere);
  });

  scatterPoints.visible = true;
  return scatterPoints;
}
