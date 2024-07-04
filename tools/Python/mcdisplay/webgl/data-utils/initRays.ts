import { RayData } from "../model/Rays";

export function initializeRays(data: any): RayData {
  return {
    ...data,
    rays: data.rays.map((ray) => ({
      ...ray,
      groups: ray.groups.map((group) => ({
        ...group,
        events: group.events.map((event) => ({
          ...event,
          args: event.args.map((arg) => arg),
        })),
      })),
    })),
  };
}
