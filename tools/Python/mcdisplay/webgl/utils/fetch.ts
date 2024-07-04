// Function to fetch JSON if it exists
export async function fetchJSON(path: string) {
  try {
    const response = await fetch(path);
    if (!response.ok) throw new Error("Network response was not ok");
    return await response.json();
  } catch (error) {
    console.error(`Failed to fetch ${path}:`, error);
    return null; // Return null or handle as needed
  }
}
