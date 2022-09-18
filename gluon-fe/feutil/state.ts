import { selector, selectorFamily } from "recoil";
import type { Project, ProjectMember, App } from "../models";
import type { AppInfo } from "../service/api_types";
import { loadJson } from "./network";

export interface ProjectInfo {
  project: Project,
  membership: ProjectMember,
}

export const projectListQuery = selector<ProjectInfo[]>({
  key: "projectListQuery",
  get: async () => await loadJson<ProjectInfo[]>("/api/project/list"),
});

export const projectSelector = selectorFamily<ProjectInfo | null, string>({
  key: "projectSelector",
  get: p => ({ get }) => {
    const projects = get(projectListQuery);
    return projects.find(p2 => p2.project.id === p) || null;
  }
});

export const firstProjectSelector = selector<ProjectInfo | null>({
  key: "firstProjectSelector",
  get: ({ get }) => {
    const projects = get(projectListQuery);
    return projects.length > 0 ? projects[0] : null;
  }
});

export const appListQuery = selectorFamily<AppInfo[], string>({
  key: "appListQuery",
  get: projectId => async () => await loadJson<AppInfo[]>("/api/app/list", {
    projectId,
  })
});

export const appSelector = selectorFamily<AppInfo | null, [string, number]>({
  key: "appSelector",
  get: ([projectId, appId]) => ({ get }) => {
    const apps = get(appListQuery(projectId));
    return apps.find(x => x.id === appId) || null;
  }
});
