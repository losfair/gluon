import { useSession } from "next-auth/react";
import { useEffect } from "react";
import { useRouter } from "next/router";
import { useAsync } from "react-use";
import { loadJson } from "../feutil/network";
import type { Project } from "../models";
import { useRecoilRefresher_UNSTABLE, useRecoilValue } from "recoil";
import { projectListQuery } from "../feutil/state";
import React from "react";
import { Container, Text } from "@nextui-org/react";

export function RequireProject({ children }: { children: React.ReactNode }) {
  return (
    <>
      <React.Suspense fallback={<ProjectInit message="Loading project" />}>
        <RequireProjectInner>{children}</RequireProjectInner>
      </React.Suspense>
    </>
  )
}


function RequireProjectInner({ children }: { children: React.ReactNode }) {
  const projectList = useRecoilValue(projectListQuery);
  const refresh = useRecoilRefresher_UNSTABLE(projectListQuery);

  useAsync(async () => {
    if (projectList.length === 0) {
      await loadJson("/api/project/create", { name: "personal-project" });
      refresh();
    }
  }, [projectList.length]);

  if (projectList.length === 0) {
    return <ProjectInit message="Initializing your first project" />
  };

  return <>{children}</>;
}

function ProjectInit({ message }: { message: string }) {
  return (
    <Container>
      <Text>{message}</Text>
    </Container>
  )
}