import { useSession } from "next-auth/react";
import { useEffect } from "react";
import { useRouter } from "next/router";
import { useAsync } from "react-use";
import { loadJson } from "../feutil/network";
import type { Project } from "../models";
import { useRecoilRefresher_UNSTABLE, useRecoilValue } from "recoil";
import { projectListQuery } from "../feutil/state";
import React from "react";
import { Container, Loading, Row, Spacer, Text } from "@nextui-org/react";

export function RequireProject({ children }: { children: React.ReactNode }) {
  return (
    <>
      <React.Suspense fallback={<ProjectInit message="Loading project" />}>
        <RequireProjectInner>{children}</RequireProjectInner>
      </React.Suspense>
    </>
  )
}

// There can be at most one project created, globally.
let firstProjectCreated = false;

function RequireProjectInner({ children }: { children: React.ReactNode }) {
  const projectList = useRecoilValue(projectListQuery);
  const refresh = useRecoilRefresher_UNSTABLE(projectListQuery);

  useAsync(async () => {
    if (projectList.length === 0 && !firstProjectCreated) {
      firstProjectCreated = true;
      const suffix = Math.floor(Math.random() * 0x100000000).toString(16).padStart(8, "0");
      await loadJson("/api/project/create", { name: `project-${suffix}` });
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
    <Container xs css={{ pt: 80 }}>
      <Row align="center" justify="center">
        <Loading size="lg"></Loading>
        <Spacer css={{ w: 16 }} />
        <Text weight="semibold" color="primary">{message}</Text>
      </Row>
    </Container>
  )
}