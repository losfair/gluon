import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text, Image, Link } from '@nextui-org/react'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../../components/require_auth'
import { RequireProject } from '../../../../components/require_project'
import { appListQuery, projectSelector } from '../../../../feutil/state'
import { loadProjectProps, ProjectProps } from '../../../../service/util'
import NextLink from 'next/link'
import type { App } from '../../../../models'
import { Footer } from '../../../../components/footer'
import { AppCard } from '../../../../components/app_card'

export const getServerSideProps = loadProjectProps;

const Apps: NextPage<ProjectProps> = ({ projectId, userId }) => {
  const project = useRecoilValueLoadable(projectSelector(projectId));
  const appList = useRecoilValueLoadable(appListQuery(projectId));

  return (
    <RequireAuth>
      <RequireProject>
        <Container xs css={{ pt: 80 }}>
          <Col>
            <Row align="center">
              <Col css={{ width: "auto" }}>
                <Text h1 size={36}>Apps</Text>
              </Col>
              <Col css={{ flexGrow: 1 }}></Col>
              <Col css={{ width: 120 }}>
                <NextLink href={`/projects/${projectId}/launch`}>
                  <Link block color="secondary">
                    Launch
                  </Link>
                </NextLink>
              </Col>
            </Row>
            {appList.state === "hasValue" && <>
              {!appList.contents.length && <Row><Text css={{ color: "gray" }}>No apps yet</Text></Row>}
              <Col>{
                appList.contents.map(app => (
                  <AppCard key={app.id} app={app} />
                ))
              }</Col>
            </>}
          </Col>
          <Footer projectId={projectId} userId={userId} />
        </Container>
      </RequireProject>
    </RequireAuth>
  )
}

export default dynamic(() => Promise.resolve(Apps), {
  ssr: false
});
