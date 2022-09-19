import { Container, Col, Row, Card, Spacer, Button, Input, Loading, Text, Image, Link } from '@nextui-org/react'
import type { NextPage } from 'next'
import { useSession } from 'next-auth/react'
import dynamic from 'next/dynamic'
import { useRecoilValueLoadable } from 'recoil'
import { RequireAuth } from '../../../../components/require_auth'
import { RequireProject } from '../../../../components/require_project'
import { appListQuery } from '../../../../feutil/state'
import { loadProjectProps, ProjectProps } from '../../../../service/util'
import NextLink from 'next/link'
import type { App } from '../../../../models'
import { Footer } from '../../../../components/footer'
import { AppCard } from '../../../../components/app_card'

export const getServerSideProps = loadProjectProps;

function BackToDashboard() {
  return (
    <div style={{ position: "absolute", marginTop: "-36px" }}>
      <NextLink href={`/dashboard`} passHref>
        <Link css={{ w: 150 }} block color="primary">
          <Row align="center">
            <svg style={{ width: 16 }} xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="w-6 h-6">
              <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 19.5L8.25 12l7.5-7.5" />
            </svg>
            <Text size={16} weight="semibold" css={{ pl: 12, color: "$currentColor" }}>Dashboard</Text>
          </Row>
        </Link>
      </NextLink>
    </div>
  )
}

const Apps: NextPage<ProjectProps> = ({ projectId, userId, role }) => {
  const appList = useRecoilValueLoadable(appListQuery(projectId));

  return (
    <Container xs css={{ pt: 80 }}>
      <BackToDashboard />
      <Col>
        <Row align="center">
          <Col css={{ width: "auto" }}>
            <Text h1 size={36}>Apps</Text>
          </Col>
          <Col css={{ flexGrow: 1 }}></Col>
          {role === "owner" && (
            <Col css={{ width: 120 }}>
              <NextLink href={`/projects/${projectId}/launch`}>
                <Link block color="secondary">
                  Launch
                </Link>
              </NextLink>
            </Col>
          )}
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
  )
}

export default dynamic(() => Promise.resolve(Apps), {
  ssr: false
});
