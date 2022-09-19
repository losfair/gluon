import { Link, Row, Text } from '@nextui-org/react'
import NextLink from 'next/link'

export function BackToAllApps({ projectId }: { projectId: string }) {
  return (
    <div style={{ position: "absolute", marginTop: "-36px" }}>
      <NextLink href={`/projects/${projectId}/apps`} passHref>
        <Link css={{ w: 150 }} block color="primary">
          <Row align="center">
            <svg style={{ width: 16 }} xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" className="w-6 h-6">
              <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 19.5L8.25 12l7.5-7.5" />
            </svg>
            <Text size={16} weight="semibold" css={{ pl: 12, color: "$currentColor" }}>All apps</Text>
          </Row>
        </Link>
      </NextLink>
    </div>
  )
}