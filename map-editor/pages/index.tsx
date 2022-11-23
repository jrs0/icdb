import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

export default function Home() {
    return (
        <ul>
            <li><Link href="/mapping">Mappings</Link></li>
            <li><Link href="/icd">ICD-10</Link></li>
        </ul>
    )
}
