//! Runtime capability identities and nested affine transfer provenance.
use crate::{tokens::Span, verify::CapabilityId};
use std::collections::{BTreeMap,BTreeSet};
#[derive(Debug,Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)] pub struct CapabilityInstanceId(u64);
impl CapabilityInstanceId{pub const fn value(self)->u64{self.0}}
#[derive(Debug,Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)] pub struct CapabilityOwnerId(u64);
impl CapabilityOwnerId{pub const fn value(self)->u64{self.0}}
#[derive(Debug,Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)] pub struct InvocationId(u64);
impl InvocationId{pub const fn value(self)->u64{self.0}}
#[derive(Debug,Clone,Copy,PartialEq,Eq,PartialOrd,Ord,Hash)] pub struct TransferId(u64);
impl TransferId{pub const fn value(self)->u64{self.0}}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub enum TransferState{Active,Returned,Consumed,Abandoned}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub enum CapabilityState{
 Available, InTransit{transfer_id:TransferId},
 Consumed{invocation_id:InvocationId,consumed_at:Span}, Abandoned{invocation_id:InvocationId}
}
#[derive(Debug,PartialEq,Eq)] pub struct CapabilityInstance{id:CapabilityInstanceId,kind:CapabilityId,owner:CapabilityOwnerId,state:CapabilityState,provenance:Span}
impl CapabilityInstance{
 pub const fn id(&self)->CapabilityInstanceId{self.id} pub const fn kind(&self)->CapabilityId{self.kind}
 pub const fn owner(&self)->CapabilityOwnerId{self.owner} pub const fn state(&self)->CapabilityState{self.state}
 pub const fn provenance(&self)->Span{self.provenance}
}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub struct CapabilityTransferRecord{
 transfer_id:TransferId,instance_id:CapabilityInstanceId,invocation_id:InvocationId,
 source_owner:CapabilityOwnerId,destination_owner:CapabilityOwnerId,parent_transfer:Option<TransferId>,state:TransferState
}
impl CapabilityTransferRecord{
 pub const fn transfer_id(&self)->TransferId{self.transfer_id} pub const fn instance_id(&self)->CapabilityInstanceId{self.instance_id}
 pub const fn invocation_id(&self)->InvocationId{self.invocation_id} pub const fn source_owner(&self)->CapabilityOwnerId{self.source_owner}
 pub const fn destination_owner(&self)->CapabilityOwnerId{self.destination_owner} pub const fn parent_transfer(&self)->Option<TransferId>{self.parent_transfer}
 pub const fn state(&self)->TransferState{self.state}
}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] enum InvocationState{Active,Returned,Abandoned}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] struct Invocation{owner:CapabilityOwnerId,parent:Option<InvocationId>,state:InvocationState}
#[derive(Debug,PartialEq,Eq)] pub struct CapabilityLedger{
 instances:BTreeMap<CapabilityInstanceId,CapabilityInstance>,owners:BTreeSet<CapabilityOwnerId>,
 transfers:BTreeMap<TransferId,CapabilityTransferRecord>,active_transfers:BTreeMap<CapabilityInstanceId,TransferId>,
 invocation_transfers:BTreeMap<InvocationId,BTreeSet<TransferId>>,invocations:BTreeMap<InvocationId,Invocation>,
 next_instance:u64,next_owner:u64,next_invocation:u64,next_transfer:u64,root:CapabilityOwnerId
}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub enum CapabilityError{
 IdExhausted,InvocationIdExhausted,TransferIdExhausted,UnknownOwner(CapabilityOwnerId),UnknownInstance(CapabilityInstanceId),
 UnknownInvocation(InvocationId),UnknownTransfer(TransferId),OwnerMismatch{instance:CapabilityInstanceId,owner:CapabilityOwnerId},
 AlreadyInTransit(CapabilityInstanceId),NotInTransit(CapabilityInstanceId),AlreadyConsumed(CapabilityInstanceId),Abandoned(CapabilityInstanceId),
 DuplicateTransfer(CapabilityInstanceId),SameOwnerTransfer,InactiveInvocation(InvocationId),InvalidInvocationParent(InvocationId),
 InvalidTransferDestination(CapabilityOwnerId),InvalidNestedTransfer(CapabilityInstanceId),ParentTransferMismatch(CapabilityInstanceId),
 UnresolvedDescendant(CapabilityInstanceId),IncompleteTransferInventory(InvocationId),InvalidOwnershipState(CapabilityInstanceId),TransferHistoryCorrupt(TransferId)
}
impl Default for CapabilityLedger{fn default()->Self{Self::new()}}
impl CapabilityLedger{
 pub fn new()->Self{let root=CapabilityOwnerId(1);Self{instances:BTreeMap::new(),owners:BTreeSet::from([root]),transfers:BTreeMap::new(),active_transfers:BTreeMap::new(),invocation_transfers:BTreeMap::new(),invocations:BTreeMap::new(),next_instance:1,next_owner:2,next_invocation:1,next_transfer:1,root}}
 pub const fn root_owner(&self)->CapabilityOwnerId{self.root}
 pub fn create_owner(&mut self)->Result<CapabilityOwnerId,CapabilityError>{let n=self.next_owner.checked_add(1).ok_or(CapabilityError::IdExhausted)?;let id=CapabilityOwnerId(self.next_owner);self.next_owner=n;self.owners.insert(id);Ok(id)}
 pub fn provision(&mut self,kind:CapabilityId,owner:CapabilityOwnerId,provenance:Span)->Result<CapabilityInstanceId,CapabilityError>{
  if !self.owners.contains(&owner){return Err(CapabilityError::UnknownOwner(owner))}
  let n=self.next_instance.checked_add(1).ok_or(CapabilityError::IdExhausted)?;let id=CapabilityInstanceId(self.next_instance);self.next_instance=n;
  self.instances.insert(id,CapabilityInstance{id,kind,owner,state:CapabilityState::Available,provenance});Ok(id)
 }
 pub fn instance(&self,id:CapabilityInstanceId)->Result<&CapabilityInstance,CapabilityError>{self.instances.get(&id).ok_or(CapabilityError::UnknownInstance(id))}
 pub fn transfer(&self,id:TransferId)->Result<&CapabilityTransferRecord,CapabilityError>{self.transfers.get(&id).ok_or(CapabilityError::UnknownTransfer(id))}
 pub fn active_transfer(&self,id:CapabilityInstanceId)->Option<TransferId>{self.active_transfers.get(&id).copied()}
 pub fn create_invocation(&mut self,owner:CapabilityOwnerId,parent:Option<InvocationId>)->Result<InvocationId,CapabilityError>{
  if !self.owners.contains(&owner){return Err(CapabilityError::UnknownOwner(owner))}
  if let Some(p)=parent{let v=self.invocations.get(&p).ok_or(CapabilityError::UnknownInvocation(p))?;if v.state!=InvocationState::Active{return Err(CapabilityError::InvalidInvocationParent(p))}}
  let n=self.next_invocation.checked_add(1).ok_or(CapabilityError::InvocationIdExhausted)?;let id=InvocationId(self.next_invocation);self.next_invocation=n;
  self.invocations.insert(id,Invocation{owner,parent,state:InvocationState::Active});self.invocation_transfers.insert(id,BTreeSet::new());Ok(id)
 }
 pub fn transfer_batch(&mut self,source:CapabilityOwnerId,dest:CapabilityOwnerId,invocation:InvocationId,ids:&[CapabilityInstanceId])->Result<Vec<TransferId>,CapabilityError>{
  let call=*self.invocations.get(&invocation).ok_or(CapabilityError::UnknownInvocation(invocation))?;
  if call.state!=InvocationState::Active{return Err(CapabilityError::InactiveInvocation(invocation))}
  if call.owner!=dest{return Err(CapabilityError::InvalidTransferDestination(dest))}
  if source==dest{return Err(CapabilityError::SameOwnerTransfer)}
  if !self.owners.contains(&source){return Err(CapabilityError::UnknownOwner(source))}
  let mut seen=BTreeSet::new();let mut parents=Vec::new();
  for id in ids{
   if !seen.insert(*id){return Err(CapabilityError::DuplicateTransfer(*id))}
   let i=self.instances.get(id).ok_or(CapabilityError::UnknownInstance(*id))?;
   if i.owner!=source{return Err(CapabilityError::OwnerMismatch{instance:*id,owner:source})}
   match(i.state,self.active_transfers.get(id).copied()){
    (CapabilityState::Available,None) if call.parent.is_none()&&source==self.root=>parents.push(None),
    (CapabilityState::InTransit{transfer_id},Some(active)) if transfer_id==active=>{
     let p=self.transfers.get(&active).ok_or(CapabilityError::TransferHistoryCorrupt(active))?;
     if p.state!=TransferState::Active||p.destination_owner!=source||Some(p.invocation_id)!=call.parent{return Err(CapabilityError::ParentTransferMismatch(*id))}
     parents.push(Some(active))
    },
    (CapabilityState::Consumed{..},_)=>return Err(CapabilityError::AlreadyConsumed(*id)),
    (CapabilityState::Abandoned{..},_)=>return Err(CapabilityError::Abandoned(*id)),
    (CapabilityState::InTransit{..},_)=>return Err(CapabilityError::InvalidNestedTransfer(*id)),
    _=>return Err(CapabilityError::InvalidOwnershipState(*id))
   }
  }
  let count=u64::try_from(ids.len()).map_err(|_|CapabilityError::TransferIdExhausted)?;
  let next=self.next_transfer.checked_add(count).ok_or(CapabilityError::TransferIdExhausted)?;
  let tids:Vec<_>=(0..count).map(|n|TransferId(self.next_transfer+n)).collect();
  for((iid,parent),tid)in ids.iter().zip(parents).zip(tids.iter().copied()){
   self.transfers.insert(tid,CapabilityTransferRecord{transfer_id:tid,instance_id:*iid,invocation_id:invocation,source_owner:source,destination_owner:dest,parent_transfer:parent,state:TransferState::Active});
   self.active_transfers.insert(*iid,tid);self.invocation_transfers.get_mut(&invocation).expect("inventory exists").insert(tid);
   let i=self.instances.get_mut(iid).expect("preflighted");i.owner=dest;i.state=CapabilityState::InTransit{transfer_id:tid};
  }
  self.next_transfer=next;Ok(tids)
 }
 pub fn consume(&mut self,owner:CapabilityOwnerId,id:CapabilityInstanceId,invocation:InvocationId,at:Span)->Result<(),CapabilityError>{
  let c=self.invocations.get(&invocation).ok_or(CapabilityError::UnknownInvocation(invocation))?;
  if c.state!=InvocationState::Active{return Err(CapabilityError::InactiveInvocation(invocation))}
  let i=self.instances.get(&id).ok_or(CapabilityError::UnknownInstance(id))?;
  if i.owner!=owner{return Err(CapabilityError::OwnerMismatch{instance:id,owner})}
  let tid=match i.state{CapabilityState::Available=>return Err(CapabilityError::NotInTransit(id)),CapabilityState::InTransit{transfer_id}=>transfer_id,CapabilityState::Consumed{..}=>return Err(CapabilityError::AlreadyConsumed(id)),CapabilityState::Abandoned{..}=>return Err(CapabilityError::Abandoned(id))};
  let t=self.transfers.get(&tid).ok_or(CapabilityError::TransferHistoryCorrupt(tid))?;
  if t.state!=TransferState::Active||t.invocation_id!=invocation||t.destination_owner!=owner{return Err(CapabilityError::InvalidOwnershipState(id))}
  self.transfers.get_mut(&tid).expect("checked").state=TransferState::Consumed;self.active_transfers.remove(&id);
  self.instances.get_mut(&id).expect("checked").state=CapabilityState::Consumed{invocation_id:invocation,consumed_at:at};Ok(())
 }
 pub fn reconcile_return(&mut self,id:InvocationId)->Result<(),CapabilityError>{self.reconcile(id,false)}
 pub fn mark_abandoned(&mut self,id:InvocationId)->Result<(),CapabilityError>{self.reconcile(id,true)}
 fn reconcile(&mut self,id:InvocationId,abnormal:bool)->Result<(),CapabilityError>{
  let c=*self.invocations.get(&id).ok_or(CapabilityError::UnknownInvocation(id))?;
  if c.state!=InvocationState::Active{return Err(CapabilityError::InactiveInvocation(id))}
  let inventory=self.invocation_transfers.get(&id).ok_or(CapabilityError::IncompleteTransferInventory(id))?;
  let tids:Vec<_>=inventory.iter().copied().collect();
  for tid in &tids{
   let t=self.transfers.get(tid).ok_or(CapabilityError::TransferHistoryCorrupt(*tid))?;
   if t.invocation_id!=id{return Err(CapabilityError::TransferHistoryCorrupt(*tid))}
   let i=self.instances.get(&t.instance_id).ok_or(CapabilityError::UnknownInstance(t.instance_id))?;
   if let Some(p)=t.parent_transfer{let parent=self.transfers.get(&p).ok_or(CapabilityError::TransferHistoryCorrupt(p))?;if parent.state!=TransferState::Active||parent.destination_owner!=t.source_owner{return Err(CapabilityError::ParentTransferMismatch(t.instance_id))}}
   if self.active_transfers.get(&t.instance_id).is_some_and(|active|*active!=*tid){return Err(CapabilityError::UnresolvedDescendant(t.instance_id))}
   match(t.state,i.state,self.active_transfers.get(&t.instance_id).copied()){
    (TransferState::Active,CapabilityState::InTransit{transfer_id},Some(active))if transfer_id==*tid&&active==*tid=>{},
    (TransferState::Active,CapabilityState::Consumed{..},None)=>{},
    (TransferState::Consumed,CapabilityState::Consumed{..},None)=>{},
    (TransferState::Active,CapabilityState::Abandoned{..},None)if abnormal=>{},
    (TransferState::Abandoned,CapabilityState::Abandoned{..},None)if abnormal=>{},
    _=>return Err(CapabilityError::TransferHistoryCorrupt(*tid))
   }
  }
  for tid in &tids{
   let t=*self.transfers.get(tid).expect("preflighted");let i=self.instances.get_mut(&t.instance_id).expect("preflighted");
   match i.state{
    CapabilityState::InTransit{transfer_id}if transfer_id==*tid=>{
     if abnormal{i.state=CapabilityState::Abandoned{invocation_id:id};self.active_transfers.remove(&t.instance_id);self.transfers.get_mut(tid).unwrap().state=TransferState::Abandoned}
     else if let Some(p)=t.parent_transfer{i.owner=t.source_owner;i.state=CapabilityState::InTransit{transfer_id:p};self.active_transfers.insert(t.instance_id,p);self.transfers.get_mut(tid).unwrap().state=TransferState::Returned}
     else{i.owner=t.source_owner;i.state=CapabilityState::Available;self.active_transfers.remove(&t.instance_id);self.transfers.get_mut(tid).unwrap().state=TransferState::Returned}
    },
    CapabilityState::Consumed{..}=>self.transfers.get_mut(tid).unwrap().state=TransferState::Consumed,
    CapabilityState::Abandoned{..}=>self.transfers.get_mut(tid).unwrap().state=TransferState::Abandoned,
    _=>unreachable!("preflight validated state")
   }
  }
  self.invocations.get_mut(&id).unwrap().state=if abnormal{InvocationState::Abandoned}else{InvocationState::Returned};Ok(())
 }
}

#[cfg(test)] mod tests{
 use super::*;fn s()->Span{Span::new(1,2)}
 fn setup()->(CapabilityLedger,CapabilityOwnerId,CapabilityOwnerId,CapabilityInstanceId,InvocationId,TransferId){
  let mut l=CapabilityLedger::new();let r=l.root_owner();let o=l.create_owner().unwrap();let i=l.provision(CapabilityId::ReadFile,r,s()).unwrap();let c=l.create_invocation(o,None).unwrap();let t=l.transfer_batch(r,o,c,&[i]).unwrap()[0];(l,r,o,i,c,t)
 }
 #[test]fn rt1_01_typed_ids_monotonic(){let(mut l,_,_,_,c,t)=setup();let o=l.create_owner().unwrap();let d=l.create_invocation(o,Some(c)).unwrap();assert_ne!(c.value(),d.value());assert_eq!(t.value(),1)}
 #[test]fn rt1_02_root_parent_none(){let(l,_,_,_,_,t)=setup();assert_eq!(l.transfer(t).unwrap().parent_transfer(),None)}
 #[test]fn rt1_03_nested_parent_link(){let(mut l,_,o,i,c,t)=setup();let d=l.create_owner().unwrap();let child=l.create_invocation(d,Some(c)).unwrap();let t2=l.transfer_batch(o,d,child,&[i]).unwrap()[0];assert_eq!(l.transfer(t2).unwrap().parent_transfer(),Some(t))}
 #[test]fn rt1_04_return_restores_parent(){let(mut l,r,o,i,c,t)=setup();let d=l.create_owner().unwrap();let child=l.create_invocation(d,Some(c)).unwrap();let t2=l.transfer_batch(o,d,child,&[i]).unwrap()[0];l.reconcile_return(child).unwrap();assert_eq!(l.instance(i).unwrap().state(),CapabilityState::InTransit{transfer_id:t});assert_eq!(l.transfer(t2).unwrap().state(),TransferState::Returned);l.reconcile_return(c).unwrap();assert_eq!(l.instance(i).unwrap().owner(),r)}
 #[test]fn rt1_05_child_consumption_terminal(){let(mut l,_,o,i,c,t)=setup();let d=l.create_owner().unwrap();let child=l.create_invocation(d,Some(c)).unwrap();let t2=l.transfer_batch(o,d,child,&[i]).unwrap()[0];l.consume(d,i,child,s()).unwrap();l.reconcile_return(child).unwrap();l.reconcile_return(c).unwrap();assert!(matches!(l.instance(i).unwrap().state(),CapabilityState::Consumed{..}));assert_eq!(l.transfer(t).unwrap().state(),TransferState::Consumed);assert_eq!(l.transfer(t2).unwrap().state(),TransferState::Consumed)}
 #[test]fn rt1_06_duplicate_atomic(){let mut l=CapabilityLedger::new();let r=l.root_owner();let o=l.create_owner().unwrap();let i=l.provision(CapabilityId::ReadFile,r,s()).unwrap();let c=l.create_invocation(o,None).unwrap();assert_eq!(l.transfer_batch(r,o,c,&[i,i]),Err(CapabilityError::DuplicateTransfer(i)));assert_eq!(l.instance(i).unwrap().state(),CapabilityState::Available)}
 #[test]fn rt1_07_wrong_owner_atomic(){let mut l=CapabilityLedger::new();let r=l.root_owner();let x=l.create_owner().unwrap();let o=l.create_owner().unwrap();let i=l.provision(CapabilityId::ReadFile,r,s()).unwrap();let c=l.create_invocation(o,None).unwrap();assert!(l.transfer_batch(x,o,c,&[i]).is_err());assert_eq!(l.instance(i).unwrap().owner(),r)}
 #[test]fn rt1_08_unknown_invocation(){let mut l=CapabilityLedger::new();let r=l.root_owner();let o=l.create_owner().unwrap();let i=l.provision(CapabilityId::ReadFile,r,s()).unwrap();assert!(matches!(l.transfer_batch(r,o,InvocationId(u64::MAX),&[i]),Err(CapabilityError::UnknownInvocation(_))))}
 #[test]fn rt1_09_closed_parent_rejected(){let(mut l,_,_,_,c,_)=setup();l.reconcile_return(c).unwrap();let o=l.create_owner().unwrap();assert_eq!(l.create_invocation(o,Some(c)),Err(CapabilityError::InvalidInvocationParent(c)))}
 #[test]fn rt1_10_bad_destination(){let mut l=CapabilityLedger::new();let r=l.root_owner();let o=l.create_owner().unwrap();let x=l.create_owner().unwrap();let i=l.provision(CapabilityId::ReadFile,r,s()).unwrap();let c=l.create_invocation(o,None).unwrap();assert_eq!(l.transfer_batch(r,x,c,&[i]),Err(CapabilityError::InvalidTransferDestination(x)));assert_eq!(l.instance(i).unwrap().state(),CapabilityState::Available)}
 #[test]fn rt1_11_parent_waits_for_child(){let(mut l,_,o,i,c,_)=setup();let d=l.create_owner().unwrap();let child=l.create_invocation(d,Some(c)).unwrap();l.transfer_batch(o,d,child,&[i]).unwrap();assert_eq!(l.reconcile_return(c),Err(CapabilityError::UnresolvedDescendant(i)))}
 #[test]fn rt1_12_wrong_source_rejected(){let(mut l,_,o,i,c,_)=setup();let d=l.create_owner().unwrap();let child=l.create_invocation(d,Some(c)).unwrap();assert!(l.transfer_batch(d,d,child,&[i]).is_err());assert_eq!(l.instance(i).unwrap().owner(),o)}
 #[test]fn rt1_13_consumption_irreversible(){let(mut l,_,o,i,c,_)=setup();l.consume(o,i,c,s()).unwrap();assert_eq!(l.consume(o,i,c,s()),Err(CapabilityError::AlreadyConsumed(i)))}
 #[test]fn rt1_14_abandon_unused(){let(mut l,_,_,i,c,t)=setup();l.mark_abandoned(c).unwrap();assert_eq!(l.instance(i).unwrap().state(),CapabilityState::Abandoned{invocation_id:c});assert_eq!(l.transfer(t).unwrap().state(),TransferState::Abandoned)}
 #[test]fn rt1_15_abandon_keeps_consumed(){let(mut l,_,o,i,c,t)=setup();l.consume(o,i,c,s()).unwrap();l.mark_abandoned(c).unwrap();assert!(matches!(l.instance(i).unwrap().state(),CapabilityState::Consumed{..}));assert_eq!(l.transfer(t).unwrap().state(),TransferState::Consumed)}
 #[test]fn rt1_16_double_reconcile_rejected(){let(mut l,_,_,_,c,_)=setup();l.reconcile_return(c).unwrap();assert_eq!(l.reconcile_return(c),Err(CapabilityError::InactiveInvocation(c)))}
 #[test]fn rt1_17_three_levels(){let(mut l,r,o,i,a,_)=setup();let m=l.create_owner().unwrap();let b=l.create_invocation(m,Some(a)).unwrap();l.transfer_batch(o,m,b,&[i]).unwrap();let n=l.create_owner().unwrap();let c=l.create_invocation(n,Some(b)).unwrap();l.transfer_batch(m,n,c,&[i]).unwrap();l.reconcile_return(c).unwrap();l.reconcile_return(b).unwrap();l.reconcile_return(a).unwrap();assert_eq!(l.instance(i).unwrap().owner(),r)}
 #[test]fn rt1_18_child_consumed_closes_ancestors(){let(mut l,_,o,i,a,t)=setup();let n=l.create_owner().unwrap();let b=l.create_invocation(n,Some(a)).unwrap();let t2=l.transfer_batch(o,n,b,&[i]).unwrap()[0];l.consume(n,i,b,s()).unwrap();l.reconcile_return(b).unwrap();l.reconcile_return(a).unwrap();assert_eq!(l.transfer(t).unwrap().state(),TransferState::Consumed);assert_eq!(l.transfer(t2).unwrap().state(),TransferState::Consumed)}
 #[test]fn rt1_19_abandoned_descendant_unwinds(){let(mut l,_,o,i,a,t)=setup();let n=l.create_owner().unwrap();let b=l.create_invocation(n,Some(a)).unwrap();let t2=l.transfer_batch(o,n,b,&[i]).unwrap()[0];l.mark_abandoned(b).unwrap();l.mark_abandoned(a).unwrap();assert_eq!(l.transfer(t).unwrap().state(),TransferState::Abandoned);assert_eq!(l.transfer(t2).unwrap().state(),TransferState::Abandoned)}
 #[test]fn rt1_20_empty_inventory(){let mut l=CapabilityLedger::new();let o=l.create_owner().unwrap();let c=l.create_invocation(o,None).unwrap();assert_eq!(l.reconcile_return(c),Ok(()))}
}
