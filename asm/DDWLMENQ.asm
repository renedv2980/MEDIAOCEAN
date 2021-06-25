*          DATA SET DDWLMENQ   AT LEVEL 001 AS OF 04/29/10                      
*PHASE WLMENQA                                                                  
         TITLE 'WLMENQ - Leave An Enqueue Around'                               
*********************************************************************           
* This program issues an enqueue for DDS WLM Services.              *           
* The presence of the enqueue means that WLM services can be        *           
* requested by other subsystems.                                    *           
* If this Job has been cancelled there will be no ENQUEUE,          *           
* and as such we have informed FAWLMCE not to create any more       *           
* new Enclaves.                                                     *           
*                                                                   *           
*********************************************************************           
WLMENQ   AMODE  31             SET 31 BIT ADDRESSING                            
WLMENQ   RMODE  ANY            SET ANY RESIDENCY                                
WLMENQ   CSECT                                                                  
         NBASE 0,*WLMBOOT,WORK=WORK                                             
*                                                                               
         WTO   'Establishing WLM Enclave Enqueue'                               
         LA    R2,SYSMAJ                                                        
         LA    R3,SYSMIN                                                        
         ENQ   ((2),(3),E,8,SYSTEMS),LINKAGE=SVC,RET=NONE                       
*                                                                               
         WTO   'WLM Enclave Enqueue Established'                                
*                                                                               
         ENQ   ((2),(3),E,8,SYSTEMS),LINKAGE=SVC,RET=TEST                       
*                                                                               
         LA    R1,WAITECB                                                       
         WAIT  ECB=(R1)                                                         
*                                                                               
         XR   RF,RF                                                             
         XBASE                                                                  
         LTORG ,              Generate Global Litteral Pool                     
*                                                                               
         DS    0D                                                               
WAITECB  DC    A(0)                                                             
         DS    0D                                                               
SYSMAJ   DC    CL8'ENQDEQ  '                                                    
SYSMIN   DC    CL8'WLMCALVE'                                                    
         DS    0D                                                               
         DC    CL8'WLMWORK*'                                                    
WORK     DC    10D'0'                                                           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDWLMENQ  04/29/10'                                      
         END                                                                    
