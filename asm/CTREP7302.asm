*          DATA SET CTREP7302  AT LEVEL 023 AS OF 06/02/10                      
*PHASE CT7302A                                                                  
         TITLE 'CT7302 - EFFECTIVE REPORT PROFILES'                             
CT7302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CT7302                                                       
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         CLI   MODE,REQFRST                                                     
         BNE   EP2                                                              
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONTROL RECORDS                                                  
         SPACE 3                                                                
EP2      CLI   MODE,PROCUSER                                                    
         BNE   XIT                 NOT A USER PROFILE                           
*                                                                               
         L     R2,ADRECORD                                                      
         USING CTUREC,R2                                                        
         OC    CTUKAGY,CTUKAGY     IS THIS A FIELD DESCRIPTION REC.             
         BNZ   EP4                 NO                                           
*                                                                               
         LR    R4,R2                                                            
         XC    TYPARRAY,TYPARRAY                                                
         MVI   ELCODE,CTFDELQ      PROFILE FIELD DEFINITION ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   XIT                 NO FIELDS IN PROFILE                         
         USING CTFDD,R4                                                         
EP3      ZIC   R1,CTFDNUM          FIELD NUMBER                                 
         LA    R1,TYPARRAY-1(R1)   ARRAY INDEX IS 0-BASED                       
         MVC   0(1,R1),CTFDTYPE    SAVE FIELD TYPE                              
         BAS   RE,NEXTEL                                                        
         BE    EP3                                                              
         B     XIT                 NO MORE FIELDS IN PROFILE                    
         DROP  R4                                                               
*                                                                               
EP4      CLI   QPROFSYS,C' '       PROFILE SYSTEM FILTER                        
         BE    *+14                                                             
         CLC   CTUKSYS,QPROFSYS                                                 
         BNE   XIT                                                              
*                                                                               
         CLC   QPROFPRG,SPACES     PROFILE PROGRAM FILTER                       
         BE    EP5                                                              
         CLI   QPROFPRG,C' '       FIRST BYTE THERE?                            
         BH    *+18                YES                                          
         CLC   CTUKPROG+1(2),QPROFPRG+1                                         
         BNE   XIT                                                              
         B     EP5                                                              
*                                                                               
         CLC   CTUKPROG,QPROFPRG                                                
         BNE   XIT                                                              
*                                                                               
EP5      CLC   QAGENCY,SPACES      AGENCY FILTER                                
         BE    *+14                                                             
         CLC   CTUKAGY,QAGENCY                                                  
         BNE   XIT                                                              
*                                                                               
         CLI   QPROFSYS,C'A'                                                    
         BE    EP9                                                              
         CLC   QMEDIA,SPACES       MEDIA FILTER                                 
         BE    *+14                                                             
         CLC   CTUKMED,QMEDIA                                                   
         BNE   XIT                                                              
*                                                                               
         CLC   QCLIENT,SPACES      CLIENT FILTER                                
         BE    EP10                                                             
         CLC   CTUKCLT,QCLIENT                                                  
         BNE   XIT                                                              
         B     EP10                                                             
*                                                                               
EP9      CLC   QMEDIA(2),SPACES    UNIT/LEDGER FILTER                           
         BE    *+14                                                             
         CLC   QMEDIA(2),CTUKUNT                                                
         BNE   XIT                                                              
*                                                                               
         CLC   QCLIENT+1(3),SPACES ACCOUNT FILTER                               
         BE    EP10                                                             
         CLC   QCLIENT+1(3),CTUKACT                                             
         BNE   XIT                                                              
         EJECT                                                                  
*              HANDLE THE PROFILE ELEMENT                                       
         SPACE 3                                                                
EP10     LR    R4,R2                                                            
         MVI   ELCODE,CTPVELQ      PROFILE VALUE ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   XIT                 NO PROFILE VALUES                            
*                                                                               
         USING CTPVD,R4                                                         
         CLI   QID+2,C'='          REQUEST ID. CAN CONTAIN A FILTER             
         BNE   EP20                THIS IS IN FORMAT NN=A                       
         PACK  DUB,QID(2)          OR NN=NNN  OR NN=?                           
         CVB   R1,DUB                                                           
         LA    R1,CTPVALUE-1(R1)   DISPLACE TO SELECTED BYTE                    
         CLI   QID+3,C'?'          TEST FOR QUESTION MARK                       
         BNE   *+16                                                             
         CLI   0(R1),0             ANY VALUE IN FIELD IS OK                     
         BE    XIT                                                              
         B     EP20                                                             
*                                                                               
         CLI   QID+4,C' '          HANDLE NN=A                                  
         BNE   *+18                                                             
         CLC   QID+3(1),0(R1)      TEST FOR MATCH                               
         BE    EP20                                                             
         B     XIT                                                              
*                                                                               
         PACK  DUB,QID+3(3)        HANDLE NN=NNN                                
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         CLC   DUB(1),0(R1)                                                     
         BNE   XIT                                                              
         EJECT                                                                  
*              PROFILE PASSED SO PRINT OUT VALUES                               
         SPACE 3                                                                
EP20     MVC   PSYS,CTUKSYS        SYSTEM                                       
         MVC   PPROG,CTUKPROG      PROGRAM                                      
         MVC   PAGY,CTUKAGY        AGENCY                                       
         MVC   PMED,CTUKMED        MEDIA                                        
         MVC   PCLT,CTUKCLT        CLIENT                                       
         CLI   CTUKSYS,C'A'        SHOW COMPANY FOR ACCPAK                      
         BNE   EP20B                                                            
         MVC   PUNITLED,CTUKUNT    UNIT/LEDGER                                  
         MVC   PACCOUNT,CTUKACT    ACCOUNT                                      
*                                                                               
EP20B    CLC   CTUKAGY,SPACES      NUMERIC USERID IN KEY?                       
         BNL   EP21                NO                                           
         EDIT  CTUKAGY,PUSERID,ALIGN=LEFT                                       
         XC    IDKEY,IDKEY         LOOK FOR ID RECORD                           
         LA    R3,IDKEY                                                         
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ    ID RECORD                                    
         MVC   CTIKNUM,CTUKAGY     USER-ID                                      
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,IDKEY,IDREC,0                         
         CLI   DMCB+8,0                                                         
         BNE   EP21                ID RECORD NOT FOUND                          
         DROP  R3                                                               
         LA    R3,IDREC+28         POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
EP20C    CLI   0(R3),0             EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                DESCRIPTION ELEMENT NOT FOUND                
         CLI   0(R3),CTDSCELQ      DESCRIPTION ELEMENT?                         
         BE    *+14                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     EP20C                                                            
         MVC   PUSERID,CTDSC-CTDSCD(R3)  ALPHA USERID                           
*                                                                               
EP21     GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,ADRECORD,IDREC,0                      
         CLI   DMCB+8,0            RESTORE READ CTFILE READ SEQUENCE            
         BE    *+6                                                              
         DC    H'0'                HOW? CONMAN GAVE IT TO US ABOVE!             
*                                                                               
         LA    R4,CTPVALUE         PROFILE VALUES                               
         DROP  R4                                                               
         LA    R3,PVALS            PRINT AREA                                   
         LA    R5,1                PROFILE BYTE COUNTER                         
*                                                                               
EP22     LA    R1,TYPARRAY-1(R5)   CORRESPONDING FIELD TYPE                     
         MVC   BYTE,0(R1)                                                       
         CLI   BYTE,0              IS FIELD UNDEFINED?                          
         BNE   EP23                                                             
         CLI   0(R4),0             VALUE BYTE BETTER BE NULL                    
         BE    EP26                                                             
         GOTO1 HEXOUT,DMCB,0(R4),1(R3),1,=C'TOG'                                
         MVI   0(R3),C'!'          INDICATE OBSOLETE VALUE                      
         B     EP26                                                             
EP23     CLI   BYTE,C'C'           CHARACTER?                                   
         BNE   *+14                                                             
         MVC   0(1,R3),0(R4)       YES: JUST PRINT THE CHARACTER                
         B     EP26                                                             
         CLI   BYTE,C'N'           NUMERIC?                                     
         BNE   EP24                NO: MUST BE HEX                              
         EDIT  (1,0(R4)),(3,0(R3)),ALIGN=LEFT                                   
         B     EP26                                                             
EP24     GOTO1 HEXOUT,DMCB,0(R4),0(R3),1,=C'TOG'                                
*                                                                               
EP26     LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         AHI   R5,1                                                             
         CHI   R5,16                                                            
         BNH   EP22                                                             
*                                                                               
         LR    R4,R2                                                            
         MVI   ELCODE,CTACTELQ     ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTACTD,R4                                                        
         GOTO1 DATCON,DMCB,(3,CTACTDT),(8,PACTDATE)                             
         DROP  R4                                                               
*                                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              STORAGE, LTORG, CSECTS & DSECTS                                  
         SPACE 3                                                                
ELCODE   DS    XL1                                                              
TYPARRAY DS    CL16                INDEXED ARRAY OF FIELD TYPES                 
IDKEY    DS    XL25                ID RECORD KEY                                
IDREC    DS    1000X               ID RECORD INPUT BUFFER                       
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTREPWORKD                                                     
         PRINT ON                                                               
         ORG   P                                                                
         DS    CL11                                                             
PSYS     DS    C                   SYSTEM                                       
         DS    CL3                                                              
PPROG    DS    CL3                 PROGRAM                                      
         DS    CL2                                                              
PAGY     DS    CL2                 AGENCY                                       
         DS    CL8                                                              
         ORG   PAGY                                                             
PUSERID  DS    CL8                 USERID                                       
         DS    CL2                                                              
PMED     DS    C                   MEDIA                                        
         DS    CL3                                                              
PCLT     DS    CL3                 CLIENT                                       
         ORG   PMED                                                             
PUNITLED DS    CL2                 ACC: UNIT/LEDGER                             
         DS    CL2                                                              
PACCOUNT DS    CL3                 ACC: ACCOUNT                                 
         DS    CL2                                                              
PVALS    DS    16CL4               PROFILE VALUES                               
         DS    CL4                                                              
PACTDATE DS    CL8                 LAST ACTIVITY DATE                           
         ORG                                                                    
         SPACE 3                                                                
         PRINT OFF                                                              
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTREP7302 06/02/10'                                      
         END                                                                    
