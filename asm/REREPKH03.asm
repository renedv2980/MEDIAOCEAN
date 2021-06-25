*          DATA SET REREPKH03  AT LEVEL 075 AS OF 05/30/96                      
*PHASE REKH02C,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'RVANSCAN2 (REKH02C) - FIND MISSING ADV ON K REC'                
*                                                                               
********************************************************************            
*                                                                  *            
*        RVANSCAN2 -- FIND MISSING ADV REFERENCES                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* TEMP VERSION                                                     *            
*                                                                  *            
* SPEED UP SKIP READING                                            *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =  STATION DISPLAY                               *            
*     QUESTOR+1   =  OFFICE DISPLAY                                *            
*     QUESTOR+2   =  AGENCY DISPLAY                                *            
*     QUESTOR+3   =  ADVERTISER DISPLAY                            *            
*     QUESTOR+4   =  SALESPERSON DISPLAY                           *            
*     QUESTOR+5   =  PRODUCT DISPLAY                               *            
*     QUESTOR+6   =  CONTRACT DISPLAY                              *            
*     QUESTOR+7   =  BUY DISPLAY                                   *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKH02   CSECT                                                                  
         NMOD1 0,*RVSCAN*,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
*                                                                               
         LA    R2,REPTAB                                                        
*                                                                               
M100     CLI   0(R2),X'FF'              END REP TABLE                           
         BE    EXIT                                                             
*                                                                               
         XC    ADVCODE,ADVCODE                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONREC,R6                                                       
         MVI   RCONQTYP,X'9C'                                                   
         MVC   RCONQREP,0(R2)                                                   
         BAS   RE,HI                                                            
M150     EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        L     RF,KEYCTR                                                        
*        LA    RF,1(RF)                                                         
*        ST    RF,KEYCTR                                                        
*        CLC   KEYCTR,=F'100'                                                   
*        BH    EXIT                                                             
*        MVC   P+1(27),KEY                                                      
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(RCONQOFF-RCONKEY),KEYSAVE                                    
         BNE   M500                                                             
         CLC   RCONQADV,ADVCODE                                                 
         BE    M180                                                             
         MVC   ADVCODE,RCONQADV                                                 
         DROP  R6                                                               
         MVC   KEY2,KEY                                                         
         XC    KEY,KEY                                                          
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,ADVCODE                                                 
         MVC   RADVKREP,=C'MR'     SET MASTER REP CODE                          
         BAS   RE,HI                                                            
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   M200                                                             
         DROP  R6                                                               
M170     MVC   KEY,KEY2                                                         
         LA    RE,KEY                                                           
         ZIC   RF,RCONQADV+3-RCONQTYP(RE)                                       
         LA    RF,1(RF)                                                         
         STC   RF,RCONQADV+3-RCONQTYP(RE)                                       
*                                  BUMP LAST CHAR OF ADV CODE BY 1              
*                                     TO START ON NEXT ADV W/IN STA             
         BAS   RE,HI               READ HIGH FOR NEXT ADV                       
         B     M150                GO BACK AND PROCESS IT                       
M180     BAS   RE,SEQ                                                           
         B     M150                                                             
*                                                                               
M200     LA    R3,KEY2                                                          
         USING RCONREC,R3                                                       
         MVC   P+10(4),RCONQADV                                                 
         EDIT  RCONQCON,(8,P+20),ALIGN=LEFT                                     
         MVC   P+30(2),RCONQREP                                                 
         GOTO1 REPORT                                                           
         B     M170                                                             
*                                                                               
M500     LA    R2,2(R2)                                                         
         B     M100                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*                                                                               
REPTAB   DC    C'AM',C'CQ',C'NK',X'FF'                                          
*                                                                               
HI       NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
SEQ      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                                                               
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ADVCODE  DS    CL4                                                              
KEY2     DS    CL(L'KEY)                                                        
KEYCTR   DS    F                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075REREPKH03 05/30/96'                                      
         END                                                                    
