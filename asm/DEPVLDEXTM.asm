*          DATA SET DEPVLDEXTM AT LEVEL 021 AS OF 08/23/00                      
*PHASE DEPVE91A PVLDEX91                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
* -------------------------------------------------------------------           
*                                                                               
*    PULL OFF JUL84 PAV RECDS FROM PURDIR/PURFIL BACKUP TAPES                   
*                                                                               
*                                                                               
* -------------------------------------------------------------------           
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
PVLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*PVLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
PVXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    PVINIT              INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PVXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    PVXEOF              END-OF-FILE                                  
         B     PVXIT                                                            
         SPACE 2                                                                
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
         SPACE 2                                                                
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     PVXIT                                                            
         SPACE 2                                                                
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PRKEY,R3                                                         
         CLC   =C'PTN',0(R3)       SAVE PRG RECDS FOR NSI TV                    
         BNE   PVXPURGE                                                         
         CLC   PRBOOK(2),=X'5B05'  KEEP MAY91 - NOV91                           
         BL    PVXPURGE                                                         
         CLC   PRBOOK(2),=X'5B0B'                                               
         BH    PVXPURGE                                                         
         B     PVXKEEP                                                          
*                                                                               
         BE    PVXPURGE                                                         
*        CLC   PRBOOK(1),=X'5A05'  KEEP MAY90 - MAR91                           
*        BL    PVXPURGE                                                         
*        CLC   PRBOOK(1),=X'5B03'                                               
*        BH    PVXPURGE                                                         
*        CLC   PRBOOK(1),=X'5A07'  EXCEPT DON'T TAKE: JUL90                     
*        BE    PVXPURGE                                                         
*        CLC   PRBOOK(1),=X'5A0A'  OR OCT90                                     
*        BE    PVXPURGE                                                         
*                                                                               
         B     PVXKEEP                                                          
*                                                                               
**********************************************************************          
*DELETE RECORD                                                                  
**********************************************************************          
PVXDELET DS    0H                  DELETE KEY                                   
*                                                                               
PVXREC51 LA    RE,COUNTS                                                        
PVXREC5A OC    0(5,RE),0(RE)       OPEN SLOT                                    
         BZ    PVXREC5B                                                         
         CLC   DUB(5),0(RE)        FIND THE SLOT                                
         BNE   *+14                                                             
         CLC   STATN,9(RE)                                                      
         BE    PVXREC5C                                                         
         LA    RE,14(RE)                                                        
         B     PVXREC5A                                                         
PVXREC5B MVC   0(5,RE),DUB         SET KEY IN TABLE                             
         MVC   9(5,RE),STATN                                                    
PVXREC5C SR    RF,RF               ADD UP RECORDS                               
         ICM   RF,15,5(RE)                                                      
         A     RF,=F'1'                                                         
         STCM  RF,15,5(RE)                                                      
         B     PVXPURGE                                                         
         DROP  R3                                                               
         EJECT                                                                  
* END-OF-FILE PROCESSING                                                        
*                                                                               
PVXEOF   MVC   P(21),=C'***RECORDS DELETED***'                                  
         GOTO1 VPRINTER                                                         
         LA    R2,COUNTS                                                        
PVXEOF1  OC    0(5,R2),0(R2)                                                    
         BZ    PVXIT                                                            
         SR    R9,R9                                                            
         ICM   R9,15,5(R2)                                                      
         MVC   P(3),0(R2)                                                       
         EDIT  (R9),(8,P+10)                                                    
         ZIC   R9,3(R2)                                                         
         EDIT  (R9),(2,P+4)                                                     
         ZIC   R9,4(R2)                                                         
         EDIT  (R9),(2,P+6)                                                     
         MVC   P+20(5),9(R2)                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,14(R2)                                                        
         B     PVXEOF1                                                          
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
KEEPS    DC    F'0'                                                             
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
STATN    DS    CL5                                                              
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DEPVLDEXTM08/23/00'                                      
         END                                                                    
