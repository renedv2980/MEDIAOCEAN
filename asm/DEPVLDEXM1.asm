*          DATA SET DEPVLDEXM1 AT LEVEL 024 AS OF 08/23/00                      
*PHASE DEPVLE6A PVLDEXT6                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* -------------------------------------------------------------------           
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
* -------------------------------------------------------------------           
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
*THIS EXTERN COPIES ALL HUTTN RECDS FROM SEP/90 FOWARD                          
*******************************************************************             
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING PMKEY,R3                                                         
         CLC   0(3,R3),=C'QNN'     OUTPUT QNN-HUTTN RECS TO TAPE                
         BNE   PVXPURGE                                                         
*********                                                                       
*        CLC   PMSTAT,=C'HUTTN'                                                 
*        BE    PVXKEEP                                                          
*        CLC   PMSTAT,=C'CBSTN'                                                 
*        BE    PVXKEEP                                                          
*        B     PVXPURGE                                                         
*********                                                                       
*                                                                               
         CLC   PMSTAT,=C'HUTTN'                                                 
         BNE   PVXPURGE                                                         
         LA    R4,PRGTBL           PT TO TABLE                                  
PVXR10   CLI   0(R4),X'FF'                                                      
         BE    PVXKEEP             EOT -- JUST SAVE RECD                        
         CLC   PMPNUM,0(R4)                                                     
         BE    PVXR20                                                           
         LA    R4,L'PRGTBL(R4)                                                  
         B     PVXR10                                                           
*                                                                               
PVXR20   LR    R2,R3               R2 -> RECD                                   
         SR    R1,R1                                                            
         LA    R2,PMDATA           PT TO FIRST ELEM                             
PVXR25   CLI   0(R2),0                                                          
         BE    PVXKEEP             EOF -- JUST KEEP IT                          
         CLI   0(R2),X'10'         LOCATE THE '10' ELEM                         
         BE    PVXR30                                                           
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     PVXR25              NEXT ELEM                                    
*                                                                               
         USING PHTELEM,R2                                                       
PVXR30   DS    0H                                                               
         MVC   PHTDTYP,3(R4)       MOVE IN CORRECTED VALUE                      
         B     PVXKEEP                                                          
*                                                                               
*--------------------------------------------------------------------           
PRNTIT   NTR1                      PRINT OUT FIELDS CHGD                        
         EDIT  PMPNUM,(6,P)        PROGRAM #                                    
         EDIT  (1,PMBOOK),(3,P+6)  BOOK                                         
         EDIT  (1,PMBOOK+1),(4,P+8)  BOOK                                       
         EDIT  (1,PHTDTYP),(2,P+12)  OLD BIT SETTING                            
         EDIT  (1,3(R4)),(1,P+14)  NEW BIT SETTING                              
         CLC   PHTDTYP,2(R4)       COMPARE TO EXPECTED VALUE                    
         BE    *+10                                                             
         MVC   P+20(9),=C'MIS-MATCH'                                            
         GOTO1 VPRINTER                                                         
PRINTITX XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
PRGTBL   DS    0CL4                                                             
         DC    AL2(9831),X'05',X'01'        SPRT ALL REG 24:TOT->REG            
         DC    AL2(9832),X'05',X'04'        SPRT ALL SPC 24:TOT->SPC            
         DC    AL2(9841),X'05',X'01'        NEWS TOTAL REG:TOT->REG             
         DC    X'FFFF'                                                          
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
**PAN#1  DC    CL21'024DEPVLDEXM108/23/00'                                      
         END                                                                    
