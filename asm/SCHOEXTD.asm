*          DATA SET SCHOEXTD   AT LEVEL 096 AS OF 05/01/02                      
*PHASE SCHOEXTA SCHOEXTD                                                        
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'PVLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
**********************************************************************          
* SCHOEXTD - DELETES SEPARETES THE CURRENT NTIFILE TO TWO PARTS:                
*    1: PRIOR TO YEAR 2000.                                                     
*    2: POST YEAR 2000.                                                         
*                                                                               
**********************************************************************          
* GENERAL EXTERN PARAMETER LIST                                                 
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
**********************************************************************          
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
*                                                                               
PVXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     PVXIT                                                            
*                                                                               
PVXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVC   WORK,SPACES         KEEP RECORD                                  
         MVC   WORK+1(23),0(R3)                                                 
         MVI   WORK,23                                                          
         BAS   RE,DUMPREC                                                       
         B     PVXIT                                                            
*                                                                               
PVINIT   LA    RE,COUNTS                                                        
         L     RF,=F'15000'                                                     
         XCEF                                                                   
         B     PVXIT                                                            
         SPACE 2                                                                
PVXIT    XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
PVXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         CLC   RECFLG,0(R3)        STILL THE SAME RECORD TYPE?                  
         BE    PVXR10                                                           
         MVC   RECFLG,0(R3)        SAVE NEW RECORD TYPE                         
         MVI   PRNTCNT,0                                                        
*                                                                               
PVXR10   CLI   0(R3),C'C'          NETWORK CORRECTION RECORDS                   
         BE    CREC                                                             
         CLI   0(R3),C'J'          J POINTERS                                   
         BE    JREC                                                             
         CLI   0(R3),C'K'          PROGRAM NAME RECORD KEY                      
         BE    KREC                                                             
         CLI   0(R3),C'N'          NETWORK DAY/TIME REOCORDS                    
         BE    NREC                                                             
         CLI   0(R3),C'P'          NETWORK PROGRAM AVERAGE RECORDS              
         BE    PREC                                                             
         CLI   0(R3),C'Q'          NETWORK PROGRAM RECORDS                      
         BE    QREC                                                             
         B     PVXKEEP                                                          
*                                                                               
         USING CHKEY,R3                                                         
CREC     OC    CHOBOOK,CHOBOOK                                                  
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   CHOBOOK,=X'6400'                                                 
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
         USING PJKEY,R3                                                         
JREC     OC    PJBOOK,PJBOOK                                                    
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   PJBOOK,=X'6400'                                                  
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
         USING PKKEY,R3                                                         
KREC     OC    PKBOOK,PKBOOK                                                    
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   PKBOOK,=X'6400'                                                  
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
         USING PNKEY,R3                                                         
NREC     OC    PNBOOK,PNBOOK                                                    
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   PNBOOK,=X'6400'                                                  
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
         USING PRKEY,R3                                                         
PREC     OC    PRBOOK,PRBOOK                                                    
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   PRBOOK,=X'6400'                                                  
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
         USING PMKEY,R3                                                         
QREC     OC    PMBOOK,PMBOOK                                                    
         BZ    PVXKEEP             DO NOT DELETE REC'S W/O BOOK                 
         CLC   PMBOOK,=X'6400'                                                  
         BNL   PVXKEEP                                                          
         B     RECX                                                             
*                                                                               
RECX     CLI   PRNTCNT,10                                                       
         BE    PVXPURGE                                                         
         ZIC   RE,PRNTCNT                                                       
         AHI   RE,1                                                             
         STC   RE,PRNTCNT                                                       
*                                                                               
         B     PVXPURGE                                                         
**********************************************************************          
PVXEOF   MVC   P(21),=CL21'***RECORDS SAVED***'                                 
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
*        GOTO1 VPRINTER                                                         
         LA    R2,9(R2)                                                         
         B     PVXEOF1                                                          
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         CLI   PRNTCNT,10                                                       
         BE    DUMPX                                                            
         GOTO1 VPRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D'              
DUMPX    XIT1                                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
LOGIO    DC    V(LOGIO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
KEEPS    DC    F'0'                                                             
SVKEY    DS    CL18                                                             
RECFLG   DS    CL1                                                              
PRNTCNT  DS    XL1                                                              
HEAD     DC    C'**RECORD**'                                                    
COUNTS   DS    15000C                                                           
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
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
**PAN#1  DC    CL21'096SCHOEXTD  05/01/02'                                      
         END                                                                    
