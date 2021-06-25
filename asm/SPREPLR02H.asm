*          DATA SET SPREPLR02H AT LEVEL 017 AS OF 05/01/02                      
*PHASE SPLR02H                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
SPLR02H  TITLE 'SPREPLR02H : CREATE PROGRAM INDEX POINTERS'                     
***********************************************************************         
*================================ MAIN ===============================*         
SPLR27   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPLR27,RR=R5,CLEAR=YES                               
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
                                                                                
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         EJECT                                                                  
*                                                                               
*---------------------------- FILE READS ----------------------------*          
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
** READ DIRECTORY RECORDS **                                                    
*                                                                               
         XC    SVSTAT,SVSTAT                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   SPLKEY(3),=C'RTN'   DEMO RECDS/TV/NIELSEN                        
*                                                                               
SPLD10   MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,READHI,DEMDIR,SVSPLKEY,SPLKEY                       
         B     SPLD10B                                                          
SPLD10A  GOTO1 DATAMGR,DMCB,READSQ,DEMDIR,SVSPLKEY,SPLKEY                       
SPLD10B  CLI   8(R1),0                                                          
         BNE   SPLPUT                                                           
         CLC   SVSPLKEY(3),SPLKEY  DO 'RTN'S ONLY                               
         BNE   SPLPUT                                                           
                                                                                
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         TM    DRSTAT,X'F0'        ALL DONE IF NUMERIC STATION                  
         BO    SPLPUT               ENCOUNTERED                                 
         OC    DRKMKT,DRKMKT       IGNORE SPILL MARKETS                         
         BNZ   SPLD10A                                                          
         CLI   DRBTYP,X'E0'        IGNORE SPECIAL SPILL                         
         BE    SPLD10A                                                          
*                                                                               
** FILTER LOGIC **                                                              
*                                                                               
         CLC   DRBOOK,TESTBOOK     PROCESS TESTBOOK ONLY                        
         BL    BOOKHIGH                                                         
         BH    BOOKLOW                                                          
         BE    SPLD20               IGNORE OTHERS FOR TESTING                   
                                                                                
BOOKHIGH MVC   DRBOOK,TESTBOOK                                                  
         XC    DRKMKT(8),DRKMKT                                                 
         B     SPLD10                                                           
                                                                                
BOOKLOW  ZIC   R1,DRSTAT+L'DRSTAT-1   PRE-OCT/92 BOOK REACHED,                  
         LA    R1,1(R1)                FORCE A READ OF NEXT STATION             
         STC   R1,DRSTAT+L'DRSTAT-1                                             
         MVC   DRBOOK,TESTBOOK                                                  
         XC    DRKMKT(8),DRKMKT        AND CLEAR OUT LOWER KEY FIELDS           
         B     SPLD10                                                           
*                                                                               
** PROCESS DIRECTORY RECORD **                                                  
*                                                                               
SPLD20   MVC   NDXDA,DRNDXDA                                                    
         MVC   IO(L'DRKMAJOR+2),DRKEY                                           
         DROP  R6                                                               
                                                                                
         BAS   RE,STTNCHG          SEE IF STATION CHANGED                       
                                                                                
         LA    R1,DIRTTL                                                        
         BAS   RE,COUNTER                                                       
                                                                                
         GOTO1 DATAMGR,DMCB,READHI,DEMFIL,NDXDA,IO                              
         B     SPLF10A                                                          
SPLF10   GOTO1 DATAMGR,DMCB,READSQ,DEMFIL,NDXDA,IO                              
SPLF10A  CLI   8(R1),0                                                          
         BNE   SPLF30                                                           
         LA    R1,FILTTL                                                        
         BAS   RE,COUNTER                                                       
         LA    R1,COUNT                                                         
         BAS   RE,COUNTER                                                       
                                                                                
         LA    R6,IO                                                            
         USING DRKEY,R6                                                         
         LA    R1,DRFRSTEL-DRKEY                                                
         STH   R1,DATADISP                                                      
         LA    R3,0(R1,R6)         R3-->1ST ELEMENT                             
         MVI   ELCODE,QHCODEQ                                                   
*                                                                               
SPLF20   BAS   RE,NEXTEL                                                        
         BNE   SPLF10                                                           
         LR    R4,R3               R4-->QUARTER HOUR ELEMENT                    
         USING QHELEM,R4                                                        
         ZIC   R1,QHELN                                                         
         LA    R5,0(R1,R4)                                                      
         CLI   0(R5),QICODEQ       WE WANT R5 TO POINT TO THE                   
         BNE   SPLF20               PROGRAM INFO ELEMENT                        
         USING QIELEM,R5                                                        
                                                                                
         CLI   QHELN,4             ADJUST ELEMENT POINTER                       
         BNE   SPLF20A              IF ELEMENT POINT BACKWARDS                  
         ZICM  R1,QHELEM+2,(3)                                                  
         SLL   R1,17               GET RID OF HIGH-ORDER-BIT                    
         SRL   R1,17                IN HALFWORD DISPLACEMENT                    
         LA    R4,DRKEY(R1)                                                     
*                                                                               
SPLF20A  CLI   QIELN,4             ADJUST ELEMENT POINTER                       
         BNE   SPLF20B              IF ELEMENT POINT BACKWARDS                  
         ZICM  R1,QIELEM+2,(3)                                                  
         SLL   R1,17               GET RID OF HIGH-ORDER-BIT                    
         SRL   R1,17                IN HALFWORD DISPLACEMENT                    
         LA    R5,DRKEY(R1)                                                     
*                                                                               
SPLF20B  OC    QIPNUM,QIPNUM       WANT PROG NUM > 0                            
         BZ    SPLF20                                                           
         CLI   QIELN,QIELNEQ1      DOES THIS HAVE PROGRAMMING SOURCE?           
         BL    SPLF22               NOPE, JUST MOVE ON                          
         CLI   QIPRSRC,C'P'         YES, IGNORE PBS STATION                     
         BE    SPLF20                                                           
*                                                                               
SPLF22   DS    0H                  THIS IS FOR DUMP/LOAD BUG FIX                
         BC    0,SPLF22X           SKIP IF DUMP/LOAD BUG RESOLVED               
         CLC   QIPNUM,=X'00FFFF'   DUMP/LOAD INADVERTENTLY DELETES              
         BNH   SPLF20               PROGRAM #S GREATER THAN THIS                
SPLF22X  EQU   *                                                                
*                                                                               
         B     SPLF25                                                           
         EJECT                                                                  
*                                                                               
** PASSED FILTER--RELEASE RECORD TO SORT **                                     
*                                                                               
SPLF25   DS    0H                                                               
         XC    TAPEBUFF,TAPEBUFF   CREATE THE POINTER HERE                      
         LA    R2,TAPEBUFF+4                                                    
         USING PIKEY,R2                                                         
         MVI   PIKCODE,PICODEQU                                                 
         MVC   PIMEDIA,DRMEDIA                                                  
         MVC   PISRC,DRSRC                                                      
         MVC   PIBOOK,DRBOOK                                                    
         MVC   PIPNUM,QIPNUM                                                    
         MVC   PISTA,DRSTAT                                                     
         MVC   PIKMKT,DRKMKT                                                    
         MVC   PIDAY,QHDAY                                                      
         MVC   PISQH,QHSQH                                                      
         MVC   PIEQH,QHEQH                                                      
         MVC   PIKEY+20(2),=X'FFFF'                                             
         DROP  R2,R4,R5,R6                                                      
                                                                                
         MVC   TAPEBUFF(2),=Y(L'PIKMAJOR+5+4)                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',TAPEBUFF                                 
         LA    R1,SINTTL                                                        
         BAS   RE,COUNTER                                                       
         B     SPLF20                                                           
*                                                                               
** EOF ON DEMFIL FOR MAJOR KEY **                                               
*                                                                               
SPLF30   MVC   SVSPLKEY,SPLKEY                                                  
         MVC   PREVKEY,SPLKEY                                                   
         B     SPLD10A                                                          
         EJECT                                                                  
*                                                                               
*---------------------------- TAPE OUTPUT ----------------------------*         
*                                                                               
                                                                                
SPLPUT   DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,PREVKEY,P,23,=C'TOG',0                           
         MVI   P+46,C':'                                                        
         L     RF,COUNT                                                         
         EDIT  (RF),(6,P+48),ZERO=NOBLANK                                       
         GOTO1 REPORT                                                           
                                                                                
         OPEN  (OUT,OUTPUT)                                                     
         XC    TAPEBUFF,TAPEBUFF                                                
         XC    PREVBUFF,PREVBUFF                                                
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    SPLPUTX                                                          
         ZICM  RF,0(R2),(3)                                                     
         BCTR  RF,0                                                             
         EXMVC RF,PREVBUFF,0(R2)                                                
         MVC   TAPEBUFF,PREVBUFF                                                
*                                                                               
SPLP10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    SPLP30                                                           
                                                                                
         ZICM  RF,0(R2),(3)                                                     
         BCTR  RF,0                                                             
         EXCLC RF,PREVBUFF,0(R2)                                                
         BE    SPLP10                                                           
         EXMVC RF,PREVBUFF,0(R2)                                                
                                                                                
         CLC   TAPEBUFF(MAJKEYL+4),0(R2)  IF MAJOR KEY CHANGED,                 
         BE    SPLP20                                                           
                                                                                
         PUT   OUT,TAPEBUFF                PUT RECORD TO TAPE                   
         LA    R1,WRITTL                                                        
         BAS   RE,COUNTER                                                       
         MVC   TAPEBUFF,PREVBUFF          MOVE IN NEXT MAJOR KEY                
         B     SPLP10                                                           
*                                                                               
SPLP20   DS    0H                         SAME MAJOR KEY,                       
         CLC   TAPEBUFF+SQHDISP(1),EQHDISP(R2)                                  
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   TAPEBUFF+EQHDISP(1),EQHDISP(R2)    UPDATE END QH                 
         B     SPLP10                                                           
*                                                                               
SPLP30   PUT   OUT,TAPEBUFF               PUT LAST RECORD TO TAPE               
         LA    R1,WRITTL                                                        
         BAS   RE,COUNTER                                                       
*                                                                               
SPLPUTX  CLOSE (OUT)                                                            
         EJECT                                                                  
*                                                                               
*-------------------------- PRE-EXIT TASKS ---------------------------*         
*                                                                               
MAINX    DS    0H                                                               
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'END',0,0                                      
         GOTO1 REPORT              PRINT BLANK LINE                             
                                                                                
         LA    R2,RPTTBLE                                                       
MAINX10  LA    R0,WRITTL                                                        
         CR    R0,R2                                                            
         BL    EXIT                                                             
         LA    R3,P                                                             
         MVC   0(L'RPTSTMT,R3),L'RPTTTL(R2)                                     
         LA    R3,L'RPTSTMT+1(R3)                                               
         MVI   0(R3),C'='                                                       
         LA    R3,2(R3)                                                         
         L     R1,0(R2)                                                         
         EDIT  (R1),(10,(R3)),COMMAS=YES,ZERO=NOBLANK                           
         GOTO1 REPORT                                                           
         LA    R2,L'RPTTBLE(R2)                                                 
         B     MAINX10                                                          
                                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*-------------------------- STATION CHANGE ---------------------------*         
*                                                                               
STTNCHG  NTR1                                                                   
         CLC   SVSTAT,SPLKEY+3                                                  
         BE    STTNCHGX                                                         
         OC    SVSTAT,SVSTAT                                                    
         BZ    STTNCHG2                                                         
                                                                                
         GOTO1 =V(HEXOUT),DMCB,PREVKEY,P,23,=C'TOG',0                           
         MVI   P+46,C':'                                                        
         L     RF,COUNT                                                         
         EDIT  (RF),(6,P+48),ZERO=NOBLANK                                       
         GOTO1 REPORT                                                           
*                                                                               
STTNCHG2 GOTO1 =V(HEXOUT),DMCB,SPLKEY,P,23,=C'TOG',0                            
         GOTO1 REPORT                                                           
                                                                                
         MVC   SVSTAT,SPLKEY+3                                                  
         XC    COUNT,COUNT                                                      
STTNCHGX XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== GETEL ===============================*         
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 3                                                                
*============================== COUNTER ==============================*         
COUNTER  DS    0H                                                               
         L     RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(R1)                                                         
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,MACRF=PM,                  +        
               LRECL=L'TAPEBUFF,BLKSIZE=8200                                    
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,23,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=27'                                    
TESTBOOK DC    AL2(XJUL94)                                                      
         SPACE 2                                                                
READHI   DC    CL7'DMRDHI'                                                      
READSQ   DC    CL7'DMRSEQ'                                                      
DEMDIR   DC    CL7'DEMDIR'                                                      
DEMFIL   DC    CL7'DEMFIL'                                                      
CMKT     DC    4C'0'                                                            
SPLRRA   DC    F'0'                                                             
SPLRRB   DC    F'0'                                                             
SPLRRC   DC    F'0'                                                             
         SPACE 2                                                                
RPTTBLE  DS    0CL(L'RPTTTL+L'RPTSTMT)                                          
RPTTTL   DS    0F                                                               
RPTSTMT  DS    0CL40                                                            
DIRTTL   DC    F'0',CL40'TOTAL DIRECTORY READS'                                 
FILTTL   DC    F'0',CL40'TOTAL FILE READS'                                      
SINTTL   DC    F'0',CL40'TOTAL RECORDS INTO SORT'                               
WRITTL   DC    F'0',CL40'TOTAL RECORDS WRITTEN'                                 
FILSBTTL DC    F'0',CL40'TOTAL RECORDS FOR THIS MAJOR KEY'                      
         SPACE 2                                                                
MAJKEYL  EQU   PIDAY-PIKEY+1                                                    
SQHDISP  EQU   PISQH-PIKEY+4                                                    
EQHDISP  EQU   PIEQH-PIKEY+4                                                    
XMAR94   EQU   X'A1FC'             INVERTED MAR94                               
XMAY94   EQU   X'A1FA'             INVERTED MAY94                               
XJUL94   EQU   X'FFFF'-X'5E07'     INVERTED JUL94                               
XOCT94   EQU   X'A1F5'             INVERTED OCT94                               
XNOV94   EQU   X'FFFF'-X'5E0B'     INVERTED NOV94                               
XJAN95   EQU   X'FFFF'-X'5F01'     INVERTED JAN95                               
XFEB95   EQU   X'FFFF'-X'5F02'     INVERTED FEB95                               
XMAR95   EQU   X'FFFF'-X'5F03'     INVERTED MAR95                               
XMAY95   EQU   X'FFFF'-X'5F05'     INVERTED MAY95                               
XJUL95   EQU   X'FFFF'-X'5F07'     INVERTED JUL95                               
XOCT95   EQU   X'FFFF'-X'5F0A'     INVERTED OCT95                               
XNOV95   EQU   X'FFFF'-X'5F0B'     INVERTED NOV95                               
XMAY96   EQU   X'FFFF'-X'6005'     INVERTED MAY96                               
***********************************************************************         
         SPACE 2                                                                
         DROP  R8,RA,RB,RC                                                      
         EJECT                                                                  
***********************************************************************         
*======================= LOCAL WORKING STORAGE =======================*         
WORKD    DSECT                                                                  
NDXDA    DS    F                                                                
RELO     DS    F                                                                
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
CSTAT    DS    CL5                 CHAR STATION                                 
SVSTAT   DS    CL5                 SAVE CHAR STATION                            
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL(L'SPLKEY)                                                     
PREVKEY  DS    CL(L'SPLKEY)                                                     
TAPEBUFF DS    CL80                                                             
PREVBUFF DS    CL(L'TAPEBUFF)                                                   
IO       DS    2000X                                                            
WORKX    EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= PRINTLINE =============================*         
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ DEMO DSECTS ============================*         
*                                                                               
*------------------------------- DBLOCK ------------------------------*         
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*----------------------------- DEDEMFILE -----------------------------*         
       ++INCLUDE DEDEMFILE                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= DDCOMFACS =============================*         
       ++INCLUDE DDCOMFACS                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ SPOT DSECTS ============================*         
*                                                                               
*----------------------------- SPREPWORK -----------------------------*         
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
*                                                                               
*----------------------------- SPREPMODE -----------------------------*         
       ++INCLUDE SPREPMODES                                                     
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPLR02H05/01/02'                                      
         END                                                                    
