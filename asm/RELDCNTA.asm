*          DATA SET RELDCNTA   AT LEVEL 029 AS OF 05/01/02                      
*PHASE RELDCNTA,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
         TITLE 'RELDCNT - LOAD/DUMP EXTERN TO COUNT RECORDS'                    
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
DMLDCNT  CSECT                                                                  
         ENTRY ACCUMC                                                           
         NMOD1 0,DMLDCNT,R9,RR=RE                                               
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         ST    RE,RELO                                                          
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         SPACE 1                                                                
         L     R0,=V(ACCUMC)                                                    
         A     R0,RELO                                                          
         ST    R0,ACCUMS                                                        
         SPACE 1                                                                
         L     RE,ACCUMS                                                        
         L     RF,CNTLNGTH         CLEAR FIRST TABLE ENTRY                      
         XCEF                                                                   
         L     RE,ACCUMS                                                        
         MVI   0(RE),X'FF'                                                      
         SPACE 1                                                                
         LA    R3,5                                                             
         LA    RE,PRNTBL                                                        
DMXINT3  L     RF,0(RE)            RESOLVE EXTERNS                              
         A     RF,RELO                                                          
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R3,DMXINT3                                                       
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         MVC   WORK+2(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,BYR3)                                    
         MVC   BYR2,BYR3                                                        
         ZIC   R3,BYR3                                                          
         BCTR  R3,0                                                             
         STC   R3,BYR2                                                          
         MVC   BYR1,BYR2                                                        
         BCTR  R3,0                                                             
         STC   R3,BYR1                                                          
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,BYR1),(2,CYR1)                                    
         GOTO1 DATCON,DMCB,(3,BYR2),(2,CYR2)                                    
         GOTO1 DATCON,DMCB,(3,BYR3),(2,CYR3)                                    
         B     DMXIT                                                            
         EJECT                                                                  
*              PROCESS RECORD LOGIC                                             
         SPACE 1                                                                
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         LA    R6,RECLIST                                                       
         SPACE 1                                                                
         USING RLISTD,R6                                                        
DMREC1   CLC   RTYPE,0(R3)         MATCH RECORD TYPE TO RECORD LIST             
         BE    DMREC3                                                           
         A     R6,RLNGTH                                                        
         CLI   RTYPE,X'FF'                                                      
         BE    DMXKEEP             NO MATCH                                     
         B     DMREC1                                                           
         SPACE 1                                                                
DMREC3   ZIC   R7,RCODE            DISPLACEMENT TO REP CODE                     
         AR    R7,R3                                                            
         L     R8,ACCUMS                                                        
         SPACE 1                                                                
         USING CNTSD,R8                                                         
DMREC4   CLI   CCODE,X'FF'                                                      
         BE    DMREC6              NEW REP CODE ADD IT                          
         CLC   CCODE,0(R7)                                                      
         BE    DMREC7              FOUND A MATCH                                
         A     R8,CNTLNGTH                                                      
         B     DMREC4                                                           
         SPACE 1                                                                
DMREC6   LR    RE,R8                                                            
         L     RF,CNTLNGTH         CLEAR TABLE AREA                             
         XCEF                                                                   
         MVC   CCODE,0(R7)         NEW REP CODE                                 
         LR    RF,R8                                                            
         A     RF,CNTLNGTH         NEW END OF TABLE                             
         MVI   0(RF),X'FF'                                                      
         SPACE 1                                                                
DMREC7   L     RF,ROUTINE          ADDRESS OF ROUTINE TO PROCESS RECORD         
         A     RF,RELO                                                          
         BR    RF                                                               
         EJECT                                                                  
         USING CNTSD,R8                                                         
         USING RECD,R3                                                          
REP      MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         SPACE 1                                                                
         USING RREPELEM,R3                                                      
         MVC   CNAME,RREPSHRT      REP NAME TO TABLE                            
         B     DMXKEEP                                                          
         SPACE 1                                                                
STATION  LA    RE,CSTAT                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
REGION   LA    RE,CREGN                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
OFFICE   LA    RE,COFFC                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
TEAM     LA    RE,CTEAM                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
SALMN    LA    RE,CMAN                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
GROUP    LA    RE,CGRP                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
ADVER    LA    RE,CADV                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
PROD     LA    RE,CPRD                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
AGENCY   LA    RE,CAGY                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
CLASS    LA    RE,CCLASS                                                        
         B     ADDONE                                                           
         SPACE 1                                                                
CATEGORY LA    RE,CCATG                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
BUDGET   LA    RE,CBUDG                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
EOM      LA    RE,CEOM                                                          
         B     ADDONE                                                           
         SPACE 1                                                                
OBUD     LA    RE,COBUD                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
AGY2     LA    RE,CAGY2                                                         
         B     ADDONE                                                           
         SPACE 1                                                                
EOPADV   LA    RE,CEOPADV                                                       
         B     ADDONE                                                           
         SPACE 1                                                                
EOPAGY   LA    RE,CEOPAGY                                                       
         B     ADDONE                                                           
         SPACE 1                                                                
EOPOFF   LA    RE,CEOPOFF                                                       
         B     ADDONE                                                           
         SPACE 1                                                                
EOPSAL   LA    RE,CEOPSAL                                                       
         B     ADDONE                                                           
         SPACE 1                                                                
OVER     LA    RE,COVER            OVERNIGHT UPLOAD                             
         B     ADDONE                                                           
         SPACE 1                                                                
DMENU    LA    RE,CDMENU           DEMO MENUE                                   
         B     ADDONE                                                           
         SPACE 1                                                                
DPT      LA    RE,CDPT             DAYPART                                      
         B     ADDONE                                                           
         SPACE 1                                                                
PGT      LA    RE,CPGT             PROGRAM TYPE                                 
         B     ADDONE                                                           
         SPACE 1                                                                
SDD      LA    RE,CSDD             STATION DAYPART DEFINITION                   
         B     ADDONE                                                           
         SPACE 1                                                                
ATNA     LA    RE,CATNA            ATHENA                                       
         B     ADDONE                                                           
         SPACE 1                                                                
SWITCH   LA    RE,CSWI             SWITCH                                       
         B     ADDONE                                                           
         SPACE 1                                                                
COMM     LA    RE,CCOMM            COMMISSION                                   
         B     ADDONE                                                           
         SPACE 1                                                                
OWNER    LA    RE,COWN             OWNERSHIP                                    
         B     ADDONE                                                           
         SPACE 1                                                                
MKT      LA    RE,CMKT             MARKET                                       
         B     ADDONE                                                           
         SPACE 1                                                                
AUR      LA    RE,CAUR             AVERAGE UNIT RATE                            
         B     ADDONE                                                           
         SPACE 1                                                                
FORE     LA    RE,CFORE            FORECAST                                     
         B     ADDONE                                                           
         SPACE 1                                                                
STDCOMM  LA    RE,CSTDCOMM         STANDARD COMMENT                             
         B     ADDONE                                                           
         SPACE 1                                                                
PAPER    LA    RE,CPAPER           PAPERWORK COUNTING                           
         B     ADDONE                                                           
         SPACE 1                                                                
TYPE     LA    RE,CTYPE            TYPE                                         
         B     ADDONE                                                           
         SPACE 1                                                                
POINTP   LA    RE,CPOINTP          POINT PERSON                                 
         B     ADDONE                                                           
         SPACE 1                                                                
CONTYPE  LA    RE,CCONTYPE         CONTRACT TYPE                                
         B     ADDONE                                                           
         SPACE 1                                                                
YADR     LA    RE,CYADR            YADR                                         
         B     ADDONE                                                           
         SPACE 1                                                                
OFFCOM   LA    RE,COFFCOM          OFFICE COMMENTS                              
         B     ADDONE                                                           
         SPACE 1                                                                
DIRRES   LA    RE,CDIRRES          DIRECT RESPONSE                              
         B     ADDONE                                                           
         SPACE 1                                                                
LABEL    LA    RE,CLABEL           CONTRACT LABEL                               
         B     ADDONE                                                           
         SPACE 1                                                                
GOAL     LA    RE,CGOAL            GOALN/GOAL                                   
         B     ADDONE                                                           
         SPACE 1                                                                
STATION2 LA    RE,CSTAT2           STATION PREVIOUS COPY                        
         B     ADDONE                                                           
         SPACE 1                                                                
ERROR    LA    RE,CERROR                                                        
         B     ADDONE                                                           
         EJECT                                                                  
         USING RECD,R3                                                          
AVAIL    LA    RE,CAVLC                                                         
         TM    RAVLCNTL,X'01'                                                   
         BO    ADDONE              IT'S CLOSED                                  
         LA    RE,CAVLA            ELSE ADD TO ACTIVE                           
         B     ADDONE                                                           
         SPACE 1                                                                
PROPOSAL LA    RE,CPRPC                                                         
         TM    RPRPCNTL,X'01'                                                   
         BO    ADDONE                                                           
         LA    RE,CPRPA                                                         
         SPACE 1                                                                
ADDONE   L     RF,0(RE)                                                         
         A     RF,ONE                                                           
         ST    RF,0(RE)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
         USING RECD,R3                                                          
BUY      LA    R1,CBUYC                                                         
         TM    RBUYCNTL,X'01'                                                   
         BO    *+8                 BUY IS CLOSED                                
         LA    R1,CBUYA                                                         
         L     RF,12(R1)           TOTAL COLUMN                                 
         A     RF,ONE                                                           
         ST    RF,12(R1)                                                        
         SPACE 1                                                                
         LA    R4,BYR1             BUCKET BY END DATE                           
         LA    R2,2                                                             
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUY5                                                             
         SPACE 1                                                                
         USING RBUYDTEL,R3                                                      
         LA    R6,RBUYDTST         IF ITS PLAN USE START DATE                   
         OC    RBUYDTED,RBUYDTED                                                
         BZ    *+8                                                              
         LA    R6,RBUYDTED                                                      
         SPACE 1                                                                
BUY3     CLC   0(3,R6),0(R4)       BUY END OR START VS BUCKET END               
         BNH   BUY5                                                             
         LA    R4,3(R4)            NEXT YEAR END                                
         LA    R1,4(R1)            NEXT BUCKET                                  
         BCT   R2,BUY3                                                          
         SPACE 1                                                                
BUY5     L     RF,0(R1)                                                         
         A     RF,ONE                                                           
         ST    RF,0(R1)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
         USING RECD,R3                                                          
CONTRACT LA    R1,CCONC                                                         
         TM    RBUYCNTL,X'01'                                                   
         BO    *+8                                                              
         LA    R1,CCONA                                                         
         L     RF,12(R1)                                                        
         A     RF,ONE                                                           
         ST    RF,12(R1)                                                        
         SPACE 1                                                                
         LA    R4,BYR1                                                          
         LA    R2,2                                                             
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   CON5                                                             
         SPACE 1                                                                
         USING RCONELEM,R3                                                      
CON3     CLC   RCONDATE+3(3),0(R4)                                              
         BNH   CON5                                                             
         LA    R4,3(R4)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,CON3                                                          
         SPACE 1                                                                
CON5     L     RF,0(R1)                                                         
         A     RF,ONE                                                           
         ST    RF,0(R1)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
         USING RECD,R3                                                          
INVENTRY CLI   RINVKSRC,0                                                       
         BNE   INVTXT              NOT A HEADER                                 
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         SPACE 1                                                                
         USING RINVPEL,R3                                                       
         MVC   INVEND,RINVPEFF+2   INVENTORY END                                
         L     RF,CINVHD+12                                                     
         A     RF,ONE                                                           
         ST    RF,CINVHD+12        ONE TO TOTAL                                 
         SPACE 1                                                                
         LA    R2,2                                                             
         LA    R1,CINVHD                                                        
         LA    R4,CYR1                                                          
         OC    INVEND,INVEND                                                    
         BNZ   *+10                                                             
         MVC   INVEND,=2X'FF'                                                   
         SPACE 1                                                                
INV3     CLC   INVEND,0(R4)                                                     
         BNH   INV5                                                             
         LA    R4,2(R4)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,INV3                                                          
         SPACE 1                                                                
INV5     L     RF,0(R1)                                                         
         A     RF,ONE                                                           
         ST    RF,0(R1)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
* FILE NOW CONTAINS NEW MARKET AND STATION FACT RECORDS.  CODE                  
* BELOW COUNTS THESE RECORDS AS GENERAL TEXT (BOOK INDEPENDENT).                
* HOWEVER, THESE RECORDS MAY HAVE A FILTER ELEMENT WHICH CAN                    
* ASSOCIATE THEM WITH A SPECIFIC BOOK. (CLOSE OUT OF 1982 DATA)                 
*                                                                               
INVTXT   DS    0H                                                               
         USING RECD,R3                                                          
         CLI   RINVKSRC,C'M'       MARKET FACT RECORD                           
         BE    INVTX1              NOT A TEXT                                   
         CLI   RINVKSRC,C'S'       STATION FACT RECORD                          
         BE    INVTX1                                                           
         CLI   RINVKSRC,X'FF'      TEXT RECORD                                  
         BNE   INVBK               NOT ANY KIND OF TEXT                         
*                                                                               
         CLC   RINVKSTA,=C'ZZZZZ'                                               
         BNE   INVTX2                                                           
*                                                                               
INVTX1   MVC   INVEND,=2X'FF'      GENERAL TEXT IS CURRENT                      
*                                                                               
INVTX2   DS    0H                                                               
         L     RF,CINVTX+12                                                     
         A     RF,ONE                                                           
         ST    RF,CINVTX+12        ONE TO TOTAL                                 
         SPACE 1                                                                
         LA    R2,2                                                             
         LA    R1,CINVTX                                                        
         LA    R4,CYR1                                                          
         SPACE 1                                                                
INVTX3   CLC   INVEND,0(R4)                                                     
         BNH   INVTX5                                                           
         LA    R4,2(R4)                                                         
         LA    R1,4(R1)                                                         
         BCT   R2,INVTX3                                                        
         SPACE 1                                                                
INVTX5   L     RF,0(R1)                                                         
         A     RF,ONE                                                           
         ST    RF,0(R1)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
         USING RECD,R3                                                          
INVBK    XC    WORK(3),WORK                                                     
         MVC   WORK(1),RINVKSRC                                                 
         MVC   WORK+1(1),RINVKBK+1 MONTH                                        
         TM    RINVCNTL,X'01'                                                   
         BNO   *+8                                                              
         OI    WORK+2,X'01'                                                     
         SPACE 1                                                                
         LA    R1,CINVBK                                                        
         LA    R4,CBOOKS           COUNTER                                      
         SPACE 1                                                                
INVBK2   CLC   WORK(3),0(R1)                                                    
         BE    INVBK3                                                           
         OC    0(3,R1),0(R1)                                                    
         BZ    INVBK3                                                           
         LA    R1,L'CINVBK(R1)                                                  
         BCT   R4,INVBK2                                                        
         DC    H'0'                TOO MANY BOOKS                               
         SPACE 1                                                                
INVBK3   MVC   0(3,R1),WORK                                                     
         LA    R1,4(R1)                                                         
         L     RF,12(R1)                                                        
         A     RF,ONE                                                           
         ST    RF,12(R1)                                                        
         SPACE 1                                                                
         LA    R4,BYR1                                                          
         LA    R2,2                                                             
         SPACE 1                                                                
INVBK4   CLC   RINVKBK(1),0(R4)                                                 
         BNH   INVBK5                                                           
         LA    R1,4(R1)                                                         
         LA    R4,3(R4)                                                         
         BCT   R2,INVBK4                                                        
         SPACE 1                                                                
INVBK5   L     RF,0(R1)                                                         
         A     RF,ONE                                                           
         ST    RF,0(R1)                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         SPACE 1                                                                
*              PRINT RECORD COUNTS                                              
         L     R8,ACCUMS                                                        
         USING CNTSD,R8                                                         
         SPACE 1                                                                
DMXEOF1  CLI   CCODE,X'FF'                                                      
         BE    DMXIT               END OF COUNTS                                
         SPACE 1                                                                
         ZAP   LINE,=P'75'         FORCE NEW PAGE                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(22),=C'REP FILE RECORD COUNTS'                             
         MVC   MID1+1(3),=C'REP'                                                
         MVC   MID1+5(2),CCODE                                                  
         MVC   MID1+8(20),CNAME                                                 
         GOTO1 DATCON,DMCB,(3,BYR1),(8,MID2+28)                                 
         GOTO1 DATCON,DMCB,(3,BYR2),(8,MID2+41)                                 
         GOTO1 DATCON,DMCB,(3,BYR3),(8,MID2+54)                                 
         MVC   MID3+27(9),=C'AND PRIOR'                                         
         MVC   MID3+53(9),=C'AND LATER'                                         
         MVC   MID3+70(5),=C'TOTAL'                                             
         SPACE 1                                                                
         USING PRTD,R7                                                          
         LA    R7,PRLIST                                                        
DMXEOF3  MVC   P+1(20),PRNAME                                                   
         L     RE,PROUTINE                                                      
         A     RE,RELO                                                          
         BR    RE                                                               
         SPACE 1                                                                
DMXEOF9  A     R7,PRLNGTH                                                       
         CLI   0(R7),X'FF'                                                      
         BNE   DMXEOF3                                                          
         SPACE 1                                                                
         A     R8,CNTLNGTH                                                      
         B     DMXEOF1                                                          
         EJECT                                                                  
*              PRINT ROUTINES                                                   
         SPACE 1                                                                
PSTAT    L     R1,CSTAT                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PREGN    L     R1,CREGN                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
POFFC    L     R1,COFFC                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PTEAM    L     R1,CTEAM                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PSALM    L     R1,CMAN                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PGRUP    L     R1,CGRP                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PADVR    L     R1,CADV                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PPRDT    L     R1,CPRD                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PAGCY    L     R1,CAGY                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PCLAS    L     R1,CCLASS                                                        
         B     PTOTAL                                                           
         SPACE 1                                                                
PEROR    L     R1,CERROR                                                        
         B     PTOTAL                                                           
         SPACE 1                                                                
PCATG    L     R1,CCATG                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PBUDG    L     R1,CBUDG                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PEOM     L     R1,CEOM                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
POBUD    L     R1,COBUD                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PAGY2    L     R1,CAGY2                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PEOPADV  L     R1,CEOPADV                                                       
         B     PTOTAL                                                           
         SPACE 1                                                                
PEOPAGY  L     R1,CEOPAGY                                                       
         B     PTOTAL                                                           
         SPACE 1                                                                
PEOPOFF  L     R1,CEOPOFF                                                       
         B     PTOTAL                                                           
         SPACE 1                                                                
PEOPSAL  L     R1,CEOPSAL                                                       
         B     PTOTAL                                                           
         SPACE 1                                                                
POVER    L     R1,COVER                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PDMENU   L     R1,CDMENU                                                        
         B     PTOTAL                                                           
         SPACE 1                                                                
PDPT     L     R1,CDPT                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PPGT     L     R1,CPGT                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PSDD     L     R1,CSDD                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PATNA    L     R1,CATNA                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PSWITCH  L     R1,CSWI                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PCOMM    L     R1,CCOMM                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
POWNER   L     R1,COWN                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PMKT     L     R1,CMKT                                                          
         MVI   SPACING+3,C'2'                                                   
         B     PTOTAL                                                           
         SPACE 1                                                                
PAUR     L     R1,CAUR                                                          
         B     PTOTAL                                                           
         SPACE 1                                                                
PFORE    L     R1,CFORE                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PSTDCOMM L     R1,CSTDCOMM         STANDARD COMMENT                             
         B     PTOTAL                                                           
         SPACE 1                                                                
PPAPER   L     R1,CPAPER           PAPERWORK COUNTING                           
         B     PTOTAL                                                           
         SPACE 1                                                                
PTYPE    L     R1,CTYPE            TYPE                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PPOINTP  L     R1,CPOINTP          POINT PERSON                                 
         B     PTOTAL                                                           
         SPACE 1                                                                
PCONTYPE L     R1,CCONTYPE         CONTRACT TYPE                                
         B     PTOTAL                                                           
         SPACE 1                                                                
PYADR    L     R1,CYADR            YADR                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
POFFCOMM L     R1,COFFCOM          OFFICE COMMENTS                              
         B     PTOTAL                                                           
         SPACE 1                                                                
PDIRRES  L     R1,CDIRRES          DIRECT RESPONSE                              
         B     PTOTAL                                                           
         SPACE 1                                                                
PLABEL   L     R1,CLABEL           CONTRACT LABEL                               
         B     PTOTAL                                                           
         SPACE 1                                                                
PGOAL    L     R1,CGOAL            GOAL                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PSTAT2   L     R1,CSTAT2           STATION PREVIOUS COPY                        
         B     PTOTAL                                                           
         SPACE 1                                                                
PAVLC    L     R1,CAVLC                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PAVLA    L     R1,CAVLA                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PAVLT    L     R1,CAVLC                                                         
         A     R1,CAVLA                                                         
         MVI   SPACING+3,C'2'                                                   
         B     PTOTAL                                                           
         SPACE 1                                                                
PPRPC    L     R1,CPRPC                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PPRPA    L     R1,CPRPA                                                         
         B     PTOTAL                                                           
         SPACE 1                                                                
PPRPT    L     R1,CPRPC                                                         
         A     R1,CPRPA                                                         
         MVI   SPACING+3,C'2'                                                   
         SPACE 1                                                                
PTOTAL   EDIT  (R1),(10,P+65)                                                   
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         B     DMXEOF9                                                          
         EJECT                                                                  
PBUYC    LA    R1,CBUYC                                                         
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PBUYA    LA    R1,CBUYA                                                         
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PBUYT    LA    R1,CBUYC                                                         
         MVI   SPACING+3,C'2'                                                   
         BAS   RE,PADDFOUR                                                      
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PCONC    LA    R1,CCONC                                                         
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PCONA    LA    R1,CCONA                                                         
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PCONT    LA    R1,CCONC                                                         
         MVI   SPACING+3,C'2'                                                   
         BAS   RE,PADDFOUR                                                      
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PINVH    LA    R1,CINVHD                                                        
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         SPACE 1                                                                
PINVR    LA    R1,CINVTX                                                        
         BAS   RE,PFOUR                                                         
         B     DMXEOF9                                                          
         EJECT                                                                  
PINVB    LA    R5,CINVSRC                                                       
         LA    R6,CBOOKS                                                        
         SPACE 1                                                                
PINVB1   CLI   0(R5),0                                                          
         BE    DMXEOF9                                                          
*                                                                               
         LA    R1,SERVICES         COUNTER                                      
         LA    RE,SERVTAB                                                       
         CLC   0(1,R5),0(RE)                                                    
         BE    *+16                FOUND                                        
         LA    RE,L'SERVTAB(RE)                                                 
         BCT   R1,*-14                                                          
         B     *+10                PRINT NOTHING IF NOT IN TABLE                
*                                                                               
         MVC   P+1(4),1(RE)                                                     
*                                                                               
         MVC   P+5(3),=C'EST'                                                   
         ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    PINVB2                                                           
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   P+5(3),0(R1)                                                     
         SPACE 1                                                                
PINVB2   MVC   P+9(6),=C'CLOSED'                                                
         TM    2(R5),X'01'                                                      
         BO    *+10                                                             
         MVC   P+9(6),=C'ACTIVE'                                                
         SPACE 1                                                                
         LA    R1,4(R5)                                                         
         BAS   RE,PFOUR                                                         
         SPACE 1                                                                
         LA    R5,L'CINVBK(R5)                                                  
         BCT   R6,PINVB1                                                        
         B     DMXEOF9                                                          
         SPACE 2                                                                
SERVTAB  DS    0CL5                                                             
         DC    C'A',CL4'ARB'                                                    
         DC    C'B',CL4'ARBP'      PROJECTED                                    
         DC    C'C',CL4'ARBT'      TIME PERIOD                                  
         DC    C'D',CL4'ARBS'      SPECIAL SURVEY                               
         DC    C'E',CL4'ARBE'      ESTIMATED                                    
         DC    C'N',CL4'NSI'                                                    
         DC    C'O',CL4'NSIP'                                                   
         DC    C'P',CL4'NSIT'                                                   
         DC    C'Q',CL4'NSIS'                                                   
         DC    C'R',CL4'NSIE'                                                   
         DC    C'T',CL4'SRC'                                                    
         DC    C'U',CL4'SRCP'                                                   
         DC    C'X',CL4'SRCE'                                                   
SERVICES EQU   (*-SERVTAB)/L'SERVTAB                                            
         EJECT                                                                  
PADDFOUR DS    0H                                                               
         ST    RE,SAVERE                                                        
         LA    R3,4                                                             
         LR    R2,R1                                                            
         L     RE,0(R2)                                                         
         A     RE,16(R2)                                                        
         ST    RE,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,*-16                                                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
PFOUR    ST    RE,SAVERE                                                        
         LR    R2,R1                                                            
         LA    R3,4                                                             
         LA    R4,P+26                                                          
         SPACE 1                                                                
PFOUR1   L     R1,0(R2)                                                         
         EDIT  (R1),(10,0(R4))                                                  
         LA    R2,4(R2)                                                         
         LA    R4,13(R4)                                                        
         BCT   R3,PFOUR1                                                        
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
ADDAY    DC    V(ADDAY)                                                         
DATCON   DC    V(DATCON)                                                        
GETBROAD DC    V(GETBROAD)                                                      
GETDAY   DC    V(GETDAY)                                                        
         SPACE 1                                                                
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
DUB      DC    D'0'                                                             
DMCB     DC    6F'0'                                                            
FULL     DC    F'0'                                                             
HALF     DC    H'0'                                                             
         SPACE 1                                                                
ELCODE   DC    XL1'00'                                                          
DATADISP DC    H'34'                                                            
WORK     DC    CL80' '                                                          
ONE      DC    F'1'                                                             
         SPACE 1                                                                
ACCUMS   DS    A                                                                
         SPACE 1                                                                
PRLNGTH  DC    AL4(PREND-PRST)                                                  
CNTLNGTH DC    AL4(CEND-CNTSL)                                                  
RLNGTH   DC    AL4(REND-RTYPE)                                                  
         SPACE 1                                                                
BYR1     DS    CL3                 BINARY LAST DAY OF YEAR                      
BYR2     DS    CL3                                                              
BYR3     DS    CL3                                                              
         SPACE 1                                                                
CYR1     DS    CL2                 COMPRESSED LAST DAY OF YEAR                  
CYR2     DS    CL2                                                              
CYR3     DS    CL2                                                              
         SPACE 1                                                                
INVEND   DS    CL2                                                              
SAVERE   DS    F                                                                
RELO     DC    A(0)                                                             
         EJECT                                                                  
*              RECORD TYPE                                                      
*              DISPLACEMENT TO REP CODE                                         
*              SPARE                                                            
*              A(ROUTINE)                                                       
         SPACE 1                                                                
RECLIST  DS    0F                                                               
         DC    X'01',AL1(RREPKREP-RREPREC),AL2(0),AL4(REP)                      
         DC    X'02',AL1(RSTAKREP-RSTAREC),AL2(0),AL4(STATION)                  
         DC    X'03',AL1(RREGKREP-RREGREC),AL2(0),AL4(REGION)                   
         DC    X'04',AL1(ROFFKREP-ROFFREC),AL2(0),AL4(OFFICE)                   
         DC    X'05',AL1(RTEMKREP-RTEMREC),AL2(0),AL4(TEAM)                     
         DC    X'06',AL1(RSALKREP-RSALREC),AL2(0),AL4(SALMN)                    
         DC    X'07',AL1(RGRPKREP-RGRPREC),AL2(0),AL4(GROUP)                    
         DC    X'08',AL1(RADVKREP-RADVREC),AL2(0),AL4(ADVER)                    
         DC    X'09',AL1(RPRDKREP-RPRDREC),AL2(0),AL4(PROD)                     
         DC    X'0A',AL1(RAGYKREP-RAGYREC),AL2(0),AL4(AGENCY)                   
         DC    X'0B',AL1(RBUYKREP-RBUYREC),AL2(0),AL4(BUY)                      
         DC    X'0C',AL1(RCONKREP-RCONREC),AL2(0),AL4(CONTRACT)                 
         DC    X'0D',AL1(RCLSKREP-RCLSREC),AL2(0),AL4(CLASS)                    
         DC    X'0F',AL1(RCTGKREP-RCTGREC),AL2(0),AL4(CATEGORY)                 
         DC    X'12',AL1(RINVKREP-RINVREC),AL2(0),AL4(INVENTRY)                 
         DC    X'13',AL1(RBUDKREP-RBUDREC),AL2(0),AL4(BUDGET)                   
         DC    X'14',AL1(RAVLKREP-RAVLREC),AL2(0),AL4(AVAIL)                    
         DC    X'16',AL1(RPRPKREP-RPRPREC),AL2(0),AL4(PROPOSAL)                 
         DC    X'18',AL1(REOMKREP-REOMREC),AL2(0),AL4(EOM)                      
         DC    X'19',AL1(17),AL2(0),AL4(OBUD)                                   
         DC    X'1A',AL1(19),AL2(0),AL4(AGY2)                                   
         DC    X'1B',AL1(12),AL2(0),AL4(EOPADV)                                 
         DC    X'1C',AL1(12),AL2(0),AL4(EOPAGY)                                 
         DC    X'1D',AL1(12),AL2(0),AL4(EOPOFF)                                 
         DC    X'1E',AL1(12),AL2(0),AL4(EOPSAL)                                 
         DC    X'22',AL1(13),AL2(0),AL4(OVER)                                   
         DC    X'23',AL1(23),AL2(0),AL4(DMENU)                                  
         DC    X'24',AL1(24),AL2(0),AL4(DPT)                                    
         DC    X'25',AL1(24),AL2(0),AL4(PGT)                                    
         DC    X'26',AL1(20),AL2(0),AL4(SDD)                                    
         DC    X'27',AL1(01),AL2(0),AL4(ATNA)                                   
         DC    X'28',AL1(13),AL2(0),AL4(SWITCH)                                 
         DC    X'29',AL1(11),AL2(0),AL4(COMM)                                   
         DC    X'2A',AL1(22),AL2(0),AL4(OWNER)                                  
         DC    X'2B',AL1(21),AL2(0),AL4(MKT)                                    
         DC    X'2C',AL1(04),AL2(0),AL4(AUR)                                    
         DC    X'2D',AL1(12),AL2(0),AL4(FORE)                                   
         DC    X'2E',AL1(15),AL2(0),AL4(STDCOMM)                                
         DC    X'2F',AL1(24),AL2(0),AL4(PAPER)                                  
         DC    X'30',AL1(17),AL2(0),AL4(TYPE)                                   
         DC    X'31',AL1(22),AL2(0),AL4(POINTP)                                 
         DC    X'32',AL1(24),AL2(0),AL4(CONTYPE)                                
         DC    X'33',AL1(17),AL2(0),AL4(YADR)                                   
         DC    X'34',AL1(20),AL2(0),AL4(OFFCOM)                                 
         DC    X'35',AL1(13),AL2(0),AL4(DIRRES)                                 
         DC    X'36',AL1(17),AL2(0),AL4(LABEL)                                  
         DC    X'37',AL1(13),AL2(0),AL4(GOAL)                                   
         DC    X'42',AL1(20),AL2(0),AL4(STATION2)                               
         DC    X'0E',AL1(1),AL2(0),AL4(ERROR)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*              RECORD NAMES AND PRINT ROUTINE                                   
         SPACE 1                                                                
PRLIST   DS    0F                                                               
         DC    CL20'STATION             ',AL4(PSTAT)                            
         DC    CL20'REGION              ',AL4(PREGN)                            
         DC    CL20'OFFICE              ',AL4(POFFC)                            
         DC    CL20'TEAM                ',AL4(PTEAM)                            
         DC    CL20'SALESMAN            ',AL4(PSALM)                            
         DC    CL20'GROUP               ',AL4(PGRUP)                            
         DC    CL20'ADVERTISER          ',AL4(PADVR)                            
         DC    CL20'PRODUCT             ',AL4(PPRDT)                            
         DC    CL20'AGENCY              ',AL4(PAGCY)                            
         DC    CL20'AGENCY PART 2       ',AL4(PAGY2)                            
         DC    CL20'CLASS               ',AL4(PCLAS)                            
         DC    CL20'ERROR               ',AL4(PEROR)                            
         DC    CL20'CATEGORY            ',AL4(PCATG)                            
         DC    CL20'BUDGET              ',AL4(PBUDG)                            
         DC    CL20'EOM                 ',AL4(PEOM)                             
         DC    CL20'OFFICE BUDGET       ',AL4(POBUD)                            
         DC    CL20'EOP ADV             ',AL4(PEOPADV)                          
         DC    CL20'EOP AGY             ',AL4(PEOPAGY)                          
         DC    CL20'EOP OFF             ',AL4(PEOPOFF)                          
         DC    CL20'EOP SAL             ',AL4(PEOPSAL)                          
         DC    CL20'OVERNIGHT UPLOAD    ',AL4(POVER)                            
         DC    CL20'DEMO MENU           ',AL4(PDMENU)                           
         DC    CL20'DAYPART             ',AL4(PDPT)                             
         DC    CL20'PROGRAM TYPE        ',AL4(PPGT)                             
         DC    CL20'STATION DAYPART DEF ',AL4(PSDD)                             
         DC    CL20'ATHENA              ',AL4(PATNA)                            
         DC    CL20'SWITCH              ',AL4(PSWITCH)                          
         DC    CL20'COMMISSION          ',AL4(PCOMM)                            
         DC    CL20'OWNERSHIP           ',AL4(POWNER)                           
         DC    CL20'MARKET              ',AL4(PMKT)                             
         DC    CL20'AVERAGE UNIT RATE   ',AL4(PAUR)                             
         DC    CL20'AVAILS-CLOSED       ',AL4(PAVLC)                            
         DC    CL20'AVAILS-ACTIVE       ',AL4(PAVLA)                            
         DC    CL20'AVAILS-TOTAL        ',AL4(PAVLT)                            
         DC    CL20'PROPOSALS-CLOSED    ',AL4(PPRPC)                            
         DC    CL20'PROPOSALS-ACTIVE    ',AL4(PPRPA)                            
         DC    CL20'PROPOSALS-TOTAL     ',AL4(PPRPT)                            
         DC    CL20'BUYS-CLOSED         ',AL4(PBUYC)                            
         DC    CL20'BUYS-ACTIVE         ',AL4(PBUYA)                            
         DC    CL20'BUYS-TOTAL          ',AL4(PBUYT)                            
         DC    CL20'CONTRACTS-CLOSED    ',AL4(PCONC)                            
         DC    CL20'CONTRACTS-ACTIVE    ',AL4(PCONA)                            
         DC    CL20'CONTRACTS-TOTAL     ',AL4(PCONT)                            
         DC    CL20'INVENTORY-HEADERS   ',AL4(PINVH)                            
         DC    CL20'INVENTORY-RATIONALE ',AL4(PINVR)                            
         DC    CL20'ARB MAY CLOSED      ',AL4(PINVB)                            
         DC    CL20'FORECAST            ',AL4(PFORE)                            
         DC    CL20'STANDARD COMMENT    ',AL4(PSTDCOMM)  X'2E'                  
         DC    CL20'PAPERWORK COUNT     ',AL4(PPAPER)    X'2F'                  
         DC    CL20'TYPE                ',AL4(PTYPE)     X'30'                  
         DC    CL20'POINT PERSON        ',AL4(PPOINTP)   X'31'                  
         DC    CL20'CONTRACT TYPE       ',AL4(PCONTYPE)  X'32'                  
         DC    CL20'YADR FORMAT         ',AL4(PYADR)     X'33'                  
         DC    CL20'OFFICE COMMENT      ',AL4(POFFCOMM)  X'34'                  
         DC    CL20'DIRECT RESPONSE     ',AL4(PDIRRES)   X'35'                  
         DC    CL20'CONTRACT LABEL      ',AL4(PLABEL)    X'36'                  
         DC    CL20'GOAL                ',AL4(PGOAL)     X'37'                  
         DC    CL20'STATION PREV COPY   ',AL4(PSTAT2)    X'42'                  
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
ACCUMC   DS    0D                                                               
         DS    400000C                                                          
         EJECT                                                                  
*              DSECT FOR THE ACCUMULATOR TABLE                                  
         SPACE 1                                                                
CNTSD    DSECT                                                                  
CNTSL    DS    0C                                                               
CCODE    DS    CL2                 REP CODE                                     
CNAME    DS    CL20                REP NAME                                     
         DS    CL2                 SPARE                                        
CROW     DS    0F                                                               
CSTAT    DS    F                   STATION                                      
CREGN    DS    F                   REGION                                       
COFFC    DS    F                   OFFICE                                       
CTEAM    DS    F                   TEAM                                         
CMAN     DS    F                   SALESMAN                                     
CGRP     DS    F                   GROUP                                        
CADV     DS    F                   ADVERTISER                                   
CPRD     DS    F                   PRODUCT                                      
CAGY     DS    F                   AGENCY                                       
CCLASS   DS    F                   CLASS                                        
CERROR   DS    F                   ERROR MESSAGES                               
CCATG    DS    F                   CATEGORY                                     
CBUDG    DS    F                   BUDGET                                       
CEOM     DS    F                   EOM                                          
COBUD    DS    F                   OFFICE BUDGET                                
CAGY2    DS    F                   AGENCY CONTINUATION                          
CEOPADV  DS    F                   EOP ADVERTISER                               
CEOPAGY  DS    F                   EOP AGENCY                                   
CEOPOFF  DS    F                   EOP OFFICE                                   
CEOPSAL  DS    F                   EOP SALESPERSON                              
COVER    DS    F                   OVERNIGHT UPLOAD                             
CDMENU   DS    F                   DEMO MENU                                    
CDPT     DS    F                   DAYPART                                      
CPGT     DS    F                   PROGRAM TYPE                                 
CSDD     DS    F                   STATION DAYPART DEFINITION                   
CATNA    DS    F                   ATHENA                                       
CSWI     DS    F                   SWITCH                                       
CCOMM    DS    F                   COMMISSION                                   
COWN     DS    F                   OWNERSHIP                                    
CMKT     DS    F                   MARKET                                       
CAUR     DS    F                   AVERAGE UNIT RATE                            
CFORE    DS    F                   FORECAST                                     
CSTDCOMM DS    F                   STANDARD COMMENT                             
CPAPER   DS    F                   PAPER WORK COUNTING                          
CTYPE    DS    F                   TYPE                                         
CPOINTP  DS    F                   POINT PERSON                                 
CCONTYPE DS    F                   CONTRACT TYPE                                
CYADR    DS    F                   YADR                                         
COFFCOM  DS    F                   OFFICE COMMENTS                              
CDIRRES  DS    F                   DIRECT RESPONSE                              
CLABEL   DS    F                   CONTRACT LABEL                               
CGOAL    DS    F                   GOALN/GOAL                                   
CSTAT2   DS    F                   STATION PREVIOUS COPY                        
         SPACE 1                                                                
CAVLC    DS    F                   CLOSED AVAILS                                
CAVLA    DS    F                   ACTIVE AVAILS                                
CPRPC    DS    F                   CLOSED PROPOSALS                             
CPRPA    DS    F                   ACTIVE PROPOSALS                             
         SPACE 1                                                                
CBUYC    DS    4F                  CLOSED BUYS BY YEAR                          
CBUYA    DS    4F                  ACTIVE BUYS BY YEAR                          
CCONC    DS    4F                  CLOSED CONTRACTS BY YEAR                     
CCONA    DS    4F                  ACTIVE CONTRACTS BY YEAR                     
         SPACE 1                                                                
CINVHD   DS    4F                  INVENTORY HEADERS                            
CINVTX   DS    4F                  RATIONALE                                    
         SPACE 1                                                                
CINVBK   DS    120CL20                                                          
CBOOKS   EQU   (*-CINVBK)/L'CINVBK LIMIT ON BOOKS                               
         ORG   CINVBK                                                           
CINVSRC  DS    CL1                 SOURCE                                       
CINVMTH  DS    CL1                 MONTH                                        
CINVSTAT DS    CL1                 X'01'- CLOSED  X'00'- ACTIVE                 
         DS    CL1                 SPARE                                        
CINVCNT  DS    4F                  BY YEAR AND TOTAL                            
         ORG                                                                    
CEND     DS    0C                                                               
         EJECT                                                                  
*              DSECT FOR RECLIST TABLE                                          
         SPACE 1                                                                
RLISTD   DSECT                                                                  
RTYPE    DS    CL1                 RECORD TYPE                                  
RCODE    DS    CL1                 DISP. TO REP CODE                            
         DS    CL2                 SPARE                                        
ROUTINE  DS    A                   A(ROUTINE)                                   
REND     DS    0C                                                               
         EJECT                                                                  
*              DSECT FOR PRLIST                                                 
         SPACE 1                                                                
PRTD     DSECT                                                                  
PRST     DS    0C                                                               
PRNAME   DS    CL20                RECORD NAME                                  
PROUTINE DS    A                   A(ROUTINE)                                   
PREND    DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         SPACE 1                                                                
*        REGENALLD                                                              
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029RELDCNTA  05/01/02'                                      
         END                                                                    
