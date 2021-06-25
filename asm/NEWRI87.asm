*          DATA SET NEWRI87    AT LEVEL 018 AS OF 03/22/07                      
*PHASE T32087A                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE MOBILE                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE PRINT                                                                  
         TITLE 'T32087 - COLGATE TAPE'                                          
T32087   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NCOL**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)         RA=2ND BASE REGISTER                         
         LA    RA,1(RA)                                                         
         USING T32087,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING          ANETWS2/ANETWS3=I/O AREAS                    
         ST    R1,SPECS                                                         
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
*                                                                               
         GOTO1 =A(INITWKA),DMCB,RR=Y              INIT WK STORAGE AREA          
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     LR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,23,A,28,31,A),FORMAT=BI,WORK=1'              
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=561'                                   
*                                                                               
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0GGAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
LR5      DS    0H                                                               
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR7                                                              
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR7                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
******   GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         DROP   R4                                                              
*                                                                               
LR7      DS    0H                                                               
         MVI   NBDATA,C'U'                                                      
         NI    NBSPLOPT,X'FF'-X'C0'     TURN OFF SPLIT OPT                      
         MVI   NBUSER+13,0                                                      
         OI    NBINDS2,NBBILLRD                                                 
         L     R1,=A(UNTIO)                                                     
         ST    R1,NBAIO                                                         
*          DATA SET NEWRI20    AT LEVEL 113 AS OF 03/18/05                      
         L     R1,=A(NEWBLRD)     NETBILLRD DSECT                               
         USING NBLBILLD,R1                                                      
         ST    R1,NBABILRD                                                      
         XC    0(NBLLENQ,R1),0(R1)                                              
         MVC   NBLUNAIO,NBAIO     NBAIO                                         
         OI    NBLFUNC,NBLSEED    SEED UNIT REC WITH ALL BILL ELEMS             
ENDNBRD  DS    0H                                                               
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALCLI                                                  
         BNE   LR12                                                             
         L     RF,ACLISTSV                                                      
         L     RE,NBAIO                                                         
         USING CKEY,RE                                                          
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  RE                                                               
LR12     CLI   NBMODE,NBPROCUN                                                  
         BE    LR20                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LR50                                                             
         B     LR10                                                             
*                                                                               
LR20     DS    0H                                                               
         BC    0,LR30                                                           
         OI    LR20+1,X'F0'                                                     
         BAS   RE,DOBILREC                                                      
LR30     BAS   RE,DOUNIT                                                        
*                                                                               
         B     LR10                                                             
         SPACE                                                                  
*                                                                               
LR50     MVI   MYBYTE,0            SET RECS FROM SORTER FLAG                    
*                                                                               
LR51     GOTO1 SORTER,DMCB,=C'GET' TAKE RECS FROM SORTER                        
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    LR70                                                             
         MVI   MYBYTE,1                                                         
         LA    R1,L'SORTREC                                                     
         MOVE  (SORTREC,(R1)),0(R3)                                             
*                                                                               
         BAS   RE,PROCRECS         PRINTS RECS                                  
*                                                                               
         B     LR51                                                             
                                                                                
* - EOF SORTER - NO MORE RECORDS                                                
LR70     DS    0H                                                               
         CLI   MYBYTE,1            ARE THERE ANY RECS                           
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,LASTREC          PRINTS/WRITES LAST RECORD                    
*                                                                               
         BAS   RE,SUMMARY          PRINTS/WRITES SUMMARY RECORD                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        PUT UNIT BILLING ELEMENTS TO SORTER                          *         
*                                                                     *         
***********************************************************************         
DOUNIT   NTR1                                                                   
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'10'        BILLING ELEM CODE                            
         BAS   RE,GETEL                                                         
         BNE   DOUX                                                             
         USING NUBILD,R6                                                        
*                                                                               
* - KEY OF SORT RECORD                                                          
*                                                                               
DOU10    MVI   SORTREC,X'40'       SET REC TO BLANKS                            
         LA    RF,SORTREC+1                                                     
         LA    R1,SORTLENQ-1                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   CGSCLT,NBCLICOD     CLIENT                                       
*                                                                               
         MVC   BYTE,NUBILPRD                                                    
         GOTO1 =A(GETPRD),DMCB,RR=Y                                             
         MVC   CGSPROD,CURPROD     3 CHARACTER PROD                             
*                                                                               
         CLI   PRODUCT,0           IF FILTERING ON PRODUCT CODE                 
         BE    *+14                                                             
         CLC   CURPROD,PRODUCT     DO IT NOW                                    
         BNE   DOU50                                                            
*                                                                               
         EDIT  (B1,NBACTEST),(3,CGSEST),FILL=0                                  
*                                                                               
******** GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,CGSINVDT)                            
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,CGSINVDT)                            
****     GOTO1 DATCON,DMCB,(2,NUBILIDT),(0,CGSINVDT)                            
         CLC   CGSINVDT,STRBDAT                                                 
         BL    DOU50                                                            
         CLC   CGSINVDT,ENDBDAT                                                 
         BH    DOU50                                                            
*                                                                               
***********************************************************************         
*        READ SPOT FILE TO GET EST AND PRD USER FIELDS                *         
***********************************************************************         
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),CGSPROD                                                 
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DEATH                                        
*                                                                               
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DEATH                                        
*                                                                               
         L     R4,AIO                                                           
         USING PRDHDR,R4                                                        
         MVC   MYPUSER,PUSER1      SAP COST CENTER                              
         OC    MYPUSER,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),CGSPROD                                                 
         MVC   KEY+7(1),NBACTEST                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DEATH                                        
*                                                                               
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DEATH                                        
*                                                                               
         L     R4,AIO                                                           
         USING ESTHDR,R4                                                        
         MVC   MYEUSER,EUSER1      SAP GL ACCOUNT                               
         DROP  R4                                                               
*                                                                               
         NETGO NVSETUNT,DMCB       RESTORE UNIT RECORD                          
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
*                                                                               
*******  END OF USER FIELD RETRIVAL ***********************************         
*                                                                               
         MVC   CGSINVNO(2),CGSINVDT+2    SET 1ST 2 DIGITS FROM DATE             
         MVC   CGSINVNO+2(4),NUBILNUM    INV NUMBER                             
*                                                                               
         XC    CGSMON,CGSMON       THIS FIELD DOESN'T APPLY TO UNIT REC         
         MVI   CGSTYPE,C'2'        DETAIL RECORD HAS TYPE OF 2                  
         MVC   CGSMEDT,NBPOSTYP    MEDIA TYPE: N, S, C AND O                    
*                                                                               
         CLI   CGSMEDT,C'O'                                                     
         BE    DOU20                                                            
         CLI   CGSMEDT,C'N'                                                     
         BE    DOU30               NOT "N" THEN MUST BE "S" OR "C"              
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,CGSINVDT),MY12BYTE,GETDAY,ADDAY             
         CLI   DMCB,X'FF'                                                       
         BE    DOU21                                                            
         MVC   TEMPDATE,MY12BYTE+6                                              
         GOTO1 ADDAY,DMCB,TEMPDATE,MYDATE,-10                                   
         MVC   CGSINVNO(2),MYDATE+2                                             
         B     DOU30                                                            
*                                                                               
DOU20    MVC   P(35),=C'ERR: MEDIA TYPE "O" IS NOT DEFINED!'                    
         BAS   RE,SPOOLIT                                                       
         B     DOU50                                                            
DOU21    MVC   P(35),=C'ERR: INVOICE DATE IS INVALID!      '                    
         BAS   RE,SPOOLIT                                                       
         B     DOU50                                                            
*                                                                               
* - SORT RECORD DATA (DETAIL RECORD)                                            
*                                                                               
DOU30    LA    R2,CGSDATA                                                       
         USING CGDATA,R2                                                        
         MVI   CGDRETY,C'2'        DETAIL REC ARE TYPE 2                        
*                                                                               
         MVC   CGDVENN,=C'1252071' VENDOR NUMBER HARD CODED                     
         MVI   CGDINUM,C'N'        INVOICE NUMBER (N-##-####)                   
         MVI   CGDINUM+1,C'-'                                                   
         MVI   CGDINUM+4,C'-'                                                   
         MVC   CGDINUM+2(2),CGSINVNO                                            
         MVC   CGDINUM+5(4),CGSINVNO+2                                          
         MVC   CGDFILB,SPACES      FILLER B (BLANK)                             
         MVC   CGDSBAR,=C'101'     SAP BUSINESS ARE HARD CODED                  
         MVI   CGDSEP1,C'-'        SEPARATOR HARD CODED                         
         MVC   CGDSGAC,MYEUSER     SAP GL ACCOUNT                               
         MVC   CGDSCCT,SPACES      SPACE FILL NEW 9 DIGIT COST                  
*                                                                               
         CLC   CGDSGAC,=C'111052'  SPECIAL CASE...                              
         BNE   *+14                                                             
**       MVC   CGDSCCT(7),=C'0000000'                                           
         MVC   CGDSCCT(9),=C'000000000'                                         
         B     *+10                                                             
         MVC   CGDSCCT,MYPUSER     SAP COST CENTER NOW 9 DIGITS                 
*                                                                               
         MVC   CGDSCCD,=C'US01'    SAP COMP CODE HARD CODED                     
         MVC   CGDFILD,SPACES      FILLER D (BLANK)                             
*                                                                               
*  GETTING MONTH OF SERVICE                                                     
*                                                                               
         MVI   MYIO,X'00'          INITIALIZE MYIO                              
         LA    RF,MYIO+1                                                        
         LA    R1,L'MYIO-1                                                      
         LA    RE,MYIO                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    R5,MOBILADS         ADDRESSES FOR MOBILE                         
         XC    0(4,R5),0(R5)       GETBROAD IS LINKED IN                        
         MVC   4(4,R5),ADDAY                                                    
         MVC   8(4,R5),GETDAY                                                   
         MVC   12(4,R5),DATCON                                                  
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R5,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK2,(0,(R5))                           
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R5,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 GETPROF,DMCB,(0,WORK),MYWORK1,(0,(R5))                           
*                                                                               
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
*                                                                               
         MVC   MYWORK1+2(1),MYWORK2                                             
         MVC   MYWORK1+6(3),MYWORK2+1                                           
         IC    R0,MYWORK1+2               DATE CONTROL                          
*                                                                               
* - SET (NBSELSTR-1 YEAR)  TO DUB FOR BROAD MOBILE DATELIST                     
*                                                                               
         GOTO1 ADDAY,DMCB,NBSELSTR,MYWORK2,-450                                 
         MVC   MYWORK2+6(6),NBSELEND                                            
*                                                                               
         LA    R5,MYWORK1                                                       
*                                                                               
         CLI   CGSMEDT,C'O'                                                     
         BE    DOU20               ERROR, THERE'S NO "O" TYPE                   
         CLI   CGSMEDT,C'N'                                                     
         BNE   MOBILE2             NOT "N" THEN MUST BE "S" OR "C"              
*                                                                               
         GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),((R0),MYIO),MOBILADS,(R5)          
         B     BPERLIST            BUILD PERLIST                                
*                                                                               
MOBILE2  GOTO1 =V(MOBILE),DMCB,(208,MYWORK2),(0,MYIO),MOBILADS                  
*                                                                               
         LA    R5,MYIO                                                          
*                                                                               
FIND00   DS    0X                                                               
         CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NBACTDAT,0(R5)      TEST DATE VS START                           
         BE    FIND01                                                           
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         CLC   NBACTDAT,2(R5)                                                   
         BNH   FIND01                                                           
         LA    R5,4(R5)            NEXT ENTRY                                   
         B     FIND00              NOTE- LIST ENDS IN FF                        
FIND01   DS    0H                                                               
*                                                                               
         MVC   MY2BYTE,2(R5)                                                    
         GOTO1 DATCON,DMCB,(2,MY2BYTE),(0,TEMPDATE)                             
*                                                                               
         GOTO1 ADDAY,DMCB,TEMPDATE,MYDATE2,-10                                  
         GOTO1 DATCON,DMCB,MYDATE2,(20,WORK)     YYYYMMDD                       
*                                                                               
         MVC   CGDDESP,SPACES      DESCRIPTION                                  
         MVC   CGDDESP(L'CGSEST),CGSEST                                         
         MVI   CGDDESP+L'CGSEST,C'_'                                            
         MVC   CGDDESP+4(2),WORK+4                                              
         MVI   CGDDESP+L'CGSEST+3,C'/'                                          
         MVC   CGDDESP+L'CGSEST+4(4),WORK                                       
         B     MOSDONE                                                          
*                                                                               
*-FIND FIRST PERIOD OF A NEW YEAR                                               
*                                                                               
BPERLIST LA    R5,MYIO                                                          
*                                                                               
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         CLI   5(R5),0             IF ZERO WE GET LOOP IN SETD8                 
         BE    SETD6                                                            
         LA    R5,4(R5)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         LA    R3,PERLIST                                                       
SETD7    DS    0H                                                               
         ZIC   R0,2(R5)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R5)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
         B     GETMOS              MONTH OF SERVICE                             
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R5)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
*-GETTING MONTH OF SERVICE FROM PERLIST TABLE                                   
*                                                                               
GETMOS   DS    0H                                                               
*                                                                               
         LA    R5,PERLIST                                                       
*                                                                               
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(R5)      TEST DATE VS START                           
         BE    GETM8                                                            
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         CLC   NBACTDAT,4(R5)                                                   
         BNH   GETM8                                                            
         LA    R5,6(R5)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
*                                                                               
         MVC   CGDDESP,SPACES      DESCRIPTION                                  
         MVC   CGDDESP(L'CGSEST),CGSEST                                         
         MVI   CGDDESP+L'CGSEST,C'_'                                            
         ZIC   R3,1(R5)            SET MONTH                                    
         EDIT  (R3),(2,CGDDESP+4),FILL=0                                        
         MVI   CGDDESP+L'CGSEST+3,C'/'                                          
         MVC   CGDDESP+L'CGSEST+4(2),=C'19'                                     
         ZIC   R3,0(R5)            SET YEAR                                     
         EDIT  (R3),(2,CGDDESP+9),FILL=0                                        
         CH    R3,=H'100'                                                       
         BL    *+10                                                             
         MVC   CGDDESP+L'CGSEST+4(2),=C'20'                                     
*                                                                               
*   MONTH OF SERVICE CODES END HERE...                                          
*                                                                               
MOSDONE  MVC   CGDFILE,SPACES      FILLER E (BLANK)                             
         MVI   CGDSIGN,C'+'        SIGN (+ OR -)  ASUUME + FOR NOW              
         MVC   CGDDISL,=13C'0'                                                  
         ICM   R3,15,NUBILNET      NET BILLING                                  
         CVD   R3,DUB                                                           
         UNPK  CGDDISL,DUB+2(6)                                                 
         MVC   CGDSRIN,=C'Y R'     SOURCE INITIALS HARD CODED                   
         MVI   CGDSRIN+1,X'50'                                                  
         MVI   CGDFILG,C' '        FILLER G (BLANK)                             
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(20,TEMPDATE)                           
         MVC   CGDARDT(4),TEMPDATE+4                                            
         MVC   CGDARDT+4(4),TEMPDATE                                            
*                                                                               
         MVC   CGSARDT,CGDARDT     AIR DATE NOW IS PART OF SORT KEY             
         MVI   CGDFIL1,C' '        FILLER 1 (BLANK)                             
*                                                                               
         MVC   WK16BYTE,NBPROGNM   PROGRAM NAME TO BE MODIFIED                  
         LA    R5,WK16BYTE+L'WK16BYTE-1                                         
PRGNM    LA    RE,WK16BYTE                                                      
         CR    R5,RE                                                            
         BL    PRGNMX                                                           
         CLI   0(R5),X'00'                                                      
         BNE   PRGNMX                                                           
         MVI   0(R5),X'40'         REPLACE NULL WITH SPACE                      
*******  MVI   WK16BYTE+15,X'40'                                                
*******  BCTR  R5,0                                                             
         S     R5,=F'1'                                                         
         B     PRGNM                                                            
*                                                                               
PRGNMX   DS    0X                                                               
*                                                                               
         CLC   CGDSGAC,=C'500008'  IS THIS PARTICULAR EST USER FIELD?           
         BE    DOU45                                                            
         CLI   CGSMEDT,C'C'        IS MEDIA TYPE CABLE?                         
         BE    DOU45                                                            
*                                                                               
         MVC   CGDSHOW,SPACES      PROGRAM NAME                                 
         MVC   CGDSHOW(L'WK16BYTE),WK16BYTE               PROGRAM NAME          
         MVI   CGDFIL2,C' '        FILLER 2 (BLANK)                             
         MVC   CGDSTAT,NBACTNET                           STATION (CL4)         
         MVI   CGDFIL3,C' '        FILLER 3 (BLANK)                             
         EDIT  (B1,NBLEN),(3,CGDLENG),FILL=0                                    
         MVC   CGSSHOW,CGDSHOW     PROGRAM NAME NOW IS PART OF SORT KEY         
         MVC   CGSSTAT,CGDSTAT     STATION NOW IS PART OF SORT KEY              
         MVC   CGSLENG,CGDLENG     LENGTH NOW IS PART OF SORT KEY               
         MVI   CGDFIL4,C' '        FILLER 4 (BLANK)                             
         B     DOU47                                                            
*                                                                               
DOU45    MVC   CGDCSHO,SPACES      PROGRAM NAME FOR CABLE SPORT                 
         MVC   CGDSHOW(L'WK16BYTE),WK16BYTE               PROGRAM NAME          
         MVI   CGDCFI2,C' '        FILLER 2 (BLANK)                             
         MVC   CGDCSTA,NBACTNET                           STATION (CL4)         
         MVI   CGDCFI3,C' '        FILLER 3 (BLANK)                             
         EDIT  (B1,NBLEN),(3,CGDCLEN),FILL=0                                    
         MVC   CGSSHOW,CGDCSHO     PROGRAM NAME NOW IS PART OF SORT KEY         
         MVC   CGSSTAT,CGDCSTA     STATION NOW IS PART OF SORT KEY              
         MVC   CGSLENG,CGDCLEN     LENGTH NOW IS PART OF SORT KEY               
         MVI   CGDCFI4,C' '        FILLER 4 (BLANK)                             
         MVC   CGDCNUN,=C'000'     NUMB OF UNIT, SET TO 0 FOR NOW               
*                                                                               
DOU47    MVC   CGDFILH,SPACES      FILLER H (BLANK)                             
*                                                                               
* - PUT TO SORTER                                                               
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,=C'UNIT PUT',SORTREC,C'DUMP',320,=C'1D'          
         BAS   RE,PUTSORT                                                       
*                                                                               
* - GET NEXT BILLING ELEMENT                                                    
*                                                                               
DOU50    BAS   RE,NEXTEL                                                        
         BE    DOU10                                                            
*                                                                               
DOUX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READ BILL RECORDS                                            *         
***********************************************************************         
DOBILREC NTR1                                                                   
         MVI   SORTSAVE,X'40'      INITIALIZE SORTSAVE                          
         LA    RF,SORTSAVE+1                                                    
         LA    R1,L'SORTSAVE-1                                                  
         LA    RE,SORTSAVE                                                      
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         LA    R3,KEY                                                           
         USING BKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   BKEYAM,NBACTAM                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     DB20                                                             
DB17     MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
                                                                                
* - FILTER BILL RECORDS                                                         
DB20     DS    0H                                                               
         CLC   KEY(2),KEYSAVE         ID/AM                                     
         BNE   DBX                                                              
         CLI   CLIENT,0                                                         
         BE    *+14                                                             
         CLC   BKEYCLT,CLIENT                                                   
         BNE   DB17                                                             
         CLI   PRODUCT,0                                                        
         BE    *+14                                                             
         CLC   BKEYPRD,PRODUCT                                                  
         BNE   DB17                                                             
         CLI   NBSELEST,0                                                       
         BE    DB30                                                             
         CLI   NBSELESE,0          IS IT RANGE                                  
         BE    DB25                                                             
         CLC   BKEYEST,NBSELEST    YES                                          
         BL    DB17                                                             
         CLC   BKEYEST,NBSELESE                                                 
         BH    DB17                                                             
         B     DB30                                                             
DB25     CLC   BKEYEST,NBSELEST                                                 
         BNE   DB17                                                             
                                                                                
DB30     DS    0H                                                               
         MVC   FILENAME,=C'SPTFILE '                                            
         MVC   AIO,ANETWS2                                                      
         GOTO1 GETREC                                                           
         DROP  R3                                                               
         L     R6,AIO                                                           
         USING BILLREC,R6                                                       
         TM    BILSTAT,X'20'       CHK TRUE AOR BILL - SKIP                     
         BO    DB17                                                             
         CLC   BDATE(6),STRBDAT    CHK BILLING DATE                             
         BL    DB17                                                             
         CLC   BDATE(6),ENDBDAT    CHK BILLING DATE                             
         BH    DB17                                                             
*                                                                               
* - BILL RECORD PASSES FILTERS                                                  
*                                                                               
         GOTO1 =A(ESTHEAD),DMCB,RR=Y         EST DESCRIPTION                    
*                                                                               
* - SET UP KEY FOR SORT RECORD                                                  
*                                                                               
         MVI   SORTREC,X'40'                                                    
         LA    R1,SORTLENQ-1                                                    
         MOVE  (SORTREC+1,(R1)),SORTREC                                         
         GOTO1 =V(CLUNPK),DMCB,BKEYCLT,CGSCLT                                   
         MVC   CGSPROD,BKEYPRD                                                  
         EDIT  (B1,BKEYEST),(3,CGSEST),FILL=0                                   
         MVC   CGSINVDT,BDATE                                                   
******   MVC   CGSINVDT,BQDATE                                                  
         MVC   CGSINVNO,BINVNO                                                  
         MVC   CGSMON,BINVNO       COPY MONTH NUMBER INTO SORT REC              
*                                                                               
         PACK  DUB,CGSINVNO(2)     CONVERT MONTH NUMBER TO MONTH                
         CVB   R5,DUB                                                           
         CH    R5,=H'12'                                                        
         BL    *+12                                                             
         SH    R5,=H'12'                                                        
         B     *-12                                                             
         STCM  R5,1,MYMONTH                                                     
         LA    R5,MONCONTB                                                      
*                                                                               
DB35     CLC   MYMONTH,0(R5)                                                    
         BE    DB40                                                             
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                HOPE THIS NEVER HAPPENS...                   
         LA    R5,MONCONLQ(R5)                                                  
         B     DB35                                                             
*                                                                               
DB40     MVC   CGSINVNO(2),1(R5)                                                
*                                                                               
         MVI   CGSTYPE,C'1'                                                     
         XC    CGSARDT,CGSARDT     THIS KEY FIELD DOESN'T APPLY HERE            
         XC    CGSSHOW,CGSSHOW     THIS KEY FIELD DOESN'T APPLY HERE            
         XC    CGSSTAT,CGSSTAT     THIS KEY FIELD DOESN'T APPLY HERE            
         XC    CGSLENG,CGSLENG     THIS KEY FIELD DOESN'T APPLY HERE            
         MVI   CGSMEDT,X'00'       THIS KEY FIELD DOESN'T APPLY HERE            
*                                                                               
* - DATA FOR SORT RECORD (HEADER RECORD)                                        
*                                                                               
         LA    R2,CGSDATA                                                       
         USING CGDATA,R2           COLGATE TAPE DSECT (DATA)                    
         MVI   CGHRETY,C'1'        HEADER RECORD TYPE = 1                       
*                                                                               
         MVC   CGHVENN,=C'1252071' VENDOR NUMBER HARD CODED                     
         MVI   CGHINUM,C'N'        INVOICE NUMBER (N-##-####)                   
         MVI   CGHINUM+1,C'-'                                                   
         MVI   CGHINUM+4,C'-'                                                   
         MVC   CGHINUM+2(2),CGSINVNO                                            
         MVC   CGHINUM+5(4),CGSINVNO+2                                          
         MVC   CGHOPID,=C'Y R'     OPERATION ID HARD CODED                      
         MVI   CGHOPID+1,X'50'                                                  
         MVI   CGHINCL,C'I'        INVOICE CLASS HARD CODED                     
         MVC   CGHCKCL,SPACES      CHECK CLASS (BLANK)                          
         MVC   CGHCOMM,ESTIDESC    EST DESCRIPTION (1ST 12 CHAR)                
**       MVC   CGHINDT(4),BQDATE+2                                              
**       MVC   CGHINDT+4(2),=C'19' ASSUME 19NN                                  
**       MVC   CGHINDT+6(2),BQDATE                                              
**       CLC   =C'80',BQDATE                                                    
**       BL    *+10                                                             
*8       MVC   CGHINDT+4(2),=C'20' YEAR 20NN                                    
         GOTO1 DATCON,DMCB,BQDATE,(X'20',WORK)                                  
         MVC   CGHINDT(4),WORK+2                                                
         MVC   CGHINDT+4(2),=C'19' ASSUME 19NN                                  
         MVC   CGHINDT+6(2),WORK                                                
         CLC   =C'80',WORK                                                      
         BL    *+10                                                             
         MVC   CGHINDT+4(2),=C'20' YEAR 20NN                                    
*        MVC   CGHPYTM,=C'0025'    PAYMENT TERMS HARD CODED                     
         MVC   CGHPYTM,=C'0030'    PAYMENT TERMS HARD CODED                     
         MVC   CGHSDTY,=C'RN'      SAP DOCUMENT TYPE HARD CODED                 
         MVC   CGHFILA,SPACES      FILLER A (BLANK)                             
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK2)  YYYYMMDD                    
         MVC   CGHDUDT(4),WORK2+4  DUE DATE (MMDDYYYY)                          
         MVC   CGHDUDT+4(42),WORK2                                              
         MVC   CGHDSDT,SPACES      DISCOUNT DATE (BLANK)                        
         MVI   CGHSIGN,C'+'        SIGN, ALWAYS POSITIVE                        
*                                                                               
         MVC   CGHGRAM,=18C'0'     GROSS BILL AMOUNT                            
******** MVC   CGHGRAM+18-L'BAMT(L'BAMT),BAMT                                   
         ICM   RE,15,BNET          NET BILL AMOUNT                              
         CVD   RE,DUB                                                           
         UNPK  CGHGRAM+5(13),DUB+2(6)                                           
*                                                                               
         MVC   CGHSDBA,SPACES      SIGN DBA (BLANK)                             
         MVC   CGHCUCD,SPACES      CURRENCY CODE HARD CODED, LEFT JUST          
         MVC   CGHCUCD(3),=C'USD'                                               
         MVC   CGH1099,SPACES      1099 DATA (BLANK)                            
         MVC   CGHLGID,=C'US01'    LEDGER ID HARD CODED                         
         MVC   CGHENTY,SPACES      ENTITY HARD CODED, LEFT JUST                 
         MVC   CGHENTY(3),=C'101'                                               
         MVC   CGHSRCD,SPACES      SOURCE CODE (BLANK)                          
*                                                                               
         MVC   MYBNET,BNET                                                      
         MVC   MYBACT,BACTUAL                                                   
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
         CLI   SORTSAVE+2,X'40'    FIRST TIME?                                  
         BNE   DB50                                                             
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)    YES/ STORE RECORD                            
         B     DB17                GOTO SEQ                                     
*                                                                               
DB50     CLC   SORTREC(CGSLENQ),SORTSAVE   MULTIPLE INVOICE SAME DATE           
         BNE   DB60                                                             
*                                                                               
         LA    R2,SRTSVDT          SORTSAVE DATA DESCT                          
         USING CGDATA,R2                                                        
*                                                                               
******** PACK  DUB,BAMT            BILLING GROSS (CURRENT)                      
******** CVB   R1,DUB                                                           
         ICM   R1,15,BNET          NET BILL AMOUNT (CURRENT)                    
         PACK  DUB,CGHGRAM+4(14)   BILLING GROSS (SORTSAVE)                     
         CVB   RE,DUB                                                           
         AR    R1,RE                                                            
         CVD   R1,DUB                                                           
         UNPK  CGHGRAM+5(13),DUB+2(6)                                           
*                                                                               
         B     DB17                ARE THERE MORE MULTIPLE INVOICES             
*                                                                               
DB60     DS    0H                                                               
*                                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTSAVE                                     
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         XC    MYBAMT,MYBAMT                                                    
         B     DB17                                                             
*                                                                               
DBX      CLI   SORTSAVE+2,X'40'                                                 
         BE    DBXX                                                             
*                                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTSAVE                                     
*                                                                               
DBXX     NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
***********************************************************************         
* - PROCESS SORT RECORDS: HEADER AND DETAIL RECORDS                   *         
*                         EACH HEADER SHOULD HAVE DETAIL RECORDS      *         
*                         AND HEADER $ MUST = TOTAL DETAIL $          *         
***********************************************************************         
PROCRECS NTR1                                                                   
*                                                                               
****     GOTO1 =V(PRNTBL),DMCB,=C'SORT',SORTREC,C'DUMP',200,=C'1D'              
         CLI   MYTOTFG,C'Y'        WENT FIRST TIME ALREADY?                     
         BE    PRC00                                                            
*                                                                               
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)    COPY SORTED REC INTO SORTSAVE                
*                                                                               
         LA    R3,SRTSVDT          SORTSAVE DATA DESCT                          
         USING CGDATA,R3                                                        
         CLI   CGHRETY,C'1'        MUST BE A HEADER RECORD!                     
         BE    *+18                                                             
         MVC   P(40),=C'ERR: FIRST RECORD IS NOT A HEADER RECORD'               
         BAS   RE,SPOOLIT                                                       
         B     EXIT                                                             
*                                                                               
         MVI   MYTOTFG,C'Y'                                                     
         B     EXIT                GET NEXT RECORD ON SORTER                    
         DROP  R3                                                               
*                                                                               
PRC00    DS    0X                                                               
*                                                                               
         LA    R2,SRTSVDT          POINT TO SORTSAVE DATA                       
         USING CGDATA,R2                                                        
*                                                                               
         CLC   SORTREC(CGSLENQ+1),SORTSAVE                                      
         BE    PRC05               MUST BE A DETAIL RECORD                      
         CLI   SORTSAVE+CGSLENQ,C'2'                                            
         BNE   *+12                                                             
         CLI   SORTREC+CGSLENQ,C'1'                                             
         BE    PRC10               NEW HEADER RECORD BEGINS                     
*                                                                               
         CLI   SORTSAVE+CGSLENQ,C'1'                                            
         BNE   *+12                                                             
         CLI   SORTREC+CGSLENQ,C'2'                                             
         BE    PRC10               RELEASE CURRENT HEADER RECORD                
*                                                                               
         MVC   P(26),=C'ERR: ERROR IN RECORD TYPE!'                             
         BAS   RE,SPOOLIT                                                       
*****    GOTO1 =V(PRNTBL),DMCB,=C'ERROR',SORTREC,C'DUMP',100,=C'1D'             
         B     EXIT                                                             
*                                                                               
PRC05    CLI   SORTREC+CGSLENQ,C'1'                                             
         BNE   *+26                                                             
         CLI   SORTSAVE+CGSLENQ,C'1'                                            
         BNE   *+18                                                             
         MVC   P(39),=C'ERR: 2 SAME CONSECUTIVE HEADER RECORDS!'                
         BAS   RE,SPOOLIT                                                       
         B     EXIT                                                             
         CLI   SORTREC+CGSLENQ,C'2'                                             
         BNE   *+12                                                             
         CLI   SORTSAVE+CGSLENQ,C'2'                                            
         BE    PRC20               MUST BE A DETAIL RECORD                      
         MVC   P(31),=C'ERR: EXPECTING A DETAIL RECORD!'                        
         BAS   RE,SPOOLIT                                                       
         B     EXIT                                                             
*                                                                               
PRC10    DS    0X                  HEADER RECORD                                
*                                                                               
         CLI   CGHRETY,C'1'        HEADER RECORD?                               
         BE    PRC15                                                            
         CLI   CGDRETY,C'2'        DETAIL RECORD?                               
         BNE   PRC14                                                            
         TM    ZEROBFG,ZNEG        NEG  BILLING?                                
         BO    PRC18ZB                                                          
         TM    ZEROBFG,ZHEADER     ZERO BILLING?                                
         BO    PRC18ZB                                                          
         MVI   MYHTDFG,C'Y'                                                     
         B     PRC30                                                            
*                                                                               
PRC14    MVC   P(25),=C'ERR: INVALID RECORD TYPE!'                              
         BAS   RE,SPOOLIT                                                       
         B     EXIT                                                             
*                                                                               
PRC15    NI    ZEROBFG,X'FF'-ZNEG  CLEAR NEGATIVE DOLLAR FLAG                   
         PACK  DUB,CGHGRAM+3(15)   TEST NEGATIVE                                
         CVB   R1,DUB                                                           
         LTR   R1,R1               IS IT NEGATIVE DOLLARS?                      
         BM    *+8              YES                                             
         B     PRC15ZA          NO                                              
         OI    ZEROBFG,ZNEG     NEGATIVE - SKIP HEADER AND ITS DETAILS          
         MVI   MYHTDFG,C'N'                                                     
         XC    TOTDAMT,TOTDAMT                                                  
         B     PRC18ZB                                                          
PRC15ZA  DS    0H                                                               
         PACK  DUB,CGHGRAM+1(14)   MAX LEN TO BE PACKED =14                     
         CVB   R1,DUB                                                           
         C     R1,=F'0'                                                         
         BNE   PRC15ZB             CASE OF ZERO BILLING                         
         OI    ZEROBFG,ZHEADER     SET ZERO BILLING FLAG TO YES                 
         B     PRC18ZB                                                          
*                                                                               
PRC15ZB  ICM   R1,15,TOTRECT       TOTAL RECORD COUNTER                         
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTRECT                                                    
*                                                                               
         ICM   R1,15,TOTHECT       TOTAL HEADER RECORD COUNTER                  
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTHECT                                                    
*                                                                               
         TM    CGHGRAM+17,X'D0'    IF NEGATIVE                                  
         BNO   *+8                                                              
         MVI   CGHSIGN,C'-'        SIGN BECOMES NEGATIVE                        
*                                                                               
         PACK  DUB,CGHGRAM+4(14)   MAX LEN TO BE PACKED IS 14                   
         CVB   R1,DUB                                                           
         ICM   R5,15,TOTHEAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTHEAMT      TOTAL HEADER AMOUNT                          
         ICM   R5,15,TOTREAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTREAMT      GRAND TOTAL OF ALL RECORDS                   
         STCM  R1,15,HEADAMT       FOR HEAD=TOTAL DETAIL CKING                  
*                                                                               
         MVI   MYHTDFG,C'N'        RESET CKING FLAG                             
         XC    TOTDAMT,TOTDAMT     RESET TOTAL DETAIL AMOUNT                    
*                                                                               
         MVC   P,SPACES                                                         
         LA    R5,P                                                             
         USING PLINED,R5           PRINT OUT HEADER REC                         
*                                                                               
         CLI   CGHSIGN,C'-'                                                     
         BNE   PRC16                                                            
         MVI   CGHINCL,C'C'        CREDIT MEMO                                  
         MVC   CGHSDTY,=C'KG'      CREDIT MEMO                                  
*                                                                               
PRC16    MVC   CGHINUM+2(2),SRTSVMON                                            
         MVC   MY2BYTE,SRTSVMON                                                 
*                                                                               
         OI    CGHGRAM+17,X'F0'    FOR PRINT/WRITE TO TAPE                      
*                                                                               
         MVC   PHVENN,CGHVENN                                                   
         MVC   PHINUM,CGHINUM                                                   
         MVC   PHOPID,CGHOPID                                                   
         MVC   PHINCL,CGHINCL                                                   
         MVC   PHCOMM,CGHCOMM                                                   
         MVC   PHINDT,CGHINDT                                                   
         MVC   PHPYTM,CGHPYTM                                                   
         MVC   PHSDTY,CGHSDTY                                                   
         MVC   PHDUDT,CGHDUDT                                                   
         MVC   PHSIGN,CGHSIGN                                                   
         MVC   PHGRAM,CGHGRAM                                                   
         MVC   PHCUCD,CGHCUCD                                                   
         MVC   PHLGID,CGHLGID                                                   
         MVC   PHENTY,CGHENTY                                                   
         MVC   PHRETY,CGHRETY                                                   
*                                                                               
         MVI   CGHSIGN,C'+'        SIGN IS ALWAYS PODITIVE                      
         MVC   PHSIGN,CGHSIGN      SOUNDS STUPID...                             
*                                                                               
         BAS   RE,SPOOLIT                                                       
         MVC   P,SPACES                                                         
         BAS   RE,SPOOLIT                                                       
         DROP  R5                                                               
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,=C'HEADER     ',SORTSAVE+SDATASTR,               
********       C'DUMP',256,=C'1D'                                               
*                                                                               
         BAS   RE,WRITAPE                                                       
         NI    ZEROBFG,X'FF'-ZHEADER   SET ZB HEADER FLAG TO NO                 
*                                                                               
PRC18ZB  DS    0X                  ZB = ZERO BILLING                            
*                                                                               
         LA    RF,SORTSAVE                                                      
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)    SORTSAVE NOW CONTAINS CURRENT REC            
*                                                                               
         B     PRCX                                                             
*                                                                               
***********************************************************************         
*                                                                               
PRC20    DS    0X                  DETAIL RECORD                                
*                                                                               
         TM    ZEROBFG,ZNEG        NEAGTIVE HEADER DOLLARS                      
         BO    PRC45ZB             YES, WE DON'T WANT DETAIL FOR NEG            
         TM    ZEROBFG,ZHEADER     ZERO BILLING?                                
         BO    PRC45ZB             YES, WE DON'T WANT DETAIL FOR ZB             
*                                                                               
******** PACK  DUB,CGDDISL                                                      
******** CVB   R1,DUB                                                           
******** CH    R1,=H'0'                                                         
******** BNE   *+8                                                              
******** OI    ZEROBFG,ZDETAIL     ZB DETAIL FLAG IS ON                         
*                                                                               
         CLC   CGDSGAC,=C'500008'  IS THIS PARTICULAR EST USER FIELD?           
         BE    PRC25                                                            
         CLI   SRTSVMT,C'C'        IS MEDIA TYPE CABLE?                         
         BE    PRC25                                                            
*                                                                               
         CLC   CGDINUM,SORTREC+SDATASTR+7                                       
         BNE   PRC30               INV NUMB DIFF, NO NEED TO COLLPASE           
         CLC   CGDDESP+4(2),SORTREC+SDATASTR+54                                 
         BNE   PRC30               MONTH OF SERV DIFF, NO COLLAPSE              
         CLC   CGDARDT,SORTREC+SDATASTR+98                                      
         BNE   PRC30               AIR DATE DIFF, NO NEED TO COLLAPSE           
         CLC   CGDSHOW,SORTREC+SDATASTR+107                                     
         BNE   PRC30               SHOW DIFF, NO NEED TO COLLAPSE               
         CLC   CGDSTAT,SORTREC+SDATASTR+135                                     
         BNE   PRC30               STATION DIFF, NO NEED TO COLLAPSE            
         CLC   CGDLENG,SORTREC+SDATASTR+140                                     
         BNE   PRC30               LENGTH DIFF, NO NEED TO COLLAPSE             
         B     PRC27                                                            
*                                                                               
PRC25    CLC   CGDINUM,SORTREC+SDATASTR+7                                       
         BNE   PRC30               INV NUMB DIFF, NO NEED TO COLLPASE           
         CLC   CGDDESP+4(2),SORTREC+SDATASTR+54                                 
         BNE   PRC30               MONTH OF SERV DIFF, NO COLLAPSE              
         CLC   CGDARDT,SORTREC+SDATASTR+98                                      
         BNE   PRC30               AIR DATE DIFF, NO NEED TO COLLAPSE           
         CLC   CGDCSHO,SORTREC+SDATASTR+107                                     
         BNE   PRC30               SHOW DIFF, NO NEED TO COLLAPSE               
         CLC   CGDCSTA,SORTREC+SDATASTR+132                                     
         BNE   PRC30               STATION DIFF, NO NEED TO COLLAPSE            
         CLC   CGDCLEN,SORTREC+SDATASTR+137                                     
         BNE   PRC30               LENGTH DIFF, NO NEED TO COLLAPSE             
*                                                                               
PRC27    PACK  DUB,CGDDISL                                                      
         CVB   R1,DUB                                                           
         PACK  DUB,SORTREC+SDATASTR+81(13)                                      
         CVB   RE,DUB                                                           
         AR    R1,RE                                                            
         CVD   R1,DUB                                                           
         UNPK  CGDDISL,DUB+2(6)                                                 
*                                                                               
         ICM   R1,15,TOTCOLL       NUMBER OF RECORDS COLLAPSED                  
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTCOLL                                                    
*                                                                               
         ICM   R1,15,TOTNUN        NUMBER OF REC COLLAPSED PER DETAIL           
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTNUN                                                     
*                                                                               
         B     EXIT                NEXT RECORD ON SORTER                        
*                                                                               
PRC30    DS    0X                                                               
*                                                                               
         TM    CGDDISL+12,X'D0'    IF NEGATIVE                                  
         BNO   *+8                                                              
         MVI   CGDSIGN,C'-'        SIGN CHANGES FROM POS(+) TO NEG(-)           
         PACK  DUB,CGDDISL                                                      
         CVB   R1,DUB                                                           
         ICM   R5,15,TOTDEAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTDEAMT      TOTAL DETAIL AMOUNT                          
         ICM   R5,15,TOTREAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTREAMT      GRAND TOTAL OF ALL RECORDS                   
         ICM   R5,15,TOTDAMT                                                    
         AR    R5,R1                                                            
         STCM  R5,15,TOTDAMT       TOTAL DETAIL AMT (SHOULD EQUAL HEAD)         
*                                                                               
PRC33ZB  CLI   MYHTDFG,C'Y'                                                     
         BNE   PRC35                                                            
         ICM   RE,15,HEADAMT                                                    
         ICM   R1,15,TOTDAMT                                                    
         CR    RE,R1                                                            
         BE    PRC35                                                            
         MVC   P(56),=C'ERR: HEADER AMOUNT AND TOTAL DETAIL AMOUNT ARE +        
               NOT EQUAL'                                                       
         BAS   RE,SPOOLIT                                                       
******** B     EXIT                                                             
*                                                                               
PRC35    DS    0X                                                               
*                                                                               
         MVC   P,SPACES                                                         
         LA    R5,P                                                             
         USING PLINED,R5           PRINT OUT DETAIL RECORD                      
*                                                                               
         MVC   CGDINUM+2(2),MY2BYTE                                             
         OI    CGDDISL+12,X'F0'    FOR PRINT/WRITE TO TAPE                      
*                                                                               
         MVC   PDVENN,CGDVENN                                                   
         MVC   PDINUM,CGDINUM                                                   
         MVC   PDSBAR,CGDSBAR                                                   
         MVC   PDSEP1,CGDSEP1                                                   
         MVC   PDSGAC,CGDSGAC                                                   
         MVC   PDSCCT,CGDSCCT      NEW 9 DIGIT COST CENTER                      
         MVC   PDSCCD,CGDSCCD                                                   
         MVC   PDDESP,CGDDESP                                                   
         MVC   PDSIGN,CGDSIGN                                                   
         MVC   PDDISL,CGDDISL                                                   
         MVC   PDSRIN,CGDSRIN                                                   
         MVC   PDARDT,CGDARDT                                                   
         MVC   PDSHOW,CGDSHOW                                                   
         MVC   PDSTAT,CGDSTAT                                                   
         MVC   PDLENG,CGDLENG                                                   
         MVC   PDRETY,CGDRETY                                                   
*                                                                               
         CLC   CGDSGAC,=C'500008'  IS THIS PARTICULAR EST USER FIELD?           
         BE    *+12                                                             
         CLI   SRTSVMT,C'C'        IS MEDIA TYPE CABLE?                         
         BNE   PRC36                                                            
*                                                                               
         ICM   R1,15,TOTNUN        NUMBER OF REC COLLAPSED PER DETAIL           
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTNUN                                                     
         EDIT  (B4,TOTNUN),(3,CGDCNUN),FILL=0                                   
         XC    TOTNUN,TOTNUN       RESET COUNTER FOR NEXT UNIT REC              
*                                                                               
         MVC   PDSHOW,SPACES                                                    
         MVC   PDSTAT,SPACES                                                    
         MVC   PDLENG,SPACES                                                    
         MVC   PDCSHO,CGDCSHO                                                   
         MVC   PDCSTA,CGDCSTA                                                   
         MVC   PDCLEN,CGDCLEN                                                   
         MVC   PDCNUN,CGDCNUN                                                   
*                                                                               
PRC36    PACK  DUB,CGDDISL                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'0'                                                         
         BNE   *+12                                                             
         OI    ZEROBFG,ZDETAIL     ZB DETAIL FLAG IS ON                         
         B     PRC39                                                            
*                                                                               
         ICM   R1,15,TOTDECT       TOTAL DETAIL RECORD COUNTER                  
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTDECT                                                    
*                                                                               
         ICM   R1,15,TOTRECT       TOTAL RECORD COUNTER                         
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTRECT                                                    
*                                                                               
         BAS   RE,SPOOLIT                                                       
*                                                                               
PRC39    CLI   MYHTDFG,C'Y'                                                     
         BNE   PRC40                                                            
         MVC   P,SPACES                                                         
         BAS   RE,SPOOLIT                                                       
         MVC   P(23),=C'*** DETAIL SUBTOTAL ***'                                
         EDIT  (B4,TOTDAMT),(13,P+30),MINUS=YES                                 
         BAS   RE,SPOOLIT                                                       
         MVI   FORCEHED,C'Y'       GO ON TO NEXT PAGE                           
*                                                                               
PRC40    DS    0X                                                               
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,=C'DETAIL     ',SORTSAVE+SDATASTR,               
********       C'DUMP',256,=C'1D'                                               
*                                                                               
         TM    ZEROBFG,ZDETAIL                                                  
         BO    PRC45ZB             WE DON'T CARE ABOUT ZB DETAILS               
         BAS   RE,WRITAPE                                                       
*                                                                               
PRC45ZB  LA    RF,SORTSAVE         ZB = ZERO BILLING                            
         LA    R1,L'SORTSAVE                                                    
         LA    RE,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)    SORTSAVE NOW CONTAINS CURRENT REC            
*                                                                               
         NI    ZEROBFG,X'FF'-ZDETAIL   SET ZB DETAIL FLAG TO NO                 
         B     PRCX                                                             
*                                                                               
         DROP  R5,R2                                                            
*                                                                               
***********************************************************************         
*                                                                               
PRCX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
WRITAPE  NTR1                                                                   
         B     SKIPHEX                                                          
         L     R1,CNTR                                                          
         C     R1,=F'10'                                                        
         BH    SKIPHEX                                                          
         LA    R1,1(R1)                                                         
         ST    R1,CNTR                                                          
         GOTO1 HEXOUT,DMCB,SRTSVDT,P,300                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
SKIPHEX  CLI   TAPEOPT,C'Y'                                                     
         BNE   EXIT                                                             
         L     R1,ANTWKTP                                                       
         PUT   (R1),SRTSVDT                   WRITE TAPE FROM SORTSAVE          
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        PRINT/WRITE OUT LAST RECORD                                  *         
***********************************************************************         
LASTREC  NTR1                                                                   
*                                                                               
         LA    R2,SRTSVDT          POINT TO SORTSAVE DATA                       
         USING CGDATA,R2                                                        
*                                                                               
         CLI   CGDRETY,C'2'                                                     
         BE    LAST10                                                           
         MVC   P(43),=C'ERR: HEADER RECORD WITH OUT DETAIL RECORDS!'            
         BAS   RE,SPOOLIT                                                       
         B     LASTRECX                                                         
*                                                                               
LAST10   DS    0X                                                               
*                                                                               
         TM    ZEROBFG,ZNEG                                                     
         BO    LASTRECX                                                         
         PACK  DUB,CGDDISL                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'0'                                                         
         BE    LAST35                                                           
         TM    ZEROBFG,ZHEADER     ZERO BILLING?                                
         BO    LASTRECX                                                         
*                                                                               
         ICM   R1,15,TOTDECT       TOTAL DETAIL RECORD COUNTER                  
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTDECT                                                    
*                                                                               
         ICM   R1,15,TOTRECT       TOTAL RECORD COUNTER                         
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTRECT                                                    
*                                                                               
         TM    CGDDISL+12,X'D0'    IF NEGATIVE                                  
         BNO   *+8                                                              
         MVI   CGDSIGN,C'-'        SIGN CHANGES FROM POS(+) TO NEG(-)           
         PACK  DUB,CGDDISL                                                      
         CVB   R1,DUB                                                           
         ICM   R5,15,TOTDEAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTDEAMT      TOTAL DETAIL AMOUNT                          
         ICM   R5,15,TOTREAMT                                                   
         AR    R5,R1                                                            
         STCM  R5,15,TOTREAMT      GRAND TOTAL OF ALL RECORDS                   
         ICM   R5,15,TOTDAMT                                                    
         AR    R5,R1                                                            
         STCM  R5,15,TOTDAMT       TOTAL DETAIL AMT (SHOULD EQUAL HEAD)         
*                                                                               
         ICM   RE,15,HEADAMT                                                    
         ICM   R1,15,TOTDAMT                                                    
         CR    RE,R1                                                            
         BE    LAST22                                                           
         MVC   P(63),=C'ERR: HEADER AMOUNT AND TOTAL DETAIL AMOUNT ARE +        
               NOT EQUAL (LAST)'                                                
         BAS   RE,SPOOLIT                                                       
******** B     LASTRECX                                                         
*                                                                               
LAST22   MVC   P,SPACES                                                         
         LA    R5,P                                                             
         USING PLINED,R5           LAST RECORD (DETAIL)                         
*                                                                               
         MVC   CGDINUM+2(2),MY2BYTE                                             
         OI    CGDDISL+12,X'F0'    FOR PRINT/WRITE TO TAPE                      
*                                                                               
         MVC   PDVENN,CGDVENN                                                   
         MVC   PDINUM,CGDINUM                                                   
         MVC   PDSBAR,CGDSBAR                                                   
         MVC   PDSEP1,CGDSEP1                                                   
         MVC   PDSGAC,CGDSGAC                                                   
         MVC   PDSCCT,CGDSCCT                                                   
         MVC   PDSCCD,CGDSCCD                                                   
         MVC   PDDESP,CGDDESP                                                   
         MVC   PDSIGN,CGDSIGN                                                   
         MVC   PDDISL,CGDDISL                                                   
         MVC   PDSRIN,CGDSRIN                                                   
         MVC   PDARDT,CGDARDT                                                   
         MVC   PDSHOW,CGDSHOW                                                   
         MVC   PDSTAT,CGDSTAT                                                   
         MVC   PDLENG,CGDLENG                                                   
         MVC   PDRETY,CGDRETY                                                   
*                                                                               
         CLC   CGDSGAC,=C'500008'  IS THIS PARTICULAR EST USER FIELD?           
         BE    *+12                                                             
         CLI   SRTSVMT,C'C'        IS MEDIA TYPE CABLE?                         
         BNE   LAST30                                                           
*                                                                               
         ICM   R1,15,TOTNUN        NUMBER OF REC COLLAPSED PER DETAIL           
         LA    R1,1(R1)                                                         
         STCM  R1,15,TOTNUN                                                     
         EDIT  (B4,TOTNUN),(3,CGDCNUN),FILL=0                                   
         XC    TOTNUN,TOTNUN       RESET COUNTER FOR NEXT UNIT REC              
*                                                                               
         MVC   PDSHOW,SPACES                                                    
         MVC   PDSTAT,SPACES                                                    
         MVC   PDLENG,SPACES                                                    
         MVC   PDCSHO,CGDCSHO                                                   
         MVC   PDCSTA,CGDCSTA                                                   
         MVC   PDCLEN,CGDCLEN                                                   
         MVC   PDCNUN,CGDCNUN                                                   
*                                                                               
LAST30   DS    0X                                                               
         BAS   RE,SPOOLIT                                                       
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,=C'DETAIL LAST',SORTSAVE+SDATASTR,               
********       C'DUMP',256,=C'1D'                                               
*                                                                               
         BAS   RE,WRITAPE                                                       
*                                                                               
LAST35   MVC   P,SPACES                                                         
         BAS   RE,SPOOLIT                                                       
         MVC   P(23),=C'*** DETAIL SUBTOTAL ***'                                
         EDIT  (B4,TOTDAMT),(13,P+30),MINUS=YES                                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
         DROP  R5,R2                                                            
*                                                                               
LASTRECX B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        PRINT/WRITE OUT SUMMARY RECORD                               *         
***********************************************************************         
SUMMARY  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'       GO ON TO NEXT PAGE                           
*                                                                               
         MVI   SORTSAVE,X'40'      INITIALIZE SORTSAVE TO SPACES                
         LA    RF,SORTSAVE+1       BUILDING SUMMARY RECORD                      
         LA    R1,L'SORTSAVE-1                                                  
         LA    RE,SORTSAVE                                                      
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    R2,SRTSVDT          POINT TO SORTSAVE DATA                       
         USING CGDATA,R2                                                        
*                                                                               
         MVC   CGTREID,=20C'9'     RECORD ID, HARD CODED TWENTY 9'S             
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',TEMPDATE)                          
         MVC   CGTCRDT(4),TEMPDATE+2                                            
         MVC   CGTCRDT+4(2),=C'19'                                              
         MVC   CGTCRDT+6(2),TEMPDATE                                            
         CLC   =C'80',TEMPDATE                                                  
         BL    *+10                                                             
         MVC   CGTCRDT+4(2),=C'20' YEAR 20NN                                    
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB,4,=C'TOG'                               
         MVC   CGTCRTM,DUB                                                      
*                                                                               
         EDIT  (B4,TOTRECT),(10,CGTTRCT),FILL=0                                 
         EDIT  (B4,TOTHECT),(10,CGTHRCT),FILL=0                                 
         EDIT  (B4,TOTDECT),(10,CGTDRCT),FILL=0                                 
*                                                                               
         EDIT  (B4,TOTHEAMT),(17,CGTGRAM+2),FILL=0,ALIGN=RIGHT                  
         EDIT  (B4,TOTDEAMT),(17,CGTDIAM+2),FILL=0,ALIGN=RIGHT                  
         MVC   CGTGRAM(2),=C'00'                                                
         MVC   CGTDIAM(2),=C'00'                                                
*                                                                               
         CLC   TOTHEAMT,TOTDEAMT                                                
         BE    *+14                                                             
         MVC   P(64),=C'ERR: TOTAL HEADER REC AMOUNT DOES NOT TO TOTAL +        
               DETAIL REC AMOUNT!'                                              
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVI   CGTRETY,C'9'        SUMMARY RECORD HAS TYPE OF C'9'              
*                                                                               
         MVC   P(43),=C'SUMMARY RECORD - TOTAL OF HEADER AND DETAIL'            
         BAS   RE,SPOOLIT                                                       
         MVC   P,SPACES                                                         
         BAS   RE,SPOOLIT                                                       
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,=C'SUMMARY    ',SORTSAVE+SDATASTR,               
********       C'DUMP',256,=C'1D'                                               
*                                                                               
         BAS   RE,WRITAPE                                                       
*                                                                               
         LA    R5,P                                                             
         USING PLINED,R5           PRINT OUT SUMMARY RECORD                     
*                                                                               
         MVC   PTREID,CGTREID                                                   
         MVC   PTCRDT,CGTCRDT                                                   
         MVC   PTCRTM,CGTCRTM                                                   
         MVC   PTTRCT,CGTTRCT                                                   
         MVC   PTHRCT,CGTHRCT                                                   
         MVC   PTGRAM,CGTGRAM                                                   
         MVC   PTDRCT,CGTDRCT                                                   
         MVC   PTDIAM,CGTDIAM                                                   
         MVC   PTRETY,CGTRETY                                                   
*                                                                               
         BAS   RE,SPOOLIT                                                       
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
SUMMARYX B     EXIT                                                             
*                                                                               
***********************************************************************         
*                                                                     *         
* TABLE FOR CONVERTING MONTH NUMBER INTO MONTH                        *         
*                                                                     *         
***********************************************************************         
MYMONTH  DS    XL1                 MONTH NUMBER TO MONTH WORK FIELD             
*                                                                               
MONCONTB DS    0X                                                               
*        DC    XL1'00',CL2'04'     YNR START USING NEW INV NUMB ON APR          
*MONCONLQ EQU   *-MONCONTB                                                      
*         DC    XL1'01',CL2'05'                                                 
*         DC    XL1'02',CL2'06'                                                 
*         DC    XL1'03',CL2'07'                                                 
*         DC    XL1'04',CL2'08'                                                 
*         DC    XL1'05',CL2'09'                                                 
*         DC    XL1'06',CL2'10'                                                 
*         DC    XL1'07',CL2'11'                                                 
*         DC    XL1'08',CL2'12'                                                 
*         DC    XL1'09',CL2'01'                                                 
*         DC    XL1'0A',CL2'02'                                                 
*         DC    XL1'0B',CL2'03'                                                 
*         DC    XL1'FF'                                                         
*                                                                               
         DC    XL1'00',CL2'08'     NEW TABLE 8/03 AS INVOICE NUMBER             
MONCONLQ EQU   *-MONCONTB          WENT FROM 99 IN JULY TO 00 IN AGU            
         DC    XL1'01',CL2'09'                                                  
         DC    XL1'02',CL2'10'                                                  
         DC    XL1'03',CL2'11'                                                  
         DC    XL1'04',CL2'12'                                                  
         DC    XL1'05',CL2'01'                                                  
         DC    XL1'06',CL2'02'                                                  
         DC    XL1'07',CL2'03'                                                  
         DC    XL1'08',CL2'04'                                                  
         DC    XL1'09',CL2'05'                                                  
         DC    XL1'0A',CL2'06'                                                  
         DC    XL1'0B',CL2'07'                                                  
         DC    XL1'FF'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        HEADER                                                      *          
***********************************************************************         
MYHEAD   NTR1                                                                   
         MVC   P,SPACES                                                         
         MVC   P(12),=C'INVOICE DATE'                                           
         MVC   P+17(14),=C'INVOICE NUMBER'                                      
         MVC   P+36(11),=C'CLT/PRD/EST'                                         
         MVC   P+52(10),=C'DUE DATE  '                                          
         MVC   P+67(13),=C'NET UNIT COST'                                       
         MVC   P+89(10),=C'UNIT DATE '                                          
         BAS   RE,SPOOLIT                                                       
         MVC   P,SPACES                                                         
         MVC   P(12),=C'------------'                                           
         MVC   P+17(14),=C'--------------'                                      
         MVC   P+36(11),=C'-----------'                                         
         MVC   P+52(10),=C'----------'                                          
         MVC   P+67(17),=C'-----------------'                                   
         MVC   P+89(10),=C'----------'                                          
         BAS   RE,SPOOLIT                                                       
*                                                                               
MYHEADX  B     EXIT                                                             
*                                                                               
***********************************************************************         
*        PUTS RECORD TO SORTER                                       *          
***********************************************************************         
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*                                                                               
SPOOLIT  NTR1                                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
***********************************************************************         
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
*                                                                               
***********************************************************************         
*                                                                               
HEADING  SSPEC H2,1,REQUESTOR                                                   
******** SSPEC H1,52,C' AT'                                                     
******** SSPEC H1,55,CL1'&&'                                                    
******** SSPEC H1,56,C'T TAPE (HARD COPY)'                                      
         SSPEC H1,52,C' COLGATE TAPE (HARD COPY) '                              
         SSPEC H2,52,C' ------------------------ '                              
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
******** SSPEC H3,1,C'VENDOR NUMBER: 1252071'                                   
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00256,                                            X        
               BLKSIZE=02560,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
MYIO     DS    CL2000                                                           
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*        INITIALIZE WORKING STORAGE AREAS                             *         
***********************************************************************         
INITWKA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TOTRECT,TOTRECT                                                  
         XC    TOTHECT,TOTHECT                                                  
         XC    TOTDECT,TOTDECT                                                  
         XC    TOTCOLL,TOTCOLL     # OF UNIT REC COLLAPSED (INTERNAL)           
         XC    TOTNUN,TOTNUN       # OF REC COLLAPSED PER DETAIL                
         XC    TOTREAMT,TOTREAMT                                                
         XC    TOTHEAMT,TOTHEAMT                                                
         XC    TOTDEAMT,TOTDEAMT                                                
         XC    HEADAMT,HEADAMT     HEADER AMOUNT                                
         XC    TOTDAMT,TOTDAMT     TOTAL DETAIL AMT (MUST EQUAL HEADER)         
*                                                                               
         XC    MYBAMT,MYBAMT                                                    
*                                                                               
         MVI   MYTOTFG,C'N'                                                     
         MVI   MYHTDFG,C'N'                                                     
         MVI   ZEROBFG,X'00'       ZERO BILLING FLAG                            
*                                                                               
INITWKAX XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        READ ESTIMATE HEADER FOR EST DESCRIPTION AND USER FIELD      *         
*        CALLED FROM BILLING RECORD READ                              *         
***********************************************************************         
ESTHEAD  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING BILLREC,R6                                                       
         MVC   KEYSV,KEY           SAVE BILL REC KEY                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BKEYAM                                                  
         MVC   KEY+2(6),BKEYCLT        CLT/PROD/EST                             
         CLC   CURESTDK,KEY            DO WE ALREADY HAVE IT                    
         BE    ESTHX                   YES                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),(KEYSAVE)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURESTDK,KEY                                                     
         L     R1,=A(MYIO)                                                      
         ST    R1,AIO                                                           
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING EKEY,R4                                                          
         MVC   ESTIDESC,EDESC             SAVE ESTIMATE DESCRIPTION             
         MVC   USER1,EUSER1               SAVE USER1 FIELD                      
         MVC   USER2,EUSER2               SAVE USER2 FIELD                      
*                                                                               
ESTHX    XC    KEY,KEY                                                          
         MVC   KEY(L'KEYSV),KEYSV         RESTORE ...BILLREC KEYS               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                               ...READ SEQUENCE              
         MVC   AIO,ANETWS2                        ...I/O AREA                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        TO GET PRD CODE FROM C LIST                                  *         
***********************************************************************         
GETPRD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   CURPROD,=C'***'    SET TO UNDEFINED                              
         B     GPX                                                              
GP12     CLC   BYTE,3(R2)                                                       
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   CURPROD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPX      XIT1                                                                   
         LTORG                                                                  
*                                                                               
UNTIO    DS    CL4000                                                           
NEWBLRD  DS    CL256                                                            
*                                                                               
*                                                                               
WORKD    DSECT                     MY WORK AREA  ANETWS4                        
*                                                                               
MYWORK   DS    D                   *  FROM EDIT MODULE *                        
RELO     DS    F                   *                   *                        
ACLISTSV DS    F                   *                   *                        
TAPEOPT  DS    CL1                 *                   *                        
CLIENT   DS    CL2                 *                   *                        
PRODUCT  DS    CL3                 *                   *                        
STARTDAT DS    CL2                 *                   *                        
ENDDAT   DS    CL2                 *                   *                        
STRBDAT  DS    CL6                 *                   *                        
ENDBDAT  DS    CL6                 *                   *                        
CLTOFFC  DS    CL1                 *  FROM EDIT MODULE *                        
*                                                                               
ANTWKTP  DS    F                                                                
CNTR     DS    F                                                                
CURPROD  DS    CL3                                                              
MYBYTE   DS    CL1                                                              
KEYSV    DS    CL20                                                             
WORK2    DS    CL40                                                             
ESTIDESC DS    CL20                                                             
USER1    DS    CL32                                                             
USER2    DS    CL16                                                             
CURESTDK DS    CL13                CURRENT EST DESCRIPTION KEY                  
*                                                                               
TOTRECT  DS    CL4                 TOTAL RECORD COUNTER                         
TOTHECT  DS    CL4                 HEADER RECORD COUNTER                        
TOTDECT  DS    CL4                 DETAIL RECORD COUNTER                        
TOTCOLL  DS    CL4                 NUMBER OF DETAIL RECORDS COLLAPSED           
TOTNUN   DS    CL4                 NUMBER OF RECS COLLAPSED PER DETAIL          
TOTREAMT DS    CL4                 TOTAL AMOUNT OF ALL REOCRDS (TEST)           
TOTHEAMT DS    CL4                 TOTAL AMOUNT FROM HEADER RECORDS             
TOTDEAMT DS    CL4                 TOTAL AMOUNT FROM DETAIL RECORDS             
HEADAMT  DS    CL4                 HEADER AMOUNT                                
TOTDAMT  DS    CL4                 TOTAL DETAIL AMT (MUST EQUAL HEADER)         
*                                                                               
MY2BYTE  DS    CL2                 FOR WORKING PURPOSES                         
MYEUSER  DS    CL20                UNIT RECORD, SAP GL ACCOUNT                  
MYPUSER  DS    CL20                UNIT RECORD, SAP COST CENTER                 
*                                                                               
MYWORK1  DS    CL20                                                             
MYWORK2  DS    CL20                                                             
WK16BYTE DS    CL16                FOR MODIFYING NULLS IN PROG NAME             
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13MNTHS X 6                          
*                                                                               
MYBAMT   DS    CL4                                                              
MYBNET   DS    CL4                                                              
MYBACT   DS    CL4                                                              
*                                                                               
MOBILADS DS    4F                  MOBILE ADDRESSES                             
*                                                                               
TEMPDATE DS    CL6                 TEMP DATE WORKING STORAGE                    
MYDATE   DS    CL6                 " " "                                        
MYDATE2  DS    CL6                 " " "                                        
MY12BYTE DS    CL12                USED FOR GETBROAD                            
*                                                                               
MYTOTFG  DS    CL1                 FLAG FOR TOTAL (SUMMARY) RECORD              
MYHTDFG  DS    CL1                 FLAG FOR CK HEAD = TOTAL DETAIL              
ZEROBFG  DS    CL1                 FLAG FOR ZERO BILLING                        
ZHEADER  EQU   X'80'               HEADER ZERO BILLINGS                         
ZDETAIL  EQU   X'40'               DETAIL ZERO BILLINGS                         
ZNEG     EQU   X'20'               HEADER IS NEGATIVE DOLLARS                   
*                                                                               
         DS    0D                                                               
         DC    C'**SORT**'                                                      
SORTREC  DS    CL(SORTLENQ)                                                     
         ORG   SORTREC                                                          
CGSYRMON DS    CL2                 YEAR/MONTH OF SERVICE (NOT USED)             
CGSINVDT DS    CL6                 INVOICE DATE                                 
CGSINVNO DS    CL6                 INVOICE NUMBER                               
CGSCLT   DS    CL3                                                              
CGSPROD  DS    CL3                                                              
CGSEST   DS    CL3                                                              
CGSLENQ  EQU   *-CGSYRMON                                                       
CGSTYPE  DS    CL1                 TYPE HEADER=1,DETAIL=2                       
CGSMEDT  DS    CL1                 MEDIA TYPE: N, S, C, AND O                   
CGSMON   DS    CL2                 MONTH NUMBER USED BY YNR                     
CGSARDT  DS    CL8                 AIR DATE FOR COLLAPSING DETAIL REC           
CGSSHOW  DS    CL16                PRG NAME FOR COLLAPSING DETAIL REC           
CGSSTAT  DS    CL4                 STATION FOR COLLAPSING DETAIL REC            
CGSLENG  DS    CL3                 LENGTH FOR COLLAPSING DETAIL REC             
SDATASTR EQU   *-CGSYRMON          DISPLACEMENT WHERE SORT DATA STARTS          
CGSDATA  DS    CL503                                                            
CGSDLENQ EQU   *-CGSDATA                                                        
SORTLENQ EQU   *-CGSYRMON                                                       
*                                                                               
SORTSAVE DS    CL(SORTLENQ)                                                     
         ORG   SORTSAVE                                                         
SRTSV    DS    CL2                 YEAR/MONTH OF SERVICE (NOT USED)             
SSVINVDT DS    CL6                 INVOICE DATE                                 
SSVINVNB DS    CL6                 INVOICE NUMBER                               
SSVCLT   DS    CL3                                                              
SSVPRD   DS    CL3                                                              
SSVEST   DS    CL3                                                              
SRTSVQ   EQU   *-SRTSV                                                          
         DS    CL1                 TYPE HEADER=1,DETAIL=2                       
SRTSVMT  DS    CL1                 MEDIA TYPE: N, S, C, AND O                   
SRTSVMON DS    CL2                 SORTSAVE MONTH NUMBER USED BY YNR            
         DS    CL8                 AIR DATE FOR COLLAPSING DETAIL REC           
         DS    CL16                PRG NAME FOR COLLAPSING DETAIL REC           
         DS    CL4                 STATION FOR COLLAPSING DETAIL REC            
         DS    CL3                 LENGTH FOR COLLAPSING DETAIL REC             
SRTSVSTR EQU   *-SRTSV             POSITION WHERE SORTSAVE DATA BEGINS          
SRTSVDT  DS    CL503               SORT SAVE DATA                               
*                                                                               
WORKLENQ EQU   *-RELO                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
PLINED   DSECT                     DSECT FOR PRINTED REPORT                     
PSTART   DS    0X                                                               
*                                                                               
PHVENN   DS    CL7                 HEAD RECORD                                  
         DS    CL1                                                              
PHINUM   DS    CL10                                                             
         DS    CL1                                                              
PHOPID   DS    CL3                                                              
         DS    CL1                                                              
PHINCL   DS    CL1                                                              
         DS    CL1                                                              
PHCOMM   DS    CL12                                                             
         DS    CL1                                                              
PHINDT   DS    CL8                                                              
         DS    CL1                                                              
PHPYTM   DS    CL4                                                              
         DS    CL1                                                              
PHSDTY   DS    CL2                                                              
         DS    CL1                                                              
PHDUDT   DS    CL8                                                              
         DS    CL1                                                              
PHSIGN   DS    CL1                                                              
PHGRAM   DS    CL18                                                             
         DS    CL1                                                              
PHCUCD   DS    CL4                                                              
         DS    CL1                                                              
PHLGID   DS    CL4                                                              
         DS    CL1                                                              
PHENTY   DS    CL4                                                              
         DS    CL1                                                              
PHRETY   DS    CL1                                                              
*                                                                               
         ORG   PSTART              DETAIL RECORD                                
*                                                                               
PDVENN   DS    CL7                                                              
         DS    CL1                                                              
PDINUM   DS    CL10                                                             
         DS    CL1                                                              
PDSBAR   DS    CL3                                                              
PDSEP1   DS    CL1                                                              
PDSGAC   DS    CL6                                                              
PDSCCT   DS    CL9                                                              
PDSCCD   DS    CL4                                                              
         DS    CL1                                                              
PDDESP   DS    CL18                                                             
         DS    CL1                                                              
PDSIGN   DS    CL1                                                              
PDDISL   DS    CL13                                                             
         DS    CL1                                                              
PDSRIN   DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
PDARDT   DS    CL8                                                              
         DS    CL1                                                              
PDSHOW   DS    CL27                                                             
         DS    CL1                                                              
PDSTAT   DS    CL4                                                              
         DS    CL1                                                              
PDLENG   DS    CL3                                                              
*                                                                               
         ORG   PDSHOW                                                           
*                                                                               
PDCSHO   DS    CL24                                                             
         DS    CL1                                                              
PDCSTA   DS    CL4                                                              
         DS    CL1                                                              
PDCLEN   DS    CL3                                                              
         DS    CL1                                                              
PDCNUN   DS    CL3                                                              
         DS    CL1                                                              
PDRETY   DS    CL1                                                              
*                                                                               
         ORG   PSTART              SUMMARY RECORD                               
PTREID   DS    CL20                                                             
         DS    CL1                                                              
PTCRDT   DS    CL8                                                              
         DS    CL1                                                              
PTCRTM   DS    CL6                                                              
         DS    CL1                                                              
PTTRCT   DS    CL10                                                             
         DS    CL1                                                              
PTHRCT   DS    CL10                                                             
         DS    CL1                                                              
PTGRAM   DS    CL19                                                             
         DS    CL1                                                              
PTDRCT   DS    CL10                                                             
         DS    CL1                                                              
PTDIAM   DS    CL19                                                             
         DS    CL1                                                              
PTRETY   DS    CL1                                                              
*                                                                               
PTLENQ   EQU   *-PSTART                                                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDCOLGATD         COLGATE TAPE DSECT                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID3D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETBILLRD                                                      
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018NEWRI87   03/22/07'                                      
         END                                                                    
