*          DATA SET NERES02    AT LEVEL 063 AS OF 05/01/02                      
*PHASE T32102,*                                                                 
T32102   TITLE '-   FLEXI'                                                      
T32102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FLEXI*,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              VALIDATE REQUEST FIELDS (KEY)                                    
         SPACE 3                                                                
VKEY     MVI   PRINTOPT,0          CLEAR OUT PRINTOPT                           
         LA    R2,FLESRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'FF'        EVERYTHING ALLOWED                           
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,FLEPSTRH         POSSIBLE START/END                           
         GOTO1 VVALSEND                                                         
         SPACE 1                                                                
         LA    R2,FLEBOOKH         VALIDATE BOOK                                
         MVI   MAX,1                                                            
         CLI   FLESRCE,C'P'                                                     
         BNE   *+8                                                              
         MVI   MAX,2               (2 FOR PROGRAM FILE)                         
         GOTO1 VVALBOOK                                                         
         SPACE 1                                                                
         LA    R2,FLEDEMOH         VALIDATE DEMOS                               
         MVI   MAX,20              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   NUMDEMS(1),ACTUAL                                                
         LA    R3,DEMOS                                                         
         CLI   ACTUAL,6            REALLY ALLOW 6 DEMOS                         
         BNH   VKEY20              (IF COST FIELD USED, ONLY ALLOW 5)           
         MVC   CONHEAD(L'MANYDEM6),MANYDEM6                                     
         B     MYEND                                                            
         SPACE 1                                                                
VKEY20   LA    R2,FLEDAYH          VALIDATE DAY/DETAIL FIELDS                   
         GOTO1 VVALDYTM,PARAS,8    8 DAY/DETAIL FIELDS                          
         SPACE 1                                                                
         LA    R2,FLENETH          VALIDATE NETWORK(S)                          
         LA    R3,9                (MAX 9)                                      
         LA    R5,5                (5 BYTES FOR EACH)                           
         CLI   FLESRCE,C'P'        EXCEPT PROGRAMS                              
         BNE   *+12                                                             
         LA    R3,8                ARE LIMITED TO 8                             
         LA    R5,7                AS THEY NEED 7 BYTES                         
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY30   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         AR    R4,R5                                                            
VKEY35   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY40                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY30                                                           
         B     VKEY35                                                           
         SPACE 1                                                                
VKEY40   LA    R2,FLEOPTH          OPTIONS                                      
         BAS   RE,VALOPT                                                        
         LA    R2,FLETITLH         OWN TITLE                                    
         GOTO1 VVALTITL                                                         
         SPACE 1                                                                
         LA    R2,FLEFILTH         POSSIBLE FILTERS                             
         GOTO1 VVALFILT                                                         
         SPACE 1                                                                
         LA    R2,FLEMINDH         POINT TO MINIMUM DEMO                        
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY70                                                           
         OI    PRINTOPT,X'40'      MIN DEMO FILTER USED                         
         GOTO1 VVALDLVL            EDIT FIELD                                   
         MVC   DEMOMIN,WORK+2                                                   
         SPACE 1                                                                
VKEY70   MVC   DEMOMAX,=X'FFFF'                                                 
         LA    R2,FLEMAXDH         POINT TO MAXIMUM DEMO                        
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY80                                                           
         OI    PRINTOPT,X'20'      MAX DEMO FILTER USED                         
         GOTO1 VVALDLVL            EDIT FIELD                                   
         MVC   DEMOMAX,WORK+2                                                   
         SPACE 1                                                                
VKEY80   LA    R2,FLEMAXRH         POINT TO RANK MAX                            
         MVC   PADMAX,=H'100'      ASSUME MAX OF 100                            
         MVC   PADWORST,=X'FFFF'   INITIALIZE PADWORST                          
         SPACE 1                                                                
         CLI   5(R2),0             INPUT IS OPTIONAL                            
         BE    VKEY90                                                           
         SPACE 1                                                                
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BO    *+12                                                             
         MVI   ERROR,3                                                          
         B     ERREND                                                           
         SPACE 1                                                                
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,PADMAX                                                        
         SPACE 1                                                                
VKEY90   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VALOPT   NTR1                                                                   
         MVI   SPACOPT,1           PRESET VALUES FOR OPTIONS                    
         MVI   BOXOPT,C'Y'                                                      
         MVI   DAYOPT,C'P'         P=POCKETPIECE                                
         SPACE 1                                                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK)                                      
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOPT                                                           
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
OPT2     CLC   12(3,R3),=C'BOX'                                                 
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(2,R3),=C'S  '                                                 
         BNE   OPT6                                                             
         MVI   SPACOPT,2                                                        
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         MVI   SPACOPT,3                                                        
         CLI   22(R3),C'3'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT6     CLC   12(3,R3),=C'DAY'                                                 
         BNE   OPT9                                                             
         CLI   22(R3),C'I'         I=INDIVIDUAL DAYS                            
         BNE   BADOPT                                                           
         MVC   DAYOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     CLC   12(3,R3),=C'DPT'    DAYPART OPTION                               
         BNE   OPT11                                                            
         CLI   FLESRCE,C'P'        ONLY FOR PROGRAM FLAVOR                      
         BNE   BADOPT                                                           
         MVC   DPTOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT11    B     BADOPT                                                           
         SPACE 1                                                                
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
         SPACE 1                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,BUFF                                                          
         ST    R1,AMYPAD                                                        
         XC    PADLINES,PADLINES                                                
         MVC   PADMAX,=H'100'      ASSUME MAX OF 100                            
         MVC   PADWORST,=X'FFFF'                                                
         LA    R2,FLEMAXRH                                                      
         CLI   5(R2),0                                                          
         BE    PREP2                                                            
         TM    4(R2),X'08'         UNLESS NUMBER IS SPECIFIED                   
         BNO   PREP2                                                            
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    PREP2                                                            
         STH   R0,PADMAX                                                        
         SPACE 1                                                                
PREP2    MVC   RESTITLE,=CL40'FLEXI DEMOGRAPHIC RANKING'                        
         SPACE 1                                                                
         CLI   FLESRCE,C'P'                                                     
         BNE   *+8                                                              
         BAS   RE,HUTINIT          DEAL WITH HUTS                               
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 2                                                                
         LA    R3,9                UP TO 9 NETWORKS                             
         LA    R2,NETSAVE          LIST OF NETWORKS IN DBSELSTA FMT             
         SPACE 1                                                                
PREP20   OC    0(5,R2),0(R2)       ANOTHER NETWORK                              
         BNZ   PREP40                                                           
         SPACE 1                                                                
PREP30   OC    PADLINES,PADLINES   ANY LINES?                                   
         BZ    XIT                                                              
         BAS   RE,RANK             RANK                                         
         BAS   RE,SPLAT            AND PRINT REPORT                             
         B     XIT                                                              
         SPACE 1                                                                
PREP40   CLI   FLESRCE,C'P'        COMPLETELY DIFFERENT FOR PROGS               
         BE    PROG2                                                            
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         MVC   DBSELSTA,0(R2)                                                   
         GOTO1 VADJSEL                                                          
         MVC   DBSELBK,BOOK+1                                                   
*                                                                               
         LA    R6,DAYTMLST         POINT TO DAY/TIME LIST                       
         ZIC   R5,DAYTIMES         NO. OF ENTRIES IN LIST                       
         SPACE 1                                                                
PREP50   ZIC   R1,0(R6)            GET DAY                                      
         LA    R1,DAYBITS(R1)                                                   
         MVC   DBSELDAY,0(R1)                                                   
         MVI   DBBEST,0                                                         
         CLI   DBSELDAY,X'7C'      ASK FOR EXACT MATCH FOR M-F                  
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
         CLI   DBSELDAY,X'7F'      ASK FOR EXACT MATCH FOR M-S                  
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
         CLI   0(R6),X'FF'         ASK FOR ALL FOR ALL                          
         BNE   PREP60                                                           
         MVI   DBSELDAY,X'7F'                                                   
         MVI   DBBEST,C'A'                                                      
*                                                                               
         EJECT                                                                  
PREP60   MVC   DBSELTIM,1(R6)      MOVE IN SELECTED TIME                        
         CLI   1(R6),X'FF'                                                      
         BNE   *+16                                                             
         MVC   DBSELTIM(2),=H'600'                                              
         MVC   DBSELTIM+2(2),=H'545'                                            
         MVC   DBDAYOPT,DAYOPT     P (POCKETPIECE) OR I (INDIVIDUAL)            
         SPACE 1                                                                
         GOTO1 DEMAND,DMCB,DBLOCKD,FILL                                         
         OC    DBDIVSOR,DBDIVSOR   TEST FOR ANY RECORDS                         
         BZ    *+14                NO                                           
         CLI   DBERROR,X'80'       TEST FOR EOF                                 
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         SPACE 1                                                                
         LA    R6,5(R6)            NEXT DAY/TIME                                
         BCT   R5,PREP50                                                        
         LA    R2,5(R2)            NEXT NETWORK                                 
         BCT   R3,PREP20                                                        
         B     PREP30                                                           
         SPACE 3                                                                
DAYBITS  DC    X'7C402010080402017F8003'                                        
         SPACE 1                                                                
* BITS REPRESENT M-F,MON,TUE,WED,THUR,FRI,SAT,SUN,M-SU,VAR,SA-SU                
         EJECT                                                                  
*              CONTROL I/O - HANDLING PROGRAM RECORDS                           
         SPACE 3                                                                
         DS    0H                                                               
PROG2    EQU   *                                                                
         LA    R4,BLOCK                                                         
         USING GUVD,R4                                                          
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVDATE,PSTART                                                   
         XC    PUEL,PUEL                                                        
         MVI   PUEL,X'31'                                                       
         MVI   PUEL+1,167                                                       
         MVI   PUEL+2,X'44'                                                     
         LA    R1,PUEL+3                                                        
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2           (HUNDREDS)                                   
         MVC   GUVAREC,AIO                                                      
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         MVI   LASTHDAY,X'FF'                                                   
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         XC    NPGKEY,NPGKEY                                                    
         XC    THISPRG,THISPRG                                                  
         MVC   NPGKTYP,=X'0DA0'                                                 
         MVC   NPGKAM,BINAGYMD                                                  
         MVC   NPGKNET,5(R2)                                                    
         GOTO1 HIGH                                                             
         B     PROG6                                                            
         SPACE 2                                                                
PROG4    LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         SPACE 2                                                                
PROG6    CLC   KEY(5),KEYSAVE      CHECK STATION C/B                            
         BE    PROG7                                                            
         LA    R2,7(R2)                                                         
         B     PREP20                                                           
         SPACE 2                                                                
PROG7    CLC   NPGKEND,PSTART      DATE FILTERS                                 
         BL    PROG4                                                            
         CLC   NPGKEND,PEND                                                     
         BH    PROG4                                                            
         MVC   THISPRG,NPGKPROG                                                 
         SPACE 2                                                                
PROG8    GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         DROP  R4                                                               
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R4                                                       
         XC    DUB,DUB                                                          
         MVC   DUB(4),NPGFILT      PROGRAM FILTERS                              
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   PROG4                                                            
         MVC   COST,NPGCOST                                                     
         LA    R1,DAYTMLST         SEE IF PROGRAM QUALIFIES                     
         ZIC   R0,DAYTIMES                                                      
         SPACE 2                                                                
PROG10   OC    0(5,R1),0(R1)                                                    
         BZ    PROG4                                                            
         MVC   DAYNUM,NPGRDAY                                                   
         CLC   NPGRDAY,0(R1)       MUST MATCH ON DAY                            
         BE    PROG11                                                           
         CLI   0(R1),X'FF'         UNLESS DAY IS ALL                            
         BNE   PROG16                                                           
         SPACE 2                                                                
PROG11   CLI   1(R1),X'FF'         ALL TIME FEATURE                             
         BE    PROG14                                                           
         MVC   WORK(2),NPGTIME                                                  
         CLC   WORK(2),=H'600'                                                  
         BNL   PROG11B                                                          
         LH    RF,WORK                                                          
         LA    RF,2400(RF)                                                      
         STH   RF,WORK                                                          
         SPACE 1                                                                
PROG11B  CLC   WORK(2),1(R1)                                                    
         BL    PROG16                                                           
         OC    3(2,R1),3(R1)                                                    
         BZ    PROG12                                                           
         CLC   WORK(2),3(R1)                                                    
         BH    PROG16                                                           
         B     PROG14                                                           
         SPACE 2                                                                
PROG12   CLC   WORK(2),1(R1)       MUST MATCH ON START IF END                   
         BNE   PROG16              TIME IS NOT SPECIFIED                        
         SPACE 2                                                                
PROG14   CLI   DPTOPT,0            DAYPART OPTION                               
         BE    PROG15                                                           
         LA    R4,IO                                                            
         DROP  R4                                                               
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROG4                                                            
         USING NPG2ELEM,R4                                                      
         CLC   DPTOPT,NPG2DYP                                                   
         BNE   PROG4                                                            
         B     PROG15                                                           
         SPACE 2                                                                
PROG15   BAS   RE,PROGHUT                                                       
         XC    SPECDATE,SPECDATE                                                
         BAS   RE,FILL                                                          
         B     PROG4                                                            
         SPACE 2                                                                
PROG16   LA    R1,5(R1)                                                         
         BCT   R0,PROG10                                                        
         B     PROG4                                                            
         EJECT                                                                  
*              ROUTINE TO GET HUT FACTOR                                        
         SPACE 3                                                                
HUTINIT  NTR1                      LOAD UP HUT VALUES                           
         LA    R3,HUTVALS                                                       
         LA    R4,9                                                             
         SPACE 1                                                                
HUTINIT2 XC    0(96,R3),0(R3)                                                   
         LA    R3,96(R3)                                                        
         BCT   R4,HUTINIT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
PROGHUT  NTR1                                                                   
         ZIC   R3,DAYNUM                                                        
         MH    R3,=H'96'                                                        
         LA    R3,HUTVALS(R3)      PICK UP HUTS FOR DAY                         
         MVC   HUTLIST(96),0(R3)                                                
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         BAS   RE,GETAHUTS                                                      
         MVC   0(96,R3),HUTLIST    ROUTINE UPDATES VALUES                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO COMPUTE HUTS                                         
         SPACE 3                                                                
GETAHUTS NTR1                                                                   
         LA    R2,NPGKTIME         CONVERT TIME TO QUARTERS                     
         LA    R3,HUTQ                                                          
         BAS   RE,GETQ                                                          
         MVC   HUTQ+1(1),HUTQ                                                   
         LA    R2,NPGKTIME+2                                                    
         LA    R3,HUTQ+1                                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    *+8                                                              
         BAS   RE,GETQ                                                          
         SPACE 1                                                                
         SR    R2,R2               ADD HUTS IN R2                               
         LA    R3,1                COUNT IN R3                                  
         SPACE 1                                                                
GETA2    ZIC   R1,HUTQ                                                          
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         LA    R1,HUTLIST(R1)      LOOK UP HUT FOR THIS 1/2 HOUR                
         BAS   RE,GETDHUTS                                                      
         AH    R2,0(R1)                                                         
         AI    HUTQ,2                                                           
         CLC   HUTQ(1),HUTQ+1                                                   
         BNL   GETA4                                                            
         LA    R3,1(R3)                                                         
         B     GETA2                                                            
         SPACE 1                                                                
GETA4    LR    R0,R2               AVERAGE HUTS                                 
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              REFRESH HUT VALUE FOR SPECIFIC DAYNUM HUTQ                       
         SPACE 3                                                                
GETDHUTS NTR1                                                                   
         OC    0(2,R1),0(R1)       HAVE WE LOOKED THIS UP BEFORE                
         BNZ   XIT                                                              
         LR    R2,R1                                                            
         LA    R5,HUTBLOCK         SET UP BLOCK FOR GETHUT                      
         USING GETHUTD,R5                                                       
         XC    GHBLOCK,GHBLOCK                                                  
         MVC   GHREPDAY,DAYNUM                                                  
         MVC   GHQUARTS,HUTQ                                                    
         MVC   GHQUARTS+1,HUTQ                                                  
         MVI   GHSCHEME,X'FE'      PRESET FROM YEAR RECORDS                     
         MVI   GH52,C'Y'           PRESET 52 WEEK OPTION                        
         MVI   GHPREVYR,C'N'       DON'T DEFAULT TO PREV. YR                    
         CLI   HUT52,0                                                          
         BE    *+10                                                             
         MVC   GH52,HUT52                                                       
         MVC   GHBKTYPE,HUTTYPE                                                 
         CLI   GHBKTYPE,C'D'                                                    
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'O'                                                    
         MVI   GHBKTYPE,C'A'                                                    
         MVC   GHBOOKS(2),BOOKS+1                                               
         MVC   GHBOOKS+2(2),BOOKS+4                                             
         MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHAGY,AGENCY                                                     
         CLI   HUTSCHEM,0                                                       
         BE    GETD2                                                            
         MVC   GHSCHEME,HUTSCHEM                                                
         MVC   GHAGYMED,BINAGYMD                                                
GETD2    GOTO1 GETHUT,DMCB,(R5)                                                 
         MVC   0(2,R2),GHHUT                                                    
         B     XIT                                                              
         SPACE 1                                                                
GETQ     NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO 1/4 HOUR                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD A PAD RECORD                                    
         SPACE 3                                                                
FILL     NTR1                                                                   
         XC    WORK,WORK           BUILD A LINE IN WORK                         
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         LA    R6,WORK                                                          
         USING PADD,R6                                                          
         CLI   FLESRCE,C'P'                                                     
         BNE   *+12                                                             
         BAS   RE,PROGFILL                                                      
         B     FILLB                                                            
         SPACE 1                                                                
         XC    DUB,DUB             DEAL WITH FILTERS HERE                       
         GOTO1 DEFINE,DMCB,=C'TYPE',DBLOCK,DUB                                  
         GOTO1 VCHEFILT,DMCB,DUB                                                
         BNE   XIT                                                              
         SPACE 1                                                                
         GOTO1 VDISPNET            GET NETWORK EDITED INTO WORK                 
         MVC   PADNET,WORK                                                      
         GOTO1 DEFINE,DMCB,=C'PROG',DBLOCK,PADPROG                              
         GOTO1 (RF),(R1),=C'TIME',DBLOCK,BLOCK                                  
         MVC   PADINV(1),BLOCK                                                  
         GOTO1 (RF),(R1),=C'DAY',DBLOCK,BLOCK                                   
         DROP  R4                                                               
         IC    R1,BLOCK+1                                                       
         SLL   R1,4                                                             
         STC   R1,PADINV+1                                                      
         SPACE 1                                                                
FILLB    LA    R2,DEMOS                                                         
         LA    R3,PADDEM1                                                       
         ZIC   R4,NUMDEMS                                                       
         SPACE 1                                                                
FILL2    OC    0(3,R2),0(R2)       NOW EXTRACT UP TO 6 DEMOS                    
         BZ    FILL4                                                            
         LA    R1,DBLOCKA                                                       
         ST    R1,DMCB+4                                                        
         GOTO1 DEMOUT,DMCB,(C'D',(R2)),,DUB                                     
         CLC   FLESRCE(3),=C'NAD'                                               
         BE    FILL2A                                                           
         TM    0(R2),X'20'                                                      
         BO    FILL2B                                                           
         SPACE 1                                                                
FILL2A   MVC   0(2,R3),DUB+2                                                    
         CLI   1(R2),C'R'                                                       
         BE    FILL3                                                            
         CLI   1(R2),C'P'                                                       
         BE    FILL3                                                            
         CLI   1(R2),C'S'                                                       
         BE    FILL3                                                            
         CLI   1(R2),C'V'                                                       
         BE    FILL3                                                            
         CLI   1(R2),C'X'                                                       
         BE    FILL3                                                            
         L     R1,DUB                                                           
         AH    R1,=H'2'                                                         
         SRL   R1,2                SHIFT FOR HALF-WORD MAX                      
         STH   R1,0(R3)                                                         
         B     FILL3                                                            
         SPACE 1                                                                
FILL2B   OC    COST,COST           HANDLE CPP/CPM FACILITY                      
         BZ    FILL3                                                            
         L     R1,COST                                                          
         OC    DUB(4),DUB                                                       
         BZ    FILL3                                                            
         LA    R0,200              CPM IS IN CENTS                              
         CLI   1(R2),C'T'                                                       
         BE    *+8                                                              
         LA    R0,20               CPP WILL BE CARRIED IN $                     
         MR    R0,R0                                                            
         D     R0,DUB                                                           
         AH    1,=H'1'                                                          
         SRA   R1,1                                                             
         LCR   R1,R1               CARRY THEM COMPLEMENTED                      
         STH   R1,0(R3)                                                         
         SPACE 1                                                                
FILL3    LA    R2,3(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,FILL2                                                         
         EJECT                                                                  
*              NOW SEE IF IT QUALIFIES                                          
         SPACE 3                                                                
FILL4    CLC   PADDEM1,DEMOMIN     MINIMUM REQUIREMENTS MET?                    
         BL    XIT                                                              
         CLC   PADDEM1,DEMOMAX     MAXIMUM EXCEEDED?                            
         BH    XIT                                                              
         CLC   PADLINES,PADMAX     IS PAD FULL                                  
         BE    FILL6                                                            
         CLC   PADDEM1,PADWORST    NO                                           
         BH    *+10                                                             
         MVC   PADWORST,PADDEM1    UPDATE WORST SO FAR                          
         LH    R1,PADLINES                                                      
         LA    R1,1(R1)                                                         
         STH   R1,PADLINES         UPDATE LINES SO FAR                          
         B     FILL8                                                            
         SPACE 1                                                                
FILL6    CLC   PADDEM1,PADWORST    TABLE IS FULL                                
         BL    XIT                 CHECK IF THIS IS WORSE THAN WORST            
         SPACE 1                                                                
FILL8    LH    R2,PADLINES         FIND N'TH POSITION                           
         BCTR  R2,0                                                             
         MH    R2,=H'40'                                                        
         A     R2,AMYPAD                                                        
         MVC   0(40,R2),WORK       AND PUT IN THIS ITEM                         
         CLC   PADLINES,PADMAX                                                  
         BL    XIT                                                              
         LH    R2,PADLINES                                                      
         L     R3,AMYPAD                                                        
         GOTO1 XSORT,PARAS,(1,(R3)),(R2),40,2,22                                
         BCTR  R2,0                                                             
         MH    R2,=H'40'                                                        
         A     R2,AMYPAD                                                        
         MVC   PADWORST,22(R2)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              FILL PAD FROM PROGRAM RECORD                                     
         SPACE 3                                                                
PROGFILL NTR1                                                                   
         LA    R6,WORK                                                          
         USING PADD,R6                                                          
         LA    R4,IO                                                            
         MVI   ELCODE,X'5D'        EVN BOOK ELEMENT CODE                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BLOW UP IF ELEMENT NOT FOUND                 
         MVC   PBEL,0(R4)          MOVE ELEMENT TO DUMMY RECORD                 
         LA    R4,IO                                                            
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         USING NPGELEM,R4                                                       
         XC    PVEL,PVEL                                                        
         MVI   PVEL+0,X'33'                                                     
         MVI   PVEL+1,119                                                       
         MVI   PVEL+2,1            1-BYTE PER VPH PER 100                       
         MVC   PVEL+3(34),NPGVPHS                                               
         ZIC   R1,1(R4)                                                         
         AR    R1,R4                                                            
         CLI   0(R1),X'93'         TEST IF NEW PROGRAM ELEMENT FOUND            
         BNE   PROG5                                                            
         USING NPG2ELEM,R1                                                      
         MVI   PVEL+2,X'42'        2-BYTES PER VPH PER 1000                     
         MVC   PVEL+3(116),NPG2VPHS                                             
         DROP  R1                                                               
*                                                                               
PROG5    XC    PREL,PREL                                                        
         MVC   PREL(3),=X'350902'  GENERATE RATINGS ELEMENT                     
         MVC   PREL+3(2),NPGSHARE                                               
         TM    NPGSTAT,X'80'                                                    
         BO    POSTD                                                            
         LH    R1,NPGSHARE         COMPUTE RTG=HUT X SHR                        
         MH    R1,HUT                                                           
         SR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,DUB                                                           
         MVC   PREL+3(2),DUB                                                    
         MVC   PREL+5(2),HUT                                                    
         MVC   PREL+7(2),NPGSHARE                                               
         SPACE 1                                                                
POSTD    MVC   PADNET,0(R2)                                                     
         MVC   DUB,NPGTIME                                                      
         LH    R1,DUB              CONVERT START TIME TO 1/4 HOUR               
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'5'                                                         
         BH    *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         CH    R1,=H'20'                                                        
         BNH   *+8                                                              
         LA    R1,20                                                            
         SLL   R1,2                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,2(R1)                                                         
         STC   R1,PADINV                                                        
         IC    R1,NPGRDAY                                                       
         SLL   R1,4                                                             
         STC   R1,PADINV+1                                                      
         MVC   PADPROG,NPGNAME                                                  
         OC    SPECDATE,SPECDATE                                                
         BZ    XIT                                                              
         MVC   PADPROG,SPACES                                                   
         LA    R4,IO                                                            
         USING NPGRECD,R4                                                       
         MVC   PADPROG(6),NPGKPROG                                              
         MVC   PADPROG+7(2),=C'ON'                                              
         GOTO1 DATCON,DMCB,(2,SPECDATE),(4,PADPROG+10)                          
         GOTO1 SQUASHER,DMCB,PADPROG,16                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO RANK THE TABLE                                        
         SPACE 3                                                                
RANK     NTR1                                                                   
         USING PADD,R6                                                          
         L     R6,AMYPAD                                                        
         LA    R2,PADDEM6          WORK BACKWARDS SO DEM1 IS LAST               
         SR    R2,R6               R2=DISPLACEMENT                              
         LA    R3,DEMOS+15         R3=DEMO                                      
         LA    R4,6                                                             
         LH    R5,PADLINES                                                      
         SPACE 1                                                                
RANK2    OC    0(3,R3),0(R3)       SORT ON DEM N                                
         BZ    RANK4                                                            
         MVC   PARAS(4),AMYPAD                                                  
         MVI   PARAS,1                                                          
         GOTO1 XSORT,PARAS,,(R5),40,2,(R2)                                      
         BAS   RE,RANK6                                                         
         SPACE 1                                                                
RANK4    SH    R2,=H'3'                                                         
         SH    R3,=H'3'                                                         
         BCT   R4,RANK2                                                         
         B     XIT                                                              
         SPACE 1                                                                
*                                  ROUTINE TO PUT IN RANK NUMBERS               
RANK6    NTR1                                                                   
         A     R2,AMYPAD           R2=A(COLUMN)                                 
         LA    R3,1                R3=PLACE NUMBER                              
         LA    R4,1                R4=ACTUAL NUMBER                             
*                                  R5=N'LINES                                   
         SR    R6,R6               R6=CURRENT VALUE                             
         SPACE 1                                                                
RANK8    STC   R4,2(R2)                                                         
         CH    R6,0(R2)            IS THIS WORSE THAN PREVIOUS                  
         BE    RANK10                                                           
         LR    R3,R4               THEN SET PLACE TO ACTUAL                     
         LH    R6,0(R2)            AND SAVE THIS VALUE                          
         B     RANK12                                                           
         SPACE 1                                                                
RANK10   STC   R3,2(R2)            TIE - USE PREVIOUS PLACE                     
         SPACE 1                                                                
RANK12   LA    R2,40(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R5,RANK8                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A REPORT FROM PAD                               
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         L     R6,AMYPAD                                                        
         USING PADD,R6                                                          
         LH    R3,PADLINES                                                      
         SPACE 1                                                                
SPLAT2   LA    R2,10               PRINT IN BLOCKS OF 10                        
         CR    R3,R2               OR LESS                                      
         BH    *+6                                                              
         LR    R2,R3                                                            
         LA    R1,1(R2)                                                         
         STC   R1,ALLOWLIN         ALLOW FOR 1 MORE LINE                        
         SPACE 1                                                                
SPLAT4   MVI   SPACING,1                                                        
         BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         XC    0(40,R6),0(R6)                                                   
         BCTR  R3,0                                                             
         LA    R6,40(R6)                                                        
         MVI   ALLOWLIN,0                                                       
         BCT   R2,SPLAT4                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         CH    R3,=H'0'                                                         
         BNE   SPLAT2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              FILL A PRINT LINE                                                
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         MVC   P+3(4),PADNET       NETWORK                                      
         CLI   P+6,C'A'            TEST ASCRIBED                                
         BNE   *+8                                                              
         MVI   P+6,C' '            REPLACE 'A' WITH BLANK                       
         MVC   P+9(16),PADPROG     PROGRAM                                      
         GOTO1 INVEDIT,DMCB,PADINV,WORK                                         
         MVC   P+26(3),WORK        DAY                                          
         MVC   P+30(6),WORK+3      TIME                                         
         DROP  R4                                                               
         LA    R2,PADDEM1          SET UP TO EDIT DEMOS                         
         LA    R3,P+38                                                          
         LA    R4,DEMOS                                                         
         ZIC   R5,NUMDEMS                                                       
         SPACE 1                                                                
FORMAT2  OC    0(3,R2),0(R2)                                                    
         BZ    FORMAT3                                                          
         TM    0(R4),X'20'         CPP/CPM EDITING                              
         BNO   FORMAT3                                                          
         CLC   FLESRCE(3),=C'NAD'                                               
         BE    FORMAT3                                                          
         LH    R1,0(R2)                                                         
         LCR   R1,R1               (UN-COMPLEMENT)                              
         ST    R1,SAVCPM                                                        
         EDIT  (R1),(6,DMCB),FLOAT=$                                            
         MVC   0(5,R3),DMCB+1                                                   
         CLI   1(R4),C'T'                                                       
         BNE   FORMAT6                                                          
         L     R1,SAVCPM                                                        
         EDIT  (R1),(8,DMCB),2,FLOAT=$                                          
         L     R1,SAVCPM                                                        
         MVC   0(5,R3),DMCB                                                     
         CH    R1,=H'9999'                                                      
         BH    FORMAT6                                                          
         MVC   0(5,R3),DMCB+3                                                   
         B     FORMAT6                                                          
         SPACE 1                                                                
FORMAT3  CLI   1(R4),C'S'                                                       
         BE    FORMAT4                                                          
         CLI   1(R4),C'R'                                                       
         BE    FORMAT4                                                          
         CLI   1(R4),C'O'          TP PUTS                                      
         BE    FORMAT4                                                          
         CLI   1(R4),C'Q'          TP SHARES                                    
         BE    FORMAT4                                                          
         CLI   1(R4),C'V'                                                       
         BE    FORMAT4                                                          
         CLI   1(R4),C'C'                                                       
         BE    FORMAT4                                                          
         CLI   1(R4),C'X'                                                       
         BE    FORMAT4                                                          
         CLI   1(R4),C'P'                                                       
         BE    FORMAT31                                                         
         EDIT  (2,0(R2)),(5,0(R3))                                              
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),0(R2)                                                   
         L     R1,DUB                                                           
         SLL   R1,2                                                             
         EDIT  (R1),(5,0(R3))                                                   
         B     FORMAT6                                                          
         SPACE 1                                                                
FORMAT31 CLI   FLESRCE,C'P'        NO DEC FOR PROGRAMS                          
         BNE   FORMAT4                                                          
         EDIT  (2,0(R2)),(5,0(R3))                                              
         B     FORMAT6                                                          
         SPACE 1                                                                
FORMAT4  EDIT  (2,0(R2)),(5,0(R3)),1                                            
         SPACE 1                                                                
FORMAT6  EDIT  (1,2(R2)),(3,6(R3)),ALIGN=LEFT                                   
         SPACE 1                                                                
FORMAT8  LA    R2,3(R2)                                                         
         LA    R3,11(R3)                                                        
         LA    R4,3(R4)                                                         
         BCT   R5,FORMAT2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK - CONTROL                                          
         SPACE 3                                                                
HOOK     NTR1                                                                   
         BAS   RE,HOOKHEAD                                                      
         BAS   RE,HOOKBOX                                                       
         GOTO1 VRESHEAD                                                         
         B     XIT                                                              
         SPACE 3                                                                
*              HOOK - DEAL WITH HEADINGS                                        
         SPACE 3                                                                
HOOKHEAD NTR1                                                                   
         MVC   RESTITA,SPACES                                                   
         MVC   RESTITB,SPACES                                                   
         MVC   RESTITC,SPACES                                                   
         MVC   RESTITA(36),=C' NETWORK     PROGRAM      DAY   TIME'             
         SPACE 1                                                                
         LA    R3,RESTITA+38       NOW POINT TO WHERE DEMOS GO                  
         LA    R2,DEMOS                                                         
         ZIC   R6,NUMDEMS                                                       
         SPACE 1                                                                
HH2      CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(7,WORK),(0,DBLOCKD)                      
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         MVC   1(7,R3),WORK                                                     
         CLI   WORK+7,C'*'                                                      
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(000)'                                              
         CLI   1(R2),C'R'                                                       
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(RTG)'                                              
         CLI   1(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(SHR)'                                              
         CLI   1(R2),C'V'                                                       
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(VPH)'                                              
         CLI   1(R2),C'U'                                                       
         BNE   *+10                                                             
         MVC   WORK+7(5),=C'(UNI)'                                              
         CLI   1(R2),C'P'                                                       
         BNE   HH3                                                              
         MVC   WORK+7(5),=C'(PUT)'                                              
         CLI   2(R2),1                                                          
         BNE   HH3                                                              
         MVI   WORK+8,C'H'                                                      
         SPACE 1                                                                
HH3      MVC   132(5,R3),WORK+7                                                 
         CLC   FLESRCE(3),=C'NAD'  SHOW CATEGORY FOR NAD                        
         BNE   *+10                                                             
         MVC   264(7,R3),WORK+12                                                
         MVC   138(3,R3),=C'RNK'                                                
         LA    R2,3(R2)                                                         
         LA    R3,11(R3)                                                        
         BCT   R6,HH2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              HOOK - BOXES                                                     
         SPACE 3                                                                
HOOKBOX  NTR1                                                                   
         MVC   RESCOLS,SPACES                                                   
         LA    R2,RESCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   08(R2),C'C'                                                      
         MVI   25(R2),C'C'                                                      
         MVI   29(R2),C'C'                                                      
         SPACE 1                                                                
         LA    R2,RESCOLS+37                                                    
         ZIC   R3,NUMDEMS          NOW SET UP FOR DEMOS                         
         SPACE 1                                                                
HB2      MVI   0(R2),C'C'                                                       
         LA    R2,11(R2)                                                        
         BCT   R3,HB2                                                           
         MVI   0(R2),C'R'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ERROR MESSAGES AND HEADLINE SPECS                                
         SPACE 3                                                                
INVTIM   DC    C'** ERROR ** INVALID TIME'                                      
INVDAY1  DC    C'** ERROR ** INVALID DAY/DETAIL'                                
MANYDEM5 DC    C'** ERROR ** TOO MANY DEMOS - LIMIT IS 5'                       
MANYDEM6 DC    C'** ERROR ** TOO MANY DEMOS - LIMIT IS 6'                       
MANYBKS  DC    C'** ERROR ** TOO MANY BOOKS - LIMIT IS 1'                       
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         SPACE 3                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,77,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
*              LITERAL POOL FOR FLEXI                                           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPGENPROG                                                        
*              NEGETHUTD                                                        
*              NERESALL                                                         
         PRINT OFF                                                              
       ++INCLUDE SPGENPROGA                                                     
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF2D                                                       
*              NEGETNUND                                                        
       ++INCLUDE NEGETNUND                                                      
         EJECT                                                                  
*              LOCAL STORAGE                                                    
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
COST     DS    F                   FOR CPP/CPM -SIGNED PENNIES (BINARY)         
PRINTOPT DS    XL1                 X'80'  WEEKS FILTER                          
*                                  X'40'  MINIMUM DEMO FILTER                   
*                                  X'20'  MAXIMUM DEMO FILTER                   
*                                  X'10'  COST FILTER                           
*                                  X'08'  ROUND DEMOS                           
*                                  X'04'  SUPPRESS PURE NO.                     
DEMOMIN  DS    H                   MINIMUM DEMO VALUE                           
DEMOMAX  DS    H                   MAXIMUM DEMO VALUE                           
SPACOPT  DS    XL1                                                              
AMYPAD   DS    A                                                                
PADLINES DS    H                                                                
PADMAX   DS    H                                                                
PADWORST DS    H                                                                
THISPROG DS    CL6                                                              
SPECDATE DS    CL2                                                              
SAVCPM   DS    F                                                                
THISPRG  DS    CL6                                                              
DPTOPT   DS    CL1                                                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN PAD                                    
         SPACE 3                                                                
PADD     DSECT                                                                  
PADREC   DS    0CL40                                                            
PADNET   DS    CL4                                                              
PADPROG  DS    CL16                                                             
PADINV   DS    CL2                                                              
PADDEM1  DS    CL2                                                              
PADRNK1  DS    CL1                                                              
PADDEM2  DS    CL2                                                              
PADRNK2  DS    CL1                                                              
PADDEM3  DS    CL2                                                              
PADRNK3  DS    CL1                                                              
PADDEM4  DS    CL2                                                              
PADRNK4  DS    CL1                                                              
PADDEM5  DS    CL2                                                              
PADRNK5  DS    CL1                                                              
PADDEM6  DS    CL2                                                              
PADRNK6  DS    CL1                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063NERES02   05/01/02'                                      
         END                                                                    
