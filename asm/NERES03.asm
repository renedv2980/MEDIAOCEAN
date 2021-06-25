*          DATA SET NERES03    AT LEVEL 044 AS OF 05/01/02                      
*PHASE T32103,*                                                                 
T32103   TITLE '-   LAYOUT REPORT'                                              
T32103   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LAYO**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     LA    R2,LAYSRCEH         VALIDATE SOURCE                              
         MVI   OPTION,X'FF'                                                     
         MVI   OPTION,X'FF'        ALL ALLOWED                                  
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
         LA    R2,LAYPSTRH         POSSIBLE START/END                           
         GOTO1 VVALSEND                                                         
         SPACE 1                                                                
         LA    R2,LAYBOOKH         VALIDATE BOOK                                
         MVI   MAX,1                                                            
         CLI   LAYSRCE,C'P'                                                     
         BNE   *+8                                                              
         MVI   MAX,2                                                            
         GOTO1 VVALBOOK                                                         
         SPACE 1                                                                
         LA    R2,LAYDEMOH         VALIDATE DEMOS                               
         MVI   MAX,8                                                            
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         SPACE 1                                                                
         LA    R2,LAYNETH          VALIDATE NETWORK                             
         LA    R3,6                                                             
         LA    R4,NETSAVE                                                       
         XC    NETSAVE,NETSAVE                                                  
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
VKEY20   GOTO1 VVALNET                                                          
         MVC   0(7,R4),ACTNET                                                   
         LA    R4,7(R4)                                                         
VKEY25   BAS   RE,BUMP                                                          
         BCT   R3,*+8                                                           
         B     VKEY30                                                           
         CLI   5(R2),0             ANOTHER NETWORK?                             
         BNE   VKEY20                                                           
         B     VKEY25                                                           
         SPACE 1                                                                
VKEY30   LA    R2,LAYDAYH          DAY                                          
         MVI   DAYNUM,X'FF'                                                     
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VKEY40                                                           
         GOTO1 VVALDAY                                                          
         MVC   DAYNUM,ACTUAL                                                    
         SPACE 1                                                                
VKEY40   LA    R2,LAYSTRTH         START TIME                                   
         GOTO1 VVALTIM                                                          
         MVC   STARTNUM,ACTUAL                                                  
         LA    R2,LAYENDH          END TIME                                     
         GOTO1 VVALTIM                                                          
         MVC   ENDNUM,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,LAYOPTH          OPTIONS                                      
         BAS   RE,VALOPT                                                        
         LA    R2,LAYTITLH         OWN TITLE                                    
         GOTO1 VVALTITL                                                         
         LA    R2,LAYFILTH         FILTERS                                      
         GOTO1 VVALFILT                                                         
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VALOPT   NTR1                                                                   
         MVI   SPACOPT,1           PRESET VALUES FOR OPTIONS                    
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
OPT2     CLC   12(2,R3),=C'S  '                                                 
         BNE   OPT4                                                             
         MVI   SPACOPT,2                                                        
         CLI   22(R3),C'2'                                                      
         BE    OPTEND                                                           
         MVI   SPACOPT,3                                                        
         CLI   22(R3),C'3'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
         SPACE 1                                                                
OPT4     CLC   12(3,R3),=C'DAY'                                                 
         BNE   OPT9                                                             
         CLI   22(R3),C'I'         I=INDIVIDUAL DAYS                            
         BNE   BADOPT                                                           
         MVC   DAYOPT,22(R3)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT9     B     BADOPT                                                           
         SPACE 1                                                                
OPTEND   LA    R3,32(R3)                                                        
         BCT   R4,OPT2                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYEND                                                            
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 3                                                                
*              INPUT               R4=A(MAIN DBLOCK)                            
         SPACE 1                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         ZIC   R0,NUMNETS          FIGURE OUT PRINT DISPLACEMENT                
         MH    R0,=H'19'                                                        
         AH    R0,=H'15'                                                        
         LA    R1,110                                                           
         SR    R1,R0                                                            
         BP    *+6                                                              
         SR    R1,R1                                                            
         SRL   R1,1                                                             
         ST    R1,DETDISP                                                       
         SPACE 1                                                                
         MVC   RESTITLE,=CL40'NETWORK LAYOUT REPORT'                            
         SPACE 1                                                                
         CLI   LAYSRCE,C'P'                                                     
         BNE   SDAY                                                             
         MVI   HUTSW,0             ENSURE HUTS INITIALIZED                      
         XC    PIO(22),PIO                                                      
         MVI   PIOEOR,0                                                         
         LA    R2,BLOCK                                                         
         USING GUVD,R2             FILL BLOCK FOR GETNUN                        
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
         BE    SDAY                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              CONTROL OF MULTIPLE/SINGLE DAY                                   
         SPACE 3                                                                
SDAY     CLI   DAYNUM,X'FF'                                                     
         BE    SD2                                                              
         BAS   RE,REPDAY                                                        
         B     XIT                                                              
         SPACE 1                                                                
SD2      LA    R2,1                                                             
         LA    R3,7                                                             
         MVC   SAVSTART,STARTNUM                                                
         SPACE 1                                                                
SD4      STC   R2,DAYNUM                                                        
         MVC   STARTNUM,SAVSTART                                                
         BAS   RE,REPDAY                                                        
         LA    R2,1(R2)                                                         
         BCT   R3,SD4                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL REPORT                                       
         SPACE 3                                                                
REPDAY   NTR1                                                                   
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELBK,BOOKS+1     SELECT BOOK                                  
         ZIC   R1,DAYNUM           SELECT DAY                                   
         LA    R1,DAYLIST(R1)      (NEED SPOT-TYPE)                             
         MVC   DBSELDAY,0(R1)                                                   
         CLC   LAYSRCE(3),=C'NTI'  IF ITS NTI                                   
         BNE   REPDAY2                                                          
         MVI   DBBEST,0                                                         
         CLI   DBSELDAY,X'7C'      FOR M-F                                      
         BNE   *+8                                                              
         MVI   DBBEST,C'L'         ONLY WANT ROTATORS                           
         CLI   DBSELDAY,X'7F'      ALSO FOR M-S                                 
         BNE   *+8                                                              
         MVI   DBBEST,C'L'                                                      
         DROP  RF                                                               
         SPACE 1                                                                
REPDAY2  MVI   FORCEHED,C'Y'                                                    
         MVI   FIRST,C'Y'                                                       
         SPACE 1                                                                
REPLOOP  BAS   RE,PROCESS                                                       
         ZIC   R4,LINE                                                          
         ZIC   R3,NUMDEMS                                                       
         LA    R3,1(R3)                                                         
         SRL   R3,1                                                             
         LA    R3,2(R3,R4)                                                      
         ZIC   R4,MAXLINES                                                      
         CR    R3,R4               WILL THE LINES FIT                           
         BL    REPLOOP2                                                         
         CLI   FIRST,C'Y'                                                       
         BE    REPLOOP2                                                         
         BAS   RE,ALLSTARS         NO - WRAP UP PAGE                            
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   FIRST,C'Y'                                                       
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
REPLOOP2 BAS   RE,SPLAT                                                         
         CLC   STARTNUM,ENDNUM                                                  
         BNL   REPLOOP4                                                         
         ZIC   R2,STARTNUM                                                      
         LA    R2,2(R2)                                                         
         STC   R2,STARTNUM                                                      
         B     REPLOOP                                                          
         SPACE 2                                                                
REPLOOP4 BAS   RE,ALLSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 3                                                                
PROCESS  NTR1                                                                   
         LA    R2,BUFF                                                          
         MVC   000(132,R2),SPACES                                               
         MVC   132(132,R2),SPACES                                               
         MVC   264(132,R2),SPACES                                               
         MVC   396(132,R2),SPACES                                               
         A     R2,DETDISP                                                       
         ZIC   R1,DAYNUM           SHOW ALPHA DAY                               
         MH    R1,=H'3'                                                         
         LA    R1,DAYALPH(R1)                                                   
         MVC   134(3,R2),0(R1)                                                  
         ZIC   R1,STARTNUM         CONVERT TO MILITARY START                    
         SR    R0,R0                                                            
         D     R0,=F'4'            (HRS IN R1, 1/4'S IN R0)                     
         LA    R1,6(R1)                                                         
         MH    R1,=H'100'                                                       
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,30(R1)                                                        
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STH   R1,MILST                                                         
         XC    DUB(4),DUB          AND TIME                                     
         MVC   DUB(2),MILST                                                     
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,DMCB,DUB,WORK                                             
         MVC   138(6,R2),WORK                                                   
         LA    R2,NETSAVE                                                       
         ZIC   R3,NUMNETS                                                       
         LA    R5,BUFF+14                                                       
         A     R5,DETDISP                                                       
         CLI   LAYSRCE,C'P'                                                     
         BE    PROG2                                                            
         SPACE 2                                                                
         LA    RF,DBLOCKA                                                       
         USING DBLOCK,RF                                                        
         MVC   DBSELTIM(2),MILST   SELECT DISCREET 1/2 HOUR                     
         MVC   DBSELTIM+2(2),MILST                                              
         SPACE 1                                                                
PROCESS2 LA    RF,DBLOCKA                                                       
         MVC   DBSELSTA,0(R2)      SELECT NETWORK                               
         MVC   DBDAYOPT,DAYOPT     P (POCKETPIECE) OR I (INDIVIDUAL)            
         GOTO1 VADJSEL             ADJUST FOR PEOPLE-METERS                     
         DROP  RF                                                               
         GOTO1 DEMAND,DMCB,DBLOCKA,PROCESS4                                     
         B     PROCESS6                                                         
         SPACE 1                                                                
PROCESS4 NTR1                                                                   
         XC    DUB,DUB                                                          
         GOTO1 DEFINE,PARAS,=C'TYPE',DBLOCKA,DUB                                
         GOTO1 VCHEFILT,PARAS,DUB                                               
         BNE   XIT                                                              
         GOTO1 DEFINE,PARAS,=C'PROGRAM',DBLOCKA,1(R5)                           
         BAS   RE,POST                                                          
         B     XIT                                                              
         SPACE 2                                                                
PROCESS6 LA    R2,7(R2)                                                         
         LA    R5,19(R5)                                                        
         BCT   R3,PROCESS2                                                      
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*              CONTROL OF I/O - NOW READING FOR PROGRAMS                        
         SPACE 3                                                                
PROG2    MVI   DMFILE,C'S'                                                      
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0DA0'                                                 
         MVC   NPGKAM,BINAGYMD                                                  
         MVC   NPGKNET,5(R2)                                                    
         MVC   NPGKDAY,DAYNUM                                                   
         MVC   NPGKTIME(2),MILST                                                
         GOTO1 HIGH                                                             
         B     PROG10                                                           
         SPACE 1                                                                
PROG8    GOTO1 SEQ                                                              
         SPACE 1                                                                
PROG10   CLC   KEY(8),KEYSAVE      CHECK MATCH ON START TIME                    
         BNE   PROG12                                                           
         CLC   NPGKEND,PSTART                                                   
         BL    PROG8                                                            
         CLC   NPGKEND,PEND                                                     
         BH    PROG8                                                            
         MVC   HUTTIME,NPGKTIME                                                 
         GOTO1 VPROGHUT                                                         
         GOTO1 GETREC                                                           
         LA    R6,IO               FILTER PROGRAMS                              
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         USING NPGELEM,R6                                                       
         XC    DUB,DUB                                                          
         MVC   DUB(4),NPGFILT                                                   
         GOTO1 VCHEFILT,DMCB,DUB   CHECK FILTERS                                
         BNE   PROG8                                                            
         BAS   RE,POST                                                          
         SPACE 1                                                                
PROG12   LA    R2,7(R2)            ON TO NEXT NETWORK                           
         OC    0(7,R2),0(R2)                                                    
         BZ    XIT                                                              
         LA    R5,19(R5)                                                        
         BCT   R3,PROG2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT VALUES AND FORMAT INTO BUFF                   
         SPACE 3                                                                
POST     NTR1                                                                   
         LA    R6,IO               R5=A(OUTPUT)                                 
         CLI   LAYSRCE,C'P'                                                     
         BNE   POSTD                                                            
         SPACE 2                                                                
POSTB    MVI   ELCODE,X'5D'        BOOK ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PBEL,0(R6)                                                       
         LA    R6,IO                                                            
         MVI   ELCODE,X'92'        SPECIAL FOR PROGRAM FILE                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         USING NPGELEM,R6                                                       
         MVC   COST,NPGCOST                                                     
         MVC   1(16,R5),NPGNAME                                                 
         OC    1(16,R5),SPACES                                                  
         XC    PVEL,PVEL                                                        
         MVI   PVEL+0,X'33'                                                     
         MVI   PVEL+1,119                                                       
         MVI   PVEL+2,1            1-BYTE PER VPH                               
         MVC   PVEL+3(34),NPGVPHS                                               
         ZIC   R1,1(R6)                                                         
         AR    R1,R6                                                            
         CLI   0(R1),X'93'         TEST IF NEW PROGRAM ELEMENT FOUND            
         BNE   POSTC                                                            
         USING NPG2ELEM,R1                                                      
         MVI   PVEL+2,X'42'        2-BYTES PER VPH                              
         MVC   PVEL+3(116),NPG2VPHS                                             
         DROP  R1                                                               
POSTC    XC    PREL,PREL                                                        
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
POSTD    GOTO1 CENTER,PARAS,1(R5),16                                            
         LA    R5,1(R5)                                                         
         SR    R4,R4                                                            
         LA    R2,DEMOS                                                         
         ZIC   R3,NUMDEMS                                                       
         SPACE 1                                                                
POST4    GOTO1 DEMOUT,DMCB,(C'D',(R2)),DBLOCKA,BLOCK                            
         CLI   DBLOCKA+3,0         CHECK DBERROR                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,BLOCK                                                         
         EJECT                                                                  
*              CPP/CPM AND DEMO EDIT                                            
         SPACE 3                                                                
         TM    0(R2),X'20'         CPP/CPM FEATURE                              
         BNO   POST5                                                            
         L     R1,COST                                                          
         LA    R0,200                                                           
         CLI   1(R2),C'T'                                                       
         BE    *+8                                                              
         LA    R0,2000                                                          
         MR    R0,R0                                                            
         LTR   R6,R6                                                            
         BZ    POST6                                                            
         DR    R0,R6                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(10,DMCB),2,FLOAT=$                                         
         MVC   132(6,R5),DMCB+4                                                 
         CLC   DMCB(4),SPACES                                                   
         BE    POST6                                                            
         MVC   132(6,R5),DMCB+1                                                 
         B     POST6                                                            
         SPACE 2                                                                
POST5    EDIT  (R6),(6,132(R5)),1                                               
         CLI   LAYSRCE,C'P'        PROGRAM PUTS HAVE NO DEC                     
         BE    *+12                                                             
         CLI   1(R2),C'P'          NTI PUTS HAVE 1 DEC                          
         BE    POST6                                                            
         CLI   1(R2),C'R'          THESE ARE ALWAYS 1 DEC                       
         BE    POST6                                                            
         CLI   1(R2),C'S'                                                       
         BE    POST6                                                            
         CLI   1(R2),C'O'          TP PUTS                                      
         BE    POST6                                                            
         CLI   1(R2),C'Q'          TP SHARES                                    
         BE    POST6                                                            
         CLI   1(R2),C'V'                                                       
         BE    POST6                                                            
         CLI   1(R2),C'C'                                                       
         BE    POST6                                                            
         CLI   1(R2),C'X'                                                       
         BE    POST6                                                            
         EDIT  (R6),(6,132(R5))                                                 
         SPACE 2                                                                
POST6    LA    R2,3(R2)                                                         
         LA    R5,8(R4,R5)         BUMP TO NEXT OR FIRST ON NEXT LINE           
         LTR   R4,R4               FLIP-FLOP R4  0/124                          
         LA    R4,116                                                           
         BZ    *+6                                                              
         SR    R4,R4                                                            
         BCT   R3,POST4                                                         
         B     XIT                                                              
         EJECT                                                                  
*              PRINTING AIDS                                                    
         SPACE 3                                                                
SPLAT    NTR1                                                                   
         BAS   RE,ALLSTARS                                                      
         CLI   FIRST,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,CLEARSUM                                                      
         MVI   FIRST,C'N'                                                       
         LA    R2,BUFF                                                          
         ZIC   R3,NUMDEMS                                                       
         LA    R3,1(R3)                                                         
         SRL   R3,1                                                             
         LA    R3,1(R3)                                                         
         GOTO1 SPOOL,PARAS,(R8)                                                 
         SPACE 2                                                                
SPLAT2   MVC   P,0(R2)                                                          
         BAS   RE,SUMSTARS                                                      
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R2,132(R2)                                                       
         BCT   R3,SPLAT2                                                        
         B     XIT                                                              
         SPACE 2                                                                
ALLSTARS NTR1                                                                   
         LA    R2,P                                                             
         A     R2,DETDISP                                                       
         MVC   0(14,R2),STARS                                                   
         LA    R2,14(R2)                                                        
         ZIC   R3,NUMNETS                                                       
         SPACE 2                                                                
ALL2     MVC   0(19,R2),STARS                                                   
         LA    R2,19(R2)                                                        
         BCT   R3,ALL2                                                          
         B     XIT                                                              
         SPACE 2                                                                
SUMSTARS NTR1                                                                   
         LA    R2,P+14                                                          
         A     R2,DETDISP                                                       
         ZIC   R3,NUMNETS                                                       
         LA    R1,P                                                             
         A     R1,DETDISP                                                       
         MVI   0(R1),C'*'                                                       
         MVI   13(R1),C'*'                                                      
         SPACE 2                                                                
SUM2     MVI   18(R2),C'*'                                                      
         LA    R2,19(R2)                                                        
         BCT   R3,SUM2                                                          
         B     XIT                                                              
         SPACE 2                                                                
CLEARSUM NTR1                                                                   
         LA    R2,BUFF+14                                                       
         A     R2,DETDISP                                                       
         LA    R3,P+14                                                          
         A     R3,DETDISP                                                       
         ZIC   R4,NUMNETS                                                       
         SPACE 2                                                                
CLEAR2   CLC   0(18,R2),SPACES     IF A PROGRAMS MISSING                        
         BNE   *+10                                                             
         MVC   0(18,R3),SPACES     TAKE OUT PRECEDING STARS                     
         LA    R2,19(R2)                                                        
         LA    R3,19(R3)                                                        
         BCT   R4,CLEAR2                                                        
         B     XIT                                                              
         SPACE 2                                                                
STARS    DC    20C'*'                                                           
DAYALPH  DC    C'M-FMONTUEWEDTHUFRISATSUNM-S'                                   
         SPACE 2                                                                
DAYLIST  DC    X'7C402010080402017F8003'                                        
         EJECT                                                                  
*              HOOK ROUTINES FOR HEADLINES                                      
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         SPACE 1                                                                
HOOK2    LA    R2,NETSAVE                                                       
         XC    RESTITA,RESTITA                                                  
         XC    RESTITB,RESTITB                                                  
         XC    RESTITC,RESTITC                                                  
         LA    R3,H8                                                            
         A     R3,DETDISP                                                       
         MVC   1(8,R3),=C'DAY/TIME'                                             
         LA    R3,15(R3)                                                        
         ZIC   R5,NUMNETS                                                       
         SPACE 1                                                                
HOOK4    MVC   0(16,R3),DASHES     PRINT NETWORK                                
         MVC   6(4,R3),0(R2)                                                    
         CLI   4(R2),C'A'          SHOW ASCRIBED                                
         BNE   *+8                                                              
         MVI   9(R3),C' '          AS ABC                                       
         CLI   4(R2),C'I'          SHOW INTEGRATED                              
         BNE   *+8                                                              
         MVI   9(R3),C'I'          AS ABCI                                      
         CLI   4(R2),C'D'          SHOW DIARY                                   
         BNE   *+8                                                              
         MVI   9(R3),C'D'          AS ABCD                                      
         CLI   4(R2),C'C'          SHOW CONFORMED                               
         BNE   *+8                                                              
         MVI   9(R3),C'C'          AS ABCC                                      
         CLI   4(R2),C'Z'          SHOW Z-BOOK (TEST)                           
         BNE   *+8                                                              
         MVI   9(R3),C'Z'          AS ABCZ                                      
         SPACE 1                                                                
HOOK6    LA    R2,7(R2)                                                         
         LA    R3,19(R3)                                                        
         BCT   R5,HOOK4                                                         
         SPACE 1                                                                
         LA    R3,H9+15            POINT WHERE 1ST DEMO PRINTS                  
         A     R3,DETDISP                                                       
         MVC   BYTEB,NUMNETS                                                    
         LA    R2,DEMOS                                                         
         MVC   BYTEA,NUMDEMS                                                    
         SPACE 1                                                                
HOOK8    LA    R5,2                                                             
         LR    R6,R3                                                            
         SPACE 1                                                                
HOOK10   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(2,WORK),(0,DBLOCKA)                      
         CLI   1(R2),C'I'          AND CHANGE BACK FOR NEXT ROUND               
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         SPACE 1                                                                
HOOK12   MVC   0(7,R6),WORK                                                     
         LA    R6,19(R6)                                                        
         ZIC   R1,BYTEB            NUMBER OF NETWORKS                           
         BCTR  R1,0                                                             
         STC   R1,BYTEB                                                         
         CLI   BYTEB,0             ANY MORE                                     
         BNE   HOOK12                                                           
         ZIC   R1,BYTEA            NUMBER OF DEMOS                              
         BCTR  R1,0                                                             
         STC   R1,BYTEA                                                         
         CLI   BYTEA,0                                                          
         BE    HOOKX                                                            
         SPACE 1                                                                
         LA    R2,3(R2)                                                         
         MVC   BYTEB,NUMNETS       REFRESH NUM. OF NETWORKS                     
         LA    R6,9(R3)            POINT TO NEXT POSITION                       
         BCT   R5,HOOK10           DO NEXT DEMO                                 
         LA    R3,132(R3)          NEXT LINE                                    
         B     HOOK8                                                            
         SPACE 1                                                                
HOOKX    GOTO1 VRESHEAD                                                         
         B     XIT                                                              
         SPACE 2                                                                
DASHES   DC    40C'-'                                                           
         EJECT                                                                  
*              COMMON ROUTINES                                                  
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
*                                                                               
*              LITERAL POOL AND CONSTANTS                                       
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 1                                                                
*                                  MY OWN ERROR MESSAGES                        
         SPACE 1                                                                
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 8'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 8'                         
TOOBIG   DC    C'* ERROR * PLEASE SHORTEN REQUEST OR RUN OVERNIGHT'             
PAVONLY  DC    C'* ERROR * PAV OPTION ONLY'                                     
NOFOUND  DC    C'* ERROR * RECORD NOT FOUND - '                                 
OPTERR   DC    C'* ERROR * INVALID OPTION'                                      
         SPACE 3                                                                
*                                  REPORT HEADLINE SPECS                        
         SPACE 1                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V'                                    
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,77,PAGE                                                       
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 3                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL220                                                           
BUFFBK   DS    CL4                                                              
BUFFPROG DS    6CL20        16 CHAR PROG NAME, 4 CHAR WEEK INDICATOR            
BUFFDEM  DS    6CL16               8 DEMOS, 2 BYTES EACH                        
         SPACE 2                                                                
*              SPGENPROG HERE                                                   
*              NERESALL                                                         
         PRINT OFF                                                              
       ++INCLUDE SPGENPROGA                                                     
       ++INCLUDE NERESALL1                                                      
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NERESF3D                                                       
         EJECT                                                                  
*              LOCAL WORKING STORAGE                                            
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
SPACOPT  DS    XL1                                                              
STARTNUM DS    XL1                                                              
ENDNUM   DS    XL1                                                              
SAVSTART DS    XL1                                                              
FIRST    DS    CL1                                                              
COST     DS    F                                                                
DETDISP  DS    F                                                                
MILST    DS    H                                                                
BYTEA    DS    C                                                                
BYTEB    DS    C                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 1                                                                
*              DDCOMFACTS, FAFACTS, & NEGETNUND                                 
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NEGETNUND                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044NERES03   05/01/02'                                      
         END                                                                    
