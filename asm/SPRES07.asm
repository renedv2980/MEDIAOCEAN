*          DATA SET SPRES07    AT LEVEL 025 AS OF 12/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T20F07A                                                                  
         TITLE 'T20F07 - RADIO TREND'                                           
T20F07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F07**,RA,RR=R2                                              
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
         L     RE,=V(SUBR07)                                                    
         A     RE,RELO                                                          
         ST    RE,VSUBR07                                                       
*                                                                               
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         DROP  RF                                                               
         CH    R0,=H'12'           PF1 - PF12 IS OKAY                           
         BNH   *+8                                                              
         SH    R0,=H'12'           PF13 - PF24 = PF1 - PF12                     
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* ********************************************************************          
*              VALIDATE REQUEST AT MODE=VALREC                                  
* ********************************************************************          
         SPACE 3                                                                
VREC     MVI   MULTCATS,C'N'                                                    
         MVI   STALSTD,C'N'                                                     
         OI    TRESTATH+6,X'80'                                                 
         LA    R2,TRESRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         CLI   DBSELMED,C'T'       IF TV                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'02'                                                     
         SPACE 1                                                                
         MVC   MEDTYPE,DBSELMED                                                 
         MVC   SRCTYPE,DBSELSRC                                                 
         XC    BOOKTYPE,BOOKTYPE                                                
*                                                                               
         CLC   TREBOOK(4),=C'LATEST'                                            
         BNE   NLATEST                                                          
         MVC   TREBOOK(5),=C'1BOOK'                                             
         MVC   TREBOOK+5(3),TREBOOK+6                                           
         MVI   TREBOOK+8,C' '                                                   
NLATEST  LA    R2,TREBOOK                                                       
         ZIC   R1,TREBOOKH+5                                                    
         LTR   R1,R1                                                            
         BZ    VREC01A                                                          
         BCTR  R1,0                                                             
         AR    R1,R2               ADDRESS OF LAST CHAR IN BOOK                 
SRCHLP   CR    R2,R1               CHECKED ALL CHARS?                           
         BH    VREC01A                                                          
         CLI   0(R2),C'('                                                       
         BE    FOUNDCH                                                          
         LA    R2,1(R2)                                                         
         B     SRCHLP                                                           
*                                                                               
FOUNDCH  CLI   2(R2),C')'          MAKE SURE ENCLOSED BY ()                     
         BNE   VREC01A                                                          
         MVC   BOOKTYPE,1(R2)                                                   
VREC01A  MVC   SVMKTH5,TREMKTH+5                                                
         XC    MKTNUM,MKTNUM                                                    
         USING ELEMDS,R2                                                        
         GOTO1 =A(AFTPARSE),DMCB,(R9),(RC),RR=RELO                              
         BZ    MKTXST                                                           
*                                                                               
OUTMRKT  L     R2,OUTAREA                                                       
         CLI   0(R2),C'='                                                       
         BE    OUTMRKT1                                                         
         CLI   0(R2),C','                                                       
         BE    OUTMRKT1                                                         
         MVC   2(3,R2),=X'02E400'                                               
OUTMRKT1 CLC   2(3,R2),=X'02E4'                                                 
         BNH   OUTMRKT2                                                         
         MVC   CONHEAD(L'MKTERR2),MKTERR2                                       
         B     CPERR                                                            
OUTMRKT2 MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     CPERR                                                            
MKTXST   L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('TESTMKE',(RC))                                    
         BNZ   OUTMRKT1                                                         
         STH   R1,MKTNUM                                                        
         GOTO1 =A(ALPHMRKT),DMCB,(R9),(RC),RR=RELO                              
         MVC   DBAREC,AIO1                                                      
         MVI   STASTYLE,C'M'                                                    
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         LA    R2,TREBOOKH         VALIDATE BOOK                                
         MVI   MAX,5                                                            
*                                                                               
         MVC   BADMULP(1),TREBOOK  SET NUMBER OF BOOKS                          
         TM    TREBOOK,X'F0'       SOFT BOOK REQUEST                            
         BNO   *+12                                                             
         BAS   RE,VMULTBK                                                       
         BE    VREC02              HAVE IT - BYPASS REGULAR EDIT                
*                                                                               
         CLI   BYTE,2              NOT ENOUGH BOOKS                             
         BNE   *+14                                                             
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         B     SPERR                                                            
*                                                                               
         GOTO1 VRADBOOK                                                         
         CLI   NBOOKS,4            TAKE UP TO 4 BOOKS                           
         BNH   VREC02                                                           
         MVC   CONHEAD(L'MNYBKS),MNYBKS     TOO MANY BOOKS                      
         B     MYEND                                                            
         SPACE 1                                                                
VREC02   LA    R2,TREMAXH          MAXIMUM STATIONS TO RANK                     
         MVI   MAXRANK,10          DEFAULT IS 10                                
         CLI   5(R2),0                                                          
         BE    VREC10                                                           
         GOTO1 VALINUM                                                          
         MVC   MAXRANK,ACTUAL                                                   
         CLI   MAXRANK,100                                                      
         BNH   VREC10                                                           
         MVC   CONHEAD(L'TOOMNY),TOOMNY                                         
         B     MYEND                                                            
         SPACE 1                                                                
VREC10   MVI   NMARKET,0                                                        
         BAS   RE,EDITMKT                                                       
         LA    R2,TREOPTH          OPTIONS                                      
         GOTO1 VSUBR07,DMCB,('EDITOPTE',(RC))                                   
         BNE   SPERR                                                            
         L     R2,OUTAREA                                                       
         GOTO1 VSUBR07,DMCB,('EDITMLE',(RC))                                    
         BNE   CPERR                                                            
*                                                                               
         OC    MKTNUM,MKTNUM                                                    
         BZ    VREC11                                                           
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BNE   BADBOOK                                                          
         SPACE 2                                                                
VREC11   LA    R2,TREDEMOH         VALIDATE DEMOGRAPHIC                         
         MVI   MAX,8                                                            
         MVI   NFLDS,1                                                          
         GOTO1 VVALDEM                                                          
         CLI   NDEMOS,1                                                         
         BNH   VREC12                                                           
         MVC   CONHEAD(L'TOOMNYDM),TOOMNYDM                                     
         B     MYEND                                                            
         SPACE 1                                                                
VREC12   MVI   MAX,8                                                            
         LA    R2,TRECATH          DEMO CATEGORY                                
         GOTO1 VVALCATS                                                         
         CLI   NCATS,1                                                          
         BE    VREC16                                                           
         MVI   MULTCATS,C'Y'                                                    
         CLI   NCATS,3                                                          
         BNH   VREC15                                                           
         MVC   CONHEAD(L'TOOMNYCT),TOOMNYCT                                     
         B     MYEND                                                            
         SPACE 1                                                                
VREC15   SR    RE,RE                                                            
         LA    RF,100              THE MAX # OF STATIONS                        
         ZIC   R5,NCATS                                                         
         DR    RE,R5               GET REAL LIMIT OF STATIONS                   
         ZIC   R5,MAXRANK                                                       
         CR    R5,RF                                                            
         BNH   VREC16                                                           
         LA    R2,TREMAXH                                                       
         MVC   CONHEAD(L'TOOMNY),TOOMNY                                         
         LA    R5,CONHEAD+23                                                    
         EDIT  (RF),(3,(R5))                                                    
         B     MYEND                                                            
         SPACE 1                                                                
VREC16   LA    R2,TREDPTH          DAYPART                                      
         BAS   RE,EDITDPT                                                       
         LA    R2,TREFILTH         FILTERS                                      
         BAS   RE,EDITFILT                                                      
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,1000(R2)                                                      
         MVC   SVMKTNUM,MKTNUM                                                  
*                                                                               
VREC17   CLI   0(R2),0             EOL                                          
         BE    VREC17W                                                          
         CLI   0(R2),1             MARKET NUMBER                                
         BE    VREC17C                                                          
VREC17B  ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     VREC17                                                           
         SPACE 1                                                                
VREC17C  TM    TREBOOK,X'F0'       CHECK FOR MULTI BOOK                         
         BNO   VREC17G             NO - CHECK REQUESTED BOOKS                   
         MVC   MKTNUM,2(R2)        YES - SEED THE MARKET NUMBER                 
         MVI   TREMKTH+5,0                                                      
         BAS   RE,VMULTBK          CHECK BOOKS FOR THIS MARKET                  
         MVC   TREMKTH+5(1),SVMKTH5                                             
         CLI   NBOOKS,0                                                         
         BE    BADBOOKM                                                         
         CLI   BYTE,2                                                           
         BNE   VREC17B                                                          
         MVC   CONHEAD(L'BADMUL),BADMUL                                         
         LH    R0,MKTNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+L'BADMUL+1(4),DUB+5(3)                                   
         B     MYEND                                                            
         SPACE 1                                                                
VREC17G  MVC   MKTNUM,2(R2)                                                     
         GOTO1 VSUBR07,DMCB,('EDITMBE',(RC))                                    
         BE    VREC17B                                                          
BADBOOKS MVC   ERRBOOK+L'ERRBOOK-7(7),=C'STATION'                               
         B     BADBOOK                                                          
         SPACE 1                                                                
BADBOOKM MVC   ERRBOOK+L'ERRBOOK-7(7),=C' MARKET'                               
BADBOOK  MVC   CONHEAD(L'ERRBOOK),ERRBOOK                                       
         MVC   CONHEAD+L'ERRBOOK+1(4),DBSELSTA                                  
         B     MYEND                                                            
         SPACE 1                                                                
VREC17W  MVC   TREMKTH+5(1),SVMKTH5   RESTORE FIELDS                            
         MVC   MKTNUM,SVMKTNUM                                                  
*                                                                               
VREC19   LA    R2,TRETTLH          CUSTOM TITLE                                 
         L     RE,=A(RESTIT)                                                    
         A     RE,RELO                                                          
         MVC   RESTITLE(L'RESTIT),0(RE)                                         
         CLI   5(R2),0                                                          
         BE    VREC20                                                           
         GOTO1 ANY                                                              
         MVC   RESTITLE,WORK                                                    
         SPACE 1                                                                
VREC20   GOTO1 CENTER,DMCB,RESTITLE,40                                          
         SPACE 1                                                                
VRECX    MVC   TRESTATH+5(1),STATLEN                                            
         B     XIT                                                              
         EJECT                                                                  
VMULTBK  NTR1                                                                   
         MVI   NBOOKS,0                                                         
         XC    BOOKS,BOOKS                                                      
         CLI   TREMKTH+5,0                                                      
         BE    *+8                                                              
         BAS   RE,EDITMKT          DO THIS TO GET MARKET #                      
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         MVC   DBSELRMK,MKTNUM                                                  
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
*                                                                               
         LA    R1,1                                                             
         CLC   TREBOOK+1(2),=C'BOOK'                                            
         BNE   XIT                                                              
         MVC   BYTE(1),TREBOOK     SET NUMBER OF BOOKS                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BH    VMULTBKX                                                         
         CLI   TREBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,TREBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOK                                        
         LA    R1,2                                                             
         CLI   NBOOKS,0                                                         
         BE    VMULTBKX                                                         
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         CLC   NBOOKS,BYTE         NOT ENOUGH BOOKS                             
         BNE   VMULTBKX                                                         
*                                                                               
         ZIC   R0,BYTE             DON'T ALLOW PREV. YEAR                       
         LA    RF,BOOKS                                                         
VMULTBK2 ZIC   R1,2(RF)            GET THE BOOK                                 
         LA    R1,WORK(R1)                                                      
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,2                SET ERROR NUMBER                             
         B     VMULTBKX                                                         
*                                                                               
         MVI   0(R1),C'0'                                                       
         LA    RF,4(RF)                                                         
         BCT   R0,VMULTBK2                                                      
*                                                                               
         SR    R1,R1                                                            
VMULTBKX LTR   R1,R1                                                            
         STC   R1,BYTE                                                          
         B     XIT                                                              
         SPACE 2                                                                
* ---------------------------------------------------------------               
BKHOOK   NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT                                                              
         CLC   SBBTYP,DBBTYPE                                                   
         BNE   XIT                                                              
* CHECK FOR DUP BOOKS                                                           
         ZIC   R1,NBOOKS                                                        
         STC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         CLC   1(2,R1),SBBOOK                                                   
         BE    XIT                                                              
*                                                                               
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,BYTE         BOOK TABLE FULL - NEED A SLIDE               
         BNE   BKHOOK2                                                          
         MVC   BOOKS(L'BOOKS-4),BOOKS+4                                         
         IC    R1,NBOOKS                                                        
*                                                                               
BKHOOK2  STC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         MVC   1(2,R1),SBBOOK                                                   
         MVC   3(1,R1),SBBTYP                                                   
         B     XIT                                                              
         EJECT                                                                  
*              EDIT MARKET (OPTIONALLY STATIONS BELOW)                          
         SPACE 3                                                                
EDITMKT  NTR1                                                                   
         OC    MKTNUM,MKTNUM                                                    
         BNZ   HAVEMKTN                                                         
         LA    R2,TREMKTH          EITHER INPUT MARKET NUMBER                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                              OR STATION LIST                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         LTR   R1,R1                                                            
         BZ    EDITLIST                                                         
         STH   R1,MKTNUM                                                        
         MVI   STASTYLE,C'M'                                                    
HAVEMKTN MVI   TREMKTN,C'*'                                                     
         BAS   RE,GETMKT                                                        
         CLI   TREMKTN,C'*'                                                     
         BE    BADMKT                                                           
         ZIC   RE,NMARKET                                                       
         LA    RE,1(RE)                                                         
         STC   RE,NMARKET                                                       
         MVC   STASTYLE,STALIST                                                 
         CLI   TRESTATH+5,0                                                     
         BNE   EDITSTAT                                                         
         B     XIT                                                              
         SPACE 1                                                                
GETMKT   NTR1                                                                   
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         B     XIT                                                              
         SPACE 1                                                                
MKTHOOK  NTR1                      HOOK FOR MARKET NAME                         
         OI    TREMKTNH+6,X'80'                                                 
         MVC   TREMKTN,SPACES                                                   
         L     R6,DBAREC                                                        
         USING DMKEY,R6                                                         
         LA    R6,DMFRSTEL                                                      
         USING DMELEM,R6                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   TREMKTN(0),DMMNAME                                               
         DROP  R6                                                               
         EJECT                                                                  
*              EDIT A LIST OF STATIONS                                          
         SPACE 3                                                                
*                                  IF MARKET WAS NOT NUMERIC                    
*                                  IT MIGHT BE A STATION LIST                   
         SPACE 1                                                                
EDITLIST CLI   TRESTATH+5,0                                                     
         BNE   BADMKT                                                           
         MVI   STASTYLE,C'L'                                                    
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,BLOCK+12                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADMKT                                                           
         GOTO1 GETREC                                                           
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
         SR    R3,R3               COUNT NUMBER OF VALID STATIONS               
         LA    R4,BUFF             STATIONS ARE IN BUFF                         
         USING STATENTD,R4                                                      
         SPACE 1                                                                
LIST2    CLI   0(R5),X'01'         DESCRIPTION ELEMENT                          
         BNE   LIST4                                                            
         USING SLDELEM,R5                                                       
         MVC   TREMKTN,SLDDESC                                                  
         OI    TREMKTNH+6,X'80'                                                 
         MVC   MKTNUM,SLDMKTA      PICK UP ARB OR BIRCH MKT NUM                 
         CLI   CTRY,C'C'                                                        
         BE    LIST4                                                            
         CLI   TRESRCE,C'B'                                                     
         BNE   *+10                                                             
         MVC   MKTNUM,SLDMKTB                                                   
         B     LISTNEXT                                                         
         SPACE 1                                                                
LIST4    CLI   0(R5),X'05'         STATION ELEMENT                              
         BNE   LISTNEXT                                                         
         USING SLSELEM,R5                                                       
         MVC   WORK(4),SLSFORM     SAVE FORMAT                                  
         LA    R6,SLSTALST                                                      
         ZIC   R1,1(R5)            N'STATION = (L-6)/5                          
         SH    R1,=H'6'                                                         
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         AR    R3,R1                                                            
         SPACE 1                                                                
LIST6    MVC   STATCALL,0(R6)      THESE ENTRIES ARE STATION                    
         MVC   STATFORM,WORK       PLUS FORMAT                                  
         LA    R4,L'STATENT(R4)                                                 
         LA    R6,5(R6)                                                         
         BCT   R1,LIST6                                                         
         SPACE 1                                                                
LISTNEXT ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BNE   LIST2                                                            
         STC   R3,NSTATS                                                        
         CLC   MAXRANK,NSTATS                                                   
         BL    *+10                                                             
         MVC   MAXRANK,NSTATS                                                   
         OC    MKTNUM,MKTNUM       SHOULD HAVE MARKET NUMBER NOW                
         BZ    BADLIST                                                          
         B     XIT                                                              
         SPACE 1                                                                
BADMKT   MVC   CONHEAD(L'MKTERR),MKTERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
BADLIST  MVC   CONHEAD(L'LISTERR),LISTERR                                       
         B     MYEND                                                            
         EJECT                                                                  
*              EDIT CUSTOM STATIONS                                             
         SPACE 3                                                                
EDITSTAT MVI   STASTYLE,C'C'                                                    
         LA    R2,TRESTATH         OR A LIST OF STATIONS                        
         LA    R3,1                COUNT NUMBER OF VALID STATIONS               
         LA    R4,BUFF             STATIONS ARE IN BUFF                         
         USING STATENTD,R4                                                      
*                                                                               
         CLI   DBSELSRC,C'M'                                                    
         BE    STAT3                                                            
         LR    RF,R4                                                            
         MVI   4(RF),C'A'                                                       
         EDIT  (2,MKTNUM),(4,(RF))                                              
*                                                                               
         LA    RE,4                                                             
         LR    R3,R4                                                            
STAT1A   CLI   0(R3),C' '                                                       
         BNE   STAT1B                                                           
         MVI   0(R3),C'0'                                                       
STAT1B   LA    R3,1(R3)                                                         
         BCT   RE,STAT1A                                                        
         LA    R4,L'STATENT(R4)                                                 
         LA    R3,2                2 STATIONS                                   
*                                                                               
STAT3    LA    R0,12               MAXIMUM OF 12 FROM SCREEN                    
         GOTO1 ANY                 MUST BE AT LEAST 1                           
         SPACE 1                                                                
STAT4    GOTO1 VVALDSTA           VALIDATE THIS ONE                             
         MVC   STATCALL,ACTSTAT                                                 
         STC   R3,NSTATS                                                        
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    STAT6                                                            
         LA    R3,1(R3)                                                         
         LA    R4,L'STATENT(R4)                                                 
         BCT   R0,STAT4                                                         
         SPACE 1                                                                
STAT6    CLC   MAXRANK,NSTATS                                                   
         BL    *+10                                                             
         MVC   MAXRANK,NSTATS                                                   
         CLI   DBSELSRC,C'M'                                                    
         BE    STA9                                                             
         ZIC   RF,MAXRANK                                                       
         LA    RF,1(RF)                                                         
         STC   RF,MAXRANK                                                       
STA9     MVI   TREMKTN,C'*'                                                     
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,ACTMKT                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,MKTHOOK                                       
         CLI   TREMKTN,C'*'                                                     
         BE    BADMKT                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              EDIT DAYPART                                                     
         SPACE 3                                                                
*              AGENCY SPECIFIED    DAY/TIMES INPUT                              
         SPACE 1                                                                
EDITDPT  NTR1                                                                   
         LA    R2,TREDPTH                                                       
         GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         LA    R1,TREDPT                                                        
         SPACE 1                                                                
VR2      MVC   DMCB+8(4),=C',=/='                                               
VR2A     CLI   0(R1),C'/'          SEE IF SLASH IS USED                         
         BE    VR2C                                                             
         CLI   0(R1),C'+'          CANADA USES +                                
         BE    VR2D                                                             
         CLI   0(R1),C'-'          EVERTHING BEFORE '-' --> '/'                 
         BL    VR2C                                                             
         CLI   0(R1),C','                                                       
         BE    VR2C                                                             
         B     VR2D                                                             
VR2C     MVI   0(R1),C'/'                                                       
VR2D     LA    R1,1(R1)                                                         
         BCT   R0,VR2A                                                          
         SPACE 1                                                                
         LA    R3,DPCDAY                                                        
         SR    R5,R5               (COUNT N'DAY/TIMES IN R5)                    
         MVI   SCANLEN,32                                                       
         XC    BLOCK(250),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK)                                     
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
VR10     ZIC   R0,0(R4)                                                         
VR13     GOTO1 DAYVAL,DMCB,((R0),12(R4)),BYTE,WORK                              
         CLI   BYTE,0                                                           
         BNE   VR20                NON-ZERO MEANS VALID INPUT                   
         SPACE 1                                                                
         BAS   RE,FRIENDLY         CHECK FOR EASY EXPRESSIONS                   
         BE    XIT                                                              
         LA    R1,SPCLDAY          CHECK FOR SPECIAL DAYS                       
         LA    R0,SPCLDAYL                                                      
         SPACE 1                                                                
VR14     CLC   12(4,R4),1(R1)                                                   
         BNE   VR16                                                             
         MVC   BYTE,0(R1)          SET VALID INPUT                              
         B     VR20                                                             
         SPACE 1                                                                
VR16     LA    R1,1(R1)                                                         
         BCT   R0,VR14                                                          
         B     BADDPT                                                           
         SPACE 1                                                                
SPCLDAY  DS    0CL6                                                             
         DC    X'03',CL5'S-S  '    DAY VALUE/INPUT STRING                       
         DC    X'03',CL5'SA-S '                                                 
         DC    X'03',CL5'S-SU '                                                 
         DC    X'7F',CL5'M-S  '                                                 
SPCLDAYL EQU   *-SPCLDAY                                                        
         SPACE 1                                                                
VR20     LA    R1,DAYLIST                                                       
         LA    R0,DAYLISTL                                                      
         SPACE 1                                                                
VR22     CLC   BYTE,0(R1)                                                       
         BE    VR24                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,VR22                                                          
         B     BADDPT                                                           
         SPACE 1                                                                
DAYLIST  DC    X'7C'               MO-FR                                        
         DC    X'7E'               MO-SA                                        
         DC    X'7F'               MO-SU                                        
         DC    X'03'               SA-SU                                        
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
DAYLISTL EQU   *-DAYLIST                                                        
         SPACE 1                                                                
VR24     MVC   0(1,R3),BYTE        MOVE DAY TO LIST                             
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0                                                          
         BNE   VR26                                                             
         MVI   ERROR,NOTIME        TELL USER TIME IS MISSING                    
         GOTO1 VCURSERR                                                         
         SPACE 1                                                                
VR26     BAS   RE,VALTIME                                                       
         BNE   BADDPT                                                           
         SPACE 1                                                                
VR28     MVC   1(2,R3),HALF        MOVE TIME TO ELEMENT                         
         SPACE 1                                                                
         LA    R3,3(R3)            ADVANCE DAY/TIME LIST POINTER                
         LA    R4,32(R4)                                                        
         LA    R5,1(R5)                                                         
         CLI   0(R4),0             TEST ANY MORE INPUT THIS FIELD               
         BNE   VR29                                                             
         B     XIT                                                              
         SPACE 1                                                                
VR29     CH    R5,=H'5'                                                         
         BE    VR30                                                             
         SPACE 1                                                                
         BAS   RE,VALTIME          VALIDATE FOR ANOTHER TIME                    
         BNE   VR10                IF INVALID, TRY FOR ANOTHER DAY              
         SPACE 1                                                                
         LR    RE,R3                                                            
         SH    RE,=H'3'                                                         
         MVC   0(1,R3),0(RE)       COPY DAY FROM PREVIOUS                       
         B     VR28                 AND CONTINUE                                
         SPACE 1                                                                
VR30     MVI   ERROR,TOOMANY                                                    
         B     ERREND                                                           
         SPACE 1                                                                
BADDPT   L     RE,=A(INVDAYPT)                                                  
         A     RE,RELO                                                          
         MVC   CONHEAD(L'INVDAYPT),0(RE)                                        
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE FOR FRIENDLY DAYPART EXPRESSION                         
         SPACE 3                                                                
FRIENDLY NTR1                                                                   
         CLI   DBSELSRC,C'M'                                                    
         BE    CFRENDLY                                                         
         LA    R1,FRIENDS                                                       
         SPACE 1                                                                
FRIEND2  CLC   0(8,R1),BLOCK+12                                                 
         BE    FRIEND4                                                          
         LA    R1,23(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT                                                           
         B     FRIEND2                                                          
         SPACE 1                                                                
FRIEND4  MVC   DPCDAY(15),8(R1)                                                 
         B     EQXIT                                                            
         SPACE 1                                                                
FRIENDS  DC    C'AMDRIVE ',X'7C',AL1(06,10),12X'00'                             
         DC    C'PMDRIVE ',X'7C',AL1(15,19),12X'00'                             
         DC    C'DRIVE   ',X'7C',AL1(06,10),X'7C',AL1(15,19),9X'00'             
         DC    X'FF'                                                            
         EJECT                                                                  
CFRENDLY L     R1,=A(CFRENDS)                                                   
         A     R1,RELO                                                          
         SPACE 1                                                                
CFREND2  CLC   0(8+7,R1),BLOCK+12                                               
         BE    CFREND4                                                          
         LA    R1,23+7(R1)                                                      
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT                                                           
         B     CFREND2                                                          
         SPACE 1                                                                
CFREND4  MVC   DPCDAY(15),8+7(R1)                                               
         B     EQXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO VALIDATE A TIME EXPRESSION                            
         SPACE 3                                                                
VALTIME  NTR1                      VALIDATE TIME EXPRESSION                     
         ZIC   R0,0(R4)                                                         
         CLI   1(R4),0             A RANGE, NO SECOND FIELD                     
         BZ    *+8                                                              
         B     NEQXIT                                                           
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),FULL                                   
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT                                                           
         SPACE 1                                                                
         MVI   ERROR,BADSTTIM                                                   
         LH    R0,FULL                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TIME MUST BE HOUR ONLY                       
         BNZ   ERREND                                                           
         STC   R1,HALF             SET START HOUR                               
         SPACE 1                                                                
         MVI   ERROR,BADNDTIM                                                   
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   ERREND                                                           
         STC   R1,HALF+1           SET END HOUR                                 
         B     EQXIT                                                            
         EJECT                                                                  
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
*              EDIT FILTERS                                                     
         SPACE 3                                                                
EDITFILT NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
FILT2    DS    0H                                                               
         SPACE 1                                                                
BADFILT  L     RE,=A(FILTERR)                                                   
         A     RE,RELO                                                          
         MVC   CONHEAD(L'FILTERR),0(RE)                                         
         B     SPERR                                                            
         SPACE 1                                                                
FILTEND  LA    R4,32(R4)                                                        
         BCT   R0,FILT2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
TMPDEMOS DC    XL60'00'                                                         
*                                                                               
PREP     L     R1,=A(HEDSPECS)     INTIALIZATION                                
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS,RUNIVS                                                    
         MVC   TMPDEMOS,DEMOS                                                   
*                                                                               
         CLI   CALCOPT,C'Y'        DO IMPRESSIONS?                              
         BNE   NOTIMPS1                                                         
*--------------------------------------------------------------                 
*  EQUATES FOR IMPRESSIONS                                                      
*    1)  F = C                                                                  
*    2)  G = D                                                                  
*    3)  H = E                                                                  
*    4)  P = Q                                                                  
*    5)  R = I                                                                  
*--------------------------------------------------------------                 
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
IMPAGN1  CLI   1(R2),C'F'                                                       
         BNE   NOEQU1A                                                          
         MVI   1(R2),C'C'                                                       
         B     IMPNXT1                                                          
NOEQU1A  CLI   1(R2),C'G'                                                       
         BNE   NOEQU2A                                                          
         MVI   1(R2),C'D'                                                       
         B     IMPNXT1                                                          
NOEQU2A  CLI   1(R2),C'H'                                                       
         BNE   NOEQU3A                                                          
         MVI   1(R2),C'E'                                                       
         B     IMPNXT1                                                          
NOEQU3A  CLI   1(R2),C'P'                                                       
         BNE   NOEQU4A                                                          
         MVI   1(R2),C'Q'                                                       
         B     IMPNXT1                                                          
NOEQU4A  CLI   1(R2),C'R'                                                       
         BNE   NOEQU5                                                           
         MVI   1(R2),C'I'                                                       
NOEQU5   CLI   1(R2),C'S'                                                       
         BNE   IMPNXT1                                                          
         CLI   DBSELSRC,C'M'       CANADIAN MARKETS DON'T HAVE TOTALS           
         BE    IMPNXT1             SKIP CONVERSION                              
         MVI   1(R2),C'I'                                                       
         ZIC   RF,USESHARE                                                      
         LA    RF,1(RF)                                                         
         STC   RF,USESHARE                                                      
IMPNXT1  LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,IMPAGN1                                                       
*                                                                               
NOTIMPS1 OC    MKTNUM,MKTNUM       RESET BOOKS FOR SOFT LIST                    
         BZ    *+16                                                             
         TM    TREBOOK,X'F0'                                                    
         BNO   *+8                                                              
         BAS   RE,VMULTBK                                                       
*                                                                               
         SR    RE,RE                                                            
         ZIC   RF,NDEMOS                                                        
         M     RE,=A(L'BUFFREC)                                                 
         ST    RF,NXTPOS                                                        
*                                                                               
         L     RE,AIO2             SET THE USER LIST POINTER                    
         LA    RE,1000(RE)                                                      
         ST    RE,ULPNTR                                                        
*                                                                               
         CLI   0(RE),1             CHECK FOR DUP MARKET                         
         BNE   PREP1                                                            
         CLC   MKTNUM,2(RE)        BYPASS IF EQUAL                              
         BNE   PREP1                                                            
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         ST    RE,ULPNTR                                                        
*                                                                               
PREP1    MVI   ANYUNIV,C'N'                                                     
         MVI   USEBUFF,C'N'        NOT USING BUFFER                             
         MVI   CMB_B4,C'N'         COMBO NOT BEFORE STATION                     
         XC    UNIVBK,UNIVBK                                                    
         MVI   SPACTSW,0                                                        
         MVI   COMBNUM,0           SET FOR THIS MARKETS COMBOS                  
*                                                                               
         LA    RE,DBRADIC          SET UP FOR COND. MARKET                      
         USING DBEXTRAD,RE                                                      
         ST    RE,DBEXTEND                                                      
         XC    0(128,RE),0(RE)                                                  
         MVC   DBRID,=C'RADI'                                                   
         MVI   DBRCOPT,C'Y'        INDICATE THAT REPORT SUPPORTS                
         DROP  RE                                                               
*                                                                               
         XC    COMBSV,COMBSV                                                    
         BAS   RE,SETDPT           COMPLETE DAYPART ENTRY                       
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         LA    RE,1000(RE)                                                      
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         OC    MKTNUM,MKTNUM       JIC A LIST ONLY INPUT                        
         BZ    PREP4                                                            
         BAS   RE,GETMKT                                                        
         TM    TREBOOK,X'F0'       RESET MULTI BOOKS                            
         BNO   *+8                                                              
         BAS   RE,VMULTBK                                                       
*                                                                               
         GOTO1 =A(COMPDISP),DMCB,(R9),(RC),RR=RELO   COMPUTE PRNT DISPS         
         GOTO1 =A(EXPSTATS),DMCB,(R9),(RC),RR=RELO   EXPLODE STATN LIST         
*                                                                               
         L     R2,ASTATS                                                        
         USING STATENTD,R2                                                      
         LA    R3,1                                                             
         L     R4,ABOOKBF                                                       
         ZIC   R0,NSTATS                                                        
         LTR   R0,R0                                                            
         BNZ   PREP2                                                            
         CLI   STASTYLE,C'L'                                                    
         BE    PREP4                                                            
         SPACE 1                                                                
PREP2    BAS   RE,FILLBF                                                        
         LA    R2,L'STATENT(R2)                                                 
         LA    R3,1(R3)                                                         
         A     R4,NXTPOS           NXTPOS = # OF CATS X 20                      
         BCT   R0,PREP2                                                         
         SPACE 1                                                                
PREP4    L     R1,ULPNTR           POINT TO CURR. USER LIST                     
         MVI   TREMKTH+5,0                                                      
         CLI   0(R1),0             NOTHING - JUST FINISH THE REPORT             
         BE    PREP20                                                           
         CLI   0(R1),1             MARKET ELEMENT                               
         BNE   PREP6                                                            
         OC    MKTNUM,MKTNUM       ANY MKT BEING PROCESSED                      
         BNZ   PREP5                                                            
         MVC   MKTNUM,2(R1)        NO - DO THIS ONE NOW                         
*        MVI   STASTYLE,C'M'                                                    
         B     PREP1                                                            
         SPACE 1                                                                
PREP5    CLC   MKTNUM,2(R1)        UL FOR THIS MARKET ?                         
         BNE   PREP20              NO - JUST PRINT REPORT                       
         ZIC   RE,1(R1)            YES - TAKE A LOOK AT IT                      
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         SPACE 1                                                                
PREP6    CLI   0(R1),3             UL - COMBO ELEMENT                           
         BE    PREP8                                                            
         CLI   0(R1),4             STATION LIST                                 
         BE    PREP15                                                           
         CLI   0(R1),2             UL - STATION ELEMENT                         
         BE    PREP15                                                           
         BL    PREP4                                                            
         ZIC   RE,1(R1)            UNDEFINED - JUST BYPASS                      
         AR    R1,RE                                                            
         ST    R1,ULPNTR                                                        
         B     PREP4                                                            
         SPACE 1                                                                
PREP8    CLI   USEBUFF,C'N'        USING BUFFER?                                
         BNE   PREP8A              YES, SKIP THIS                               
         LR    RE,R1                                                            
*------------------------------------------------------------                   
* CHECK WHETHER OR NOT TO CLEAR BUFFER                                          
* BY CHECKING IF THERE IS A STATION LIST AFTER THIS BUT BEFORE                  
* A NEW MARKET OR BEFORE THE END OF USER LIST                                   
*------------------------------------------------------------                   
PREP8B1  ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    PREP8BX                                                          
         CLI   0(RE),1                                                          
         BE    PREP8BX                                                          
         CLI   0(RE),2             STATION BEFORE 1 AND 0                       
         BE    PREP8BC             CLEAR THE BUFFER                             
         B     PREP8B1                                                          
*                                                                               
PREP8BC  LA    RE,BUFF                                                          
         LA    RE,1000(RE)                                                      
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         LA    R3,1                                                             
         L     R4,ABOOKBF                                                       
         XC    NSTATS,NSTATS                                                    
         L     R2,ASTATS                                                        
         USING STATENTD,R2                                                      
         CLI   DBSELSRC,C'M'                                                    
         BE    PREP8BX                                                          
         GOTO1 =A(MKTSTAT),DMCB,(R2),(R9),(RC),RR=RELO                          
         BAS   RE,FILLBF                                                        
         LA    R2,L'STATENT(R2)                                                 
         LA    R3,2                                                             
         A     R4,NXTPOS                                                        
         L     R1,ULPNTR           RESTORE R1                                   
*                                                                               
PREP8BX  MVI   CMB_B4,C'Y'         COMBO BEFORE STATION                         
         MVI   USEBUFF,C'Y'        USING BUFFER, DON'T CLEAR NEXT TIME          
*                                                                               
PREP8A   ZIC   RF,1(R1)                                                         
         AR    RF,R1               POINT TO NEXT ELEMENT                        
         LA    R1,11(R1)           POINT TO STATION                             
         ST    R1,REGSV            DON'T CLOBBER                                
         ST    RF,REGSV+4                                                       
         SPACE 1                                                                
*--DO SOME INITIALIZATIONS                                                      
*                                                                               
         ZIC   RE,NSTATS           ADD ENTRY TO STAT. LIST                      
         LA    RE,1(RE)                                                         
         STC   RE,NSTATS                                                        
*                                                                               
         STM   R2,R3,REGSV+8                                                    
         SR    R3,R3               RE=#STATIONS IN THIS COMBO                   
         LA    R3,1(R3)            COUNT # STATIONS - AT LEAST 1                
         LA    R1,5(R1)            NEXT STATION (R1 PTS TO STATN LIST)          
         CR    R1,RF               END OF STATN LIST?                           
         BNE   *-10                                                             
*                                                                               
         L     R2,REGSV            GET ADDRESS OF BEG OF STATN LIST             
         GOTO1 XSORT,DMCB,(R2),(R3),5,5,0                                       
         LM    R2,R3,REGSV+8                                                    
*                                                                               
         MVI   CNOSTA,0            INIT BUFFERS,#STATNS,CUMERTGS                
         XC    CUMERAT(12*4),CUMERAT    4BOOKS X 3DEMOS X 4BYTES                
         XC    SUMBUF(12*4),SUMBUF                                              
         XC    TMPBUF(12*4),TMPBUF                                              
*                                                                               
         LA    R1,CUMERAT                                                       
         LA    RE,12               12 BUCKETS=4BOOKX X 3 DEMOS                  
         MVI   3(R1),1             INIT CUME RATING TO 1                        
         LA    R1,4(R1)            NEXT FULL WORD CUMERAT BUCKET                
         BCT   RE,*-8              BRANCH TO THE: 'MVI' INSTRUCTION             
*                                                                               
         MVI   TYPEDEM,C'C'        SAVE DEMOS(TSLDEMO). SET 'X' TO 'C'          
         BAS   RE,REPLDEM          REPLACE TSL WITH CUME                        
         MVC   ORGDEMO,TSLDEMO     SAVE AWAY ORIGINAL DEMO LIST                 
*                                                                               
*--END OF INITIALIZATIONS                                                       
*                                                                               
         SPACE 1                                                                
         L     R1,REGSV            RESTORE (CLOBBERED) STATION                  
         L     RF,REGSV+4          RESTORE (CLOBBERED) ELEMENT                  
*                                                                               
PREP9    CR    R1,RF                                                            
         BNL   PREP10              END OF THIS ONE                              
         MVC   STATCALL,0(R1)      SET THE CALL LETTERS                         
         LA    R1,5(R1)                                                         
         LR    R0,R1                                                            
         LR    R5,RF                                                            
         BAS   RE,FILLBF                                                        
         CLI   CNOSTA,0            ONLY SET THE UNIVERSES ONCE AT START         
         BNE   PREP9A                                                           
         GOTO1 VSUBR07,DMCB,('SETUNIVE',(RC))                                   
*                                                                               
PREP9A   GOTO1 =A(AVECOMPB),DMCB,(R9),(RC),RR=RELO                              
         DS    0H                                                               
         MVI   TYPEDEM,C'I'        SET DEMO LIST TO READ IMPRESSIONS            
         BAS   RE,REPLDEM          REPLACE DEMO LIST                            
         ST    R4,REGSV            SAVE ADDR OF BUFFER FOR REAL DEMOS           
         LA    R4,TMPBUF           PT TO TEMP BUFF TO PUT IMPR INTO             
         BAS   RE,FILLBF           READ IN IMPRESSIONS                          
         BAS   RE,RESTDEM          RESTORE DEMO LIST                            
         L     R4,REGSV            RESTORE ADDR OF BUFFER FOR CUMES             
*                                                                               
PREP9B   LR    RF,R5                                                            
         LR    R1,R0                                                            
         B     PREP9                                                            
         SPACE 1                                                                
*                                                                               
PREP10   MVC   TSLDEMO,ORGDEMO     SAVE AWAY ORIGINAL DEMO LIST                 
         BAS   RE,RESTDEM          RESET DEMO LIST TO THE ORIGINAL ONE          
         GOTO1 =A(TSLCALC),DMCB,(R9),(RC),RR=RELO     CALCULATE TSL             
         L     R1,ULPNTR                                                        
         MVC   STATCALL,2(R1)      SET THE DESCRIPTION                          
         MVC   STATCALL(3),=C'CMB' SET UP THE COMBO                             
         ZIC   RE,COMBNUM                                                       
         LA    RE,1(RE)                                                         
         STC   RE,COMBNUM                                                       
         STC   RE,STATCALL+3                                                    
         OI    STATCALL+3,X'F0'                                                 
         BCTR  RE,0                SAVE A POINTER                               
         STC   RE,STATCALL+4                                                    
         SLL   RE,2                                                             
         LA    RE,COMBSV(RE)                                                    
         ST    R1,0(RE)                                                         
         SPACE 1                                                                
         LA    R2,L'STATENT(R2)    AND BUMP THE POINTERS AND COUNTERS           
         LA    R3,1(R3)                                                         
         A     R4,NXTPOS                                                        
         ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         ST    R1,ULPNTR                                                        
         ZIC   RE,NSTATS                                                        
         LA    RE,1(RE)                                                         
         STC   RE,NSTATS                                                        
         B     PREP4               PROCESS NEXT COMBO                           
         SPACE 1                                                                
*                                  WE'RE DONE WITH THE STATIONS                 
PREP15   DS    0H                                                               
         CLI   USEBUFF,C'N'        NOT USING BUFFER, COULD CLEAR IT             
         BNE   PREP17                                                           
*                                                                               
         MVI   USEBUFF,C'Y'                                                     
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         LA    RE,1000(RE)                                                      
         L     RF,=F'2000'                                                      
         XCEF                                                                   
         LA    R3,1                                                             
         L     R4,ABOOKBF                                                       
         XC    NSTATS,NSTATS                                                    
         L     R2,ASTATS                                                        
         USING STATENTD,R2                                                      
*                                                                               
         CLI   DBSELSRC,C'M'                                                    
         BE    PREP17                                                           
         GOTO1 =A(MKTSTAT),DMCB,(R2),(R9),(RC),RR=RELO                          
         BAS   RE,FILLBF                                                        
         LA    R2,L'STATENT(R2)                                                 
         LA    R3,2                                                             
         A     R4,NXTPOS                                                        
         L     R1,ULPNTR           RESTORE R1                                   
*                                                                               
PREP17   ZIC   RF,1(R1)            LENGTH OF ELEMENT                            
         LA    R5,2(R1)            POINT TO FIRST STATION                       
         SH    RF,=H'2'                                                         
         CLI   0(R1),4                                                          
         BNE   PREP17A                                                          
         LA    R5,4(R5)            SKIP OVER FORMAT                             
         SH    RF,=H'4'            FORMAT FOR STALIST                           
PREP17A  SR    RE,RE                                                            
         D     RE,=F'5'                                                         
         ZIC   RE,NSTATS                                                        
         AR    RE,RF                                                            
         STC   RE,NSTATS                                                        
         LR    R0,RF               SAVE NUMBER OF NEW STATIONS                  
*                                                                               
PREP18   MVC   0(5,R2),0(R5)                                                    
         CLI   0(R1),4                                                          
         BNE   *+10                                                             
         MVC   5(4,R2),2(R1)                                                    
         BAS   RE,FILLBF                                                        
         LA    R2,L'STATENT(R2)                                                 
         LA    R3,1(R3)                                                         
         A     R4,NXTPOS                                                        
         LA    R5,5(R5)                                                         
         L     R1,ULPNTR                                                        
         BCT   R0,PREP18                                                        
*                                                                               
PREP18A  L     R1,ULPNTR                                                        
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         ST    R1,ULPNTR                                                        
         B     PREP4                                                            
*-------------------------------------------------------------------            
*                                                                               
         LA    R3,1                                                             
         L     R4,ABOOKBF                                                       
         ZIC   R0,NSTATS                                                        
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
PREP19   BAS   RE,FILLBF                                                        
         LA    R2,L'STATENT(R2)                                                 
         LA    R3,1(R3)                                                         
         A     R4,NXTPOS                                                        
         BCT   R0,PREP19                                                        
         SPACE 1                                                                
PREP20   CLI   DBSELSRC,C'M'                                                    
         BE    PREP20A                                                          
         GOTO1 VSUBR07,DMCB,('SETUNIVE',(RC))                                   
PREP20A  DS    0H                                                               
         GOTO1 =A(AVEBUFF),DMCB,(R9),RR=RELO                                    
         DS    0H                                                               
         BAS   RE,COMPRANK                                                      
         DS    0H                                                               
         GOTO1 =A(PRNTBUFF),DMCB,(R9),(RC),TMPDEMOS,RR=RELO                     
         MVI   RUNIFLG,C'N'                                                     
         XC    RUNIVS(16),RUNIVS                                                
         L     R1,ULPNTR                                                        
PREP22   CLI   0(R1),0             OK - WERE DONE                               
         BE    PREP26                                                           
         CLI   0(R1),1             MORE MARKETS                                 
         BE    PREP24                                                           
         ZIC   RE,1(R1)            ??? - JUST BYPASS                            
         AR    R1,RE                                                            
         B     PREP1                                                            
PREP24   MVC   DBSELRMK,2(R1)      SET THE MARKET NAME                          
         XC    MKTNUM,MKTNUM                                                    
         ST    R1,ULPNTR           SAVE THIS POINTER                            
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         B     PREP1                                                            
PREP26   MVC   TREMKTH+5(1),SVMKTH5                                             
         MVC   TRESTATH+5(1),STATLEN                                            
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
**********************************************************************          
*              ROUTINE FIGURES OUT WHETHER DAYPART IS STANDARD                  
*                                                                               
*              INPUTS              DPTENTRY                                     
*              OUTPUT              CHANGES DAYPART TO STANDARD                  
*                                  ENTRY IF A MATCH IS FOUND                    
*                                  PICKS OUT DAYPART NAME IF NEEDED             
* ********************************************************************          
         SPACE 1                                                                
SETDPT   NTR1                                                                   
         LA    R2,TREDPTH          DAYPART - REEDIT                             
         BAS   RE,EDITDPT                                                       
* CHECK FOR CUMES ACTIVE - USE CUME DAYPARTS                                    
         MVI   CUMESW,0                                                         
         LA    R2,DEMOS            CHECK ALL DEMOS FOR CUME DATA                
         ZIC   R0,NDEMOS                                                        
TSTCUM   LA    RE,CUMMOD                                                        
TSTCUM2  CLC   1(1,R2),0(RE)       FOUND A CUME DEMO                            
         BE    TSTCUMY             INDICATE IT                                  
         LA    RE,1(RE)            NO KEEP SEARCHING                            
         CLI   0(RE),X'FF'         EOT                                          
         BNE   TSTCUM2                                                          
         LA    R2,3(R2)            TEST NEXT DEMO                               
         BCT   R0,TSTCUM           UNTIL EXHAUSTED                              
         B     *+8                                                              
TSTCUMY  MVI   CUMESW,C'C'                                                      
         B     SETDPTS1                                                         
CUMMOD   DC    C'CEFHJT',X'FF'                                                  
         DS    0H                                                               
*                                                                               
SETDPTS1 L     R2,=A(STANDPTS)     LOOK UP STANDARD DAYPARTS                    
         A     R2,RELO                                                          
         USING STANDENT,R2                                                      
*                                                                               
         CLI   DBSELSRC,C'M'       BBM HAS DIFFERENT DAYPARTS                   
         BNE   *+12                                                             
         L     R2,=A(BBMDPTS)                                                   
         A     R2,RELO                                                          
*                                                                               
         MVI   DPCTYPE,C'C'        PRESET TYPE TO CUSTOM                        
         MVC   DPDESC,TREDPT       AND TAKE SCREEN INPUT                        
         SPACE 1                                                                
LUKDPT2  CLI   0(R2),X'FF'         IF NO MATCH IS FOUND                         
         BE    XIT                    IT IS A CUSTOM ONE                        
         CLC   STANDDTS(15),DPCDAY LOOK FOR A MATCH ON DAY/TIMES                
         BNE   LUKDPT4                                                          
*                                                                               
         CLI   CUMESW,0            INCLUDE CUME DPTS IF CUME ACTIVE             
         BNE   *+12                                                             
         CLI   STANDESC+19,C'C'    BYPASS CUME ONLY DAYPARTS                    
         BE    LUKDPT4                                                          
*                                                                               
         MVI   DPSTYPE,C'S'        MATCHED - SO THIS ONE IS STANDARD            
         MVC   DPSPROG,STANDPRG    ALL WE NEED IS THE PROGRAM CODE              
         XC    DPSSPARE,DPSSPARE                                                
         MVC   DPDESC,STANDESC     TAKE THE STANDARD DESCRIPTION                
         B     XIT                                                              
         SPACE 1                                                                
LUKDPT4  LA    R2,L'STANDENT(R2)                                                
         B     LUKDPT2                                                          
         DROP  R2                                                               
         EJECT                                                                  
* *********************************************************************         
* COMPRANK - COMPUTE RANK NUMBERS                                               
*              OUTPUTS             RANKNUMS FOR EACH BOOK                       
* *********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
COMPRANK NTR1                                                                   
         L     R2,ABOOKBF                                                       
         USING BUFFD,R2                                                         
         L     R6,NXTPOS                                                        
         ZIC   R3,NSTATS                                                        
         LA    R4,BUFFDEM-BUFFREC  R4=DISPLACEMENT INTO BUFF                    
         CLI   DBSELSRC,C'M'                                                    
         BE    COMPR1                                                           
*                                                                               
         AR    R2,R6               SKIP THE MARKET STATION                      
         BCTR  R3,0                LESS ONE FOR MARKET STATION                  
*                                                                               
COMPR1   LA    R0,5                UP TO 4 BOOKS + AVG BOOK                     
*                                                                               
COMPR2   OC    0(2,R2),0(R2)       ANY DATA IN BUFFSTAT/BUFFFLG?                
         BE    *+8                                                              
         BAS   RE,COMPR4           COMPUTE FOR EACH BOOK                        
         LA    R4,BUFFDMQ(R4)                                                   
         BCT   R0,COMPR2                                                        
         SPACE 1                                                                
*                                  NOW PHYSICALLY RANK BUFFER                   
*                                  ON LAST SPECIFIED BOOK                       
         ZIC   R4,NBOOKS                                                        
         BCTR  R4,0                                                             
         MH    R4,=Y(BUFFDMQ)      DISPL TO NEXT BOOK                           
         LA    R4,L'BUFFDEM(R4)    PT PAST THE DEMO TO THE RANK                 
         CLI   RANKOPT,C'N'        UNLESS OPTION NOT TO RANK                    
         BE    COMP3                                                            
         GOTO1 XSORT,DMCB,(1,(R2)),(R3),(R6),2,(R4)   L'KEY=2=MIN XSRT          
         B     COMPRXIT                                                         
         SPACE 1                                                                
COMP3    SR    R4,R4               WHEN WE WILL RESORT ON STATION               
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),(R6),2,(R4)                             
COMPRXIT B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*                                                                               
COMPR4   NTR1                                                                   
*                                  RANK ON THIS BOOK:DEMLNQ=L'BUFFDEM           
         GOTO1 XSORT,DMCB,(1,(R2)),(R3),(R6),DEMLNQ,(R4)                        
         AR    R2,R4               R2 NOW POINTS TO DEMO                        
         LA    R4,1                R4 WILL HOLD RANK # (VALUE TO SET)           
         LA    R5,1                R5 HAS TABLE NUMBER                          
         XC    LASTVAL,LASTVAL                                                  
         SPACE 1                                                                
COMPR6   CLC   LASTVAL,0(R2)                                                    
         BE    COMPR8                                                           
         MVC   LASTVAL,0(R2)                                                    
         LR    R4,R5               THIS NOT EQUAL TO LAST SO RESET RANK         
         STC   R4,L'BUFFDEM(R2)                                                 
         CLC   0(L'BUFFDEM,R2),L'BUFFREC(R2)   CHECK AGNST NXT STATN            
         BNE   COMPR10                                                          
         OI    L'BUFFDEM(R2),X'80'  THIS EQUALS NXT, MRK RANK X'80' (=)         
         B     COMPR10                                                          
         SPACE 1                                                                
COMPR8   STC   R4,L'BUFFDEM(R2)    THIS EQUAL TO LAST                           
         OI    L'BUFFDEM(R2),X'80'         SO MARK EQUAL                        
         SPACE 1                                                                
COMPR10  AR    R2,R6               GO ON TO NEXT STATION                        
         LA    R5,1(R5)                                                         
         BCT   R3,COMPR6                                                        
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* ********************************************************************          
*              HEADLINE HOOK                                                    
* ********************************************************************          
         SPACE 3                                                                
HOOK     NTR1                                                                   
         GOTO1 =A(SETBOX),DMCB,(R9),(RC),RR=RELO                                
         GOTO1 =A(SETTITLE),DMCB,(R9),(RC),RR=RELO                              
*        BAS   RE,SETBOX                                                        
*        BAS   RE,SETTITLE                                                      
         MVC   BLOCK(42),SPACES    MARKET NUMBER AND NAME                       
         EDIT  (2,MKTNUM),(4,BLOCK)                                             
         MVC   BLOCK+5(30),TREMKTN                                              
         CLI   STALSTD,C'Y'                                                     
         BNE   HOOK000                                                          
         MVC   BLOCK+36(6),=C'*LIST*'                                           
         OI    TREMKTNH+6,X'88'                                                 
HOOK000  GOTO1 VSQUASH,DMCB,BLOCK,42                                            
         MVC   H4+7(42),BLOCK                                                   
         SPACE 1                                                                
         L     RE,DBEXTEND         CHECK FOR CONDENSED MARKETS/DEMOS            
         USING DBEXTRAD,RE                                                      
HOOK0    LTR   RE,RE                                                            
         BZ    HOOK1                                                            
         CLC   0(4,RE),=C'RADI'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     HOOK0                                                            
         CLI   DBRADEMT,C'R'       IS THIS A RESTRICTED DEMO                    
         BNE   HOOK1                                                            
         L     RF,=A(CNDHDR)                                                    
         A     RF,RELO                                                          
         MVC   H8+12(L'CNDHDR),0(RF)                                            
         L     RF,=A(CNDHDR2)                                                   
         A     RF,RELO                                                          
         MVC   H9+12(L'CNDHDR2),0(RF)                                           
         SPACE 1                                                                
HOOK1    MVC   BLOCK(60),SPACES                                                 
         MVC   BLOCK(8),TRESRCE    SOURCE, DEMO AND CATEGORY                    
         MVC   BLOCK+10(8),TREDEMO                                              
         MVC   BLOCK+20(40),TRECAT                                              
         GOTO1 VSQUASH,DMCB,BLOCK,40                                            
         MVC   H5(40),BLOCK                                                     
         SPACE 1                                                                
         MVC   H6+8(19),DPDESC     DAYPART                                      
         SPACE 1                                                                
         LA    R2,H1+30            TITLE                                        
         MVC   0(40,R2),RESTITLE   ALREADY CENTERED                             
         GOTO1 UNDERLIN,DMCB,(40,(R2)),(X'BF',132(R2))                          
*                                                                               
         LA    R1,ARBTBL           IF ARB RADIO OR TV -> COPYRT MSG             
HOOK2    CLI   0(R1),X'FF'         EOT, NOT ARB DON'T PRINT MSG                 
         BE    HOOK3                                                            
         CLC   0(6,R1),TRESRCE                                                  
         BE    *+12                                                             
         LA    R1,L'ARBTBL(R1)                                                  
         B     HOOK2                                                            
         MVC   H3+27(55),=C'AUDIENCE ESTIMATES: COPYRIGHT XXXX THE ARBIX        
               TRON COMPANY'                                                    
         GOTO1 DATCON,DMCB,(3,BTODAY),(21,WORK)                                 
         MVC   H3+31+26(4),WORK+6                                               
         SPACE 1                                                                
HOOK3    LA    R2,H4+41            UNIVERSES                                    
         CLI   ANYMSA,C'Y'             MSA                                      
         BNE   *+18                                                             
         MVC   0(10,R2),MSASAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         CLI   ANYADI,C'Y'             ADI                                      
         BNE   *+18                                                             
         MVC   0(10,R2),ADISAVE                                                 
         BAS   RE,IUNV                                                          
         LA    R2,132(R2)                                                       
         CLI   ANYTSA,C'Y'             TSA                                      
         BNE   XIT                                                              
         MVC   0(10,R2),TSASAVE                                                 
         BAS   RE,IUNV                                                          
         SPACE 1                                                                
HOOKX    B     XIT                                                              
         SPACE 2                                                                
IUNV     NTR1                      INSERT POPULATION CAPTION                    
         LA    R0,12                                                            
         CLI   0(R2),C'='          IN FRONT OF '='                              
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     XIT                                                              
         MVC   DUB,0(R2)                                                        
         MVC   9(8,R2),DUB                                                      
         MVC   0(9,R2),=C' UNIV(00)'                                            
         B     XIT                                                              
*                                                                               
ARBTBL   DS    0CL6                                                             
         DC    C'ARB   '                                                        
         DC    C'RARB  '                                                        
         DC    C'DRARB '                                                        
         DC    X'FF'                                                            
*                                                                               
* *******************************************************************           
* SUMBUF/CUMERAT: 4 BOOKS ACCROSS X 3 DEMOS DOWN                                
TMPBUF   DC    12F'00'             IMPRESSIONS FOR TSL COMBO                    
SUMBUF   DC    12F'00'             SUM OF CUME FOR COMBOS                       
CUMERAT  DC    12F'00'             CUME RATING FOR COMBOS                       
TSLDEMO  DC    XL3'00'             MAX 3 DEMOS                                  
ORGDEMO  DC    XL3'00'             MAX 3 DEMOS                                  
TYPEDEM  DC    X'00'               TYPE OF DEMO TO REPLACE WITH                 
CNOSTA   DC    X'00'               NUMBER OF STATIONS IN COMBO SO FAR           
PACK1    DC    X'00'               NUMBER OF STATIONS IN COMBO SO FAR           
REGSV    DS    5F                  TEMP STORAGE FOR A REGISTER                  
*                                                                               
* *******************************************************************           
* REPLDEM- REPLACE DEMO WITH THAT SPECIFIED BY:TYPEDEM                          
*        - IF TYPEDEM=C'I' THEN ALL ALL DEMOS ARE OVERWRITTEN WITH 'I'          
*        - REGARDLESS OF WHAT'S IN THE ORIG DEMO LIST                           
* *******************************************************************           
*                                                                               
REPLDEM  DS    0H                                                               
         NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    RE,DEMOS                                                         
         LA    RF,TSLDEMO          SAVE ORIG DEMO LIST HERE                     
*                                                                               
REPLD1   MVC   0(1,RF),1(RE)       CONVERT TSL TO CUME                          
         CLI   TYPEDEM,C'I'        IF IMPRESNS, DO ALL THE DEMOS                
         BE    REPLD3                                                           
         CLI   1(RE),C'X'                                                       
         BNE   *+10                                                             
REPLD3   MVC   1(1,RE),TYPEDEM                                                  
         LA    RF,1(RF)                                                         
         LA    RE,3(RE)                                                         
         BCT   R1,REPLD1                                                        
         B     XIT                                                              
         SPACE 4                                                                
* *******************************************************************           
* RESTDEM - RESTORE DEMO LIST FROM TSLDEMOS                                     
* *******************************************************************           
*                                                                               
RESTDEM  DS    0H                                                               
         NTR1                                                                   
         ZIC   R1,NDEMOS                                                        
         LA    R2,DEMOS                                                         
         LA    R3,TSLDEMO                                                       
RESTDM1  MVC   1(1,R2),0(R3)                                                    
         LA    R2,3(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,RESTDM1                                                       
         B     XIT                                                              
         SPACE 4                                                                
* ********************************************************************          
*              COMMON ROUTINES                                                  
* ********************************************************************          
*                                                                               
FILLBF   NTR1                                                                   
         GOTO1 =A(FILLBUFF),DMCB,(R2),(R3),(R4),(R6),(R9),(RC),RR=RELO          
         B     XIT                                                              
*                                                                               
* -----------------------------------------------------------------             
NEQXIT   LA    R1,1                                                             
         B     *+6                                                              
*                                                                               
EQXIT    SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
* -----------------------------------------------------------------             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* -----------------------------------------------------------------             
*                                                                               
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         MVC   WORK,CONHEAD                                                     
ERREND   GOTO1 VGETERR                                                          
         SPACE 2                                                                
SPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
*                                                                               
* -----------------------------------------------------------------             
*                                                                               
CPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCPRSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
GETELM   LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* *********************************************************************         
*              ERRORS SPECS AND CONSTANTS                                       
*                                  MY ERROR MESSAGES                            
* *********************************************************************         
*                                                                               
TOOMNY   DC    C'** ERROR ** LIMITED TO 100 STATIONS'                           
MNYBKS   DC    C'** ERROR ** LIMITED TO 4 BOOKS'                                
TOOMNYDM DC    C'** ERROR ** ONLY 1 DEMO ALLOWED'                               
TOOMNYCT DC    C'** ERROR ** ONLY 1 CATEGORY ALLOWED'                           
MKTERR   DC    C'** ERROR ** MARKET NOT FOUND'                                  
MKTERR2  DC    C'** MARKET MISSING OR MISSING M= IN USER LIST'                  
LISTERR  DC    C'** ERROR ** NEED GOOD MARKET IN LIST'                          
ERRBOOK  DC    C'REQUESTED BOOK(S) NOT FOUND- MARKET'                           
BADMUL   DC    C'MARKET IS NOT SWEPT (N) TIMES A YEAR'                          
BADMULP  EQU   BADMUL+21                                                        
INVDAYPT DC    C'** ERROR ** INVALID DAYPART'                                   
FILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
RESTIT   DC    CL40'RADIO TREND REPORT'                                         
VIO1     DC    C'#=VIOLATION: SPECIAL STATION ACTIVITIES. DETAILS: P.13+        
                ARB MKT. REPORT'                                                
VIO2     DC    C'A = STATION CITED  Z = STATION LISTED BELOW THE LINE'          
*NDHDR   DC    C'**CONDENSED MARKET - EXERCISE DISCRETION WHEN USING TH         
*              ESE ESTIMATES**'                                                 
CNDHDR   DC    C'DUE TO SMALLER SAMPLE SIZES IN CONDENSED MARKETS FOR TX        
               HIS DEMOGRAPHIC'                                                 
CNDHDR2  DC    C'AND/OR DAYPART THE USER SHOULD USE DISCRETION WHEN USIX        
               NG THESE ESTIMATES'                                              
         SPACE 1                                                                
         SPACE 1                                                                
HEDSPECS DS    0H                  HEADLINE SPECS                               
         SPACE 1                                                                
         SSPEC H1,1,C'DDS RADIO RESEARCH'                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,C'MARKET'                                                   
         SSPEC H6,1,C'DAYPART'                                                  
         SPACE 1                                                                
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REPORT                                                     
         SSPEC H5,93,PAGE                                                       
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              STANDARD DAYPART TABLE                                           
         SPACE 3                                                                
       ++INCLUDE RADSDPL                                                        
         EJECT                                                                  
       ++INCLUDE RADSDPLC                                                       
         EJECT                                                                  
CFRENDS  DC    C'BREAKFAST      ',X'7C',AL1(01,01),12X'00'                      
         DC    C'DAY            ',X'7C',AL1(02,02),12X'00'                      
         DC    C'DRIVE          ',X'7C',AL1(03,03),12X'00'                      
         DC    C'EVENING        ',X'7C',AL1(04,04),12X'00'                      
         DC    C'BR+DA          ',X'7C',AL1(09,09),12X'00'                      
         DC    C'BR+DR          ',X'7C',AL1(10,10),12X'00'                      
         DC    C'BR+EV          ',X'7C',AL1(11,11),12X'00'                      
         DC    C'DA+DR          ',X'7C',AL1(12,12),12X'00'                      
         DC    C'DA+EV          ',X'7C',AL1(13,13),12X'00'                      
         DC    C'DR+EV          ',X'7C',AL1(14,14),12X'00'                      
         DC    C'BR+DA+DR       ',X'7C',AL1(15,15),12X'00'                      
         DC    C'BR+DA+EV       ',X'7C',AL1(16,16),12X'00'                      
         DC    C'BR+DR+EV       ',X'7C',AL1(17,17),12X'00'                      
         DC    C'DA+DR+EV       ',X'7C',AL1(18,18),12X'00'                      
         DC    C'BR+DA+DR+EV    ',X'7C',AL1(19,19),12X'00'                      
         DC    C'SATURDAY       ',X'02',AL1(05,05),12X'00'                      
         DC    C'SUNDAY         ',X'01',AL1(06,06),12X'00'                      
         DC    C'SA+SU          ',X'03',AL1(20,20),12X'00'                      
         DC    C'BR+SA          ',X'7E',AL1(21,21),12X'00'                      
         DC    C'DA+SA          ',X'7E',AL1(22,22),12X'00'                      
         DC    C'DR+SA          ',X'7E',AL1(23,23),12X'00'                      
         DC    C'BR+DR+SA       ',X'7E',AL1(24,24),12X'00'                      
         DC    C'BR+SA+SU       ',X'7F',AL1(25,25),12X'00'                      
         DC    C'DA+SA+SU       ',X'7F',AL1(26,26),12X'00'                      
         DC    C'DR+SA+SU       ',X'7F',AL1(27,27),12X'00'                      
         DC    C'EV+SA+SU       ',X'7F',AL1(28,28),12X'00'                      
         DC    C'BR+DA+DR+SA    ',X'7E',AL1(29,29),12X'00'                      
         DC    C'BR+DA+SA+SU    ',X'7F',AL1(30,30),12X'00'                      
         DC    C'BR+DA+DR+SA+SU ',X'7F',AL1(31,31),12X'00'                      
         DC    C'BR+DA+DR+EV+SA ',X'7E',AL1(32,32),12X'00'                      
         DC    C'ALL            ',X'7F',AL1(33,33),12X'00'                      
         DC    C'REACH          ',X'7F',AL1(07,07),12X'00'                      
         DC    C'M-S 5A-1A      ',X'7F',AL1(07,07),12X'00'                      
         DC    X'FF'                                                            
         EJECT                                                                  
* ********************************************************************          
* TSLCALC-     CALCULATE TSL FROM (R4)=CUMES AND TMPBUF=IMPRESSIONS             
*                  (R4)   - CUMES                                               
*                  TMPBUF - IMPRESSIONS                                         
*                  DEMOS  - REAL DEMOS WITH THE 'X' FOR TSL                     
*              OUTPUT- OVERWRITE THE CUMES IN (R4) FOR TSL DEMS (X)             
* ********************************************************************          
         SPACE 1                                                                
TSLCALC  NMOD1 0,**TSLCLC***                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
*        L     R7,AMYD                                                          
         LR    RE,R4               PT TO CUMES                                  
         LA    RF,TMPBUF           PT TO IMPRESSIONS                            
         LA    RE,2(RE)            PT TO FIRST DEMO (BUFF OVERHEAD)             
         LA    RF,2(RF)                                                         
*                                                                               
         ZIC   R2,NDEMOS           LOOP THRU ALL DEMOS IN BUFFER                
         LA    R3,DEMOS                                                         
         STM   RE,RF,DUB                                                        
         BAS   RE,QTRHRSCT         COUNT # OF QTR HOURS IN DAYPART              
         LM    RE,RF,DUB                                                        
*                                                                               
TSLCLC02 STM   RE,RF,DUB           SAVE ADDR OF THE BEG DEMO                    
         CLI   1(R3),C'X'          LOOP THRU DEMOS. TSL?                        
         BNE   TSLCLC05                                                         
         LA    R1,4                FOR EACH DEMO,DO 4 BOOKS                     
*                                                                               
TSLCLC03 SR    R4,R4               GO THRU BOOKS FOR EACH DEMO                  
         SR    R5,R5               STUPID ZICM'S DON'T ALLOW EQU MASKS!         
         ICM   R5,BUFFICM,0(RF)    GET IMPRESSIONS                              
         M     R4,=F'100'                                                       
         M     R4,NOQTRHRS         MULT BY # OF QTR HOURS                       
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    TSLCLC04            KEEP IT AS ZERO                              
         SR    R0,R0                                                            
         ICM   R0,BUFFICM,0(RE)                                                 
         DR    R4,R0               DIVIDE BY CUME                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
         STCM  R5,BUFFICM,0(RE)    REPLACE IT WITH TSL                          
*                                                                               
TSLCLC04 LA    RE,BUFFDMQ(RE)      NEXT BOOK'S DEMOS                            
         LA    RF,BUFFDMQ(RF)                                                   
         BCT   R1,TSLCLC03         DO ALL 4 BOOKS                               
*                                                                               
TSLCLC05 LM    RE,RF,DUB           RESTORE ADDR OF PREV DEMOS                   
         LA    RE,L'BUFFREC(RE)    NEXT DEMO IN CUME BUFFER                     
         LA    RF,L'BUFFREC(RF)    NEXT DEMOS IN IMPS BUFFER                    
         LA    R3,3(R3)            NEXT DEMO                                    
         BCT   R2,TSLCLC02                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
TMPWRD   DC    F'01'                                                            
NOQTRHRS DC    F'80'                                                            
*------------------------------------------------------------------             
QTRHRSCT NTR1                                                                   
         LA    R4,DPENTRY                                                       
         XC    NOQTRHRS,NOQTRHRS                                                
         CLI   0(R4),C'S'          STANDARD DAYPART                             
         BNE   XIT                 CUSTOM DAYPART, TSL = 0                      
         LA    RE,STANDPTS         PT TO STANDARD DAYPT TABLE                   
         USING STAND,RE                                                         
QHDAYSCH CLI   STANDDTS,X'FF'      END OF TABLE?                                
         BE    XIT                                                              
         CLC   1(1,R4),STANDDTS    CHECKING DAY RANGE                           
         BNE   QHDAYS2                                                          
         CLC   2(1,R4),STANDPRG                                                 
         BE    QHFNDDPT                                                         
QHDAYS2  LA    RE,L'STANDENT(RE)                                                
         B     QHDAYSCH                                                         
QHFNDDPT LR    R4,RE                                                            
*                                                                               
QHCUST1  CLI   0(R4),0             FINISHED?                                    
         BZ    XIT                                                              
         ZIC   R1,2(R4)            END HOUR                                     
         ZIC   R2,1(R4)            START HOUR                                   
         SR    R1,R2                                                            
         MH    R1,=H'4'            4 QTR HOURS PER HOUR                         
         SR    RF,RF                                                            
         ZIC   R2,0(R4)                                                         
QHCUST2  LTR   R2,R2                                                            
         BZ    QHCUST9                                                          
         BP    *+8                                                              
         LA    RF,1(RF)                                                         
         SLL   R2,1                                                             
         B     QHCUST2                                                          
*                                                                               
QHCUST9  SR    R0,R0                                                            
         MR    R0,RF                                                            
         A     R1,NOQTRHRS                                                      
         ST    R1,NOQTRHRS                                                      
         LA    R4,3(R4)                                                         
         B     QHCUST1                                                          
*                                                                               
         B     XIT                                                              
         DROP  RE                                                               
*                                                                               
TSLCLCX  XMOD1                                                                  
*                                                                               
* ********************************************************************          
* SETBOX -     ROUTINE TO SET UP BOXES                                          
*              OUTPUT              BOXCOLS BOXROWS                              
* ********************************************************************          
         SPACE 1                                                                
SETBOX   NMOD1 0,**SETBOX***                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   BOXOPT,C'N'         OPTION TO SUPPRESS                           
         BE    SETBOXX                                                          
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    SETBOXX                                                          
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+07,C'T'                                                  
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      NOW DO THE COLUMNS                           
         LA    R1,BOXCOLS                                                       
         A     R1,DISP                                                          
         SPACE 1                                                                
BOXSET2  MVI   0(R1),C'L'          LEFT                                         
         LA    R1,8(R1)                                                         
         CLI   COMBNUM,0           COMBOS CAN BE 9 CHARS                        
         BE    *+8                                                              
         LA    R1,2(R1)                                                         
         MVI   0(R1),C'C'                                                       
         CLI   FORMOPT,C'Y'                                                     
         BNE   BOXSET4                                                          
         LA    R1,5(R1)                                                         
         MVI   0(R1),C'C'                                                       
         SPACE 1                                                                
BOXSET4  LA    R1,12(R1)                                                        
         ZIC   R0,NBOOKS                                                        
         CLI   NBOOKS,1                                                         
         BE    BOXSET8                                                          
         BCTR  R0,0                                                             
         SPACE 1                                                                
BOXSET6  MVI   0(R1),C'C'                                                       
         LA    R1,18(R1)                                                        
         BCT   R0,BOXSET6                                                       
         SPACE 1                                                                
BOXSET8  MVI   0(R1),C'C'                                                       
         MVI   12(R1),C'R'                                                      
*                                                                               
SETBOXX  XMOD1                                                                  
         EJECT                                                                  
* ********************************************************************          
* SETTITLE -   SET UP TITLES FOR MULTIPLE BOOKS                                 
*              OUTPUTS             TITLES                                       
* ********************************************************************          
*                                                                               
SETTITLE NMOD1 0,**SETBOX**                                                     
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         LA    R3,H9+1                                                          
         A     R3,DISP                                                          
         MVC   0(7,R3),=C'STATION'                                              
         LA    R3,8(R3)                                                         
         CLI   ADDROPT,C'N'                                                     
         BNE   SETT1                                                            
         MVC   H2+76(32),SPACES                                                 
SETT1    CLI   COMBNUM,0           COMBOS ARE 9 CHAR LONG                       
         BE    *+8                                                              
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         CLI   FORMOPT,C'Y'                                                     
         BNE   SETT1A                                                           
         MVC   0(4,R3),=C'FORM'                                                 
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
SETT1A   CLI   NCATS,1                                                          
         BE    SETT2                                                            
         MVC   1(4,R3),=C'CTGY'                                                 
         LA    R3,7(R3)                                                         
         SPACE 1                                                                
SETT2    LA    R2,BOOKS+1          FIRST BOOK                                   
         GOTO1 =A(CONVBOOK),DMCB,(R9),(RC),RR=RELO                              
         MVC   1(9,R3),WORK                                                     
         MVC   133(4,R3),=C'RANK'                                               
         CLI   NCATS,1                                                          
         BH    SETT2A                                                           
         MVC   138(5,R3),CATTITS                                                
*                                                                               
SETT2A   CLI   RANKOPT,C'S'        SUPRESS RANK                                 
         BNE   SETT2B                                                           
         XC    133(10,R3),133(R3)                                               
         CLI   NCATS,1                                                          
         BH    SETT2B                                                           
         MVC   133(5,R3),CATTITS                                                
*                                                                               
SETT2B   LA    R3,12(R3)                                                        
         CLI   NBOOKS,1                                                         
         BE    SETT6                                                            
         ZIC   R0,NBOOKS                                                        
         BCTR  R0,0                                                             
         SPACE 1                                                                
SETT4    LA    R2,4(R2)            SUBSEQUENT BOOKS                             
         GOTO1 =A(CONVBOOK),DMCB,(R9),(RC),RR=RELO                              
         MVC   4(9,R3),WORK                                                     
         MVC   133(4,R3),=C'RANK'                                               
         CLI   NCATS,1                                                          
         BH    SETT4A                                                           
         MVC   138(5,R3),CATTITS                                                
*                                                                               
SETT4A   CLI   RANKOPT,C'S'        SUPRESS RANK                                 
         BNE   SETT4B                                                           
         XC    133(10,R3),133(R3)                                               
         CLI   NCATS,1                                                          
         BH    SETT4B                                                           
         MVC   133(5,R3),CATTITS                                                
*                                                                               
SETT4B   MVC   144(5,R3),=C'DIFF%'                                              
         LA    R3,18(R3)                                                        
         BCT   R0,SETT4                                                         
         SPACE 1                                                                
SETT6    CLI   AVEOPT,C'N'         OPTION TO SUPRESS                            
         BE    SETTX                                                            
         MVC   2(7,R3),=C'AVERAGE' AND AVERAGE                                  
         MVC   133(4,R3),=C'RANK'                                               
         CLI   NCATS,1                                                          
         BH    SETT6A                                                           
         MVC   138(5,R3),CATTITS                                                
*                                                                               
SETT6A   CLI   RANKOPT,C'S'        SUPRESS RANK                                 
         BNE   SETT7                                                            
         XC    133(10,R3),133(R3)                                               
         CLI   NCATS,1                                                          
         BH    SETT7                                                            
         MVC   133(5,R3),CATTITS                                                
*                                                                               
SETT7    LA    R3,14(R3)                                                        
         MVC   0(7,R3),=C'STATION'                                              
*                                                                               
SETTX    XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
*              ROUTINE TO CONVERT BOOK FOR HEADINGS                             
**********************************************************************          
         SPACE 3                                                                
CONVBOOK NMOD1 0,**CNVBOOK**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,PPMTAB                                                 
         ICM   RE,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0 HAS LENGTH OF TABLE ENTRY                 
         LR    R1,RE               R1=A(TABLE)                                  
         USING PPMTABD,R1                                                       
CB10     CLC   =X'FFFF',0(R1)                                                   
         BE    CB70                                                             
         OC    CITYCODE,CITYCODE                                                
         BZ    CB20                                                             
         CLC   PPMAMKT,CITYCODE                                                 
         BNE   CB40                                                             
         B     CB30                                                             
CB20     DS    0C                                                               
         CLC   PPMNMKT,MKTNUM                                                   
         BNE   CB40                                                             
CB30     CLC   0(L'PPMSTRTP,R2),PPMSTRTP   PRELIM OR LATER?                     
         BNL   CB50                                                             
CB40     AR    R1,R0                                                            
         B     CB10                                                             
*                                                                               
CB50     MVC   WORK(6),=C'MON/NN'                                               
         MVC   WORK+6(3),=C'( )'                                                
         MVC   WORK+7(1),BOOKTYPE                                               
         LA    R1,PPMLIST                                                       
CB60     CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R2),3(R1)                                                    
         BNE   *+18                                                             
         MVC   WORK(3),0(R1)                                                    
         LA    R3,WORK+4                                                        
         B     CBX                                                              
         LA    R1,4(R1)                                                         
         B     CB60                                                             
*                                                                               
CB70     CLI   BOOKTYPE,C'B'       BLACK                                        
         BE    CNVEBTY                                                          
         CLI   BOOKTYPE,C'H'       AND HISPANIC - FUNNY BOOKS                   
         BE    CNVEBTY                                                          
*                                                                               
         MVC   WORK(9),=C' FALL/NN '                                            
         LA    R3,WORK+6                                                        
         CLI   1(R2),11            NOVEMBER=FALL                                
         BE    CBX                                                              
         MVC   WORK(9),=C'WINTER/NN'                                            
         LA    R3,WORK+7                                                        
         CLI   1(R2),1             JAN=WINTER FOR BBM                           
         BE    CBX                                                              
         CLI   1(R2),2             FEBRUARY=WINTER                              
         BE    CBX                                                              
         MVC   WORK(6),=C'SPRING'                                               
         CLI   1(R2),5             MAY=SPRING                                   
         BE    CBX                                                              
         MVC   WORK(6),=C'SUMMER'                                               
         CLI   1(R2),7                                                          
         BE    CBX                                                              
         MVC   WORK(6),=C'??????'                                               
         B     CBX                                                              
         SPACE 1                                                                
CNVEBTY  L     R3,E1BOOK           CHECK FOR 1 BOOK ETHNICS                     
CNVEBTY1 CLI   0(R3),X'FF'                                                      
         BE    CNVEBTY3                                                         
         CLC   0(1,R3),BOOKTYPE                                                 
         BNE   CNVEBTY2                                                         
         CLC   1(2,R3),MKTNUM                                                   
         BNE   CNVEBTY2                                                         
         CLC   0(2,R2),3(R3)                                                    
         BL    CNVEBTY2                                                         
         CLC   0(2,R2),5(R3)                                                    
         BH    CNVEBTY2                                                         
         B     CNVEBTY6                                                         
CNVEBTY2 LA    R3,7(R3)                                                         
         B     CNVEBTY1                                                         
*                                                                               
CNVEBTY3 MVC   WORK(9),=C'WINSPR/NN '   BOOK NAMES FOR 2 BOOK MARKETS           
         LA    R3,WORK+7                                                        
         CLI   1(R2),5                                                          
         BE    CBX                                                              
         MVC   WORK(9),=C'SUMFAL/NN '                                           
         LA    R3,WORK+7                                                        
         CLI   1(R2),11                                                         
         BE    CBX                                                              
         MVC   WORK(6),=C'??????'                                               
         B     CBX                                                              
         SPACE 1                                                                
CNVEBTY6 MVC   WORK(7),=C'FWS/NN ' BOOK NAMES FOR 1 BOOK MARKETS                
         LA    R3,WORK+4                                                        
         CLI   1(R2),5                                                          
         BE    CBX                                                              
         MVC   WORK(6),=C'??????'                                               
         B     CBX                                                              
         SPACE 1                                                                
CBX      EDIT  (1,0(R2)),(2,(R3)),WRK=DMCB                                      
         XMOD1                                                                  
*                                                                               
PPMLIST  DC    C'JAN',AL1(01)                                                   
         DC    C'FEB',AL1(02)                                                   
         DC    C'MAR',AL1(03)                                                   
         DC    C'APR',AL1(04)                                                   
         DC    C'MAY',AL1(05)                                                   
         DC    C'JUN',AL1(06)                                                   
         DC    C'JUL',AL1(07)                                                   
         DC    C'AUG',AL1(08)                                                   
         DC    C'SEP',AL1(09)                                                   
         DC    C'OCT',AL1(10)                                                   
         DC    C'NOV',AL1(11)                                                   
         DC    C'DEC',AL1(12)                                                   
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* COMPDISP - ROUTINE TO COMPUTE PRINT AND BUFFER DISPLACEMENTS                  
*              OUTPUTS             DISP                                         
*                                  ABOOKBF                                      
***********************************************************************         
         SPACE 1                                                                
COMPDISP NMOD1 0,**COMPDSP**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         LA    R2,BUFF             STATION LIST IN BUFF                         
         ST    R2,ASTATS                                                        
         LA    R2,1000(R2)         BUMP PAST THIS TO BOOK BUFFER                
         ST    R2,ABOOKBF                                                       
         SPACE 1                                                                
         XC    DISP,DISP                                                        
         CLI   LEFTOPT,C'Y'        NO CENTERING IF LEFT OPTION                  
         BE    COMPDSPX                                                         
         LA    R1,5                                                             
         ZIC   R0,NBOOKS                                                        
         SR    R1,R0                                                            
         MH    R1,=H'9'                                                         
         CLI   FORMOPT,C'Y'                                                     
         BE    *+8                                                              
         LA    R1,2(R1)                                                         
         ST    R1,DISP                                                          
*                                                                               
COMPDSPX XMOD1                                                                  
         EJECT                                                                  
* *****************************************************************             
* EXPSTATS- ROUTINE TO BUILD STATIONS IN MARKET                                 
*              INPUT               MKTNUM                                       
*              OUTPUTS             ASTATS = A(100 5 BYTE STATIONS)              
*                                  NSTATS   ACTUAL NUMBER OF STATIONS           
* *****************************************************************             
         SPACE 1                                                                
*EXPSTATS NTR1                                                                  
EXPSTATS NMOD1 0,**EXPSTAT**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   STASTYLE,C'M'       IF MARKET WAS SELECTED                       
         BNE   EXPSTATX                                                         
         XC    PREVSTA,PREVSTA                                                  
         MVC   ATHISTAT,ASTATS                                                  
         MVI   NSTATS,0                                                         
         CLI   DBSELSRC,C'M'       SKIP MARKET RECORDS FOR CANADA               
         BE    EXPSTA1C            THEY DON'T HAVE IT YET                       
         SPACE 1                                                                
*                                                                               
         GOTO1 =A(MKTSTAT),DMCB,ATHISTAT,(R9),(RC),RR=RELO                      
         L     RF,ATHISTAT                                                      
         LA    RF,L'STATENT(RF)                                                 
         ST    RF,ATHISTAT                                                      
* WE WANT TO BUILD THE LIST FROM THE LAST AVAILABLE BOOK                        
EXPSTA1C ZIC   R0,NBOOKS           SET UP THE BOOKS                             
EXPSTAT2 LA    RE,BOOKS            AND PROCESS IN REVERSE SEQUENCE              
         LTR   RF,R0               END OF BOOK LIST                             
         BZ    EXPSTATX                                                         
         BCTR  RF,0                                                             
         LR    R0,RF               SAVE DECREMENT FOR NEXT LOOP                 
         SLL   RF,2                                                             
         AR    RE,RF               POINT TO A BOOK                              
         MVC   DBSELBK,1(RE)       AND BUILD LIST FROM IT                       
         MVC   DBBTYPE,3(RE)                                                    
         SPACE 1                                                                
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         GOTO1 DEMAND,DMCB,DBLOCK,EXPHOOK                                       
         CLI   NSTATS,0            ANY STATIONS FOR THIS BOOK                   
         BE    EXPSTAT2            NO TRY NEXT BOOK                             
****************************DEAD CODE*********************************          
*        CLI   DBSELSRC,C'M'                                         *          
*        BNE   EXPSTATX                                              *          
*        L     RF,ASTATS                                             *          
*        LA    RE,L'STATENT(RF)    SINCE CANADA DOESN'T HAVE MARKET  *          
*        MVC   0(5,RF),0(RE)       RECORDS, RESTORE FIRST STATION    *          
**********************************************************************          
         B     EXPSTATX                                                         
         SPACE 1                                                                
EXPHOOK  NTR1                                                                   
         CLI   EXPANOPT,C'Y'       EXPANSION WANTED                             
         BE    EXPHOOK6             DON'T CHECK THE STATIONS                    
         L     R1,ULPNTR                                                        
EXPHOOK2 CLI   0(R1),0             END OF LIST                                  
         BE    EXPHOOK6                                                         
         CLI   0(R1),3             MATCH TO COMBO ELEMENT                       
         BE    EXPHOOK4                                                         
EXPHOOK3 ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     EXPHOOK2                                                         
         SPACE 1                                                                
EXPHOOK4 ZIC   RE,1(R1)                                                         
         AR    RE,R1               FIND THE END OF LIST                         
         LA    RF,11(R1)           POINT TO STATIONS                            
EXPHOOK5 CR    RF,RE               END                                          
         BNL   EXPHOOK3                                                         
         CLC   MLSTAT,0(RF)        MATCH SO BYPASS                              
         BE    EXPSTATX                                                         
         LA    RF,5(RF)            NEXT STATION                                 
         B     EXPHOOK5                                                         
         SPACE 1                                                                
EXPHOOK6 CLI   MLSTAT,C'Z'         CUT OUT MARKET TOTALS                        
         BH    EXPSTATX                                                         
         SPACE 1                                                                
         CLI   SIMULOPT,C'N'       EXCLUDE SIMULCAST OPTION                     
         BNE   *+12                                                             
         CLI   MLSTAT+4,C'B'       BYE,BYE IF SIMULCAST                         
         BE    EXPSTATX                                                         
         CLI   MLSTAT+4,C'C'       BYE,BYE IF SIMULCAST                         
         BE    EXPSTATX                                                         
         CLI   MLSTAT+4,C'D'       BYE,BYE IF SIMULCAST                         
         BE    EXPSTATX                                                         
         SPACE 1                                                                
         CLI   SPILLOPT,C'Y'       SPILL ONLY OPTION                            
         BNE   EXPHOK10                                                         
         CLI   MLHOME,0                                                         
         BNE   *+14                CONTINUE WORKING UNTIL FILE CHANGE           
         OC    MLKMKT,MLKMKT                                                    
         BZ    EXPSTATX                                                         
         CLI   MLHOME,C'S'         CONTINUE WORKING AFTER FILE CHANGE           
         BNE   EXPSTATX                                                         
         SPACE 1                                                                
EXPHOK10 CLI   SPILLOPT,C'N'       OPTION TO SUPPRESS SPILL                     
         BNE   EXPHOK12                                                         
         CLI   MLHOME,0                                                         
         BNE   *+14                CONTINUE WORKING UNTIL FILE CHANGE           
         OC    MLKMKT,MLKMKT                                                    
         BNZ   EXPSTATX                                                         
         CLI   MLHOME,C'S'         CONTINUE WORKING AFTER FILE CHANGE           
         BE    EXPSTATX                                                         
         SPACE 1                                                                
EXPHOK12 L     R2,ATHISTAT                                                      
         USING STATENTD,R2                                                      
         CLC   PREVSTA,MLSTAT                                                   
         BE    EXPSTATX                                                         
         MVC   STATCALL,MLSTAT                                                  
         MVC   PREVSTA,MLSTAT                                                   
         OC    MLKMKT,MLKMKT       NOTE SPILL MARKETS                           
         BZ    *+8                                                              
         NI    STATCALL+4,X'7F'    BY TURNING OFF X'80' BIT                     
         LA    R2,L'STATENT(R2)                                                 
         ST    R2,ATHISTAT                                                      
         AI    NSTATS,1                                                         
         CLI   NSTATS,100                                                       
         BNH   EXPSTATX                                                         
         DC    H'0'                OUT OF SPACE IN STATION LIST                 
         DROP  R4                                                               
*                                                                               
EXPSTATX XMOD1                                                                  
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* AVECOMPB- CALCULATE COMPONENTS OF THE RANDOM DUPL FORMULA AND                 
*           SAVE AWAY IN CUMERAT.                                               
*           --INPUT - (R4) PTS TO DEMO DATA IS IN ABOOKBF                       
*                   - OPER - TELLS WHETHER TO DO CALC OF NEW CUME OR            
*                            CALC CUME RTG AND SAVE SUME SUM                    
*           --OUTPUT- SUMBUF=SUM OF CUMES, CUMERAT=CUME RATING                  
* ********************************************************************          
*                                                                               
AVECOMPB DS    0H                                                               
         NMOD1 0,**AVECOMP**                                                    
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         ZIC   R1,CNOSTA           INCR # STATIONS IN COMBO                     
         LA    R1,1(R1)                                                         
         STC   R1,CNOSTA                                                        
*                                                                               
         LA    R3,SUMBUF           SAVE AREA FOR CUME SUMS                      
         LA    R4,BUFFDEM-BUFFD(R4) PTS TO DEMO DATA                            
         ZIC   R5,NBOOKS           NUMBER BOOKS REQUESTED                       
         LA    R6,CUMERAT          SAVE AREA FOR CUME RATINGS                   
         LA    R7,DEMOS            TYPE OF DEMO LIST                            
         ZIC   R8,NDEMOS           NUMBER DEMOS IN LIST                         
*                                                                               
AVECMP2  CLI   1(R7),C'C'          ONLY PROCESS CUME DEMOS                      
         BNE   AVECMP4             NOT A CUME, SKIP CUME CALCS                  
         BAS   RE,CUMERTG                                                       
         CLI   CNOSTA,2            CALC NEWCUME IF 2 OR MORE STATIONS           
         BL    AVECMP4             ONLY 1 STATION SO FAR READ                   
         BAS   RE,NEWCUMES                                                      
*                                                                               
AVECMP4  LA    R3,16(R3)           NEXT SUMBUF-SAVE-AREA                        
         LA    R4,L'BUFFREC(R4)    NEXT DEMO DATA FROM ABOOKBF                  
         LA    R6,16(R6)           NEXT CUMERTG-SAVE-AREA                       
         LA    R7,3(R7)            NEXT DEMO TYPE                               
         BCT   R8,AVECMP2          LOOP THRU ALL DEMOS                          
         XMOD1                                                                  
         EJECT                                                                  
* ******************************************************************            
* CUMERTG - CALC SUM OF CUMES AND CUME RATING FOR DEMO IN EA BOOK               
* ******************************************************************            
*                                                                               
CUMERTG  DS    0H                                                               
         NTR1                                                                   
*                                                                               
CUMERG1  SR    R1,R1                                                            
         ICM   R1,BUFFICM,0(R4)    GET CUME FOR THIS BOOK                       
         A     R1,0(R3)            SUM WITH PREV STATION DEMO                   
         ST    R1,0(R3)            SAVE SUM IN SUMBUF                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,BUFFICM,0(R4)    GET CUME FOR THIS BOOK                       
         SR    R0,R0                                                            
         M     R0,=F'10000'                                                     
         STM   R0,R1,DUB                                                        
         BAS   RE,FNDUNIVS         FIND UNIV -PLACES IT IN R1                   
         LTR   R8,R1               MOVE IT INTO R8 TO DO THE DIVIDE             
         BNZ   CUMERG5                                                          
         ST    R8,0(R6)            CUME RATING=0                                
         B     CUMERG8                                                          
*                                                                               
CUMERG5  LM    R0,R1,DUB           RESTORE NUMERATOR                            
         DR    R0,R8               DIVIDE CUME BY UNIVERSE                      
         AH    R1,=H'5'            ROUND                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         SR    R0,R0                                                            
         M     R0,0(R6)            MULTIPLY BY PREV STATN CUME RATING           
         ST    R1,0(R6)            SAVE NEW (MULTIPLIED) CUME RATING            
*                                                                               
CUMERG8  LA    R3,4(R3)            NEXT SUMBUF SAVE AREA                        
         LA    R4,BUFFDMQ(R4)      NEXT BOOK DEMO IN ABOOKBF                    
         LA    R6,4(R6)            NEXT CUME RTG SAVE AREA                      
         BCT   R5,CUMERG1          DO ALL BOOKS FOR THIS DEMO                   
*                                                                               
CUMERTGX XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
* ********************************************************************          
* NEWCUMES- GOES THRU ALL BOOKS WITHIN A CUME DEMO AND CALCULATES               
*           THE NEW CUME, NEW CUME RATING (STORE IN CUMERAT). IT WILL           
*           PLACE THE NEW CUME INTO SUMBUF.                                     
* ********************************************************************          
*                                                                               
NEWCUMES DS    0H                                                               
         NTR1                                                                   
         LR    RF,R3               RF - PTS TO SUMBUF (INSTEAD OF R3)           
NEWCUME  BAS   RE,FNDUNIVS         GET UNIV FOR THIS BOOK. R1=UNIV              
         LTR   R3,R1               SAVE UNIV IN R3                              
         BNZ   NEWCUM5                                                          
         STCM  R3,BUFFICM,0(R4)    NEWCUME=0                                    
         ST    R3,0(R6)            CUMERTG=0                                    
         ST    R3,0(RF)            CUME SUM=0                                   
         B     NEWCUM8                                                          
*                                                                               
NEWCUM5  ST    R3,DMCB             SAVE AWAY UNIVERSE                           
         SR    R2,R2                                                            
         M     R2,0(R6)            UNIVERSE*CUME RATING                         
*                                                                               
         D     R2,=F'1000'         DIVIDE FOR STATIONS-1                        
         SR    R2,R2                                                            
         D     R2,=F'100'                                                       
         A     R3,=F'5'                                                         
         SR    R2,R2                                                            
         D     R2,=F'10'                                                        
         L     R1,0(RF)            SUM OF CUMES                                 
         SR    R1,R3               SUM OF CUMES-(CUME RTGS * UNIVERSE)          
         ST    R1,0(RF)            SAVE NEW CUME                                
         STCM  R1,BUFFICM,0(R4)    SAVE NEW CUME IN ABOOKBF                     
*                                                                               
         SR    R0,R0               CALCULATE NEW CUME RATING                    
         M     R0,=F'10000'                                                     
         D     R0,DMCB             NEW CUME RATING=NEW CUME/UNIVERSE            
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R6)            SAVE NEW CUME RATING                         
*                                                                               
NEWCUM8  LA    RF,4(RF)            NEXT SUMBUF SAVE AREA                        
         LA    R4,BUFFDMQ(R4)      NEXT BOOK FOR THIS DEMO IN ABOOKBF           
         LA    R6,4(R6)            NEXT CUME RTG SAVE AREA                      
         BCT   R5,NEWCUME          DO ALL BOOKS FOR THIS DEMO                   
*                                                                               
NEWCUMEX XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* FNDUNIVS                                                                      
**********************************************************************          
FNDUNIVS DS    0H                                                               
         NTR1                                                                   
         ZIC   RE,NBOOKS           CALC DISP TO UNIV                            
         SR    RE,R5                                                            
         LA    RF,16               DISP TO NEXT LINE OF BOOK UNIVS              
         MR    RE,RE               DISP OFF ACTUNIVS IN RF                      
         LA    RE,ACTUNIVS         PT TO UNIV FOR 1ST BOOK                      
         CLI   0(R7),2             TSA?                                         
         BL    FNDUV6              NO, MSA                                      
         BH    FNDUV4              NO,ADI                                       
         LA    RE,ACTUNIVS+4       TSA                                          
         B     FNDUV6                                                           
FNDUV4   LA    RE,ACTUNIVS+8       ADI                                          
*                                                                               
FNDUV6   AR    RE,RF               BOOK DISPLACEMENT                            
         L     R1,0(RE)            PICK UP UNIVERSE                             
         XIT1  REGS=(R1)           SEND IT BACK                                 
*                                                                               
         EJECT                                                                  
* ********************************************************************          
*              ROUTINE TO GENERATE AN AVERAGE BOOK BUFFER                       
         SPACE 3                                                                
*              INPUTS              ABOOKBF NBOOKS NSTATS                        
*              OUTPUT              BUFF                                         
* ********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
AVEBUFF  NMOD1 0,**AVEBUF**                                                     
         PRINT GEN                                                              
         L     R9,0(R1)                                                         
*                                                                               
         CLI   AVEOPT,C'N'         SUPRESS AVERAGES                             
         BE    AVEXMOD                                                          
         L     R2,ABOOKBF                                                       
         USING BUFFD,R2                                                         
         ZIC   R0,NSTATS                                                        
AVE1     ZIC   R4,NDEMOS                                                        
         LA    R3,BUFFAVG          PT TO AVERAGE STORE AREA IN BUFFDEM          
         SPACE 1                                                                
AVE2     BAS   RE,AVE4             HANDLE FOR EACH STATION                      
         LA    R2,L'BUFFREC(R2)                                                 
         LA    R3,L'BUFFREC(R3)                                                 
         BCT   R4,AVE2                                                          
         BCT   R0,AVE1                                                          
         B     AVEXMOD                                                          
         SPACE 1                                                                
AVEXMOD  XMOD1                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
AVE4     NTR1                                                                   
         SR    R1,R1               COUNT DEMOS IN R1                            
         ZIC   R0,NBOOKS           (UP TO 5 BOOKS)                              
         LA    R2,BUFFDEM                                                       
         SPACE 1                                                                
AVE6     SR    RF,RF               PICK UP DEMO FOR THIS BOOK                   
         ICM   RF,BUFFICM,0(R2)                                                 
         CLI   WORSTOPT,C'Y'                                                    
         BNE   AVE7                                                             
         LCR   RF,RF                                                            
         N     RF,=F'65535'                                                     
AVE7     AR    R1,RF                                                            
         SPACE 1                                                                
AVE8     LA    R2,BUFFDMQ(R2)      BUMP TO NEXT BOOK                            
         BCT   R0,AVE6                                                          
         SPACE 1                                                                
         ZIC   RE,NBOOKS           NOW DIVIDE BY N'BOOKS                        
         SLL   R1,1                                                             
         DR    R0,RE                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   R1,R1                                                            
         STCM  R1,BUFFICM,0(R3)          AND STICK IN AVERAGE BOOK              
         XIT1                                                                   
         LTORG                                                                  
         PRINT NOGEN                                                            
         EJECT                                                                  
* *********************************************************************         
* AFTPARSE -                                                                    
* *********************************************************************         
*                                                                               
AFTPARSE NMOD1 0,**AFTPARSE**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         MVI   STALIST,C'M'                                                     
         L     RE,AIO2             CLEAR A BIG AREA                             
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
* COMBINE MARKET AND USER LISTS                                                 
*                                                                               
         LA    R2,TREMKTH          COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'1'         1 ENTRY                                      
         LA    R2,TREUL1H          AND INCLUDE USER LISTS                       
         ST    R2,LTABLE+4                                                      
         MVI   LTABLE+4,X'3'       3 ENTRIES                                    
         LA    R2,OUTAREA1                                                      
         CLI   TREMKTH+5,0         CHECK IF EMPTY                               
         BZ    HHHH                                                             
         SPACE 1                                                                
         USING ELEMDS,R2                                                        
         SPACE 1                                                                
* NOT EMPTY, PUT IN M=                                                          
         SPACE 1                                                                
         MVI   TREMKTH+4,X'C0'                                                  
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E0102E400D4'                            
         LA    R2,ELEMBDAT+1(R2)                                                
HHHH     GOTO1 VMINIPAR,DMCB,LTABLE,SCTABL,(X'FF',(R2))                         
*                                                                               
         LA    R2,OUTAREA1         MUST HAVE A MARKET FIRST                     
         CLI   ELEMSTOP,C'='                                                    
         BNE   AFTERROR                                                         
         CLI   ELEMDATA,C'M'                                                    
         BNE   AFTERROR                                                         
*                                                                               
*                                                                               
* COMBINE STATIONS                                                              
*                                                                               
         XC    LTABLEDS,LTABLEDS                                                
         LA    R2,TRESTATH         COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'0C'         12 ENTRIES                                  
         LA    R2,OUTAREA2                                                      
         XC    STATLEN,STATLEN                                                  
         CLI   TRESTATH+5,0                                                     
         BZ    AFTPARX2                                                         
         SPACE 1                                                                
* NOT EMPTY, PUT IN S=                                                          
         SPACE 1                                                                
         MVC   STATLEN,TRESTATH+5                                               
         MVI   TRESTATH+4,X'C0'                                                 
         MVC   ELEMSTOP(ELEMBDAT+1),=X'7E01033900E2'                            
         LA    R2,ELEMBDAT+1(R2)                                                
         GOTO1 VMINIPAR,DMCB,LTABLE,SCTABL,(X'50',(R2))                         
*-----------------------------------------------------------------              
* INSERTION OF STATION INTO MARKET AND USER LIST                                
*-----------------------------------------------------------------              
*                                                                               
         MVI   TRESTATH+5,0                                                     
         MVC   OUTAREA3,OUTAREA1                                                
         MVC   OUTAREA1,=CL128' '                                               
         LA    R1,OUTAREA1                                                      
         LA    R2,OUTAREA2                                                      
         LA    R3,OUTAREA3                                                      
* COPY FIRST ELEM TO AREA3                                                      
         MVC   0(ELEMBDAT,R1),0(R3)    FIRST ONE HAS NO ERROR                   
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
         LA    R4,ELEMBDAT+1(R4)       1 FOR EX, 4 FOR HEADER                   
         AR    R3,R4                                                            
         AR    R1,R4                                                            
*                                                                               
         MVC   0(ELEMBDAT,R1),0(R3)    FIRST ONE HAS NO ERROR                   
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
         LA    R4,ELEMBDAT+1(R4)       1 FOR EX, 4 FOR HEADER                   
         AR    R3,R4                                                            
         AR    R1,R4                                                            
*                                                                               
TWO_3    CLI   0(R2),X'FF'                                                      
         BE    ONE_3                                                            
         CLI   0(R2),X'0'                                                       
         BE    ONE_3                                                            
         MVC   0(ELEMBDAT,R1),0(R2)                                             
         ZIC   R4,1(R2)            LENGTH OF ELEM                               
         LTR   R4,R4                                                            
         BNZ   TWO_3A                                                           
         BCTR  R4,0                                                             
         B     TWO_3B                                                           
TWO_3A   BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R2)                                      
TWO_3B   LA    R4,ELEMBDAT+1(R4)   1 FOR EX, 4 FOR HEADER                       
         AR    R2,R4                                                            
         AR    R1,R4                                                            
         B     TWO_3                                                            
*                                                                               
ONE_3    CLI   0(R3),X'FF'                                                      
         BE    AFTPARXT                                                         
         CLI   0(R3),X'0'                                                       
         BE    AFTPARXT                                                         
         MVC   0(ELEMBDAT,R1),0(R3)                                             
         ZIC   R4,1(R3)            LENGTH OF ELEM                               
         LTR   R4,R4                                                            
         BNZ   ONE_3A                                                           
         BCTR  R4,0                                                             
         B     ONE_3B                                                           
ONE_3A   BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ELEMBDAT(0,R1),ELEMBDAT(R3)                                      
ONE_3B   LA    R4,ELEMBDAT+1(R4)   1 FOR EX, 4 FOR HEADER                       
         AR    R3,R4                                                            
         AR    R1,R4                                                            
         B     ONE_3                                                            
*                                                                               
AFTPARXT MVC   0(4,R1),0(R3)                                                    
AFTPARX2 LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         C     R1,OUTAREA          NO ERROR                                     
         XMOD1                                                                  
AFTERROR LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
*        MVC   OUTAREA1+3(3),=X'02E400'                                         
         LTR   R1,R1               ERROR                                        
         XMOD1                                                                  
         SPACE 2                                                                
         SPACE 2                                                                
OUTAREA1 DC    CL255' '                                                         
OUTAREA2 DC    CL255' '                                                         
OUTAREA3 DC    CL255' '                                                         
TEMPR1   DC    F'0'                                                             
TEMPR3   DC    F'0'                                                             
LTABLEDS DS    0FL8                                                             
LTABLE   DC    8F'0'                                                            
SCTABL   DC    C'=,',X'FF'                                                      
         LTORG                                                                  
         EJECT                                                                  
* ********************************************************************          
* FILLBUF -    ROUTINE TO FILL BUFFER FOR THIS STATION                          
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  R3=STATION NUMBER                            
*                                  R4=A(BUFFER FOR STATION)                     
*              OUTPUT              BUFFDEM(S)                                   
* ********************************************************************          
         SPACE 1                                                                
         DS    0H                                                               
FILLBUFF NMOD1 0,**FLBF**                                                       
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
         L     R5,12(R1)                                                        
         L     R9,16(R1)                                                        
         L     RC,20(R1)                                                        
*                                                                               
         USING BUFFD,R4                                                         
         STC   R3,BUFFSTAT                                                      
         MVI   DBSELMED,C'R'                                                    
         USING STATENTD,R2                                                      
         XC    DBSELMK,DBSELMK                                                  
         CLI   TRESRCE,C'A'                                                     
         BE    FILLB1                                                           
         TM    STATCALL+4,X'80'                                                 
         BO    *+10                                                             
FILLB1   MVC   DBSELMK,MKTNUM                                                   
         OI    STATCALL+4,X'80'                                                 
         MVC   DBSELSTA,STATCALL                                                
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBAREC,AIO1                                                      
         SPACE 1                                                                
         ST    R4,BUFFCURR                                                      
         LA    R2,BOOKS            SET UP FOR MULTIPLE BOOKS                    
         LA    R4,BUFFDEM                                                       
         ZIC   R7,NBOOKS                                                        
         SPACE 1                                                                
FILL2    MVC   DBSELBK,1(R2)                                                    
         MVC   DBBTYPE,3(R2)                                                    
         CLI   DBSELBK,87          ALWAYS NEED AFTER 86 BOOKS                   
         BL    *+10                                                             
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM(12),THISDEM   CLEAR BUFFER FOR STATION/BOOK              
         XC    THISWT,THISWT                                                    
         MVC   DBFILE,=C'RDP'      READ RDP FOR STANDARD                        
         CLI   DPSTYPE,C'S'        STANDARD SETTING                             
         BNE   FILL6                                                            
         MVC   DBSELDAY,DPSDAY     NEEDS DAY                                    
         XC    DBSELTIM,DBSELTIM   DOES NOT NEED TIME                           
*                                  BUT DOES NEED PROGRAM                        
         MVC   DBSELPRG+1(1),DPSPROG                                            
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         B     FILLNEXT                                                         
         SPACE 1                                                                
FILL6    MVC   DBFILE,=C'TP '      READ TP FOR CUSTOM DAYPARTS                  
         LA    R3,DPCDAY           CUSTOM HAS UP TO 5 DAY/TIMES                 
         XC    DBSELPRG,DBSELPRG                                                
         LA    R5,5                                                             
         SPACE 1                                                                
FILL8    CLI   0(R3),0                                                          
         BE    FILLNEXT                                                         
         MVC   DBSELDAY,0(R3)      MOVE IN DAY                                  
         ZIC   R1,1(R3)            WORK OUT MILITARY TIMES                      
         MH    R1,=H'100'                                                       
         STH   R1,DUB                                                           
         ZIC   R1,2(R3)                                                         
         MH    R1,=H'100'                                                       
         STH   R1,DUB+2                                                         
         MVC   DBSELTIM,DUB        START AND END                                
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         LA    R3,3(R3)                                                         
         BCT   R5,FILL8                                                         
         SPACE 1                                                                
FILLNEXT L     RE,THISDEM          NOW AVERAGE DEMO WE GOT                      
         OC    THISWT,THISWT                                                    
         BZ    FILLNXT2                                                         
         SRDA  RE,31                                                            
         D     RE,THISWT                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   RF,RF                                                            
         CLI   DEMOS+1,C'C'        IS THIS A CUME?                              
         BE    FNXT2               YES, DON'T SUM, JUST STORE                   
         SR    R0,R0                                                            
         ICM   R0,BUFFICM,0(R4)                                                 
         AR    RF,R0                                                            
FNXT2    STCM  RF,BUFFICM,0(R4)                                                 
*                                                                               
         CLI   NDEMOS,2                                                         
         BL    FILLNXT2                                                         
         L     RE,THISDEM+4        NOW AVERAGE DEMO WE GOT                      
         SRDA  RE,31                                                            
         D     RE,THISWT                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   RF,RF                                                            
         CLI   DEMOS+3+1,C'C'        IS THIS A CUME?                            
         BE    FNXT4               YES, DON'T SUM, JUST STORE                   
         SR    R0,R0                                                            
         ICM   R0,BUFFICM,L'BUFFREC(R4)                                         
         AR    RF,R0                                                            
FNXT4    STCM  RF,BUFFICM,L'BUFFREC(R4)                                         
*                                                                               
         CLI   NDEMOS,3                                                         
         BL    FILLNXT2                                                         
         L     RE,THISDEM+8        NOW AVERAGE DEMO WE GOT                      
         SRDA  RE,31                                                            
         D     RE,THISWT                                                        
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CLI   WORSTOPT,C'Y'                                                    
         BNE   *+6                                                              
         LCR   RF,RF                                                            
         CLI   DEMOS+3+3+1,C'C'        IS THIS A CUME?                          
         BE    FNXT6               YES, DON'T SUM, JUST STORE                   
         SR    R0,R0                                                            
         ICM   R0,BUFFICM,L'BUFFREC+L'BUFFREC(R4)                               
         AR    RF,R0                                                            
FNXT6    STCM  RF,BUFFICM,L'BUFFREC+L'BUFFREC(R4)   3RD DEMO SAVE AREA          
*                                                                               
FILLNXT2 LA    R2,4(R2)                                                         
         LA    R4,BUFFDMQ(R4)            NEXT BOOK                              
         CLI   DBSELSRC,C'M'                                                    
         BNE   FILL9                                                            
         CLI   RUNIFLG,C'Y'                                                     
         BE    FILL9                                                            
         CLI   CALCOPT,C'Y'                                                     
         BNE   FILL9                                                            
*                                                                               
         BAS   RE,FRELUNIV                                                      
FILL9    BCT   R7,FILL2                                                         
         CLI   DBSELSRC,C'M'                                                    
         BNE   FILL9B                                                           
         OC    RUNIVS,RUNIVS                                                    
         BZ    FILL9B                                                           
         CLI   RUNIFLG,C'Y'                                                     
         BE    FILL9B                                                           
         BAS   RE,FAVEUNIV                                                      
         MVI   RUNIFLG,C'Y'                                                     
FILL9B   XMOD1                                                                  
         SPACE 3                                                                
FRELUNIV NTR1                                                                   
         LA    R0,4                                                             
         LA    R2,UNIVS                                                         
         LA    R3,RUNIVS                                                        
FRELUNV1 L     R5,0(R2)                                                         
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,FRELUNV1                                                      
         B     XIT                                                              
         SPACE 3                                                                
* **********************************************************                    
* FAVEUNIV                                                                      
* **********************************************************                    
FAVEUNIV NTR1                                                                   
         LA    R0,4                                                             
         ZIC   R5,NBOOKS                                                        
         LA    R7,RUNIVS                                                        
FAVUNILP SR    R2,R2                                                            
         L     R3,0(R7)                                                         
         DR    R2,R5                                                            
         ST    R3,0(R7)                                                         
         LR    R3,R2                                                            
         SR    R2,R2                                                            
         LA    R5,10                                                            
         MR    R2,R5                                                            
         ZIC   R5,NBOOKS                                                        
         DR    R2,R5                                                            
         CH    R3,=H'5'                                                         
         BL    FAVNXTLP                                                         
         L     R3,0(R7)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,0(R7)                                                         
FAVNXTLP LA    R7,4(R7)                                                         
         BCT   R0,FAVUNILP                                                      
         B     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* ********************************************************************          
* DEMHOOK -    HOOK FROM DEMAND                                                 
*              INPUTS              R2=A(THISBUFF FOR DAYPART)                   
*                                  DEMOS NDEMOS                                 
*              OUTPUT              ADD WEIGHTED DEMOS TO THIS BUFF              
* ********************************************************************          
         SPACE 1                                                                
DEMHOOK  NTR1                                                                   
         CLI   RUNIFLG,C'Y'                                                     
         BE    DEMHK0              DON'T NEED UNIVERSES AGAIN                   
         CLI   DBSELSRC,C'M'                                                    
         BNE   DEMHK0                                                           
         GOTO1 VSUBR07,DMCB,('SETUNIBE',(RC))                                   
*                                  ADD DEMOS TO THISBUFF                        
DEMHK0   L     RE,DBAREC                                                        
         LA    RE,23(RE)                                                        
         USING MARELEM,RE                                                       
         CLI   MARCODE,MARCODEQ    '01' ELEMENT?                                
         BNE   DEMHK1                                                           
         CLC   MARNO,MKTNUM                                                     
         BNE   DEMHK1                                                           
         CLI   MARELN,MARLNEQ2     EXTENDED '01' ELEMENT?                       
         BL    DEMHK1              NO                                           
         CLI   MARACTCD,C' '                                                    
         BNH   DEMHK1                                                           
         L     R1,BUFFCURR                                                      
         MVC   BUFFFLAG-BUFFREC(1,R1),MARACTCD                                  
         DROP  RE                                                               
         SPACE 1                                                                
DEMHK1   GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,THISLINE                         
         SPACE 1                                                                
         ZIC   R0,NDEMOS                                                        
         LA    RE,THISLINE                                                      
         LA    RF,THISDEM                                                       
DEMHK2   L     R1,0(RE)                                                         
         MH    R1,DBFACTOR                                                      
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,DEMHK2                                                        
         L     R1,THISWT                                                        
         AH    R1,DBFACTOR                                                      
         ST    R1,THISWT                                                        
         SPACE 1                                                                
         CLI   TRACEOPT,C'Y'       OPTION TO TRACE                              
         BNE   DEMXIT                                                           
         MVC   P(5),DBSELSTA                                                    
         LA    R2,P+6                                                           
         GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,WORK                                  
         MVC   0(4,R2),WORK+2                                                   
         LA    R2,5(R2)                                                         
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,WORK                                 
         GOTO1 UNTIME,DMCB,WORK+2,(R2)                                          
         LA    R2,12(R2)                                                        
         EDIT  (2,DBFACTOR),(5,(R2))                                            
         LA    R2,6(R2)                                                         
         EDIT  (4,THISLINE),(5,(R2))                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
DEMXIT   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
MKTSTAT  NMOD1 0,**MKTSTAT**                                                    
         L     R2,0(R1)            ADDRESS OF WHERE TO ADD STATION              
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   DBSELSRC,C'M'                                                    
         BE    MKTXIT                                                           
         MVI   4(R2),C'A'                                                       
         EDIT  (2,MKTNUM),(4,(R2))                                              
*                                                                               
         LA    RE,4                                                             
         LR    R3,R2                                                            
MKTSTA1A CLI   0(R3),C' '                                                       
         BNE   MKTSTA1B                                                         
         MVI   0(R3),C'0'                                                       
MKTSTA1B LA    R3,1(R3)                                                         
         BCT   RE,MKTSTA1A                                                      
*                                                                               
         MVI   NSTATS,1                                                         
MKTXIT   XMOD1                                                                  
         EJECT                                                                  
*-------------------------------------------------------------                  
* THIS PROCEDURE IS TO FIND THE ALPHA MARKET FOR A GIVEN                        
* MARKET NUMBER.                                                                
* THE INPUTS NEEDED ARE: MARKET NUMBER    (MKTNUM)                              
*                        MEDIA TYPE       (MEDTYPE)                             
*                        RATING SOURCE    (SRCTYPE)                             
*                        BOOK TYPE        (BOOKTYPE)                            
* THE OUTPUT WILL BE AN ALPHA MARKET THAT GOES IN CITYCODE                      
*-------------------------------------------------------------                  
ALPHMRKT NMOD1 0,**ALPHMRKT**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         LA    R5,CODETABL                                                      
         MVC   0(1,R5),MEDTYPE                                                  
         MVC   1(1,R5),SRCTYPE                                                  
         MVC   2(1,R5),BOOKTYPE                                                 
         MVC   3(2,R5),MKTNUM                                                   
         GOTO1 VNUM2CDE                                                         
         MVC   CITYCODE,5(R5)                                                   
         CLC   =CL3' ',CITYCODE                                                 
         BNE   XITCALPH                                                         
XITCALFE MVC   CITYCODE,=X'000000'                                              
XITCALPH XMOD1                                                                  
         SPACE 1                                                                
CODETABL DC    CL8' '                                                           
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF BUFFER                            
         SPACE 3                                                                
*              OUTPUT              PRINT LINES                                  
         SPACE 1                                                                
         DS    0H                                                               
PRNTBUFF NMOD1 0,**PRNTBUFF**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
         L     R5,8(R1)            TMPDEMOS ADDRESS                             
         LR    R6,R5                                                            
*                                                                               
         CLI   LONGOPT,C'Y'                                                     
         BNE   PB0                                                              
         LA    RF,103                                                           
         B     PB0A                                                             
PB0      LA    RF,60                                                            
PB0A     STC   RF,MAXLINES                                                      
         CLI   CALCOPT,C'Y'                                                     
         BNE   PB1                                                              
         GOTO1 VSUBR07,DMCB,('SETNEWUE',(RC))                                   
PB1      L     R2,ABOOKBF                                                       
         USING BUFFD,R2                                                         
         CLI   DBSELSRC,C'M'       CANADIAN?                                    
         BE    *+8                 NO MARKET RECORD TO SKIP                     
         A     R2,NXTPOS           SKIP MARKET RECORD                           
         ZIC   R0,MAXRANK                                                       
         SPACE 1                                                                
PB2      CLI   0(R2),0                                                          
         BE    PB4                                                              
         MVI   NTHDEM,1                                                         
         BAS   RE,FORMLINE                                                      
         MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
         ZIC   RE,0(R2)            GET STATNUM                                  
         LA    R2,L'BUFFREC(R2)                                                 
         CLI   NDEMOS,2                                                         
         BL    PB3                                                              
         STC   RE,0(R2)            SHOULD BE SAME STATION                       
         MVI   NTHDEM,2                                                         
         LA    R5,3(R5)                                                         
         BAS   RE,FORMLINE                                                      
         MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
         ZIC   RE,0(R2)            GET STATNUM                                  
         LA    R2,L'BUFFREC(R2)                                                 
         CLI   NDEMOS,3                                                         
         BL    PB3                                                              
         STC   RE,0(R2)            SHOULD BE SAME STATION                       
         MVI   NTHDEM,3                                                         
         LA    R5,3(R5)                                                         
         BAS   RE,FORMLINE                                                      
         MVC   SPACING,SPACOPT                                                  
         GOTO1 SPOOL,DMCB,(R8)     ONE MORE LINE FOR BOX BOTTOM                 
         LA    R2,L'BUFFREC(R2)                                                 
PB3      LR    R5,R6               RESTORE FIRST DEMO                           
         BCT   R0,PB2                                                           
         SPACE 1                                                                
PB4      CLI   COMBNUM,0                                                        
         BE    PB9                                                              
         ZIC   R0,COMBNUM                                                       
         LA    R2,COMBSV           POINT TO A LIST OF PNTRS                     
         LA    R3,P+1                                                           
         ST    R3,DUB                                                           
PB5      L     RF,0(R2)            GET A COMBO ELEMENT                          
         ZIC   RE,1(RF)                                                         
         AR    RE,RF               SET END                                      
         MVC   0(9,R3),2(RF)       AND MOVE IN THE COMBO NAM                    
         MVI   10(R3),C'='                                                      
         LA    R3,11(R3)                                                        
         LA    RF,11(RF)                                                        
PB6      CR    RF,RE                                                            
         BNL   PB7                                                              
         MVC   0(4,R3),0(RF)       MOVE IN AND FORMAT THE STATIONS              
         MVC   4(3,R3),=C'- M'                                                  
         MVC   5(1,R3),4(RF)                                                    
         LA    R3,7(R3)                                                         
         LA    RF,5(RF)                                                         
         CR    RF,RE                                                            
         BNL   PB7                                                              
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     PB6                                                              
         SPACE 1                                                                
PB7      LA    R2,4(R2)            CONTINUE TILL END                            
         L     R3,DUB                                                           
         GOTO1 VSQUASH,DMCB,(R3),(0,200)                                        
         L     R3,DUB                                                           
         A     R3,DMCB+4                                                        
         LA    R3,2(R3)                                                         
         BCT   R0,PB5                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PB9      CLI   SPACTSW,0                                                        
         BE    PBXIT                                                            
         L     RE,=A(VIO1)                                                      
         A     RE,RELO                                                          
         MVC   P+1(L'VIO1),0(RE)                                                
         L     RE,=A(VIO2)                                                      
         A     RE,RELO                                                          
         MVC   P2+1(L'VIO2),0(RE)                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
PBXIT    XMOD1                                                                  
         SPACE 3                                                                
NTHDEM   DC    X'0'                                                             
         EJECT                                                                  
* *******************************************************************           
* FORMLINE -   FORMAT AND PRINT A REPORT LINE                                   
*              INPUTS              R2=A(STATION BUFFER)                         
*                                  ASTATS=A(5 CHARACTER STATION CODES)          
*                                  NBOOKS                                       
*                                  DISP                                         
* *******************************************************************           
*                                                                               
         USING BUFFD,R2                                                         
FORMLINE NTR1                                                                   
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         SPACE 1                                                                
         CLI   BUFFSTAT,0          INDEX INTO STATION LIST                      
         BE    FORM6                                                            
         ZIC   R1,BUFFSTAT                                                      
         BCTR  R1,0                                                             
         LA    R0,L'STATENT                                                     
         MR    R0,R0                                                            
         A     R1,ASTATS                                                        
         CLI   NTHDEM,1                                                         
         BH    FORM6                                                            
         USING STATENTD,R1                                                      
         SPACE 1                                                                
         CLC   STATCALL(3),=C'CMB'                                              
         BNE   FORM4                                                            
         MVI   USECMB,C'Y'                                                      
         TM    STATCALL+3,X'F0'    TEST FOR COMBO STATION                       
         BNO   FORM4                                                            
         ZIC   RE,STATCALL+4       YES - GET THE NAME                           
         SLL   RE,2                                                             
         LA    RE,COMBSV(RE)                                                    
         L     RE,0(RE)                                                         
         MVC   0(9,R3),2(RE)                                                    
         B     FORM6                                                            
         SPACE 1                                                                
FORM4    MVI   USECMB,C'N'                                                      
         MVC   0(4,R3),STATCALL    OUTPUT WABC                                  
         MVI   4(R3),C'-'                 WABC-                                 
         MVC   5(1,R3),STATCALL+4         WABC-A                                
         MVI   6(R3),C'M'                 WABC-AM                               
         CLI   5(R3),C'C'              OR                                       
         BNE   *+8                                                              
         MVI   6(R3),C'O'                 WABC-CO                               
         CLI   5(R3),C'B'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R3),=C'AA'             WABC-AA                               
         CLI   5(R3),C'D'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R3),=C'FF'             WABC-FF                               
         CLI   3(R3),C' '          CHANGE WOR -AM                               
         BNE   *+10                                                             
         MVC   3(4,R3),4(R3)           TO WOR-AM                                
         CLI   BUFFFLAG,C' '                                                    
         BNH   FORM6                                                            
         MVI   SPACTSW,1                                                        
         MVC   4(1,R3),5(R3)                                                    
         MVI   5(R3),C'#'                                                       
         MVC   6(1,R3),BUFFFLAG                                                 
         B     FORM6                                                            
         SPACE 1                                                                
FORM6    LA    R3,8(R3)            BUMP PAST THE CALL LETTERS                   
         CLI   COMBNUM,0           ADJUST FOR COMBO LENGTH                      
         BE    *+8                                                              
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         CLI   FORMOPT,C'Y'        OPTION TO SHOW STATION FORMAT                
         BNE   FORM7                                                            
         MVC   0(4,R3),STATFORM                                                 
         LA    R3,5(R3)                                                         
         SPACE 1                                                                
FORM7    CLI   NCATS,1                                                          
         BE    FORM8                                                            
         CLI   NTHDEM,1                                                         
         BH    FORM7_2                                                          
         MVC   0(5,R3),CATTITS                                                  
         B     FORM7X                                                           
FORM7_2  CLI   NTHDEM,2                                                         
         BH    FORM7_3                                                          
         MVC   0(5,R3),CATTITS+5                                                
         B     FORM7X                                                           
FORM7_3  MVC   0(5,R3),CATTITS+10                                               
FORM7X   LA    R3,7(R3)                                                         
         SPACE 1                                                                
FORM8    LA    R4,BUFFDEM                                                       
         MVI   DIFFSW,C'N'         (NO DIFFERENCE FOR FIRST BOOK)               
         ZIC   R0,NBOOKS                                                        
         BAS   RE,IMPRESS                                                       
         SPACE 1                                                                
FORM10   BAS   RE,FORMBOOK         FORMAT A BOOK                                
         MVI   DIFFSW,C'Y'                                                      
         MVC   LASTDEM,0(R4)                                                    
         LA    R4,BUFFDMQ(R4)                                                   
         BCT   R0,FORM10                                                        
         SPACE 1                                                                
         CLI   AVEOPT,C'N'         OPTION TO SUPRESS AVERAGE                    
         BE    FORM15                                                           
         MVI   DIFFSW,C'N'         THEN PRINT THE AVERAGE BOOK                  
         LA    R4,BUFFAVG                                                       
         MVC   XNBOOK,NBOOKS                                                    
         MVI   NBOOKS,1                                                         
         BAS   RE,IMPRESS                                                       
         MVC   NBOOKS,XNBOOK                                                    
         BAS   RE,FORMBOOK                                                      
*                                                                               
FORM15   CLI   NTHDEM,1            WRITE STATION AGAIN                          
         BNE   XIT                 AT THE END                                   
         LA    R4,P+1                                                           
         A     R4,DISP                                                          
         MVC   2(7,R3),0(R4)                                                    
         CLI   COMBNUM,0                                                        
         BE    XIT                                                              
         MVC   2(9,R3),0(R4)                                                    
         B     XIT                                                              
XNBOOK   DC    X'00'                                                            
USECMB   DC    C'N'                                                             
         EJECT                                                                  
IMPRESS  NTR1                                                                   
         CLI   CALCOPT,C'Y'                                                     
         BNE   XIT                                                              
         L     R8,ABOOKBF                                                       
         CLI   NTHDEM,2                                                         
         BL    IMPRESS1                                                         
         LA    R8,L'BUFFREC(R8)                                                 
         CLI   NTHDEM,3                                                         
         BL    IMPRESS1                                                         
         LA    R8,L'BUFFREC(R8)                                                 
IMPRESS1 LA    R8,2(R8)            PT TO LAST  DEMO                             
         ZIC   R0,NBOOKS                                                        
*                                                                               
         CLI   NBOOKS,1                                                         
         BNE   IMPRESS2                                                         
         LA    R1,RUNIVS           AVERAGED UNIVERSES                           
         LA    R8,BUFFAVG-BUFFDEM(R8)   USE THE AVERAGE IF 1 BOOK               
         B     *+8                                                              
IMPRESS2 LA    R1,ACTUNIVS                                                      
*                                                                               
DIVLOOP  LR    R7,R1                                                            
         CLI   1(R5),C'F'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R5),C'G'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R5),C'H'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R5),C'P'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R5),C'R'                                                       
         BE    DIVUNIV                                                          
         CLI   1(R5),C'S'          NEED TO CHANGE SHARE TO IMPRESSIONS          
         BNE   FINIDIV                                                          
         CLI   DBSELSRC,C'M'       CANADIAN MARKETS DON'T HAVE TOTALS           
         BE    FINIDIV             NOT GONNA DO IT,NOT AT THIS JUNCTURE         
         B     DIVTOTS                                                          
DIVUNIV  SR    RE,RE                                                            
         SR    RF,RF                                                            
         CLI   0(R5),2                                                          
         BL    DIVUNIV0                                                         
         LA    R7,4(R7)                                                         
         CLI   0(R5),3                                                          
         BL    DIVUNIV0                                                         
         LA    R7,4(R7)                                                         
DIVUNIV0 ICM   RF,BUFFICM,0(R4)    LOAD IN THE DEMO VALUE                       
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVUNIV1                                                         
         LCR   RF,RF                                                            
         N     RF,=F'65535'                                                     
DIVUNIV1 L     R3,=F'10000'                                                     
         MR    RE,R3               (1000*DEMO)                                  
         L     R3,0(R7)                                                         
         LTR   R3,R3                                                            
         BNZ   DIVUNIV2                                                         
         SR    RF,RF                                                            
         B     FINIDIVP                                                         
DIVUNIV2 DR    RE,R3               (1000*DEMO)/UNIVERSE                         
         AH    RF,=H'5'            CHECK IF WE NEED TO ROUND                    
         SR    RE,RE                                                            
         LA    R3,10                                                            
         DR    RE,R3                                                            
         CLI   WORSTOPT,C'Y'                                                    
         BNE   FINIDIVP                                                         
         LCR   RF,RF                                                            
FINIDIVP STCM  RF,BUFFICM,0(R4)                                                 
FINIDIV  LA    R4,BUFFDMQ(R4)                                                   
         LA    R1,16(R1)           NEXT ACTUAL UNIVERSE                         
FINIDIV1 BCT   R0,DIVLOOP                                                       
         B     XIT                                                              
*                                                                               
DIVTOTS  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,BUFFICM,0(R4)                                                 
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVTOTS0                                                         
         LCR   RF,RF                                                            
         N     RF,=F'65535'                                                     
DIVTOTS0 L     R3,=F'10000'                                                     
         MR    RE,R3               (10000*DEMO)                                 
         SR    R3,R3                                                            
         ICM   R3,BUFFICM,0(R8)                                                 
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVTOT0A                                                         
         LCR   R3,R3                                                            
         N     R3,=F'65535'                                                     
DIVTOT0A LTR   R3,R3                                                            
         BNZ   DIVTOTS1                                                         
         SR    RF,RF                                                            
         B     DIVTOTS2                                                         
DIVTOTS1 DR    RE,R3               (1000*DEMO)/UNIVERSE                         
         AH    RF,=H'5'            ROUNDING                                     
         SR    RE,RE                                                            
         LA    R3,10                                                            
         DR    RE,R3                                                            
         CLI   WORSTOPT,C'Y'                                                    
         BNE   DIVTOTS2                                                         
         LCR   RF,RF                                                            
DIVTOTS2 STCM  RF,BUFFICM,0(R4)                                                 
         LA    R8,BUFFDMQ(R8)                                                   
         B     FINIDIV                                                          
         EJECT                                                                  
* *********************************************************************         
* FORMBOOK -   FORMAT FOR A BOOK                                                
*              INPUT               R4=A(DEMO AND RANK)                          
*                                  LASTDEM=VALUE OF PREVIOUS BOOK               
*                                  DIFFSW SET TO Y FOR DIFF                     
*                                  R3=A(OUTPUT AREA)                            
* *********************************************************************         
         SPACE 1                                                                
FORMBOOK NTR1                                                                   
         CLI   RANKOPT,C'S'                                                     
         BE    FORMBK1                                                          
         IC    R1,L'BUFFDEM(R4)    BUMP PAST DEMO TO EDIT RANK NUMBER           
         SLL   R1,25                                                            
         SRL   R1,25                                                            
         EDIT  (R1),(3,1(R3))                                                   
         TM    L'BUFFDEM(R4),X'80'   CHECK FOR EQUALITY (IN RANK)               
         BNO   *+8                                                              
         MVI   4(R3),C'='                                                       
         LA    R3,6(R3)                                                         
         SPACE 1                                                                
FORMBK1  SR    R1,R1                                                            
         ICM   R1,BUFFICM,0(R4)                                                 
         CLI   WORSTOPT,C'Y'                                                    
         BNE   FORMBK1A                                                         
         LCR   R1,R1                                                            
         N     R1,=F'65535'                                                     
FORMBK1A STCM  R1,BUFFICM,0(R4)    DO WE NEED THIS?                             
         EDIT  (R1),(5,0(R3)),1    DISPLAY DEMOS.                               
         CLI   1(R5),C'R'          1 DEC. FOR RATINGS ETC.                      
         BE    FORMBK2                                                          
         CLI   1(R5),C'F'                                                       
         BE    FORMBK2                                                          
         CLI   1(R5),C'G'                                                       
         BE    FORMBK2                                                          
         CLI   1(R5),C'H'                                                       
         BE    FORMBK2                                                          
         CLI   1(R5),C'L'                                                       
         BE    FORMBK2                                                          
         CLI   1(R5),C'S'                                                       
         BE    FORMBK2                                                          
         CLI   1(R5),C'X'                                                       
         BE    FORMBK10                                                         
         EDIT  (R1),(5,0(R3))      ELSE NO DECIMAL PLACES                       
*                                                                               
         CLI   USECMB,C'Y'                                                      
         BNE   FORMBK2                                                          
*        CLI   1(R5),C'C'          SUPPRESS CUMES FOR COMBOS                    
         B     FORMBK2                                                          
FORMBK1B MVC   0(5,R3),=C' N/A '                                                
FORMBK2  CLI   RANKOPT,C'S'        CORRECT FOR NO RANK                          
         BNE   *+8                                                              
         LA    R3,6(R3)                                                         
         SPACE 1                                                                
         LA    R3,6(R3)                                                         
         CLI   DIFFSW,C'Y'                                                      
         BNE   FORMBKX                                                          
         SR    R0,R0                                                            
         ICM   R0,BUFFICM,LASTDEM                                               
         SR    R1,R0               FIGURE OUT DIFFERENCE                        
         LTR   R1,R1                                                            
         MVC   1(3,R3),=C'N/C'     MAY BE NO CHANGE                             
         BZ    FORMBK6                                                          
         MVI   SIGN,C'+'           SET SIGN                                     
         BP    FORMBK4                                                          
         LCR   R1,R1                                                            
         MVI   SIGN,C'-'                                                        
         SPACE 1                                                                
FORMBK4  SR    RE,RE                                                            
         ICM   RE,BUFFICM,LASTDEM          COMPUTE AS PERCENT OF LAST           
         LTR   RE,RE                                                            
         BZ    FORMBK6                                                          
         SR    R0,R0                                                            
         M     R0,=F'200'                                                       
         DR    R0,RE                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         MVC   0(4,R3),=C'HIGH'                                                 
         CH    R1,=H'999'                                                       
         BH    FORMBK6                                                          
         EDIT  (R1),(4,(R3))                                                    
         MVI   4(R3),C'%'          PERCENT SIGN                                 
         LR    RE,R3                                                            
         CLI   1(RE),C' '                                                       
         BNE   FORMBK5                                                          
         LA    RE,1(RE)                                                         
         CLI   1(RE),C' '                                                       
         BNE   FORMBK5                                                          
         LA    RE,1(RE)                                                         
         SPACE 1                                                                
FORMBK5  MVC   0(1,RE),SIGN                                                     
         SPACE 1                                                                
FORMBK6  LA    R3,6(R3)                                                         
         SPACE 1                                                                
FORMBKX  XIT1  REGS=(R3)           PASS BACK PRESENT PRINT POSITION             
         SPACE 2                                                                
*                                                                               
FORMBK10 XC    0(5,R3),0(R3)                                                    
         MVI   2(R3),C':'                                                       
         SR    R0,R0                                                            
         M     R0,=F'15'                                                        
         D     R0,=F'600'          QUARTER HOURS W/O DECIMAL                    
         LR    RF,R0               EVEN REGISTER IS REMAINDER                   
         EDIT  (R1),(2,0(R3)),ZERO=NOBLANK      # OF HOURS                      
         AH    RF,=H'5'                                                         
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         EDIT  (RF),(2,3(R3)),FILL=0                                            
         SR    R1,R1                                                            
         ICM   R1,BUFFICM,0(R4)                                                 
         B     FORMBK2                                                          
         DROP  R2                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* *******************************************************************           
* SUBR07 - CSECT                                                                
* *******************************************************************           
SUBR07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR07**,RA,RR=R7,END=SUBR07X                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R7,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
VALDSTAE EQU   (VALDSTA#-*)/4+1                                                 
EDITMLE  EQU   (EDITML#-*)/4+1                                                  
WRTWAE   EQU   (WRTWA#-*)/4+1                                                   
RESTWAE  EQU   (RESTWA#-*)/4+1                                                  
M1INITE  EQU   (M1INIT#-*)/4+1                                                  
GENDPTE  EQU   (GENDPT#-*)/4+1                                                  
EDITMBE  EQU   (EDITMB#-*)/4+1                                                  
EDITOPTE EQU   (EDITOPT#-*)/4+1                                                 
TESTMKE  EQU   (TESTMK#-*)/4+1                                                  
SETNEWUE EQU   (SETNEWU#-*)/4+1                                                 
SETUNIVE EQU   (SETUNIV#-*)/4+1                                                 
SETUNIBE EQU   (SETUNIB#-*)/4+1                                                 
         SPACE 1                                                                
VALDSTA# B     VALDSTA        1    VALIDATE A STAT. ON DEM FILE                 
EDITML#  B     EDITML         2    EDIT A USER LIS                              
WRTWA#   B     WRTWA          3    WRITE A TWA (TWANUM=WHICH ONE)               
RESTWA#  B     RESTWA         4    RESTORE A TWA                                
M1INIT#  B     M1INIT         5    INITIALIZE A MKT LIST SCREEN                 
GENDPT#  B     GENDPT         6    GENERATE DAYPART SCREEN                      
EDITMB#  B     EDITMB         7    VALIDATE A LIST OF BOOKS FOR MKT             
EDITOPT# B     EDITOPT        8    VALIDATE OPTIONS                             
TESTMK#  B     TESTMRKT       9    TEST MARKET                                  
SETNEWU# B     SETNEWUN      10                                                 
SETUNIV# B     SETUNIVS      11                                                 
SETUNIB# B     SETUNIBS      12                                                 
         EJECT                                                                  
HSTAT    DC    X'0'                                                             
CMBCOUNT DC    X'0'                                                             
TEMPMRKT DC    H'0'                                                             
         SPACE 2                                                                
* EDIT USER LISTS                                                               
         USING ELEMDS,R2                                                        
EDITML   DS    0H                                                               
         L     R4,AIO2                                                          
         ST    R4,SCANADDR                                                      
         LA    R4,1000(R4)                                                      
         XC    PARAS(16),PARAS                                                  
         ST    R4,PARAS+12                                                      
*                                                                               
PARDOK   CLI   ELEMSTOP,X'FF'         END OF LIST, ERROR                        
         BE    NEEDMRKT               NEED A MARKET                             
         CLI   ELEMSTOP,X'0'          DITTO                                     
         BE    NEEDMRKT                                                         
EDITML2  CLI   ELEMDATA,C'+'                                                    
         BE    EDMLC                                                            
         CLI   ELEMDATA,C'M'                                                    
         BE    EDMLM                                                            
         CLI   ELEMDATA,C'S'                                                    
         BE    EDMLS                                                            
         B     EDITML1                                                          
         SPACE 1                                                                
EDITML1  MVC   CONHEAD(L'NEEDKW),NEEDKW                                         
         LA    RE,CONHEAD+L'NEEDKW+1  GIVE SOME IDEA OF WHERE                   
         B     REGERR                                                           
         SPACE 1                                                                
EDITML1A MVC   CONHEAD(L'ERRSTA),ERRSTA                                         
         LA    RE,CONHEAD+L'ERRSTA+1  GIVE SOME IDEA OF WHERE                   
         B     REGERR                                                           
EDITML1C MVC   CONHEAD(L'ERRCBL),ERRCBL                                         
         B     REGERR                                                           
         SPACE 1                                                                
NEEDMRKT MVC   CONHEAD(L'NEEDMKT),NEEDMKT                                       
         LA    RE,CONHEAD+L'NEEDMKT+1                                           
         B     REGERR                                                           
         SPACE 1                                                                
NEEDSTAL MVC   CONHEAD(L'NEEDSL),NEEDSL                                         
         LA    RE,CONHEAD+L'NEEDSL+1                                            
         B     REGERR                                                           
NEEDSCBN MVC   CONHEAD(21),=C'FIELD LENGTH TOO LONG'                            
         B     ULXIT                                                            
NEEDCMBN MVC   CONHEAD(54),=C'NEED COMBO NAME FOR COMBOS WITH MORE THANX        
                TWO STATIONS'                                                   
         B     ULXIT                                                            
         SPACE 1                                                                
REGERR   ZIC   RF,ELEMLEN                                                       
         LTR   RF,RF                                                            
         BZ    ULXIT                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     ULXIT                                                            
         MVC   0(0,RE),ELEMDATA                                                 
         EJECT                                                                  
EDMLM    BAS   RE,NEXTELEM         NEXT ELEMENT                                 
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,X'0'       SHORTENED LIST?                              
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,C'='                                                    
         BE    NEEDMRKT                                                         
MOREMRKT CLI   ELEMLEN,0           NOTHING IN DATA                              
         BZ    NEEDMRKT                                                         
*---------------------------------------------------------------                
* CHECK IF THE MARKET GIVEN IS A STATION LIST                                   
*---------------------------------------------------------------                
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,=CL8' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SLKNAME(0),ELEMDATA                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MKTNSTL             MARKET NOT A STATION LIST                    
         GOTO1 GETREC                                                           
         MVI   STALSTD,C'Y'                                                     
         SPACE 1                                                                
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
EDMLM1   CLI   0(R5),X'01'         DESCRIPTION?                                 
         BNE   EDMLM2              NO, COULD BE THE STATIONS                    
         USING SLDELEM,R5                                                       
         SR    RF,RF                                                            
         MVC   TEMPMRKT,SLDMKTA                                                 
         CLI   CTRY,C'C'                                                        
         BE    EDMLM3                                                           
         CLI   TRESRCE,C'A'                                                     
         BE    EDMLM1A                                                          
         MVC   TEMPMRKT,SLDMKTB                                                 
EDMLM1A  BAS   RE,GOTTAMKT                                                      
EDMLM2   CLI   0(R5),X'05'                                                      
         BNE   EDMLM3                                                           
         USING SLSELEM,R5                                                       
         MVI   STASTYLE,C'L'       STATION LIST                                 
         L     R4,PARAS+12                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
*                                                                               
         ZIC   R1,SLSLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SLSELEM     MOVE ALL THE STATIONS                        
*                                  1 FOR BCTR                                   
         MVI   0(R4),X'04'         STATION LIST                                 
EDMLM3   ZIC   R1,1(R5)                                                         
         AR    R5,R1               GET NEXT ELEMENT                             
         CLI   0(R5),X'00'                                                      
         BNZ   EDMLM1                                                           
         B     EDMLM4                                                           
         SPACE 1                                                                
MKTNSTL  ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         ZIC   RF,DMCB                                                          
         LTR   RF,RF               SEE IF INVALID                               
         BNZ   NOTYET1             ALPHA                                        
         MVC   TEMPMRKT(2),DMCB+6                                               
         LH    R1,TEMPMRKT                                                      
         B     GOTMRKT                                                          
NOTYET1  ZIC   RF,ELEMLEN          COULD BE ALPHA MARKET                        
         CH    RF,=H'3'            DATA LONGER THAN 3 CHAR                      
         BH    NEEDMRKT            YES, ALPHA MARKET AT MOST 3                  
         BCTR  RF,0                                                             
         MVC   CITYCODE,=CL3' '                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),ELEMDATA                                             
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NEEDMRKT                                                         
         STH   R1,TEMPMRKT                                                      
GOTMRKT  BAS   RE,GOTTAMKT                                                      
EDMLM4   BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,C','       STILL MARKETS?                               
         BNE   EDMLM5                                                           
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         B     MOREMRKT            YES GET MORE                                 
EDMLM5   CLI   ELEMSTOP,X'FF'      FINISHED, LEAVE PROGRAM                      
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'00'                                                   
         BE    EQXIT2                                                           
         B     EDITML2                                                          
         EJECT                                                                  
EDMLS    XC    HSTAT,HSTAT         STATION HERE BEFORE                          
         BAS   RE,NEXTELEM         GET NEXT ELEM                                
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,X'00'      SHORTENED LIST?                              
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML1A                                                         
EDMLS2   CLI   ELEMLEN,0                                                        
         BZ    EDITML1A                                                         
         OC    MKTNUM,MKTNUM                                                    
         BZ    NEEDMRKT                                                         
* DON'T NEED THIS NOW WITH THE PARSER                                           
*        OC    PARAS+4(4),PARAS+4  CAN'T HAVE STATIONS                          
*        BNZ   NEEDMRKT                                                         
         L     R4,PARAS+12                                                      
EDSTAT1  ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         OC    HSTAT,HSTAT                                                      
         BZ    EDSTAT2                                                          
         CLI   DMCB,0                                                           
         BZ    NEEDSTAL                                                         
         L     R4,PARAS+4                                                       
         ZIC   R1,1(R4)                                                         
         LA    R0,5(R1)                                                         
         STC   R0,1(R4)                                                         
         AR    R4,R1                                                            
         B     EDSTAT3                                                          
EDSTAT2  CLI   DMCB,0                                                           
         BZ    NEEDSTAL                                                         
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
         MVC   0(2,R4),=X'0207'                                                 
*DSTAT3  BAS   RE,NEXTELEM                                                      
*        BNE   NEEDSTAL                                                         
EDSTAT3  BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
         OC    HSTAT,HSTAT                                                      
         BZ    EDSTAT4                                                          
         MVC   0(5,R4),ACTSTAT                                                  
         B     EDSTAT5                                                          
EDSTAT4  MVC   2(5,R4),ACTSTAT                                                  
EDSTAT5  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'0'                                                    
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         OI    HSTAT,X'1'                                                       
         B     EDSTAT1                                                          
         EJECT                                                                  
EDMLC    XC    CMBCOUNT,CMBCOUNT   INIT COMBO COUNTER                           
         CLI   ELEMLEN,0           COMBO NAME AT LEAST 1                        
         BZ    EDITML1                                                          
         CLI   ELEMLEN,10                                                       
         BH    NEEDSCBN                                                         
         OC    MKTNUM,MKTNUM       NEED A MARKET                                
         BNZ   *+14                                                             
         OC    PARAS(4),PARAS      FROM SOMEPLACE                               
         BZ    EDITML1                                                          
         CLI   ELEMSTOP,C'='                                                    
         BE    EDMLC1                                                           
*--------------------------------------------------------------------           
* HERE'S WHERE I HAVE TO CHECK FOR A COMBOLIST.                                 
* IF IT IS THEN ADD IN ALL THE COMBO'S IN THAT RECORD                           
* IF NOT, THEN IT IS A REGULAR COMBO                                            
*--------------------------------------------------------------------           
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         MVC   CLKBTYP,BOOKTYPE                                                 
         MVC   CLKRSRV,SRCTYPE                                                  
         MVC   CLKNAME,=CL5' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                FOR '+'                                      
         LTR   RF,RF                                                            
         BZ    EDMLC1              STANDARD COMBO                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CLKNAME(0),ELEMDATA+1   COMBO NAME                               
         MVC   TEMPNAME,CLKNAME                                                 
         MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
         GOTO1 HIGH                SEARCH FOR IT                                
         CLC   KEY(13),KEYSAVE     DO WE HAVE IT?                               
         BNE   EDMLC0              NO, STANDARD COMBO                           
         GOTO1 GETREC              YES, GET THE RECORD                          
         B     EDCMB                                                            
*                                                                               
EDMLC0   XC    KEY,KEY                                                          
         MVI   SUBSID,C'N'                                                      
         GOTO1 =A(CKSUBSID),DMCB,(R9),(RC),RR=RELO                              
         BNZ   EDMLC01                                                          
         MVI   SUBSID,C'Y'                                                      
*                                                                               
EDMLC01  XC    KEY,KEY                                                          
         MVI   GOTMATCH,C'N'                                                    
         USING CLKEY,R5                                                         
         MVC   CLKTYPE(2),=X'0D5E'                                              
         MVC   CLKAM,BAGYMD                                                     
         CLI   SUBSID,C'Y'         IS IT A SUBSID OF INTEREP?                   
         BNE   AGY00002                                                         
         MVI   CLKAM,X'B2'         MAKE SURE WE USE INTEREP                     
AGY00002 MVC   CLKMKAL,CITYCODE    ALPHA CODE                                   
         GOTO1 HIGH                SEARCH FOR IT                                
*                                                                               
EDMLC0A  CLC   KEY(6),KEYSAVE      FIRST PART MATCHING?                         
         BE    EDMLC0B                                                          
         CLI   GOTMATCH,C'Y'       DID WE GET SOMETHING CLOSE?                  
         BE    EDCMB               YES, LEAVE LOOP AND CONTINUE                 
         B     EDMLCERR                                                         
*                                                                               
EDMLC0B  CLC   CLKNAME,TEMPNAME    ARE THE COMBO NAMES EQUAL?                   
         BNE   EDMLC0X                                                          
         MVI   GOTMATCH,C'Y'                                                    
         GOTO1 GETREC                                                           
         CLC   CLKBTYP,BOOKTYPE                                                 
         BNE   EDMLC0X                                                          
         B     EDCMB                                                            
EDMLC0X  GOTO1 SEQ                                                              
         B     EDMLC0A                                                          
*--------------------------------------------------------------------           
EDCMB    L     R5,AIO1                                                          
         SR    R1,R1                                                            
         BAS   RE,CKCMBLST                                                      
         BE    EDCMB_1                                                          
         LTR   R1,R1                                                            
         BZ    EDITML1C                                                         
         B     EDMLCERR                                                         
EDCMB_1  LA    R5,CLDELEM                                                       
EDCMB0   CLI   0(R5),X'00'         END OF LIST?                                 
         BE    EDCMBOK                                                          
EDCMB1   CLI   0(R5),X'01'         DESCRIPTION?                                 
         BNE   EDCMB2              NO, MAYBE AN ELEMENT                         
         USING CLDELEM,R5                                                       
EDCMB1A  ZIC   R0,CLDLEN                                                        
         AR    R5,R0               SKIP DESCRIPTIONS                            
         B     EDCMB0                                                           
EDCMB2   CLI   0(R5),X'05'         ELEMENT?                                     
         BNE   EDCMB1A             UNDEFINED                                    
         USING CLSELEM,R5                                                       
         L     R4,PARAS+12                                                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         ST    R4,PARAS+4                                                       
         ST    R4,PARAS+12                                                      
         ZIC   RF,CLSLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),CLSELEM                                                  
         MVI   0(R4),X'03'         STATION CODE                                 
         ZIC   R0,CLSLEN                                                        
         AR    R5,R0                                                            
         B     EDCMB0                                                           
EDCMB3   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     EDCMB0                                                           
EDCMBOK  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,C','       MORE COMBOS?                                 
         BE    EDMLC               YES, GO BACK                                 
         CLI   ELEMSTOP,X'FF'                                                   
         BE    EQXIT2                                                           
         CLI   ELEMSTOP,X'00'                                                   
         BE    EQXIT2                                                           
         B     EDITML2                                                          
*                                                                               
EDMLCERR MVC   CONHEAD(L'CMBELMER),CMBELMER                                     
         LA    RE,CONHEAD+L'CMBELMER+1                                          
         MVC   0(5,RE),TEMPNAME                                                 
         B     ULXIT                                                            
*---------------------------------------------------------------                
EDMLC1   L     R4,PARAS+12         GET LAST ELEMENT                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVC   0(2,R4),=X'0310'    SET COMBO ELEMENT                            
* NEED EX STATMENT ON NEXT ONE                                                  
         MVC   2(9,R4),=CL9' '      SET COMBO CAPTION                           
         ZIC   RF,ELEMLEN                                                       
         LTR   RF,RF               COULD BE ZERO                                
         BZ    EDCOMB0             YES, LEAVE IT AS SPACES                      
         BCTR  RF,0                ONE FOR '+'                                  
         LTR   RF,RF               COULD BE ZERO                                
         BNZ   EDMLC1A                                                          
         ST    R2,DISP                                                          
         B     EDCOMB0             YES, LEAVE IT AS SPACES                      
EDMLC1A  BCTR  RF,0                ANOTHER FOR EX INSTRUCTION                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),ELEMDATA+1  SET COMBO CAPTION                            
EDCOMB0  BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         BE    EDITML1A                                                         
         CLI   ELEMSTOP,X'0'                                                    
         BZ    EDITML1A                                                         
         CLI   ELEMSTOP,C'='                                                    
         BE    EDITML1A                                                         
EDCOMB1  BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
*        OC    CMBCOUNT,CMBCOUNT                                                
*        BZ    EDCOMB2                                                          
*        MVC   0(5,R4),ACTSTAT                                                  
*        B     EDCOMB3                                                          
EDCOMB2  MVC   11(5,R4),ACTSTAT                                                 
EDCOMB3  ST    R4,PARAS+12                                                      
         ST    R4,PARAS+8                                                       
EDCOMB4  ZIC   RF,CMBCOUNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,CMBCOUNT                                                      
         BAS   RE,NEXTELEM                                                      
         CLI   ELEMSTOP,X'FF'                                                   
         BE    TESTEND                                                          
         CLI   ELEMSTOP,X'0'                                                    
         BE    TESTEND                                                          
         CLI   ELEMDATA,C'+'       A NEW COMBO?                                 
         BE    TESTEND             PREPARE FOR IT                               
         CLI   ELEMSTOP,C'='                                                    
         BNE   EDMLC2                                                           
TESTEND  CLI   CMBCOUNT,2                                                       
         BL    NEEDSTAL                                                         
         L     R4,PARAS+12                                                      
         CLC   2(9,R4),=CL9' '                                                  
         BNE   TESTENDX                                                         
         CLI   CMBCOUNT,2                                                       
         BE    TESTEND1                                                         
         L     R2,DISP                                                          
         B     NEEDCMBN                                                         
TESTEND1 CLC   11(4,R4),16(R4)     STATION CALL LETTERS SAME?                   
         BNE   TESTEND5                                                         
         MVC   2(4,R4),11(R4)      YES, PUT '/AF' BEHIND IT                     
         MVC   6(3,R4),=C'/AF'                                                  
         B     TESTENDX                                                         
TESTEND5 MVC   2(4,R4),11(R4)      NO, SEPARATE WITH '/'                        
         MVI   6(R4),C'/'                                                       
         MVC   7(4,R4),16(R4)                                                   
TESTENDX CLI   ELEMSTOP,C'='                                                    
         BE    EDITML2                                                          
         CLI   ELEMDATA,C'+'                                                    
         BE    EDITML2                                                          
         B     EQXIT2                                                           
EDMLC2   ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         CLI   DMCB,0                                                           
         BZ    NEEDMRKT                                                         
         L     R4,PARAS+12                                                      
         ZIC   R1,1(R4)                                                         
         CLI   1(R4),46                                                         
         BL    EDMLC3                                                           
         B     NEEDMRKT                                                         
EDMLC3   LR    R0,R1                                                            
         LA    R1,5(R1)                                                         
         STC   R1,1(R4)                                                         
         AR    R4,R0                                                            
         BAS   RE,EDMVSTA                                                       
         BNE   EDITML1A                                                         
         MVC   0(5,R4),ACTSTAT                                                  
*        DC    H'0'                                                             
         B     EDCOMB4                                                          
         SPACE 3                                                                
TESTMRKT BAS   RE,NEXTELEM         NEXT ELEMENT                                 
         CLI   ELEMSTOP,X'FF'      END OF LIST?                                 
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,X'0'       SHORTENED LIST?                              
         BE    NEEDMRKT            YES, NEED MARKET ERROR                       
         CLI   ELEMSTOP,C'='                                                    
         BE    NEEDMRKT                                                         
         CLI   ELEMLEN,0           NOTHING IN DATA                              
         BZ    NEEDMRKT                                                         
         ZIC   RF,ELEMLEN                                                       
         LA    R3,ELEMDATA                                                      
         GOTO1 VNUMVAL,DMCB,(R3),(RF)                                           
         ZIC   RF,DMCB                                                          
         LTR   RF,RF               SEE IF INVALID                               
         BNZ   NOTYET2             ALPHA                                        
         MVC   TEMPMRKT(2),DMCB+6                                               
         LH    R1,TEMPMRKT                                                      
         B     TESTMXIT                                                         
NOTYET2  ZIC   RF,ELEMLEN          COULD BE ALPHA MARKET                        
         CH    RF,=H'3'            DATA LONGER THAN 3 CHAR                      
         BH    NOTYET3             YES, BUT COULD BE STATION LIST               
         BCTR  RF,0                                                             
         MVC   CITYCODE,=CL3' '                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),ELEMDATA                                             
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NEEDMRKT                                                         
         STH   R1,TEMPMRKT                                                      
         B     TESTMXIT                                                         
*---------------------------------------------------------------                
NOTYET3  LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKEY,R5                                                         
         MVC   SLKTYPE(2),=X'0D5B'                                              
         MVC   SLKAM,BAGYMD                                                     
         MVC   SLKNAME,=CL8' '                                                  
         ZIC   RF,ELEMLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SLKNAME(0),ELEMDATA                                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEEDMRKT            MARKET NOT A STATION LIST                    
         GOTO1 GETREC                                                           
         L     R5,AIO1                                                          
         LA    R5,SLDELEM                                                       
         USING SLDELEM,R5                                                       
         MVC   TEMPMRKT,SLDMKTA                                                 
         LH    R1,TEMPMRKT                                                      
         CLI   CTRY,C'C'                                                        
         BE    TESTMXIT                                                         
         CLI   TRESRCE,C'A'                                                     
         BE    TESTMXIT                                                         
         MVC   TEMPMRKT,SLDMKTB                                                 
         LH    R1,TEMPMRKT                                                      
         SPACE 1                                                                
TESTMXIT SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XIT1  REGS=(R1)                                                        
         SPACE 3                                                                
OLDADDSP DC    XL3'0'                                                           
NEXTELEM NTR1                      GETS NEXT ELEMENT OF PARSED LIST             
         SPACE 1                                                                
         MVC   OLDADDSP(3),2(R2)                                                
         CLI   0(R2),X'FF'         DON'T GET NEXT ELEMENT                       
         BE    NXTELMER            IF END OF LIST OR SHORTENED LIST             
         CLI   0(R2),X'0'                                                       
         BZ    NXTELMER                                                         
         ZIC   RF,1(R2)                                                         
         LA    RF,ELEMBDAT(RF)                                                  
         AR    R2,RF                                                            
         CR    R2,R2                                                            
         B     NXTELMXT                                                         
NXTELMER LA    R1,1                                                             
         LTR   R1,R1                                                            
NXTELMXT XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
EDMVSTA  NTR1                                                                   
         MVC   THISLINE(8),TREUL1H                                              
         MVC   THISLINE+8(7),=CL7' '                                            
         ZIC   RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   EDMVSTA1                                                         
         CLI   1(R2),1             CAUSE AN ERROR CODE                          
         B     XIT2                                                             
EDMVSTA1 BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   THISLINE+8(0),ELEMBDAT(R2)                                       
         MVI   THISLINE+4,X'C0'                                                 
         MVC   THISLINE+5(1),1(R2)                                              
*                                                                               
         LA    R2,THISLINE                                                      
         GOTO1 VSUBR07,DMCB,('VALDSTAE',(RC))                                   
         B     XIT2                                                             
         SPACE 1                                                                
ULXIT    CLI   0(R2),X'FF'                                                      
         BE    ULXIT1                                                           
         CLI   0(R2),X'0'                                                       
         BNZ   ULXIT2                                                           
ULXIT1   MVC   2(3,R2),OLDADDSP    INSURE CURSOR POINTS SOMEWHERE               
ULXIT2   LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
EDITOPT  MVI   BOXOPT,C'Y'                                                      
         MVI   TRACEOPT,C'N'                                                    
         MVI   RANKOPT,C'Y'                                                     
         MVI   FORMOPT,C'N'                                                     
         MVI   WORSTOPT,C'N'                                                    
         MVI   SPILLOPT,0                                                       
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   EXPANOPT,C'N'                                                    
         MVI   SIMULOPT,C'Y'                                                    
         MVI   AVEOPT,C'Y'                                                      
         MVI   CALCOPT,C'Y'                                                     
         CLI   CTRY,C'C'                                                        
         BNE   *+8                                                              
         MVI   CALCOPT,C'N'                                                     
         MVI   ADDROPT,C'Y'                                                     
         MVI   LONGOPT,C'N'                                                     
         MVI   AUTOOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    EQXIT2                                                           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT4                                                             
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT6                                                             
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT6     CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT8                                                             
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE 1                                                                
OPT8     CLC   12(5,R4),=C'TRACE'  OPTION TO TRACE DEMAND HOOKS                 
         BNE   OPT10                                                            
         MVI   TRACEOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT10    CLC   12(4,R4),=C'RANK'   OPTION TO PRINT RANK NUMBERS                 
         BNE   OPT12                                                            
         MVC   RANKOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT12    CLC   12(4,R4),=C'FORMAT' OPTION TO PRINT STATION FORMAT               
         BNE   OPT14                                                            
         MVI   FORMOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT14    CLC   12(5,R4),=C'WORST'  OPTION TO SHOW WORST STATIONS!               
         BNE   OPT16                                                            
         MVI   WORSTOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT16    CLC   12(5,R4),=C'SPILL'  OPTION TO SHOW SPILL ONLY                    
         BNE   OPT18                                                            
         MVI   SPILLOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT18    CLC   12(5,R4),=C'NOSPILL'       TO SUPPRESS SPILL                     
         BNE   OPT20                                                            
         MVI   SPILLOPT,C'N'                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT20    CLC   12(4,R4),=C'EXPA'   OPTION TO EXPAND COMBOS                      
         BNE   OPT22                 (SHOW INDIV STATIONS + COMBOS)             
         MVC   EXPANOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT22    CLC   12(4,R4),=C'SIMU'   OPTION TO SUPPRESS SIMULCASTS                
         BNE   OPT24                                                            
         MVC   SIMULOPT,22(R4)                                                  
         B     OPTEND                                                           
         SPACE 1                                                                
OPT24    CLC   12(3,R4),=C'AVE'    OPTION TO SUPPRESS AVERAGE COLS              
         BNE   OPT25                                                            
         MVC   AVEOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE 1                                                                
OPT25    CLC   12(4,R4),=C'CALC'   OPTION TO SUPPRESS AVERAGE COLS              
         BNE   OPT26                                                            
         CLC   22(3,R4),=C'BOOK'                                                
         BE    OPT25A                                                           
         B     OPTEND                                                           
OPT25A   MVI   CALCOPT,C'N'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT26    CLC   12(4,R4),=C'ADDR'   OPTION TO SUPPRESS ADDRESS                   
         BNE   OPT27                                                            
         MVC   ADDROPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT27    CLC   12(4,R4),=C'LONG'   OPTION TO SUPPRESS ADDRESS                   
         BNE   OPT28                                                            
         MVC   LONGOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE 1                                                                
OPT28    CLC   12(4,R4),=C'AUTO'   OPTION TO SUPPRESS ADDRESS                   
         BNE   OPT30                                                            
         CLC   =C'DELETE',22(R4)                                                
         BNE   OPTEND                                                           
         MVI   AUTOOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT30    DS    0H                                                               
         SPACE 1                                                                
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     NEQXIT2                                                          
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     EQXIT2                                                           
*                                                                               
OPTERR   DC    C'** ERROR ** INVALID OPTION'                                    
         EJECT                                                                  
         SPACE 2                                                                
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*  USING DEMO FILE STATIONS AND MARKETS                                         
         SPACE 1                                                                
VALDSTA  XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    EQXIT2                                                           
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BO    BADSTA              YES - INPUT IS NOT VALID                     
*                                                                               
         CLI   0(R4),5             MUST BE STAT+BAND                            
         BNE   *+14                                                             
         MVC   ACTSTAT(5),12(R4)                                                
         B     DSTA10                                                           
*                                                                               
         CLI   0(R4),3                                                          
         BL    BADSTA                                                           
         CLI   0(R4),4                                                          
         BH    BADSTA                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    BADSTA                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   DBSELMED,C'R'       TEST RADIO                                   
         BE    DSTA6                                                            
         CLI   1(R4),0             TEST MEDIA ENTERED                           
         BNE   DSTA4                                                            
         MVI   ACTSTAT+4,C'T'                                                   
         B     DSTA10                                                           
*                                                                               
DSTA4    MVC   ACTSTAT+4(1),22(R4)                                              
         SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'TV' *EXECUTED*                                       
         BE    DSTA10                                                           
         B     BADSTA                                                           
         SPACE 2                                                                
DSTA6    CLI   1(R4),2                                                          
         BH    BADSTA                                                           
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,DSTAAM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAFM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTACO                                                        
         BE    DSTA10                                                           
         B     BADSTA                                                           
DSTAAM   CLC   22(0,R4),=C'AM'                                                  
DSTAFM   CLC   22(0,R4),=C'FM'                                                  
DSTACO   CLC   22(0,R4),=C'CO'                                                  
         SPACE 1                                                                
* READ STATION RECORD                                                           
*                                                                               
DSTA10   MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         CLI   NBOOKS,1            IS THERE ONLY 1 BOOK?                        
         BH    DSTA13              NO, USE THE HIGHEST BOOK FOR VAL.            
         MVC   DBSELBK,BOOKS+1                                                  
         MVC   DBBTYPE,BOOKS+3                                                  
         B     DSTA15                                                           
DSTA13   ZIC   RE,NBOOKS           GET THE HIGHEST BOOK                         
         BCTR  RE,0                                                             
         LA    R1,BOOKS                                                         
         LA    RF,BOOKS                                                         
DSTA14   LA    RF,4(RF)                                                         
         CLC   1(2,R1),1(RF)                                                    
         BH    DSTA14A                                                          
         LR    R1,RF                                                            
DSTA14A  BCT   RE,DSTA14                                                        
         MVC   DBSELBK,1(R1)                                                    
         MVC   DBBTYPE,3(R1)                                                    
DSTA15   MVC   DBSELSTA,ACTSTAT                                                 
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'                                                    
         BE    BADSTA                                                           
         SPACE 2                                                                
* GET MARKET NAME                                                               
         SPACE 1                                                                
DSTA20   MVC   ACTMKT,DBACTRMK     MOVE MARKET NUMBER                           
         B     EQXIT2                                                           
*                                                                               
BADSTA   MVI   DBERROR,X'10'                                                    
         B     NEQXIT2                                                          
         EJECT                                                                  
M1INIT   MVC   CONHEAD(31),=C'SELECT DESIRED MARKETS (5 MAX.)'                  
         LA    RE,BUFF                                                          
         USING SVTD,RE                                                          
         XC    BUFF(200),BUFF                                                   
         MVC   SVTSRC,DBSELSRC                                                  
         MVC   SVTBOOK,BOOKS+1                                                  
         MVC   SVTBTYP,BOOKS+3                                                  
         MVC   SVTFILE,DBFILE                                                   
         MVC   SVTMED(1),DBSELMED                                               
         MVC   SVTFLTR,TREMKT                                                   
         SPACE 1                                                                
         MVI   TWANUM,2            SAVE IN TWA 2                                
         MVC   COMAND2,=CL8'DMWRT'                                              
         GOTO1 VSUBR07,DMCB,('WRTWAE',(RC))                                     
         BNE   NEQXIT2                                                          
*                                  GET MARKET LIST SCREEN                       
         GOTO1 CALLOV,DMCB,(X'A3',CONTAGH),0,0                                  
         LA    R2,RMKMK1H                                                       
         MVC   RMKWORK(200),BUFF                                                
         B     EQXIT2                                                           
         EJECT                                                                  
*   TWA HANDLING STUFF                                                          
WRTWA    MVC   COMAND2,=C'DMWRT   '                                             
         B     TWAIO                                                            
         SPACE 1                                                                
RESTWA   MVC   COMAND2,=C'DMREAD  '                                             
         SPACE 1                                                                
TWAIO    XC    DMCB(8),DMCB                                                     
         ZIC   R2,TWANUM                                                        
         SLL   R2,32-8                                                          
         ICM   R2,3,TERM                                                        
         GOTO1 DATAMGR,DMCB,COMAND2,=C'TEMPSTR',(R2),ATWA,0                     
         CLI   8(R1),0                                                          
         BE    EQXIT2                                                           
         B     NEQXIT2                                                          
         EJECT                                                                  
*   SCREEN GENERATORS                                                           
GENDPT   SR    RF,RF               COUNT THE ACTIVE DAYPARTS                    
         L     RE,=A(STANDPTS)                                                  
         A     RE,RELO                                                          
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BE    *+16                                                             
         LA    RE,L'STANDENT(RE)                                                
         LA    RF,1(RF)                                                         
         B     *-16                                                             
         STC   RF,BYTE             SAVE NUMBER OF DAYPARTS                      
         L     RE,AIO2             AREA FOR TWABLD LIST                         
         LA    RF,1000                                                          
         XCEF                                                                   
         ZIC   R0,BYTE                                                          
         SR    R5,R5                                                            
         SR    R3,R3                                                            
GENDPT3  CH    R3,=H'3'            3 GROUPS PER LINE                            
         BNE   *+6                                                              
         SR    R3,R3                                                            
         LR    R4,R3               SET INDEX                                    
         SLL   R4,1                *2                                           
         LA    R4,DPDISP(R4)                                                    
         MVC   DPL1+3(1),0(R4)     POS FIELD 1                                  
         MVC   DPL2+3(1),1(R4)     POS FIELD 2                                  
         LR    R4,R5               RESET INDEX FOR NAME                         
         MH    R4,=AL2(L'STANDENT)                                              
         A     R4,=A(STANDPTS)     POINT TO NAME                                
         A     R4,RELO                                                          
         USING STAND,R4                                                         
         MVC   DPL2C,STANDESC      AND MOVE IT IN                               
         DROP  R4                                                               
         LA    R4,DPL1             SCREEN LINE 1                                
         PRINT GEN                                                              
GENDPT4  GOTO1 HELLO,DMCB,(C'P',=C'CORETAB'),AIO2,(R4),=C'ADD=END'              
         PRINT NOGEN                                                            
         CLI   16(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,1(R4)            NEXT FIELD                                   
         AR    R4,RE                                                            
         CLI   0(R4),X'FF'         END OF PAIRS                                 
         BNE   GENDPT4                                                          
         LA    R5,1(R5)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,GENDPT3          DO IT FOR NUMBER OF ENTRIES                  
         L     R4,AIO2                                                          
         LA    R4,2(R4)                                                         
         GOTO1 VTWABLD,DMCB,ATWA,(R4),CONTAGH,2304                              
         B     EQXIT2                                                           
         EJECT                                                                  
EDITMB   LA    R4,BOOKS            CHECK MARKET FOR ALL BOOKS                   
         CLI   DBSELSRC,C'M'       BBM RADIO DOESN'T HAVE MKT LEVELS            
         BE    EQXIT2                                                           
         SPACE 1                                                                
EDITMB2  CLI   1(R4),0             EOL                                          
         BE    EQXIT2                                                           
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         MVC   DBSELBK,1(R4)                                                    
         MVC   DBBTYPE,3(R4)                                                    
         LH    R0,MKTNUM           SET STATION = MARKET                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'                                                  
         MVC   DBSELMK,MKTNUM      AND MARKET IN KEY                            
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'       NOT FOUND --ERROR                            
         BE    NEQXIT2                                                          
         LA    R4,4(R4)            NEXT BOOK                                    
         B     EDITMB2                                                          
         SPACE 2                                                                
EDITSB   LA    R4,BOOKS            CHECK STATION OR ALL BOOKS                   
         CLI   DBSELSRC,C'M'       BBM RADIO DOESN'T HAVE MKT LEVELS            
         BE    EQXIT2                                                           
         SPACE 1                                                                
EDITSB2  CLI   1(R4),0             EOL                                          
         BE    EQXIT2                                                           
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLSTBK                                                 
         MVC   DBSELSTA,ACTSTAT    SET STATION                                  
         MVC   DBSELBK,1(R4)       BOOK                                         
         MVC   DBBTYPE,3(R4)       BOOK TYPE                                    
         MVC   DBSELMK,MKTNUM      AND MARKET IN KEY                            
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'       NOT FOUND --ERROR                            
         BE    NEQXIT2                                                          
         LA    R4,4(R4)            NEXT BOOK                                    
         B     EDITSB2                                                          
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES                        
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
         SPACE 1                                                                
         PRINT NOGEN                                                            
SETUNIVS XC    UNIVS(32),UNIVS     CLEAR UNIVS AND RUNIVS                       
         XC    ACTUNIVS(64),ACTUNIVS     CLEAR ACTUAL UNIVERSES                 
*                                                                               
         LA    R6,BOOKS                                                         
         ZIC   R7,NBOOKS                                                        
         LA    R5,ACTUNIVS                                                      
*                                                                               
SETUNV1  MVC   DBSELBK,1(R6)                                                    
         MVC   DBBTYPE,3(R6)                                                    
         MVI   DBSELMED,C'R'                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELMK,MKTNUM                                                   
         XC    THISDEM(12),THISDEM                                              
         MVI   DBSELSTA+4,C'A'                                                  
         EDIT  (2,MKTNUM),(4,DBSELSTA)                                          
*                                                                               
         LA    R2,4                                                             
         LA    R3,DBSELSTA                                                      
SETUNV2  CLI   0(R3),C' '                                                       
         BNE   SETUNV3                                                          
         MVI   0(R3),C'0'                                                       
SETUNV3  LA    R3,1(R3)                                                         
         BCT   R2,SETUNV2                                                       
*                                                                               
         MVC   DBFILE,=C'RDP'                                                   
         MVI   DBSELDAY,X'7C'                                                   
         XC    DBSELTIM,DBSELTIM                                                
         MVI   DBSELPRG+1,1                                                     
         MVC   DBAREC,AIO1                                                      
         MVC   MSASAVE(4),=C'MSA='                                              
*                                                                               
         CLI   DBSELSRC,C'M'       FIX UP FOR BBM                               
         BNE   *+14                                                             
         MVC   MSASAVE(4),=C'CMA='                                              
         B     *+16                                                             
*                                                                               
         MVC   TSASAVE(4),=C'TSA='                                              
         MVC   ADISAVE(4),=C'ADI='                                              
         SPACE 1                                                                
         MVC   UNIVLIST,UNIVCATS                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         GOTO1 DEMAND,DMCB,DBLOCK,UNVHOOK                                       
         SPACE 1                                                                
         BAS   RE,REALUNIV                                                      
         LA    R6,4(R6)            NEXT BOOK                                    
         MVC   0(16,R5),UNIVS      COPY ACTUAL UNIVERSE                         
         LA    R5,16(R5)           NEXT ACTUAL UNIVERSE                         
         BCT   R7,SETUNV1                                                       
SETUXIT  BAS   RE,AVEUNIV                                                       
*                                                                               
         LA    R2,DEMOS                                                         
         ZIC   RE,0(R2)                                                         
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,UNIVS(RE)                                                     
         BNZ   SETANYS                                                          
         XC    UNIVBK,UNIVBK                                                    
         MVI   ANYUNIV,C'N'                                                     
SETANYS  CLI   0(R2),1                                                          
         BNE   *+8                                                              
         MVI   ANYMSA,C'Y'                                                      
         CLI   0(R2),2                                                          
         BNE   *+8                                                              
         MVI   ANYTSA,C'Y'                                                      
         CLI   0(R2),3                                                          
         BNE   *+8                                                              
         MVI   ANYADI,C'Y'                                                      
*                                                                               
         B     EQXIT2                                                           
         SPACE 1                                                                
UNIVCATS DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
         DC    X'FF'                                                            
*                                                                               
UNVHOOK  NTR1                                                                   
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
         LM    R2,R4,UNIVS                                                      
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
         B     XIT2                                                             
*                                                                               
REALUNIV NTR1                                                                   
         LA    R0,4                                                             
         LA    R2,UNIVS                                                         
         LA    R3,RUNIVS                                                        
REALUNV1 L     R5,0(R2)                                                         
         A     R5,0(R3)                                                         
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,REALUNV1                                                      
         B     XIT2                                                             
         SPACE 3                                                                
AVEUNIV  NTR1                                                                   
         LA    R0,4                                                             
         ZIC   R5,NBOOKS                                                        
         LA    R7,RUNIVS                                                        
AVEUNILP SR    R2,R2                                                            
         L     R3,0(R7)                                                         
         DR    R2,R5                                                            
         ST    R3,0(R7)                                                         
         LR    R3,R2                                                            
         SR    R2,R2                                                            
         LA    R5,10                                                            
         MR    R2,R5                                                            
         ZIC   R5,NBOOKS                                                        
         DR    R2,R5                                                            
         CH    R3,=H'5'                                                         
         BL    AVENXTLP                                                         
         L     R3,0(R7)                                                         
         LA    R3,1(R3)                                                         
         ST    R3,0(R7)                                                         
AVENXTLP LA    R7,4(R7)                                                         
         BCT   R0,AVEUNILP                                                      
         B     XIT2                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES   (OLD WAY)            
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
         SPACE 1                                                                
         PRINT GEN                                                              
         PRINT NOGEN                                                            
SETUNIBS CLC   DBSELBK,UNIVBK                                                   
         BNH   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         CLI   ANYUNIV,C'Y'                                                     
         BE    SETUBX                                                           
         MVC   UNIVBK,DBSELBK                                                   
         MVI   ANYUNIV,C'Y'                                                     
         MVC   MSASAVE(4),=C'MSA='                                              
*                                                                               
         CLI   DBSELSRC,C'M'       FIX UP FOR BBM                               
         BNE   *+14                                                             
         MVC   MSASAVE(4),=C'CMA='                                              
         B     *+16                                                             
*                                                                               
         MVC   TSASAVE(4),=C'TSA='                                              
         MVC   ADISAVE(4),=C'ADI='                                              
         SPACE 1                                                                
         MVC   UNIVLIST,UNIVCATB                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
         LM    R2,R4,UNIVS                                                      
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
*        ZIC   RE,0(R2)           IF NO UNIVERSE THEN FORCE TO TRY              
*        BCTR  RE,0               AGAIN. SOME STATIONS ARE IN A MARKET          
*        SLL   RE,2               BUT NOT ALL AREAS SO .....                    
*        LA    RE,UNIVS(RE)       THERE MAY BE A UNIVERSE ON THE                
*        OC    0(4,RE),0(RE)      NEXT STATION                                  
*        BNZ   SETANYSB                                                         
*        XC    UNIVBK,UNIVBK                                                    
*        MVI   ANYUNIV,C'N'                                                     
         SPACE 1                                                                
SETANYSB CLI   0(R2),1             CHECK TO SEE WHICH AREAS WERE                
         BNE   NOMSAUB             REQUESTED. THIS AFFECTS UNIV DISPLAY         
         MVI   ANYMSA,C'Y'                                                      
         OC    UNIVS(4),UNIVS                                                   
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOMSAUB  CLI   0(R2),2                                                          
         BNE   NOTSAUB                                                          
         MVI   ANYTSA,C'Y'                                                      
         OC    UNIVS+4(4),UNIVS+4                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOTSAUB  CLI   0(R2),3                                                          
         BNE   NOADIUB                                                          
         MVI   ANYADI,C'Y'                                                      
         OC    UNIVS+8(4),UNIVS+8                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOADIUB  LA    R2,3(R2)                                                         
         BCT   R0,SETANYSB                                                      
SETUBX   B     EQXIT2                                                           
         SPACE 1                                                                
UNIVCATB DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SETNEWUN L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LM    R2,R4,RUNIVS                                                     
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELMED,C'M'       BBM FIXES                                    
         BE    SETNEWU3                                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
SETNEWU3 B     EQXIT2                                                           
         EJECT                                                                  
*------------------------------------------------------------------             
* GOTTAMKT, SAVE MARKET NUMBER FOR EDMLM                                        
*------------------------------------------------------------------             
GOTTAMKT NTR1                                                                   
         L     R4,PARAS+12                                                      
         ZIC   R3,1(R4)                                                         
         AR    R4,R3                                                            
         SR    R3,R3                                                            
         XC    PARAS(16),PARAS                                                  
         ST    R4,PARAS                                                         
         ST    R4,PARAS+12                                                      
         MVC   0(2,R4),=X'0104'                                                 
         LH    R1,TEMPMRKT                                                      
         STCM  R1,3,2(R4)                                                       
         MVC   DBSELMK,TEMPMRKT                                                 
         OC    MKTNUM,MKTNUM                                                    
         BNZ   GOTTAMXT                                                         
         MVC   MKTNUM,TEMPMRKT                                                  
GOTTAMXT CLC   TREBOOK+1(4),=C'BOOK'                                            
         BNE   XIT2                                                             
         BAS   RE,FMULTBK                                                       
         B     XIT2                                                             
         EJECT                                                                  
FMULTBK  NTR1                                                                   
         MVI   NBOOKS,0                                                         
*        MVC   BADMULP(1),TREBOOK                                               
         XC    BOOKS,BOOKS                                                      
         LH    RE,TEMPMRKT         NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,TEMPMRKT                                                
*                                                                               
         LA    R1,1                                                             
         CLC   TREBOOK+1(4),=C'BOOK'                                            
         BNE   XIT2                                                             
         MVC   BYTE(1),TREBOOK     SET NUMBER OF BOOKS                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BH    FMULTBKX                                                         
         CLI   TREBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,TREBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOKF                                       
         LA    R1,1                                                             
         CLI   NBOOKS,0            NOT FOUND                                    
         BE    FMULTBKX                                                         
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         CLC   NBOOKS,BYTE         NOT ENOUGH BOOKS                             
         BNE   FMULTBKX                                                         
*                                                                               
         ZIC   R0,BYTE             DON'T ALLOW PREV. YEAR                       
         LA    RF,BOOKS                                                         
FMULTBK2 ZIC   R1,2(RF)            GET THE BOOK                                 
         LA    R1,WORK(R1)                                                      
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,2                SET ERROR NUMBER                             
         B     FMULTBKX                                                         
*                                                                               
         MVI   0(R1),C'0'                                                       
         LA    RF,4(RF)                                                         
         BCT   R0,FMULTBK2                                                      
*                                                                               
         SR    R1,R1                                                            
FMULTBKX LTR   R1,R1                                                            
         STC   R1,BYTE                                                          
         B     XIT2                                                             
*                                                                               
BKHOOKF  NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT2                                                             
         CLC   SBBTYP,DBBTYPE                                                   
         BNE   XIT2                                                             
         ZIC   R1,NBOOKS                                                        
         LA    R1,1(R1)                                                         
         CLC   NBOOKS,BYTE         BOOK TABLE FULL - NEED A SLIDE               
         BNE   BKHOOK2F                                                         
         MVC   BOOKS(L'BOOKS-4),BOOKS+4                                         
         IC    R1,NBOOKS                                                        
*                                                                               
BKHOOK2F STC   R1,NBOOKS                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                *4                                           
         LA    R1,BOOKS(R1)                                                     
         MVC   1(2,R1),SBBOOK                                                   
         MVC   3(1,R1),SBBTYP                                                   
         B     XIT2                                                             
         EJECT                                                                  
CKCMBLST NTR1                                                                   
         USING CLKEY,R5                                                         
         MVI   ACTELOPT,C'N'                                                    
CKCMB0   LA    R5,CLDELEM                                                       
         MVI   NUMCMBS,0           CLEAR # OF COMBOS                            
CKCMB1   CLI   0(R5),0             END OF LIST?                                 
         BE    CKCMB6                                                           
CKCMB2   CLI   0(R5),1             DESCRIPTION ELEMENT?                         
         BNE   CKCMB3                                                           
         USING CLDELEM,R5                                                       
CKCMB2A  ZIC   R0,CLDLEN                                                        
         AR    R5,R0                                                            
         B     CKCMB1                                                           
CKCMB3   CLI   0(R5),X'05'                                                      
         BNE   CKCMB2A                                                          
*                                                                               
         AI    NUMCMBS,1           INCREMENT # OF COMBOS                        
         MVI   NUMSTAT,0           CLEAR # OF STATIONS                          
         MVI   CHNGLST,C'N'        COMBO LIST DOESN'T NEED CHANGES              
         MVI   CHNGLSTC,11         MINIMUM W/O STATIONS                         
         MVC   NWCMBLST,0(R5)      SAVE OLD ELEM KEY                            
         XC    NEWCLIST,NEWCLIST   CLEAR NEW COMBO LIST                         
         LA    R6,NEWCLIST                                                      
*                                                                               
         USING CLSELEM,R5                                                       
         ZIC   R4,CLSLEN                                                        
         AR    R4,R5               LIMIT OF ELEMENT                             
         LA    R3,CLSTALST         FIRST STATION IN CMBLIST                     
CKCMB4   MVC   THISLINE(8),TREUL1H    MAKE UP A FAKE FIELD HEADER               
         MVC   THISLINE+8(5),0(R3)                                              
         MVI   THISLINE+5,5        LENGTH OF STATION                            
         MVI   THISLINE+4,X'C0'                                                 
         LA    R2,THISLINE                                                      
         GOTO1 VSUBR07,DMCB,('VALDSTAE',(RC))                                   
         BNE   CKCMB5              A STATION IS INVALID                         
         AI    NUMSTAT,1           INCREMENT # OF STATIONS                      
         MVC   0(5,R6),0(R3)       SAVE VALID STATION                           
         LA    R6,5(R6)            WIDTH OF STATION                             
         ZIC   R0,CHNGLSTC                                                      
         AH    R0,=H'5'                                                         
         STC   R0,CHNGLSTC                                                      
CKCMB4A  LA    R3,L'CLSTALST(R3)   GET NEXT STATION                             
         CR    R3,R4                                                            
         BL    CKCMB4                                                           
         CLI   CHNGLST,C'Y'        WAS COMBO LIST CHANGED?                      
         BNE   CKCMB4B             NO, CONTINUE                                 
         GOTO1 GETREC                                                           
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFIL'),(X'05',AIO1),(9,NWCMBLST+2)C        
               ,0                                                               
         CLI   NUMSTAT,1                                                        
         BNH   CKCMB4A5                                                         
         MVC   NWCMBLST+1(1),CHNGLSTC   COPY NEW LENGTH                         
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),AIO1,NWCMBLST,0,0,0                 
CKCMB4A5 GOTO1 PUTREC                                                           
         L     R5,AIO1                                                          
         B     CKCMB0                                                           
CKCMB4B  LR    R5,R4                                                            
         B     CKCMB1              KEEP DOING UNTIL LIST FINISHED               
*                                                                               
CKCMB5   CLI   PFKEY,4                                                          
         BE    CKCMB5A                                                          
         CLI   AUTOOPT,C'Y'                                                     
         BNE   NEQXIT2                                                          
* PUSH THE REST OF THE LIST TO THE LEFT                                         
CKCMB5A  MVI   CHNGLST,C'Y'        LIST WAS CHANGED                             
         B     CKCMB4A                                                          
*                                                                               
CKCMB6   DS    0H                                                               
* HAVE TO CHECK IF THERE WERE ANY COMBOS                                        
         CLI   NUMCMBS,0                                                        
         BNZ   EQXIT2                                                           
         MVI   RDUPDATE,C'Y'       MAKE SURE IT'S DELETED                       
         GOTO1 GETREC              NO COMBOS, HAVE TO DELETE IT                 
         L     R5,AIO1                                                          
         USING CLKEY,R5                                                         
         OI    CLRCNTL,X'80'                                                    
         GOTO1 PUTREC                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R5)                                                    
         MVI   RDUPDATE,C'Y'       MAKE SURE KEY IS DELETED                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
         LA    R1,1                THIS IS WHEN CMBLIST DELETED                 
         LTR   R1,R1                                                            
         XIT1  REGS=(R1)           GIVE BACK ERROR CODE                         
         SPACE 3                                                                
CHNGLST  DC    C'N'                                                             
CHNGLSTC DC    X'00'                                                            
NWCMBLST DC    XL11'00'                                                         
NEWCLIST DC    XL240'00'                                                        
NUMSTAT  DC    X'00'                                                            
NUMCMBS  DC    X'00'                                                            
         EJECT                                                                  
NEQXIT2  LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
EQXIT2   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT2     XIT1                                                                   
         SPACE 2                                                                
MAXCMB   DC    C'COMBO LIMITED TO 7 STATIONS '                                  
ERRSTA   DC    C'INVALID STATION FOR BOOK OR MARKET '                           
ERRCBL   DC    C'COMBO LIST STATION(S) INVALID, PRESS PF4 FOR AUTO-DELEX        
               TION'                                                            
NEEDKW   DC    C'KEYWORD MISSING'                                               
NEEDMKT  DC    C'MARKET MISSING'                                                
NEEDSL   DC    C'STATION LIST REQUIRED'                                         
TEMPNAME DC    CL5' '                                                           
CMBELMER DC    C'COULD NOT FIND COMBO ELEMENT'                                  
GOTMATCH DC    C' '                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         SPACE 3                                                                
*   DAYPART SCREEN GENERATOR STUFF                                              
DPDISP   DC    AL1(1,3)            FIRST                                        
         DC    AL1(25,27)          SECOND                                       
         DC    AL1(52,54)          THIRD                                        
         SPACE 1                                                                
DPL1     DC    AL1(3,7,0,1,1,INTNOR,0)     SELECT FIELD                         
DPL2     DC    AL1(3,7+20,0,3,20,PROT,0)   NAME FIELD                           
DPL2C    DC    CL20' '                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
SUBR07X  EQU   *                                                                
         EJECT                                                                  
*--------------------------------------------------------------*                
* CKSUBSID: CHECK IF CURRENT AGENCY IS SUBSIDARY OF INTEREP    *                
*--------------------------------------------------------------*                
CKSUBSID NMOD1 0,**CKSUBSID**                                                   
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         BAS   RE,SWTCHREP                                                      
         BNZ   CKSBSDER                                                         
         LA    RE,KEY                                                           
         USING RREPKEY,RE                                                       
         MVI   RREPKTYP,1                                                       
         MVC   RREPKREP,=C'IR'     INTEREP                                      
         DROP  RE                                                               
         BAS   RE,REPHIGH                                                       
         BNZ   CKSBSDER                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CKSBSDER                                                         
         L     R6,AIO1                                                          
         BAS   RE,REPGETRC                                                      
         MVC   DATADISP,=H'34'                                                  
         MVI   ELCODE,2            JUST NEED SUBSIDS                            
         BAS   RE,GETEL                                                         
         BNE   CKSBSDER            THERE HAS TO BE A 2 ELEMENT                  
         USING RREPSUB,R6                                                       
         ZIC   R0,RREPSCNT         # OF SUBSIDS                                 
         LA    R6,RREPSCOD                                                      
CKSUBS   CLC   AGENCY,0(R6)        IS AGENCY IN SUBSID LIST?                    
         BE    CKSUBXST            YES, LEAVE                                   
         LA    R6,2(R6)                                                         
         BCT   R0,CKSUBS                                                        
CKSBSDER BAS   RE,SWTCHSPT                                                      
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         XMOD1                                                                  
CKSUBXST BAS   RE,SWTCHSPT                                                      
         SR    R1,R1                                                            
         XMOD1                                                                  
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH INTO REP SYSTEM TO CHECK SUBSIDARIES OF INTEREP                        
*----------------------------------------------------------                     
SWTCHREP NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'08'          REP SYSTEM #                                 
         GOTO1 (RF),DMCB           JUMP INTO REP                                
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         BE    EQXIT2                                                           
         B     NEQXIT2             THERE WAS AN ERROR                           
         EJECT                                                                  
*----------------------------------------------------------                     
* SWITCH BACK TO SPOT SYSTEM                                                    
*----------------------------------------------------------                     
SWTCHSPT NTR1                                                                   
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
         CLI   4(R1),0             CHECK IF SYSTEM IS NOT OPEN                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EQXIT2              EVERYTHING OK                                
         EJECT                                                                  
*----------------------------------------------------------                     
* DATA MANAGER CALLS TO USE IN REP SYSTEM                                       
*----------------------------------------------------------                     
REPHIGH  MVC   COMMAND,=C'DMRDHI'                                               
         B     REPDIR                                                           
*                                                                               
REPDIR   NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERM                                                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR  ',KEYSAVE,KEY,   X        
               ((R3),0),0                                                       
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   NEQXIT2                                                          
         B     EQXIT2                                                           
*                                                                               
REPGETRC MVC   COMMAND,=C'GETREC'                                               
         NTR1                                                                   
         LA    R2,KEY+28           GET DISK ADDRESS                             
         IC    R3,TERM                                                          
         IC    R4,DMINBTS                                                       
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFIL  ',               X        
               (R2),AIO1,((R3),DMWORK),0                                        
         B     EQXIT2                                                           
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PROT     EQU   X'20'                                                            
TRAILB   EQU   X'80'                                                            
LOWCASE  EQU   X'40'                                                            
NUMBR    EQU   X'10'                                                            
INTHI    EQU   X'08'                                                            
INTLO    EQU   X'0C'                                                            
INTNOR   EQU   X'00'                                                            
MODIFY   EQU   X'01'                                                            
         SPACE 1                                                                
* COMBO SCREEN GENERATOR STUFF                                                  
T1T      DC    AL1(3,7+4,0,1,4,PROT+INTLO,0)                                    
         DC    CL4'CMB1'                                                        
T1T2     DC    AL1(3,7+4,2,0,4,PROT,0)                                          
         DC    CL4'1234'                                                        
         DC    AL1(3,7+24,0,9,24,PROT,0)                                        
         DC    CL24'MARKET NAME'                                                
T1T3     DC    AL1(3,7+11,1,0,11,PROT,0)                                        
         DC    CL11'COMBO NAME'                                                 
         DC    AL1(3,7,0,15,9,INTNOR,0)                                         
         DC    AL1(3,7+7,1,0,7,PROT,0)                                          
         DC    CL7'STATION'                                                     
         DC    AL1(3,7,0,10,7,INTNOR,0)                                         
         DC    AL1(3,7,0,20,7,INTNOR,0)                                         
         DC    AL1(3,7,0,30,7,INTNOR,0)                                         
         DC    AL1(3,7,0,40,7,INTNOR,0)                                         
         DC    AL1(3,7,0,50,7,INTNOR,0)                                         
         DC    AL1(3,7,0,60,7,INTNOR,0)                                         
         DC    AL1(3,7,0,70,7,INTNOR,0)                                         
         EJECT                                                                  
ELEMDS   DSECT                                                                  
ELEMSTOP DS    C                                                                
ELEMLEN  DS    X                                                                
ELEMADDR DS    XL2                                                              
ELEMDISP DS    X                                                                
ELEMBDAT EQU   *-ELEMSTOP                                                       
ELEMDATA DS    0C                                                               
         SPACE 3                                                                
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
STAND    DSECT                                                                  
STANDENT DS    0CL36                                                            
STANDDTS DS    XL15                UP TO 15 DAY/HOUR/HOUR                       
STANDPRG DS    XL1                 'PROGRAM' CODE                               
STANDESC DS    CL20                DESCRIPTION                                  
         SPACE 2                                                                
SVTD     DSECT TWA SAVE DESCRIPTION                                             
SVTKMKT  DS    CL12                PREV MARKET KEY                              
SVTSRC   DS    CL1                 SOURCE                                       
SVTBOOK  DS    CL2                 BOOK                                         
SVTBTYP  DS    C                   BOOK TYPE                                    
SVTFILE  DS    CL3                 DBFILE                                       
SVTMED   DS    C                   MEDIA                                        
SVTFLTR  DS    CL8                 MARKET FILTER                                
SVTMKTS  DS    13CL3               ROOM FOR 13 MARKETS                          
         DS    C                   TERMINATOR FOR MARKET LIST                   
         EJECT                                                                  
*              BUFFER ARRANGEMENT                                               
         SPACE 3                                                                
*                                  THIS IS CONTENTS OF BUFF                     
         SPACE 1                                                                
*                                  UP TO 100 10 BYTE STATION ENTRIES            
         SPACE 1                                                                
*              DSECT TO COVER STATION ENTRY                                     
         SPACE 1                                                                
STATENTD DSECT                                                                  
STATENT  DS    0CL10                                                            
STATCALL DS    CL5                 CALL LETTERS                                 
STATFORM DS    CL4                 FORMAT                                       
         DS    CL1                                                              
         SPACE 1                                                                
*                                  THERE ARE 100 STATION ENTRIES                
*                                  EACH IS 20 BYTES                             
         SPACE 1                                                                
*                                  BUFFERS ARE FOR 1-5 BOOKS                    
*                                  PLUS A 6TH FOR AVERAGE OF BOOKS              
         SPACE 2                                                                
* *********************************************************************         
*              DSECT TO COVER RECORDS IN BUFF                                   
* *********************************************************************         
*                                                                               
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL22                                                            
BUFFSTAT DS    XL1                 STATION NUMBER                               
BUFFFLAG DS    CL1                 SPANN-NOA FLAGS                              
*                                  FOR EACH BOOK THERE WILL BE                  
BUFFDEM  DS    XL3                 DEMO                                         
BUFFRANK DS    XL1                 RANK NUMBER (X'80'= EQUAL)                   
BUFFDMQ  EQU   *-BUFFDEM           DISPLACEMENT TO NEXT DEMO                    
BUFFICM  EQU   7                   ICM MASK FOR BUFFDEM                         
         DS    XL12                MAX 4 BOOKS ALLOWED                          
******   DS    XL(BUFFDMQ)         SPARE???? (PREV PRG HAD 6BKS???)             
BUFFAVG  DS    XL(BUFFDMQ)         ONE MORE BOOK TO HOLD AVERAGE                
DEMLNQ   EQU   L'BUFFDEM           LENGTH OF DEMO                               
*                                                                               
         SPACE 3                                                                
         EJECT                                                                  
* *********************************************************************         
* INCLUDES                                                                      
* *********************************************************************         
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE SPGENCUSDP                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
       ++INCLUDE SPGENCOMBO                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDTWABLDD                                                      
MARKETD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPRESWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESA3D                                                       
         EJECT                                                                  
*              STORAGE FOR RANKER                                               
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MAXRANK  DS    XL1                 MAX STATIONS TO RANK                         
WIDEOPT  DS    XL1                 WIDTH 1-3 = 80/110/132                       
TRACEOPT DS    CL1                 Y=TRACE DEMAND HOOKS                         
RANKOPT  DS    CL1                 Y=PRINT RANK NUMBERS                         
FORMOPT  DS    CL1                 Y=PRINT STATION FORMAT                       
WORSTOPT DS    CL1                 Y=SHOW WORST STATIONS                        
SPILLOPT DS    CL1                 Y=SPILL ONLY N=NO SPILL                      
LEFTOPT  DS    CL1                 Y=LEFT ALIGN                                 
SPACOPT  DS    XL1                 SPACING                                      
EXPANOPT DS    CL1                 EXPAND COMBO OPTION                          
SIMULOPT DS    CL1                 SIMULCAST OPTION                             
AVEOPT   DS    CL1                 AVERAGE OPTION                               
CALCOPT  DS    CL1                 CALCULATE IMPRESSION METHOD OPTION           
ADDROPT  DS    CL1                 USE ADDRESS IN REPORTS                       
LONGOPT  DS    CL1                 USE 25 MORE LINES FOR REPORTS                
AUTOOPT  DS    CL1                 USE AUTO DELETE ON CMBLIST                   
RELO     DS    A                                                                
RELO2    DS    A                                                                
MYBASE   DS    A                                                                
MYBASE2  DS    A                                                                
VSUBR07  DS    A                                                                
ULPNTR   DS    F                                                                
BUFFCURR DS    F                                                                
         SPACE 1                                                                
COMBSV   DS    9F                                                               
COMBNUM  DS    C                                                                
TWANUM   DS    C                                                                
COMAND2  DS    CL8                                                              
         SPACE 1                                                                
*                                                                               
LASTDEM  DS    XL(L'BUFFDEM)       USED FOR BOOK/BOOK COMP                      
SPACTSW  DS    C                                                                
DIFFSW   DS    CL1                 FORMAT SWITCH                                
SIGN     DS    CL1                 SET TO = OR - FOR DIFF                       
MSASAVE  DS    CL10                ROOM FOR MSA=NNNNNN                          
TSASAVE  DS    CL10                ROOM FOR TSA=NNNNNN                          
ADISAVE  DS    CL10                ROOM FOR ADI=NNNNNN                          
ANYMSA   DS    CL1                 SET TO Y IF MSA SPECIFIED                    
ANYTSA   DS    CL1                 SET TO Y IF TSA SPECIFIED                    
ANYADI   DS    CL1                 SET TO Y IF ADI SPECIFIED                    
ANYUNIV  DS    CL1                 Y=UNIVERSES DONE                             
THISTYPE DS    CL1                 C(USTOM) OF S(TANDARD)                       
DISP     DS    F                   DISPLACEMENT INTO PRINT LINE                 
MKTNUM   DS    H                   MARKET NUMBER SELECTED                       
ASTATS   DS    A                   ADDRESS OF STATION LIST                      
WSTATS   DS    F                   WIDTH OF STATION LIST                        
ABOOKBF  DS    A                   ADDRESS OF FIRST DAYPART BEST                
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
STATNUM  DS    XL1                 NUMBER OF STATION BEING PROCESSED            
THISDEM  DS    3F                  BUFFER FOR STATION IN PROGRESS               
THISWT   DS    F                   BUFFER FOR STATION IN PROGRESS               
THISLINE DS    4F                                                               
UNIVLIST DS    XL10                DEMOUT LIST FOR UNIVS                        
UNIVS    DS    4F                  UNIVS FOR MSA TSA ADI                        
RUNIVS   DS    4F                  UNIVS FOR IMPRESSION METHOD                  
ACTUNIVS DS    16F                 STORAGE OF ALL UNIVS FOR ALL BOOKS           
LASTVAL  DS    XL(L'BUFFDEM)       VALUE OF DEMO USED  DURING RANKING           
         SPACE 1                                                                
DPENTRY  DS    0CL36               DAYPART ENTRY                                
DPSTYPE  DS    CL1                 FOR STANDARD, TYPE = S                       
DPSDAY   DS    XL1                          AND  DAY CODE                       
DPSPROG  DS    CL1                          AND  'PROGRAM' CODE 1-18            
DPSSPARE DS    XL13                                                             
         ORG   DPSTYPE                                                          
DPCTYPE  DS    CL1                 FOR CUSTOM,   TYPE = C                       
DPCDAY   DS    XL1                          AND  DAY CODE,                      
DPCSTART DS    XL1                               START HOUR NUMBER              
DPCEND   DS    XL1                               END HOUR NUMBER                
DPCOTHER DS    XL12                         OR   UP TO 4 MORE DAY/TIMES         
         SPACE 1                                                                
DPDESC   DS    CL19                FOR BOTH, DESCRIPTION                        
         DS    CL1                                                              
STASTYLE DS    CL1                 C=CUSTOM L=LIST M=MARKET                     
CUMESW   DS    CL1                                                              
UNIVBK   DS    CL2                                                              
NMARKET  DS    C                                                                
SVMKTH5  DS    C                                                                
SVMKTNUM DS    CL2                                                              
DBRADIC  DS    CL128                                                            
MEDTYPE  DS    C                                                                
SRCTYPE  DS    C                                                                
BOOKTYPE DS    C                                                                
CITYCODE DS    CL3                                                              
MISCOMMA DS    C                                                                
PREVSTA  DS    CL5                                                              
OUTAREA  DS    A                                                                
STALIST  DS    C                                                                
USEBUFF  DS    C                   USING BUFFER?                                
CMB_B4   DS    C                   COMBO BEFORE STATION?                        
STATLEN  DS    X                   STATION LENGTH                               
RUNIFLG  DS    C                   REAL UNIVERSES CALCULATED?                   
USESHARE DS    C                   NO OF SHARES USED                            
MULTCATS DS    C                                                                
NXTPOS   DS    F                                                                
PFKEY    DS    C                                                                
SUBSID   DS    C                   INTEREP SUBSID?                              
STALSTD  DS    C                   USED STATION LISTS?                          
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK))   SPARE                                  
*                                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPRES07   12/09/20'                                      
         END                                                                    
