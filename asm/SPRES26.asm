*          DATA SET SPRES26    AT LEVEL 053 AS OF 02/24/15                      
*PHASE T20F26A                                                                  
         TITLE 'T20F26 - ON-LINE LIST OF SIMULCASTS'                            
T20F26   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYDX-MYD,T20F26**,RA,R6,RR=R2,CLEAR=YES                          
         LR    R3,RC               SAVE A(MY STORAGE FOR THE MOMENT)            
         USING MYD,R3                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    R2,NRELO            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
         ST    R3,AMYD                                                          
         LA    RE,THISBUFF                                                      
         ST    RE,ATHISBUF                                                      
*                                                                               
         L     RE,=V(SUBR07)                                                    
         A     RE,RELO                                                          
         ST    RE,VSUBR07                                                       
         ST    RE,VSUBR07A                                                      
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              VALIDATE REQUEST AT MODE=VALREC                                  
*                                                                               
VREC     L     R2,ATIOB            CHECK FOR PF KEYS                            
         LTR   R2,R2                                                            
         BZ    VREC1                                                            
         USING TIOBD,R2                                                         
         CLI   TIOBAID,0                                                        
         BE    VREC1                                                            
*                                                                               
         CLI   ACTNUM,ACTREP       ONLY FOR ACTION REPORT                       
         BNE   PFKERR                                                           
*                                                                               
         CLI   TIOBAID,12          EQUATE 13-24 TO 1-12                         
         BNH   *+18                                                             
         ZIC   R1,TIOBAID                                                       
         SH    R1,=H'12'                                                        
         STC   R1,TIOBAID                                                       
*                                                                               
         OI    CONHEADH+6,X'80'                                                 
         OI    CONSERVH+6,X'81'                                                 
*                                                                               
         CLI   TIOBAID,2                                                        
         BNE   VRECT1                                                           
         CLC   RMKMKID,=C'MID1'    ALREADY HAVE TREAT AS ENTER                  
         BE    VRECT0A                                                          
         LA    R2,RNKSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'02'                                                     
         SPACE 1                                                                
         LA    R2,RNKBOOKH         VALIDATE BOOK                                
         MVI   MAX,1                                                            
         GOTO1 VRADBOOK                                                         
         GOTO1 VSUBR07,DMCB,('M1INITE',(RC))                                    
         USING SVTD,RE                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     *+8                                                              
VRECT0A  BAS   RE,SELMKT                                                        
         LA    RE,RMKWORK                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY                                                  
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELSRC,SVTSRC                                                  
         MVC   DBSELBK,SVTBOOK                                                  
         MVC   DBBTYPE,SVTBTYP                                                  
         MVC   DBFILE,SVTFILE                                                   
         MVC   DBSELMED,SVTMED                                                  
         BAS   RE,BLDMKT                                                        
         B     MYEND                                                            
*                                                                               
VRECT1   CLI   TIOBAID,12          PF12 - RETURN TO RANKER SCREEN               
         BNE   VRECT2                                                           
         CLC   RMKMKID,=C'MID1'    PROPER SCREEN                                
         BNE   MYEND                                                            
         BAS   RE,SELMKT                                                        
         MVC   BUFF(200),RMKWORK                                                
*                                                                               
         LA    RE,RMKWORK                                                       
         LA    RE,SVTMKTS          CHECK FOR ANY COMBO MARKETS                  
VRECT1A  CLI   0(RE),0             END OF MARKET LIST                           
         BE    VRECT1B                                                          
         LA    RE,3(RE)                                                         
         B     VRECT1A                                                          
*                                                                               
VRECT1B  MVI   TWANUM,2            SET THE TWA NUMBER                           
         GOTO1 VSUBR07,DMCB,('RESTWAE',(RC))                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CONRECH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0                                                          
         BNE   *-8                                                              
         MVC   1(2,R2),=X'0101'                                                 
         LA    R2,RNKSRCEH                                                      
         OI    6(R2),X'01'                                                      
         MVC   CONHEAD(32),=C'MAKE CHANGES OR ENTER TO PROCESS'                 
*        BAS   RE,BLDUL            BUILD THE USER LISTS                         
         B     SPERR                                                            
*                                                                               
VRECT2   MVC   CONHEAD(16),=C'PLEASE HIT ENTER'                                 
         CLI   TIOBAID,3                                                        
         BNE   VREC1                                                            
*                                                                               
VREC1    LA    R2,RMKMK1H          SET TO FIRST MARKET LIST FLD                 
         CLI   RNKMKTH+5,0                                                      
         BNE   VREC1A                                                           
         LA    R2,RNKMKTH                                                       
         MVC   CONHEAD(19),=C'** ERROR   PLEASE ENTER MARKET'                   
         B     MYEND                                                            
VREC1A   MVC   SVMKTH5,RNKMKTH+5                                                
         CLC   RMKMKID,=C'MID1'    ENSURE THE PROPER SCREEN                     
         BE    VRECT0A                                                          
         LA    R2,RNKSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         MVI   DBSELMED,C'R'       (ADJUST FOR RADIO)                           
         MVC   MEDTYPE,DBSELMED                                                 
         MVC   SRCTYPE,DBSELSRC                                                 
         XC    BOOKTYPE,BOOKTYPE                                                
         CLC   RNKBOOK(4),=C'LATEST'                                            
         BNE   NLATEST                                                          
         MVC   RNKBOOK(5),=C'1BOOK'                                             
         MVC   RNKBOOK+5(3),RNKBOOK+6                                           
         MVI   RNKBOOK+8,C' '                                                   
NLATEST  LA    R2,RNKBOOK                                                       
         ZIC   R1,RNKBOOKH+5                                                    
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
VREC01A  NI    BAGYMD,X'F0'                                                     
         OI    BAGYMD,X'02'                                                     
         MVC   WSTATS,=F'5'                                                     
         SPACE 1                                                                
         XC    MKTNUM,MKTNUM                                                    
         LR    R0,R3               SAVE R3 FOR LATER                            
         TM    RNKMKTH+4,X'08'     IS IT A NUMBER?                              
         BO    ISNUM                                                            
         TM    RNKMKTH+4,X'04'     IS IT A ALPHA MARKET?                        
         BO    ISALF                                                            
NOTVALID LA    R2,RNKMKTH                                                       
         MVC   CONHEAD(28),=C'** ERROR **   INVALID MARKET'                     
         B     MYEND                                                            
ISNUM    ZIC   R3,RNKMKTH+5                                                     
         GOTO1 VNUMVAL,DMCB,RNKMKT,(R3)                                         
         CLI   DMCB,X'FF'                                                       
         BE    NOTVALID                                                         
         L     R3,DMCB+4                                                        
         STH   R3,MKTNUM                                                        
         MVI   STASTYLE,C'M'                                                    
         B     MKTOK                                                            
         MVI   MEDTYPE,C'R'        DEIS SAYS: WE CAN NEVER GET HERE!!!          
         MVC   SRCTYPE,RNKSRCE                                                  
ISALF    MVC   CITYCODE,=C'   '                                                 
         ZIC   R3,RNKMKTH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CITYCODE(0),RNKMKT                                               
         LA    R5,MEDTYPE                                                       
         GOTO1 VCTYMRKT                                                         
         BZ    NOTVALID                                                         
         STH   R1,MKTNUM                                                        
*----------------------------------------------------------                     
         USING ELEMDS,R2                                                        
MKTOK    MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKTNUM                                                  
         LA    R2,RNKBOOKH                                                      
         MVI   MAX,4                                                            
         MVI   RNKMKTH+5,0                                                      
         OI    RNKMKTH+6,X'80'                                                  
         BAS   RE,VMULTBK          CHECK BOOKS FOR THIS MARKET                  
         CLI   BYTE,1                                                           
         BE    BADBOOKM                                                         
         CLI   BYTE,0              WAS A MULTI-BOOK?                            
         BZ    BKOK1               YES, SKIP CHECKING SINGLE BOOK               
         GOTO1 VRADBOOK                                                         
BKOK1    LR    R3,R0                                                            
         MVC   RNKMKTH+5(1),SVMKTH5                                             
         B     XIT                                                              
*----------------------------------------------------------                     
*----------------------------------------------------------                     
BADBOOKS MVC   ERRBOOK+L'ERRBOOK-7(7),=C'STATION'                               
         B     BADBOOK                                                          
         SPACE 1                                                                
BADBOOKM MVC   ERRBOOK+L'ERRBOOK-7(7),=C' MARKET'                               
BADBOOK  MVC   CONHEAD(L'ERRBOOK),ERRBOOK                                       
         MVC   CONHEAD+L'ERRBOOK+1(4),DBSELSTA                                  
         B     MYEND                                                            
         SPACE 1                                                                
VREC17W  MVC   RNKMKTH+5(1),SVMKTH5   RESTORE FIELDS                            
         MVC   MKTNUM,SVMKTNUM                                                  
VREC17X  DS    0H                                                               
         MVC   RESTITLE,=CL40'SIMULCAST STATIONS'                               
         SPACE 1                                                                
VREC20   GOTO1 CENTER,DMCB,RESTITLE,40                                          
         MVC   THISBUFF(256),COMBLIST                                           
         MVC   THISBUFF+256(CUMESW-COMBLIST-256),COMBLIST+256                   
         SPACE 1                                                                
VRECX    B     XIT                                                              
         SPACE 1                                                                
PFKERR   MVC   CONHEAD(L'PFKERRM),PFKERRM                                       
         B     MYEND                                                            
         DROP  R3                                                               
         EJECT                                                                  
VMULTBK  NTR1                                                                   
         MVI   NBOOKS,0                                                         
         MVC   BADMULP(1),RNKBOOK                                               
         XC    BOOKS,BOOKS                                                      
         CLI   RNKMKTH+5,0                                                      
         BE    *+8                                                              
         BAS   RE,EDITMKT          DO THIS TO GET MARKET #                      
         LH    RE,MKTNUM           NOW SET MARKET IN STATION FIELD              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DBSELSTA(4),DUB+5(3)                                             
         MVI   DBSELSTA+4,C'A'     BY CONVENTION                                
         MVC   DBSELRMK,MKTNUM                                                  
*                                                                               
         LA    R1,1                                                             
         CLC   RNKBOOK+1(4),=C'BOOK'                                            
         BNE   XIT                                                              
         MVC   BYTE(1),RNKBOOK     SET NUMBER OF BOOKS                          
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,4                                                           
         BH    VMULTBKX                                                         
         CLI   RNKBOOK+5,C'('                                                   
         BNE   *+10                                                             
         MVC   DBBTYPE,RNKBOOK+6                                                
*                                                                               
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR MARKET                         
         GOTO1 DEMAND,DMCB,DBLOCK,BKHOOK                                        
         LA    R1,1                                                             
         CLI   NBOOKS,0            NOT FOUND                                    
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
*                                                                               
BKHOOK   NTR1                                                                   
         L     R4,DBAREC                                                        
         USING SBKEY,R4                                                         
         TM    SBBOOK,X'80'        BYPASS REVERSE SEQ BOOKS                     
         BO    XIT                                                              
         CLC   SBBTYP,DBBTYPE                                                   
         BNE   XIT                                                              
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
         OC    MKTNUM,MKTNUM       DO WE HAVE MARKET #?                         
         BNZ   HAVEMKTN            GOT IT ALREADY                               
         XC    WSTATS,WSTATS                                                    
         MVI   WSTATS+3,5          STATION TABLE WIDTH SET TO 5                 
         LA    R2,RNKMKTH          EITHER MARKET NUMBER OR STALIST              
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         CLI   8(R2),C'='                                                       
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         LTR   R1,R1                                                            
*        BZ    EDITLIST                                                         
         STH   R1,MKTNUM                                                        
         MVI   STASTYLE,C'M'                                                    
HAVEMKTN MVI   RNKMKTN,C'*'                                                     
         BAS   RE,GETMKT                                                        
         CLI   RNKMKTN,C'*'                                                     
*        BE    BADMKT                                                           
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         ZIC   RE,NMARKET                                                       
         LA    RE,1(RE)                                                         
         STC   RE,NMARKET                                                       
         MVC   STASTYLE,STALIST                                                 
         DROP  R4                                                               
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
         OI    RNKMKTNH+6,X'80'                                                 
         MVC   RNKMKTN,SPACES                                                   
         L     R4,DBAREC                                                        
         USING DMKEY,R4                                                         
         LA    R4,DMFRSTEL                                                      
         USING DMELEM,R4                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   RNKMKTN(0),DMMNAME                                               
         DROP  R4                                                               
         EJECT                                                                  
BLDMKT   NTR1                                                                   
         MVI   DBFUNCT,DBGETMKB                                                 
         LA    RE,BUFF                                                          
         LR    R2,RE                                                            
         L     RF,=F'7000'                                                      
         XCEF                                                                   
         ST    R2,FULL                                                          
         XC    DUB,DUB                                                          
         GOTO1 DEMAND,DMCB,DBLOCK,MKBHOOK                                       
         OC    DUB(4),DUB                                                       
         BZ    XIT                                                              
         BAS   RE,MKBFILL                                                       
         B     XIT                                                              
         SPACE 2                                                                
MKBHOOK  NTR1                                                                   
         L     R2,FULL                                                          
         L     R4,DBAREC                                                        
         USING DMKEY,R4                                                         
         MVC   0(2,R2),DMRMKT                                                   
         LA    R4,DMFRSTEL                                                      
         USING DMELEM,R4                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         CH    R1,=H'12'                                                        
         BL    *+8                                                              
         LA    R1,11                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R2),DMMNAME                                                  
         L     R1,DUB                                                           
         LA    R1,1(R1)                                                         
         ST    R1,DUB                                                           
         LA    R2,14(R2)                                                        
         ST    R2,FULL                                                          
         B     XIT                                                              
MKBFILL  NTR1                                                                   
         L     R2,DUB                                                           
         GOTO1 XSORT,DMCB,(0,BUFF),(R2),14,12,2                                 
         LA    R2,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         TWAXC RMKMK1H,(R2),PROT=Y                                              
         LA    R1,RMKMKLH+(RMKMKA1H-RMKMK1H)                                    
         LA    R8,BUFF                                                          
         LA    RE,RMKWORK                                                       
         LA    R2,RMKMKA1H                                                      
MKBFILL2 CR    R2,R1                                                            
         BH    XIT                                                              
         CLC   SVTKMKT(12),2(R8)                                                
         BNL   MKBFILL3                                                         
*                                                                               
         CLI   SVTFLTR,C'='        FILTER ACTIVE                                
         BNE   *+14                                                             
         CLC   SVTFLTR+1(1),2(R8)                                               
         BH    MKBFILL3                                                         
*                                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   SVTKMKT(12),2(R8)                                                
         EDIT  (B2,0(R8)),(4,DUB)                                               
         MVC   RMKMKA1-RMKMKA1H(4,R2),DUB                                       
         MVC   RMKMKA1-RMKMKA1H+5(L'RMKMKA1-5,R2),2(R8)                         
         LA    R2,RMKMK2H-RMKMK1H(R2)                                           
MKBFILL3 LA    R8,14(R8)                                                        
         OC    0(2,R8),0(R8)                                                    
         BNZ   MKBFILL2                                                         
         XC    RMKWORK(12),RMKWORK                                              
         B     XIT                                                              
         EJECT                                                                  
SELMKT   NTR1                                                                   
         LA    RE,RMKWORK                                                       
         LA    R4,SVTMKTS          POINT TO SAVED MARKET LIST                   
         OC    0(3,R4),0(R4)       FIND AN OPEN SLOT                            
         BZ    *+12                                                             
         LA    R4,3(R4)                                                         
         B     *-14                                                             
         SPACE 1                                                                
         LA    RF,RMKMKLH+(RMKMKA1H-RMKMK1H)   SET FOR END                      
         LA    R2,RMKMK1H          SET TO START                                 
SELMKT2  CR    R2,RF               END OF INPUT                                 
         BH    XIT                                                              
         LA    RE,RMKWORK                                                       
         LA    RE,SVTMKTS+39       STOP IF MAX.                                 
         CR    R4,RE                                                            
         BNL   XIT                                                              
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BE    SELMKT4             NO - ON TO NEXT                              
         MVC   0(1,R4),8(R2)                                                    
         BAS   RE,BUMP                                                          
         MVC   FULL,8(R2)          SET MARKET NUMBER                            
         OC    FULL,=C'0000'       MAKE IT NUMBERS                              
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         LTR   R1,R1               IGNORE IF NO MARKET                          
         BZ    *+12                                                             
         STCM  R1,3,1(R4)          SAVE THE MARKET                              
         LA    R4,3(R4)                                                         
         B     *+8                 ALREADY PAST FIRST FIELD                     
         SPACE 1                                                                
SELMKT4  BAS   RE,BUMP             BYPASS FIRST                                 
         BAS   RE,BUMP             BYPASS SECOND                                
         B     SELMKT2                                                          
         EJECT                                                                  
*              CONTROL REPORT PRINTING                                          
         SPACE 3                                                                
TMPSEPBF DC    XL21'00'            1 FOR LEN, 20 FOR 6 DEM/CAT                  
         DC    X'FF'               END OF DEMO LIST                             
TMPDEMOS DC    XL60'00'                                                         
PRPNDEMS DC    X'00'                                                            
PRPDEMOS DC    XL24'00'                                                         
PTMPR3   DC    F'0'                                                             
*                                                                               
         USING MYD,R3                                                           
PREP     L     R1,=A(HEDSPECS)     INTIALIZATION                                
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         ST    R3,PTMPR3                                                        
*                                                                               
         OC    MKTNUM,MKTNUM       RESET BOOKS FOR SOFT LIST                    
         BZ    PREPSM                                                           
         TM    RNKBOOK,X'F0'                                                    
         BNO   PREPSM                                                           
         BAS   RE,VMULTBK                                                       
*                                                                               
PREPSM   L     RE,AIO2                                                          
         LA    RE,1000(RE)                                                      
         ST    RE,MKTPNTR                                                       
         MVI   ANYUNIV,C'N'                                                     
         MVI   USEBUFF,C'N'                                                     
         MVI   STACTSW,0                                                        
         XC    UNIVBK,UNIVBK                                                    
         LA    RE,BUFF             INITIALIZE MAIN BUFFER                       
         LA    RE,500(RE)                                                       
         L     RF,=AL4(L'BUFFREC*40*15+100)                                     
         XCEF                                                                   
*------------------------------------------------------------                   
         GOTO1 =A(COMPDISP),DMCB,(R8),(R9),(RC),RR=RELO                         
         MVI   NSTATS,0                                                         
         BAS   RE,EXPSTATS                                                      
         CLI   NSTATS,0                                                         
         BE    NOSTATS                                                          
         BAS   RE,PRNTBUFF                                                      
         B     XIT                                                              
NOSTATS  MVC   CONHEAD(23),=C'NO SIMULCASTS STATIONS.'                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD STATIONS IN MARKET                              
         SPACE 3                                                                
*              INPUT               MKTNUM                                       
*              OUTPUTS             ASTATS = A(100 5 BYTE STATIONS)              
*                                  NSTATS   ACTUAL NUMBER OF STATIONS           
         SPACE 1                                                                
EXPSTATS NTR1                                                                   
         CLI   NSTATS,0                                                         
         BNE   XIT                                                              
EXPSTA1  MVC   ATHISTAT,ASTATS                                                  
         SPACE 1                                                                
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
*                                                                               
         ZIC   RF,NBOOKS         BASE BOOK LIST ON LATEST BOOK IN LIST          
         LA    RE,BOOKS                                                         
         XC    DBSELBK,DBSELBK                                                  
         XC    PREVSTAT,PREVSTAT                                                
EXPSTAT2 CLC   DBSELBK,1(RE)                                                    
         BH    EXPSTAT4                                                         
         MVC   DBSELBK,1(RE)                                                    
         MVC   DBBTYPE,3(RE)                                                    
EXPSTAT4 LA    RE,4(RE)                                                         
         BCT   RF,EXPSTAT2                                                      
*                                                                               
         L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         GOTO1 DEMAND,DMCB,DBLOCK,EXPHOOK                                       
         B     XIT                                                              
         SPACE 1                                                                
EXPHOOK  NTR1                                                                   
         CLI   MLSTAT+4,C'B'       KEEP IF SIMULCAST STATION                    
         BE    EXPHOOK1                                                         
         CLI   MLSTAT+4,C'C'                                                    
         BE    EXPHOOK1                                                         
         CLI   MLSTAT+4,C'D'                                                    
         BE    EXPHOOK1                                                         
         B     XIT                 LEAVE IF NOT                                 
*                                                                               
EXPHOOK1 CLI   MLSTAT,C'Z'         CUT OUT MARKET TOTALS                        
         BH    XIT                                                              
         SPACE 1                                                                
EXPHOOK4 L     R2,ATHISTAT                                                      
         CLC   MLSTAT,PREVSTAT                                                  
         BE    XIT                                                              
         MVC   0(5,R2),MLSTAT                                                   
         MVC   PREVSTAT,MLSTAT                                                  
         OC    MLKMKT,MLKMKT       NOTE SPILL                                   
         BZ    *+8                                                              
         NI    4(R2),X'7F'         BY TURNING OFF X'80' BIT                     
         LA    R2,5(R2)                                                         
         ST    R2,ATHISTAT                                                      
         AI    NSTATS,1                                                         
         CLI   NSTATS,100                                                       
         BNH   XIT                                                              
         DC    H'0'                OUT OF SPACE IN STATION LIST                 
         SPACE 2                                                                
FLTCMB   NTR1                                                                   
         L     RE,AMYD                                                          
         USING MYD,RE                                                           
         LA    R1,1                                                             
         LA    R7,COMBLIST                                                      
FLTCMB1  CLI   0(R7),0             END OF COMBOS                                
         BE    FLTCMBX                                                          
         LA    R2,9(R7)            POINT TO STATIONS                            
FLTCMB2  CLI   0(R2),0             END OF STATIONS                              
         BE    FLTCMB3                                                          
         CLC   MLSTAT,0(R2)                                                     
         BNE   *+10                                                             
         SR    R1,R1                                                            
         B     FLTCMBX                                                          
         LA    R2,5(R2)                                                         
         B     FLTCMB2                                                          
*                                                                               
FLTCMB3  LA    R7,LNCOMB(R7)       NEXT COMBO LIST                              
         B     FLTCMB1                                                          
*                                                                               
FLTCMBX  LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CONTROL PRINTING OF BUFFER                            
         SPACE 3                                                                
*              INPUTS              NDPTS   DPTLIST                              
*                                  NDEMOS  DEMOS                                
*                                  UP      DPTGAP   DPTWIDTH DISP               
*              OUTPUT              PRINT LINES                                  
         SPACE 1                                                                
PRNTBUFF NTR1                                                                   
         MVC   P+1(8),=C'STATIONS'                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+133(8),=C'--------'                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ZIC   R0,NSTATS                                                        
         L     R1,ASTATS                                                        
PB0      LA    R2,P+1                                                           
         OI    4(R1),X'80'                                                      
         BAS   RE,FORMLINE                                                      
         LA    R1,5(R1)                                                         
         BCT   R0,PB0                                                           
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT AND PRINT A REPORT LINE                                   
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS TO PRINT               
*                                  R3=A(FIRST BUFFER LINE)                      
*                                  ASTATS=A(5 CHARACTER STATION CODES)          
*                                  NDEMOS DEMOS                                 
*                                  DISP  DPTWIDTH  DPTGAP                       
         SPACE 1                                                                
         USING BUFFD,R3                                                         
FORMLINE NTR1                                                                   
         BAS   RE,FORM4            PRINT STATION AND DEMOS FOR DAYPART          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
FORM4    NTR1                                                                   
         MVC   0(4,R2),0(R1)       OUTPUT WABC                                  
         CLI   4(R1),0             COMBOS - LEFT ALONE                          
         BE    FORM6                                                            
         MVI   4(R2),C'-'                 WABC-                                 
         MVC   5(1,R2),4(R1)              WABC-A                                
         MVI   6(R2),C'M'                 WABC-AM                               
         CLI   5(R2),C'C'              OR                                       
         BNE   *+8                                                              
         MVI   6(R2),C'O'                 WABC-CO                               
         CLI   5(R2),C'B'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R2),=C'AA'             WABC-AA                               
         CLI   5(R2),C'D'              OR                                       
         BNE   *+10                                                             
         MVC   5(2,R2),=C'FF'             WABC-FF                               
         CLI   DBSELSRC,C'M'       BBM SPECIAL SUFFIXES                         
         BNE   *+8                                                              
         BAS   RE,CNVBBMS                                                       
         CLI   3(R2),C' '          CHANGE WOR -AM                               
         BNE   *+10                                                             
         MVC   3(4,R2),4(R2)           TO WOR-AM                                
         CLI   BUFFSAN,C' '                                                     
         BNH   FORM6A                                                           
         MVC   6(1,R2),BUFFSAN                                                  
         MVC   4(1,R2),5(R2)                                                    
         MVI   5(R2),C'#'                                                       
         B     FORM6A                                                           
         SPACE 1                                                                
FORM6    DS    0H                                                               
FORM6A   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
CNVBBMS  NTR1                                                                   
         LA    RE,BBMSUF           CONVERT BBM SUFFIXES                         
CNVBBMS2 CLI   0(RE),X'FF'         PROTECT FOR GARBAGE                          
         BE    CNVBBMSX                                                         
         CLC   5(1,R2),2(RE)       FIND EXPANSION                               
         BE    *+12                                                             
         LA    RE,L'BBMSUF(RE)                                                  
         B     CNVBBMS2                                                         
CNVBBMS4 MVC   5(2,R2),0(RE)       SEND IT TO OUTPUT                            
CNVBBMSX B     XIT                                                              
BBMSUF   DS    0CL3                                                             
       ++INCLUDE DEBBMSUFX                                                      
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,AMYD                                                          
         USING MYD,R4                                                           
         MVC   BLOCK(36),SPACES    MARKET NUMBER AND NAME                       
         EDIT  (2,MKTNUM),(4,BLOCK)                                             
         MVC   BLOCK+5(30),RNKMKTN                                              
         GOTO1 SQUASHER,DMCB,BLOCK,36                                           
         MVC   H4+7(36),BLOCK                                                   
         SPACE 1                                                                
         MVC   BLOCK(60),SPACES    SOURCE AND BOOK                              
         MVC   BLOCK(8),RNKSRCE                                                 
         LA    R1,BLOCK+10                                                      
         ZIC   R0,NBOOKS                                                        
         LA    R2,BOOKS+1                                                       
         BAS   RE,CONVBOOK                                                      
         MVC   0(9,R1),WORK                                                     
         LA    R1,10(R1)                                                        
         LA    R2,4(R2)                                                         
         BCT   R0,*-18                                                          
*        MVC   BLOCK+10(40),RNKBOOK                                             
         GOTO1 SQUASHER,DMCB,BLOCK,60                                           
         MVC   H5(44),BLOCK                                                     
         L     RE,DBEXTEND         CHECK FOR CONDENSED MARKETS/DEMOS            
         USING DBEXTRAD,RE                                                      
HOOK0    LTR   RE,RE                                                            
         BZ    HOOK1                                                            
         CLC   0(4,RE),=C'RADI'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     HOOK0                                                            
         CLI   DBRADEMT,C'R'                                                    
         BNE   HOOK1                                                            
         L     RF,=A(CNDHDR)                                                    
         A     RF,RELO                                                          
         MVC   H8+12(L'CNDHDR),0(RF)                                            
HOOK1    LA    R2,H1+40            TITLE                                        
         CLI   ADDROPT,C'N'                                                     
         BNE   HOOK1B                                                           
         CLI   RCSUBPRG,1                                                       
         BE    HOOK1A                                                           
         MVC   H2+99(32),SPACES                                                 
         B     HOOK1B                                                           
HOOK1A   MVC   H2+75(32),SPACES                                                 
HOOK1B   CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         LA    R2,11(R2)                                                        
         MVC   RESTITLE,=CL40'SIMULCAST STATIONS'                               
         MVC   0(40,R2),RESTITLE   ALREADY CENTERED                             
         GOTO1 UNDERLIN,DMCB,(40,(R2)),(X'BF',132(R2))                          
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONVERT BOOK FOR HEADINGS                             
         SPACE 3                                                                
CONVBOOK NTR1                                                                   
         MVC   WORK(9),=C' FALL/NN '                                            
         LA    R3,WORK+6                                                        
         CLI   1(R2),11            NOVEMBER=FALL                                
         BE    CONVB2                                                           
         MVC   WORK(9),=C'WINTER/NN'                                            
         LA    R3,WORK+7                                                        
         CLI   1(R2),1             JAN=WINTER                                   
         BE    CONVB2               FOR BBM                                     
         CLI   1(R2),2             FEBRUARY=WINTER                              
         BE    CONVB2                                                           
         MVC   WORK(6),=C'SPRING'                                               
         CLI   1(R2),5             MAY=SPRING                                   
         BE    CONVB2                                                           
         MVC   WORK(6),=C'SUMMER'                                               
         CLI   1(R2),7                                                          
         BE    CONVB2                                                           
         MVC   WORK(6),=C'??????'                                               
         SPACE 1                                                                
CONVB2   EDIT  (1,0(R2)),(2,(R3)),WRK=DMCB                                      
         B     XIT                                                              
         EJECT                                                                  
*              COMMON ROUTINES                                                  
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         MVC   WORK,CONHEAD                                                     
ERREND   GOTO1 VGETERR                                                          
         SPACE 2                                                                
SPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
CPERR    MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,CONHEAD                                                     
         GOTO1 VCPRSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
VSUBR07  DS    A                                                                
         SPACE 2                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
         EJECT                                                                  
*              LTORG                                                            
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ERRORS SPECS AND CONSTANTS                                       
         SPACE 3                                                                
*                                  MY ERROR MESSAGES                            
         SPACE 1                                                                
HEDSPECS DS    0H                  HEADLINE SPECS                               
         SPACE 1                                                                
         SPROG 1,2                                                              
         SSPEC H1,52,C'SIMULCAST STATIONS'                                      
         SSPEC H1,1,C'DDS RADIO RESEARCH'                                       
         SSPEC H2,52,C'------------------'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,C'MARKET'                                                   
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REPORT                                                     
         SSPEC H5,93,PAGE                                                       
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H5,115,PAGE                                                      
         DC    X'00'                                                            
*                                  DISPLACEMENT TABLE                           
         SPACE 1                                                                
DISPTAB  DC    AL1(1,1,3,3,4)      EACH ENTRY IS                                
         DC    AL1(1,2,3,3,4)      WIDTH (1-3)                                  
         DC    AL1(1,3,2,6,10)     NUMBER OF DEMO CATEGORIES (1-4)              
         DC    AL1(1,4,2,4,4)      UP                                           
         DC    AL1(2,1,5,1,0)      GAP BETWEEN DAYPARTS                         
         DC    AL1(2,2,5,1,0)      DISPLACEMENT FROM LEFT HAND EDGE             
         DC    AL1(2,3,4,0,1)                                                   
         DC    AL1(2,4,3,5,0)                                                   
         DC    AL1(3,1,6,1,0)                                                   
         DC    AL1(3,2,6,1,0)                                                   
         DC    AL1(3,3,4,4,6)                                                   
         DC    AL1(3,4,4,0,0)                                                   
         DC    X'FF'                                                            
*        SPACE 2                                                                
MSG      DS    0C                                                               
FILTERR  DC    C'** ERROR ** INVALID FILTER'                                    
INVDAYPT DC    C'** ERROR ** INVALID DAYPART'                                   
MKTERR   DC    C'** ERROR ** MARKET NOT FOUND'                                  
MKTERR2  DC    C'** MARKET MISSING OR MISSING M= IN USER LIST'                  
LISTERR  DC    C'** ERROR ** NEED GOOD MARKET NUMBER IN LIST'                   
PFKERRM  DC    C'** PFKEYS FOR ACTION "REPORT" ONLY**'                          
BADDEM   DC    C'** ERROR ** INVALID DEMO EXPRESSION'                           
SOONIT   DC    C'BOOKS X MARKETS > '                                            
SOONITSP DC    C'BKS X MKTS X DEMS > '                                          
SOONITN  DC    C'5 REQUEST SOON OR OVERNITE'                                    
SOONITS  DC    C'20 REQUEST OVERNITE'                                           
SOONITO  DC    C'32 SPLIT THE REQUEST'                                          
         SPACE 1                                                                
NEEDKW   DC    C'KEYWORD MISSING'                                               
NEEDMKT  DC    C'MARKET MISSING'                                                
NEEDSL   DC    C'STATION LIST REQUIRED'                                         
MAXERR   DC    C'TOO MANY ITEMS IN LIST - SPLIT REQUEST'                        
BADMUL   DC    C'MARKET IS NOT SWEPT (N) TIMES A YEAR'                          
BADMULP  EQU   BADMUL+21                                                        
ERRSTA   DC    C'INVALID STATION FOR BOOK OR MARKET'                            
ERRBOOK  DC    C'REQUESTED BOOK(S) NOT FOUND- MARKET'                           
TOOMNY   DC    C'** ERROR ** LIMITED TO 40 STATIONS'                            
TOOMNYDM DC    C'** ERROR ** TOO MANY DEMOGRAPHICS'                             
TOOMNYCT DC    C'** ERROR ** TOO MANY CATEGORIES'                               
MAXCMBA  DC    C'MAX OF 8 COMBOS FOR ONE REQUEST'                               
MAXCMB   DC    C'COMBO LIMITED TO 7 STATIONS '                                  
VIO1     DC    C'#=VIOLATION: SPECIAL STATION ACTIVITIES. DETAILS: P.13+        
                ARB MKT. REPORT'                                                
VIO2     DC    C'A = STATION CITED  Z = STATION LISTED BELOW THE LINE'          
CNDHDR   DC    C'**CONDENSED MARKET - EXERCISE DISCRETION WHEN USING THX        
               ESE ESTIMATES**'                                                 
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
         DC    X'00'                                                            
*                                                                               
         DS    0H                                                               
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
         LA    R2,RNKMKTH          COMBINE MARKET                               
         ST    R2,LTABLE                                                        
         MVI   LTABLE,X'1'         1 ENTRY                                      
         LA    R2,OUTAREA1                                                      
         SPACE 1                                                                
         USING ELEMDS,R2                                                        
         SPACE 1                                                                
* NOT EMPTY, PUT IN M=                                                          
         SPACE 1                                                                
         MVI   RNKMKTH+4,X'C0'                                                  
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
AFTPARXT MVC   0(4,R1),0(R3)                                                    
AFTPARX2 LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         C     R1,OUTAREA          NO ERROR                                     
         XMOD1                                                                  
AFTERROR LA    R1,OUTAREA1                                                      
         ST    R1,OUTAREA                                                       
         LTR   R1,R1               ERROR                                        
         XMOD1                                                                  
         SPACE 2                                                                
         SPACE 2                                                                
OUTAREA1 DC    CL256' '                                                         
LTABLEDS DS    0FL8                                                             
LTABLE   DC    8F'0'                                                            
SCTABL   DC    C'=,',X'FF'                                                      
         LTORG                                                                  
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
*              ROUTINE TO FILL BUFFER FOR THIS STATION                          
         SPACE 3                                                                
*              INPUTS              R2=A(5-BYTE STATION)                         
*                                  NDPTS DPTLIST                                
*              OUTPUT              THISBUFF (R2)                                
         SPACE 1                                                                
*              ROUTINE TO COMPUTE PRINT AND BUFFER DISPLACEMENTS                
         SPACE 3                                                                
*              INPUTS              NDPTS    NSTATS   NDEMOS                     
*                                  WIDEOPT  MAXRANK  BOXOPT                     
*              OUTPUTS             DISP     DPTGAP   UP      DPTWIDTH           
*                                  ASTATS   ABEST    AWORST                     
         SPACE 1                                                                
         DS    0H                                                               
COMPDISP NMOD1 0,**COMPDISP**                                                   
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         LA    R2,BUFF             STATION LIST IN BUFF                         
         ST    R2,ASTATS                                                        
         LA    R2,500(R2)          BUMP PAST THIS TO DEMO BUFFER                
         ST    R2,ABEST            FIRST LINE IS BEST                           
         ZIC   R1,MAXRANK                                                       
         BCTR  R1,0                                                             
         MH    R1,=AL2(L'BUFFREC)  (EACH LINE IS 9 BYTES)                       
         AR    R2,R1                                                            
         ST    R2,AWORST                                                        
         SPACE 1                                                                
         MVI   RCSUBPRG,1                                                       
         CLI   WIDEOPT,3                                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         ZIC   R1,NDEMOS           COMPUTE WIDTH OF DAYPART CHUNK               
         MH    R1,=H'6'            (N'DEMOS X 6) + 9                            
         LA    R1,9(R1)                                                         
         CLI   RANKOPT,C'Y'        OPTION TO SHOW RANK NUMBER?                  
         BNE   *+8                                                              
         LA    R1,5(R1)            INCREASES WIDTH                              
         CLI   FORMOPT,C'Y'        OPTION TO SHOW STATION FORMAT                
         BNE   *+8                                                              
         LA    R1,5(R1)            INCREASES WIDTH                              
*                                                                               
         L     R4,AMYD             STION WIDTH = 9 FOR COMBO                    
         USING MYD,R4                                                           
         CLI   COMBLIST,0                                                       
         BE    *+8                                                              
         LA    R1,2(R1)                                                         
         DROP  R4                                                               
*                                                                               
         CH    R1,=H'21'           MINIMUM WIDTH OF A CHUNK IS 21               
         BH    *+8                                                              
         LA    R1,21                                                            
         ST    R1,DPTWIDTH                                                      
         SPACE 1                                                                
         LA    R1,80               GET MAX WIDTH INTO R1                        
         CLI   WIDEOPT,2                                                        
         BL    COMPD2                                                           
         LA    R1,110                                                           
         BE    COMPD2                                                           
         LA    R1,132                                                           
         SPACE 1                                                                
COMPD2   SR    R0,R0                                                            
         D     R0,DPTWIDTH         FIGURE HOW MANY WILL FIT                     
         ST    R1,UP                                                            
         BCTR  R1,0                NUMBER OF GAPS IS UP-1                       
         LR    R2,R1               (NOW IN R2)                                  
         SRDL  R0,32               REMAINING BYTES IS NOW IN R1                 
         DR    R0,R2                                                            
         ST    R1,DPTGAP           R1 NOW HAS WIDTH OF EACH GAP                 
         SRL   R0,1                                                             
         ST    R0,DISP             AND HALF THE REMAINS IS DISPLACEMENT         
         XMOD1                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP BOXES FOR THIS SET OF DAYPARTS                 
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS IN THIS CHUNK          
*                                  DISP DPTWIDTH DPTGAP                         
*                                  LINE MAXRANK                                 
*              OUTPUT              BOXCOLS BOXROWS                              
         SPACE 1                                                                
SETBOX   NMOD1 0,**SETBOX**                                                     
         L     R8,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   BOXOPT,C'N'         OPTION TO SUPPRESS                           
         BE    SETBOX_X                                                         
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    SETBOX_X                                                         
         SPACE 1                                                                
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
         MVC   BOXROWS,SPACES                                                   
         ZIC   R1,LINE             PICK UP PRESENT LINE NUMBER                  
         BCTR  R1,0                                                             
         LA    R1,BOXROWS(R1)      AND SET THE ROWS                             
         MVI   0(R1),C'T'                                                       
         MVI   3(R1),C'M'                                                       
         ZIC   RE,MAXRANK                                                       
         LA    R1,4(R1,RE)                                                      
         MVI   0(R1),C'B'                                                       
         SPACE 1                                                                
         MVC   BOXCOLS,SPACES      NOW DO THE COLUMNS                           
         LA    R1,BOXCOLS                                                       
         A     R1,DISP                                                          
         SPACE 1                                                                
BOXSET2  MVI   0(R1),C'L'          LEFT                                         
         A     R1,DPTWIDTH                                                      
         BCTR  R1,0                                                             
         MVI   0(R1),C'R'          AND RIGHT FOR EACH DAYPART                   
         LA    R1,1(R1)                                                         
         A     R1,DPTGAP                                                        
         BCT   R0,BOXSET2                                                       
SETBOX_X XMOD1                                                                  
         EJECT                                                                  
*              SET UP TITLES FOR DAYPARTS                                       
         SPACE 3                                                                
*              INPUTS              R0=NUMBER OF DAYPARTS IN THIS SET            
*                                  R2=A(DPENTRIES)                              
*              OUTPUTS             TITLES WITH SURROUNDING BOXES                
         SPACE 1                                                                
DPTITLES NMOD1 0,**DPTITLES**                                                   
         L     R2,0(R1)                                                         
         L     R8,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         USING DPENTRY,R2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P+1                                                           
         A     R3,DISP                                                          
         SPACE 1                                                                
DPTIT2   MVC   DPDESC,SPACES                                                    
         MVC   0(8,R3),=C'STATIONS'                                             
         SPACE 1                                                                
DPTITNXT LR    R4,R3                                                            
         LA    R4,132(R4)                                                       
         MVC   0(8,R4),=C'--------'                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XMOD1                                                                  
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SUBR07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SR07**,RA,RR=R7                                              
         USING MYD,R3                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R3,AMYD                                                          
         USING MYD,R3                                                           
         ST    R7,RELO2            SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE2                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         MVC   CONHEAD(L'CONHEAD),SPACES                                        
         L     R1,0(R1)                                                         
         SRL   R1,24               GET THE ROUTINE NUMBER                       
         SLL   R1,2                AND BRANCH ADDRESS                           
         LA    RF,*+2(R1)                                                       
         BR    RF                                                               
WRTWAE   EQU   (WRTWA#-*)/4+1                                                   
RESTWAE  EQU   (RESTWA#-*)/4+1                                                  
M1INITE  EQU   (M1INIT#-*)/4+1                                                  
         SPACE 1                                                                
WRTWA#   B     WRTWA          3    WRITE A TWA (TWANUM=WHICH ONE)               
RESTWA#  B     RESTWA         4    RESTORE A TWA                                
M1INIT#  B     M1INIT         5    INITIALIZE A MKT LIST SCREEN                 
         EJECT                                                                  
         PRINT GEN                                                              
SETUNIVS DS    0H                                                               
         B     XIT2                                                             
         PRINT NOGEN                                                            
HSTAT    DC    X'0'                                                             
CMBCOUNT DC    X'0'                                                             
TEMPMRKT DC    H'0'                                                             
         SPACE 2                                                                
M1INIT   MVC   CONHEAD(31),=C'SELECT DESIRED MARKETS (5 MAX.)'                  
         LA    RE,BUFF                                                          
         USING SVTD,RE                                                          
         XC    BUFF(200),BUFF                                                   
         MVC   SVTSRC,DBSELSRC                                                  
         MVC   SVTBOOK,BOOKS+1                                                  
         MVC   SVTBTYP,BOOKS+3                                                  
         MVC   SVTFILE,DBFILE                                                   
         MVC   SVTMED(1),DBSELMED                                               
         MVC   SVTFLTR,RNKMKT                                                   
         SPACE 1                                                                
         MVI   TWANUM,2            SAVE IN TWA 2                                
         MVC   COMAND2,=CL8'DMWRT'                                              
         PRINT GEN                                                              
         GOTO1 VSUBR07A,DMCB,('WRTWAE',(RC))                                    
         PRINT NOGEN                                                            
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
         EJECT                                                                  
TMPMKT   DC    H'0'                                                             
NEQXIT2  LA    R1,1                                                             
         B     *+6                                                              
EQXIT2   SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT2     XIT1                                                                   
         SPACE 1                                                                
TEMPNAME DC    CL5' '                                                           
CMBELMER DC    C'COULD NOT FIND COMBO ELEMENT'                                  
GOTMATCH DC    C' '                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DAYPART SCREEN GENERATOR STUFF                                              
         SPACE 1                                                                
DPDISP   DC    AL1(1,3)            FIRST                                        
         DC    AL1(25,27)          SECOND                                       
         DC    AL1(52,54)          THIRD                                        
         SPACE 1                                                                
DPL1     DC    AL1(3,7,0,1,1,INTNOR,0)     SELECT FIELD                         
DPL2     DC    AL1(3,7+20,0,3,20,PROT,0)   NAME FIELD                           
DPL2C    DC    CL20' '                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR HEADLINES                        
         SPACE 3                                                                
*              INPUT               FIRST DEMO IN DEMOS                          
*              OUTPUTS             ADISAVE MSASAVE TSASAVE                      
         SPACE 1                                                                
         DS    0H                                                               
SETUNIVA NMOD1 0,**SETUNIVA**      GENERAL PRINT AREAS                          
         L     R3,0(R1)                                                         
         L     R7,4(R1)                                                         
         L     R8,8(R1)                                                         
         L     R9,12(R1)                                                        
         L     RC,16(R1)                                                        
*                                                                               
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         CLC   DBSELBK,UNIVBK                                                   
         BNH   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         CLI   ANYUNIV,C'Y'                                                     
         BE    SETUNIVX                                                         
         MVI   ANYUNIV,C'Y'                                                     
         MVC   UNIVBK,DBSELBK                                                   
*                                                                               
         CLI   DEMSTYLE,C'D'                                                    
         BE    SETDUNS                                                          
         MVC   MSASAVE(4),=C'MSA='                                              
         CLI   DBSELSRC,C'M'       FIX UP FOR BBM                               
         BNE   *+14                                                             
         MVC   MSASAVE(4),=C'CMA='                                              
         B     *+16                                                             
         MVC   TSASAVE(4),=C'TSA='                                              
         MVC   ADISAVE(4),=C'ADI='                                              
         SPACE 1                                                                
         MVC   UNIVLIST,UNIVCATS                                                
         MVC   UNIVLIST+2(1),DEMOS+2                                            
         MVC   UNIVLIST+5(1),DEMOS+2                                            
         MVC   UNIVLIST+8(1),DEMOS+2                                            
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
SETUNIV2 LM    R2,R4,UNIVS                                                      
         LA    R5,MSASAVE+4                                                     
         EDIT  (R2),(6,(R5)),ALIGN=LEFT                                         
         CLI   DBSELMED,C'M'       BBM FIXES                                    
         BE    SETUNIV3                                                         
         LA    R5,TSASAVE+4                                                     
         EDIT  (R3),(6,(R5)),ALIGN=LEFT                                         
         LA    R5,ADISAVE+4                                                     
         EDIT  (R4),(6,(R5)),ALIGN=LEFT                                         
SETUNIV3 DS    0H                                                               
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         ZIC   R0,NDEMOS                                                        
         SPACE 1                                                                
SETANYS  CLI   0(R2),1             CHECK TO SEE WHICH AREAS WERE                
         BNE   NOMSAU              REQUESTED. THIS AFFECTS UNIV DISPLAY         
         MVI   ANYMSA,C'Y'                                                      
         OC    UNIVS(4),UNIVS                                                   
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOMSAU   CLI   0(R2),2                                                          
         BNE   NOTSAU                                                           
         MVI   ANYTSA,C'Y'                                                      
         OC    UNIVS+4(4),UNIVS+4                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOTSAU   CLI   0(R2),3                                                          
         BNE   NOADIU                                                           
         MVI   ANYADI,C'Y'                                                      
         OC    UNIVS+8(4),UNIVS+8                                               
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
NOADIU   LA    R2,3(R2)                                                         
         BCT   R0,SETANYS                                                       
SETUNIVX SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XMOD1                                                                  
         SPACE 1                                                                
UNIVCATS DC    X'01',C'U',X'00'    MSA                                          
         DC    X'02',C'U',X'00'    TSA                                          
         DC    X'03',C'U',X'00'    ADI                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT UNIVERSES FOR MULTIPLE DEMOS                   
         SPACE 3                                                                
*              INPUT               DEMOS NDEMOS                                 
*              OUTPUTS             DUNBLOCK                                     
         SPACE 1                                                                
SETDUNS  MVC   UNIVLIST,DEMOS                                                   
         MVI   UNIVLIST+1,C'U'                                                  
         MVI   UNIVLIST+4,C'U'                                                  
         MVI   UNIVLIST+7,C'U'                                                  
         MVI   UNIVLIST+10,C'U'                                                 
         GOTO1 DEMOUT,DMCB,(C'L',UNIVLIST),DBLOCK,UNIVS                         
         OC    UNIVS(12),UNIVS     SET TO RETRY IF NO UNIV FOR THIS REC         
         BNZ   *+8                                                              
         MVI   ANYUNIV,C'N'                                                     
         SPACE 1                                                                
         LA    R2,DEMOS                                                         
         LA    R3,UNIVS                                                         
         LA    R4,DUNBLOCK                                                      
         ZIC   R5,NDEMOS                                                        
         MVC   DUNBLOCK,SPACES                                                  
         SPACE 1                                                                
SETDUNS2 GOTO1 DEMOCON,DMCB,(R2),(5,WORK),DBLOCK                                
         MVC   0(5,R4),WORK        W1834                                        
         MVI   5(R4),C'='          W1834=                                       
         EDIT  (4,(R3)),(6,6(R4)),ALIGN=LEFT                                    
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,SETDUNS2                                                      
         B     SETUNIVX                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
STATENTD DSECT                                                                  
STATENT  DS    0CL10                                                            
STATCALL DS    CL5                                                              
STATFORM DS    CL4                                                              
         DS    C                                                                
         SPACE 2                                                                
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
         SPACE 1                                                                
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
*              MY LITTLE NMOD AREA                                              
         SPACE 1                                                                
MYD      DSECT                                                                  
DPTLIST  DS    15CL36                                                           
RANKNUMS DS    15XL40                                                           
COMBLIST DS    10XL49                                                           
LNCOMB   EQU   49                                                               
CUMESW   DS    C                                                                
THISBUFF DS    22XL(L'THISREC)                                                  
*MARKET  DS    C                                                                
SVMKTH5  DS    C                                                                
SVMKTNUM DS    H                                                                
MKTPNTR  DS    F                                                                
NEWSTAT  DS    C                                                                
CMBCNTR  DS    C                                                                
         SPACE 2                                                                
DBRADIC  DS    CL128                                                            
DBFORMC  DS    900C                                                             
MYDX     EQU   *                                                                
         SPACE 2                                                                
*              DSECT TO COVER A DAYPART ENTRY                                   
         SPACE 1                                                                
DPED     DSECT                                                                  
DPENTRY  DS    0CL36                                                            
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
         EJECT                                                                  
*              BUFFER ARRANGEMENT                                               
         SPACE 3                                                                
*              THERE ARE 15 DAYPART BUFFERS                                     
*                                                                               
*              EACH OF THESE IS COMPOSED OF UP TO 40 STATION LINES              
*              AFTER EACH STATION IS PROCESSED, EACH DAYPART                    
*              BUFFER WILL BE SORTED WITH THE BEST UP FRONT                     
         SPACE 2                                                                
*              DSECT TO COVER RECORDS IN BUFF                                   
         SPACE 1                                                                
BUFFD    DSECT                                                                  
BUFFREC  DS    0CL10                                                            
BUFFSTAT DS    XL1                 STATION NUMBER                               
BUFFDEMS DS    XL8                 UP TO 4 DEMOS                                
BUFFSAN  DS    C                   SPECIAL ANNOUNCEMENT                         
         SPACE 3                                                                
*              DSECT TO COVER RECORDS IN THISBUFF                               
         SPACE 3                                                                
THISD    DSECT                                                                  
THISREC  DS    0CL24                                                            
THISWT   DS    F                   WEIGHT                                       
THISDEMS DS    4F                  UP TO 4 DEMOS                                
THISNOA  DS    C                   NOT ON AIR IND                               
THISANN  DS    C                   SPECIAL ANNOUNCEMENT IND                     
         DS    CL2                 SPARE                                        
*                                                                               
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCUSDP                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTLST                                                     
       ++INCLUDE SPGENCOMBO                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDTWABLDD                                                      
MARKETD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPRESWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT TO COVER SCREEN                                            
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESE6D                                                       
         EJECT                                                                  
*              DSECT TO COVER SCREEN MARKET SELECT                              
         SPACE 3                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESA3D                                                       
         EJECT                                                                  
*              STORAGE FOR RANKER                                               
         SPACE 3                                                                
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MAXRANK  DS    XL1                 MAX STATIONS TO RANK                         
SVMAXRNK DS    XL1                 SAVE MAX RANK IN CASE OF LIST                
WIDEOPT  DS    XL1                 WIDTH 1-3 = 80/110/132                       
TRACEOPT DS    CL1                 Y=TRACE DEMAND HOOKS                         
RANKOPT  DS    CL1                 Y=PRINT RANK NUMBERS                         
FORMOPT  DS    CL1                 Y=PRINT STATION FORMAT                       
WORSTOPT DS    CL1                 Y=SHOW WORST STATIONS                        
SPILLOPT DS    CL1                 Y=SPILL ONLY N=NO SPILL                      
EXPANOPT DS    CL1                 Y=SHOW STATIONS INCLUDED IN COMBOS           
SIMULOPT DS    CL1                 N=SUPPRESS SIMULCAST STATIONS                
HOUROPT  DS    CL1                 Y=USE HOURLY DATA ONLY                       
CALCOPT  DS    CL1                 Y=USE IMPRESSION DATA                        
SEPAOPT  DS    C                   Y=PRINT MULTIPLE DEMOS                       
ADDROPT  DS    C                   Y=PRINT AGENCY ADDRESS                       
LONGOPT  DS    C                   Y=PRINT REPORT 25 MORE LINES                 
TWANUM   DS    C                                                                
STACTSW  DS    C                                                                
COMAND2  DS    CL8                                                              
MYBASE   DS    A                                                                
RELO2    DS    F                                                                
MYBASE2  DS    F                                                                
ATHISBUF DS    F                                                                
         SPACE 1                                                                
MSASAVE  DS    CL10                ROOM FOR MSA=NNNNNN                          
TSASAVE  DS    CL10                ROOM FOR TSA=NNNNNN                          
ADISAVE  DS    CL10                ROOM FOR ADI=NNNNNN                          
ANYMSA   DS    CL1                 SET TO Y IF MSA SPECIFIED                    
ANYTSA   DS    CL1                 SET TO Y IF TSA SPECIFIED                    
ANYADI   DS    CL1                 SET TO Y IF ADI SPECIFIED                    
         ORG   MSASAVE                                                          
DUNBLOCK DS    CL48                OR UP TO 4 W1834=123456                      
ANYUNIV  DS    CL1                 Y=UNIVERSES DONE                             
NDPTS    DS    XL1                 NUMBER OF DAYPARTS                           
THISTYPE DS    CL1                 C(USTOM) OF S(TANDARD)                       
FIRSTDP  DS    CL1                 SWITCH FOR PRINTING                          
DISP     DS    F                   DISPLACEMENT INTO PRINT LINE                 
DPTGAP   DS    F                   GAP BETWEEN DAYPARTS                         
DPTWIDTH DS    F                   WIDTH OF EACH DAYPART CHUNK                  
DPTSLEFT DS    F                   NUMBER OF DAYPARTS STILL TO PRINT            
UP       DS    F                   NUMBER OF DAYPARTS                           
MKTNUM   DS    H                   MARKET NUMBER SELECTED                       
AMYD     DS    A                   ADDRESS OF MY STORAGE                        
ATHISB2  DS    A                   ADDRESS OF EXTRA BUFFER                      
ASTATS   DS    A                   ADDRESS OF STATION LIST                      
WSTATS   DS    F                   WIDTH OF STATION LIST                        
ABEST    DS    A                   ADDRESS OF FIRST DAYPART BEST                
AWORST   DS    A                   ADDRESS OF FIRST DAYPART WORST               
ADPTBEST DS    A                   ADDRESS OR FIRST FOR THIS DP SET             
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
STATNUM  DS    XL1                 NUMBER OF STATION BEING PROCESSED            
DPTNUM   DS    XL1                 NUMBER OF DAYPART BEING PROCESSED            
SAVDPTN  DS    XL1                 FIRST DAYPART BEING PROCESSED                
*HISBUFF DS    15XL(L'THISREC)     BUFFER FOR STATION IN PROGRESS               
THISLINE DS    XL(L'THISREC)       TEMPORARY BUFFER FOR WEIGHTING               
UNIVLIST DS    XL16                DEMOUT LIST FOR UNIVS                        
UNIVS    DS    4F                  UNIVS FOR MSA TSA ADI                        
RUNIVS   DS    4F                  REAL UNIVS FOR MULTIPLE BOOKS                
LASTVAL  DS    XL2                 VALUE DURING RANKING                         
DEMSTYLE DS    CL1                 D=DEMO C=CATEGORY                            
STASTYLE DS    CL1                 M=MARKET L=LIST C=CUSTOM                     
UNIVBK   DS    CL2                 UNIVERSE BOOK                                
NCOMBOS  DS    CL1                 NUMBER OF COMBO STATIONS                     
PREVSTAT DS    CL5                                                              
MEDTYPE  DS    C                                                                
SRCTYPE  DS    C                                                                
BOOKTYPE DS    C                                                                
CITYCODE DS    CL3                                                              
MISCOMMA DS    C                                                                
OUTAREA  DS    A                                                                
STALIST  DS    C                                                                
USEBUFF  DS    C                   USING BUFFER?                                
CMB_B4   DS    C                   COMBO BEFORE STATION?                        
FIRSTTM  DS    C                   FIRST TIME PRINTING?                         
PRNTD    DS    C                   PRINTED NEW MARKET NUMBER                    
STATLEN  DS    C                   STATIONS LEN                                 
NMARKET  DS    X                                                                
RUNIFLG  DS    C                   REAL UNIVERSE FLAG                           
NCMBDPT  DS    C                                                                
DPTTABL  DS    A                   TABLE OF DAYPART NAMES                       
PRDRSLT  DS    X                   PRODUCT RESULT OF MARKET X DEMOS             
NRELO    DS    A                   ANOTHER RELOCATABLE                          
VSUBR07A DS    A                                                                
         SPACE 1                                                                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPRES26   02/24/15'                                      
         END                                                                    
