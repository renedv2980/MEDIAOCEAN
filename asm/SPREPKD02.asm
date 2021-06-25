*          DATA SET SPREPKD02  AT LEVEL 102 AS OF 05/02/19                      
*PHASE SPKD02A                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'SBS DSTA DELTA'                                                 
         SPACE 1                                                                
SPKD02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPKD02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPKD02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPKD02,RB,RC                                                     
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    MAIN00                                                           
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         J     XIT                                                              
NO       LTR   RB,RB               SET CC NOT EQUAL                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REGARDLESS OF DATES                                                           
***********************************************************************         
MAIN00   DS    0H                                                               
         LHI   R6,IO-SPKD02                                                     
         AR    R6,RB               ALWAYS KILLS ME IN DADDS                     
         ST    R6,AREC             IF I FORGET THIS                             
*                                                                               
         CLI   QOPT4,C'Y'          USING EMAIL?                                 
         BNE   MAIN10                                                           
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         MVC   MYVSMTP,VSMTP                                                    
         DROP  RE                                                               
         GOTO1 MYVSMTP,DMCB,('SMTPAINI',JESMAIL)                                
         GOTOR MYVSMTP,DMCB,('SMTPATCS',SMTPTO),(L'SMTPSUBJ,SMTPSUBJ), +        
               (0,SMTPCC),(0,SMTPBCC)                                           
         MVI   SMTPLINS,10                                                      
*                                                                               
MAIN10   OPEN  (DLTADSTA,INPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GET   DLTADSTA,IO3        SHOW 1ST LINE (FILE IDENTIFIER)              
         MVC   P,IO3                                                            
         BRAS  RE,EMAILLIN                                                      
*****    MVI   LINE,1           NEED THE FIRST PAGE BREAK FOR ARC               
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,MYSUB                                                         
*                                                                               
MAIN50   CLOSE DLTADSTA                                                         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT4,C'Y'          USING EMAIL?                                 
         BNE   MAINX                                                            
         MVC   SMTPLINE,SPACES                                                  
         MVC   SMTPLINE(19),=C'** END OF REPORT **'                             
         GOTO1 MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
         GOTO1 MYVSMTP,DMCB,('SMTPASND',0)    SEND OUT THE EMAIL                
*                                                                               
         BRAS  RE,EMAILSKP                                                      
*                                                                               
         GOTO1 MYVSMTP,DMCB,('SMTPAEND',0)                                      
*                                                                               
MAINX    GOTO1 AENDREQ                                                          
         EJECT                                                                  
***********************************************************************         
* MYSUB SUBROUTINE                                                              
***********************************************************************         
MYSUB    ST    RE,ADDSAVE                                                       
         L     RE,UTL                                                           
         MVI   4(RE),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'UGENDIR UGENFIL X',DMWORK,0                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,ACOMFACS                                                      
         USING COMFACSD,R2                                                      
         MVC   ASCANNER,CSCANNER-COMFACSD(R2)                                   
*                                                                               
         L     RF,CCALLOV-COMFACSD(R2)                                          
         XC    DMCB,DMCB           CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A1F' GET ADDR OF DDDAREREPS VIA CALLOV         
         GOTO1 (RF),DMCB           RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         BNE   *+6                 NO, IT'S IN 1ST PARAMETER                    
         DC    H'0'                YES, DIE HORRIBLY                            
         MVC   AREPIDS,DMCB        SAVE THAT ADDRESS FOR LATER                  
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)                                  
         GOTO1 DATCON,DMCB,(5,0),(3,FULL)                                       
         XR    R0,R0                                                            
         ICM   R0,7,FULL                                                        
         LNR   R0,R0                                                            
         STCM  R0,7,NDTTODAY       STORE NEGATED BINARY DATE                    
*                                                                               
         LA    R8,P                                                             
         USING PRTLINED,R8                                                      
*****************************************                                       
*    R5 IS ALWAYS POINTING TO DLTAINFO                                          
* &  R6 POINTS TO THE DSTA RECORD                                               
*****************************************                                       
         LA    R5,IO2                                                           
         USING DLTAINFO,R5                                                      
*                                                                               
MYSUBLP  BAS   RE,GTSTADLT         GET SBS STATION RECORD                       
         BNE   MYSUB100             NOT VALID, GET NEXT SBS STATION REC         
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING STAKEYD,R6                                                       
         MVI   STAKTYP,STAKTYPQ    DSTA RECORDS ARE X'005A'                     
         MVI   STAKMEDA,C'R'                                                    
         MVC   STAKSTIN,DLTCRSTA  LOOK FOR THE CURRENT CALLS                    
*                                                                               
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'GENDIR'),BIGKEYSV,    X        
               BIGKEY,0                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MYSUBDCK DS    0H                                                               
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   MYSUB010                                                         
         MVC   P(10),=C'KEYSAVE = '                                             
         MVC   P+10(L'BIGKEYSV),BIGKEYSV                                        
         GOTO1 REPORT                                                           
         MVC   P(10),=C'  KEY   = '                                             
         MVC   P+10(L'BIGKEY),BIGKEY                                            
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
MYSUB010 CLC   BIGKEY(STAKEFDA-STAKEY),BIGKEYSV  WE NEVER HAD THIS DSTA         
         BE    MYSUB020                                                         
         BAS   RE,ADDDSTA          WE'RE GOING TO HAVE TO ADD                   
         BNE   MYSUB100                                                         
*                                                                               
         CLC   DLTPRSTA,SPACES     DO WE HAVE PREVIOUS CALLS?                   
         BH    MYSUB070            YES, ANOTHER STEP TO PERFORM                 
         B     MYSUB100            NO, DONE WITH THIS STATION                   
*                                                                               
***********************************                                             
* CHECK OVERLAP DUPLICATES - DSTA WOULD BE THERE IF DUPLICATE                   
* SINCE WE STILL HAVE CHANGES FOR THE LAST 3 DAYS, WE WOULD EXPECT DSTA         
* RECORDS TO ALREADY BE OUT THERE. WHAT WE SHOULD EXPECT IS A DSTA THAT         
* DOES NOT MATCH THE CURRENT REP OF THE DELTA FILE                              
***********************************                                             
MYSUB020 CLC   DLTPRSTA,SPACES     DO WE HAVE PREVIOUS CALLS?                   
         BH    MYSUB050             YES!                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),(0,=C'GENFIL'),         X        
               BIGKEY+36,AREC,DMWORK                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         USING STAKEYD,R6                                                       
         LA    R6,STAFSTEL                                                      
         USING STAREPD,R6                                                       
*                                                                               
         CLC   STAREPCR,DLTCRDDS   CURRENT REP IS THE SAME?                     
         BNE   MYSUB035                                                         
*                                  DSTA W/ SAME EFF DATE ALREADY THERE?         
         CLC   BIGKEY+STAKEFDA-STAKEYD(3),DLTSTADD                              
         BE    MYSUB030             YES, NOTHING TO DO HERE, SEND ERROR         
*                                                                               
         LR    R1,R6               SEE IF DSTA RECORD HAS NEW CALLS             
MYSUB025 LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             NO NEW CALLS, SO NOTHING DIFFERENT           
         BE    MYSUB100            NOTHING TO DO FOR THIS SBS ENTRY             
*                                                                               
         CLI   0(R1),STASTACQ      X'20' - NEW STATION CALLS ELEM?              
         BNE   MYSUB025                                                         
         B     MYSUB040            YES WE HAVE ONE, NEW DSTA FOR SBS            
*                                                                               
MYSUB030 OI    EMAILFLG,WARNLIN+WARNFLG                                         
         MVC   P(11),=C'**WARNING *'                                            
         MVI   P+9,C'1'                    **WARNING1                           
         MVC   P+12(5),=C'CALLS'                                                
         MVC   P+18(5),DLTCRSTA                                                 
         MVC   P+24(5),=C'(EFF:'                                                
         MVI   P+37,C')'                                                        
         XR    R1,R1                                                            
         ICM   R1,7,DLTSTADD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+29)                                    
         MVC   P+39(15),=C'ALREADY EXISTS!'                                     
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     MYSUB100            YES, NOTHING TO DO HERE                      
****                                                                            
MYSUB035 CLC   DLTPRDDS,SPACES     DO WE HAVE A PREVIOUS REP?                   
         BH    MYSUB045            YES, TREAT AS A CHANGE TO DSTA               
* AT THIS POINT, SBS IS TRYING TO ADD A STATION THAT IS ALREADY A DSTA          
* REC, BUT THE CURRENT REP DOES NOT MATCH AND THERE IS NOT REP CHANGE           
*                                                                               
*  IF THE EFFECTIVE DATE ON THE DELTA MATCHES THE EFFECTIVE DATE OF THE         
*  DSTA RECORD, THEN WE SHOULD SEND AN ERROR.                                   
         CLC   BIGKEY+STAKEFDA-STAKEYD(3),DLTSTADD                              
         BNE   MYSUB040            OKAY TO ADD A NEW DSTA FOR THIS              
*                                                                               
         OI    EMAILFLG,WARNLIN+WARNFLG  CAN'T ADD A NEW DSTA AS THE            
         MVC   P(11),=C'**WARNING *'     THE REP IS DIFFERENT AND THERE         
         MVI   P+9,C'3'                  IS NO REP CHANGE                       
         MVC   P+12(5),=C'CALLS'                                                
         MVC   P+18(5),DLTCRSTA                                                 
         MVC   P+24(5),=C'(EFF:'                                                
         MVI   P+37,C')'                                                        
         XR    R1,R1                                                            
         ICM   R1,7,DLTSTADD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+29)                                    
         MVC   P+39(40),=C'ALREADY EXISTS, REP: XXX   DELTA: YYY !!'            
         MVC   P+60(L'STAREPCR),STAREPCR                                        
         MVC   P+73(L'DLTCRDDS),DLTCRDDS                                        
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     MYSUB100                                                         
*                                                                               
MYSUB040 BAS   RE,ADDDSTA          OTHERWISE NEW DSTA RECORD                    
         B     MYSUB100                                                         
*                                                                               
MYSUB045 BAS   RE,CHGDSTA                                                       
         B     MYSUB100                                                         
*                                                                               
***********************************                                             
* BECAUSE WE HAVE PREVIOUS CALLS, WE HAVE TO ADD A NEW DSTA FOR CURRENT         
*   AND CHANGE THE DSTA FOR THE PREVIOUS TO HAVE THE NEW CALLS                  
***********************************                                             
*                                            DSTA W/ EFF ALREADY THERE?         
MYSUB050 CLC   BIGKEY+STAKEFDA-STAKEYD(3),DLTSEFNG                              
         BH    MYSUB060                      NO, ADD CURR AND CHG PREV          
         MVC   P(11),=C'**WARNING *'                                            
         MVI   P+9,C'2'                    **WARNING2                           
         MVC   P+39(15),=C'ALREADY EXISTS!'                                     
         BE    MYSUB051                                                         
         MVI   P+9,C'3'                    **WARNING3                           
         MVC   P+39(24),=C'IS OLD. NEWER ONE EXISTS!'                           
                                                                                
MYSUB051 OI    EMAILFLG,WARNLIN+WARNFLG                                         
         MVC   P+12(5),=C'CALLS'                                                
         MVC   P+18(5),DLTCRSTA                                                 
         MVC   P+24(5),=C'(EFF:'                                                
         MVI   P+37,C')'                                                        
         GOTO1 DATCON,DMCB,(6,DLTSEFJU),(8,P+29)                                
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
***NOP   B     MYSUB070                                                         
         B     MYSUB100              IF ERROR, DON'T CHANGE PREVIOUS            
*                                                                               
MYSUB060 BAS   RE,ADDDSTA          CREATE DSTA RECORD FOR CURRENT               
         BNE   MYSUB100              IF ERROR, DON'T CHANGE PREVIOUS            
*                                                                               
*****                                                                           
* CHANGE DSTA FOR PREVIOUS TO HAVE NEW CALLS                                    
*****                                                                           
MYSUB070 XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING STAKEYD,R6                                                       
         MVI   STAKTYP,STAKTYPQ    DSTA RECORDS ARE X'005A'                     
         MVI   STAKMEDA,C'R'                                                    
         MVC   STAKSTIN,DLTPRSTA   MOST RECENT DSTA FOR PREV CALLS              
         MVC   STAKEFDA,DLTSTADD                                                
*                                                                               
MYSUB075 MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),(0,=C'GENDIR'),BIGKEYSV,    X        
               BIGKEY,0                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BNE   MYSUB080                                                         
         MVC   P(10),=C'KEYSAVE = '                                             
         MVC   P+10(L'BIGKEYSV),BIGKEYSV                                        
         GOTO1 REPORT                                                           
         MVC   P(10),=C'  KEY   = '                                             
         MVC   P+10(L'BIGKEY),BIGKEY                                            
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
MYSUB080 CLC   BIGKEY(STAKEFDA-STAKEY),BIGKEYSV  DSTA FOR PREV EXISTS?          
         BNE   MYSUB085                          NO                             
*                                            YES, FOUND A RECORD                
         CLI   BIGKEYSV+STAKEFDA-STAKEY,0    DID WE RD W/EFFDATE?               
         BNE   MYSUB082                       YES                               
         MVC   P(11),=C'**WARNING *'                                            
         MVI   P+9,C'4'                       **WARNING4                        
         B     MYSUB087                       NO - JUST UPDATE LATEST           
MYSUB082 CLC   STAKEFDA,DLTSTADD              YES - MATCH ON EFFDATE?           
         BE    MYSUB090                        YES                              
MYSUB083 MVC   BIGKEY,BIGKEYSV                READ STATION AGAIN                
         XC    STAKEFDA,STAKEFDA              BUT W/O EFF DATE                  
         B     MYSUB075                                                         
*                                                                               
MYSUB085 CLI   BIGKEYSV+STAKEFDA-STAKEY,0     DID WE RD W/EFFDATE?              
         BNE   MYSUB083                        YES, GO READ W/O EFFDATE         
*                                                                               
         MVC   P(11),=C'**WARNING *'                                            
         MVI   P+9,C'5'                       **WARNING5                        
MYSUB087 MVC   P+12(14),=C'CALLS CHG FROM'                                      
         MVC   P+27(L'DLTPRSTA),DLTPRSTA                                        
         MVC   P+33(2),=C'TO'                                                   
         MVC   P+36(L'DLTCRSTA),DLTCRSTA                                        
         MVC   P+41(7),=C', CALLS'                                              
         MVC   P+49(L'DLTPRSTA),DLTPRSTA                                        
         MVC   P+55(10),=C'NOT FOUND!'                                          
         CLI   P+9,C'4'                        WARNING4?                        
         BNE   MYSUB088                                                         
         MVC   P+55(5),=C'(EFF:'                                                
         MVI   P+68,C')'                                                        
         ICM   R1,7,DLTSTADD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+60)                                    
         MVC   P+70(10),=C'NOT FOUND!'                                          
*                                                                               
MYSUB088 OI    EMAILFLG,WARNLIN+WARNFLG                                         
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         CLI   P+9,C'4'            WARNING4?                                    
         BE    MYSUB089                                                         
         GOTO1 REPORT                                                           
         B     MYSUB100                                                         
MYSUB089 GOTO1 REPORT                                                           
*                                                                               
MYSUB090 GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),(0,=C'GENFIL'),         X        
               BIGKEY+36,AREC,DMWORK                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC                                                          
         BAS   RE,CALLCHNG         CALL LETTER CHANGE                           
*                                                                               
MYSUB100 TM    EMAILFLG,SKIPLIN+WARNLIN   ANY WARNING OR LINES SKIPPED?         
         BZ    MYSUBLP                                                          
         NI    EMAILFLG,X'FF'-SKIPLIN-WARNLIN                                   
         MVC   P+10(132),IO3       PRINT THE INPUT LINE BEFORE                  
         BRAS  RE,EMAILLIN           MOVING ON TO THE NEXT                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         BRAS  RE,EMAILLIN           SKIP A LINE                                
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     MYSUBLP                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A DDS DSTA RECORD                                              
***********************************************************************         
ADDDSTA  NTR1                                                                   
         L     R0,AREC                                                          
         LA    R1,4000                                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AREC                                                          
         USING STAKEYD,R6                                                       
         MVI   STAKTYP,STAKTYPQ DSTA RECORD START WITH X'005A'                  
         MVI   STAKMEDA,C'R'          RADIO HERE                                
         MVC   STAKSTIN,DLTCRSTA                                                
*                                                                               
         MVC   STAKEFDA,DLTSTADD      STATION ADD DATE NEGATED                  
         OC    DLTSEFNG,DLTSEFNG      ANY STA EFF DATE TO WORRY ABOUT?          
         BZ    ADSTA005                                                         
         CLC   DLTSTADD,DLTSEFNG      NEGATED: STA ADD DATE < STA EFF           
         BNH   *+10                                                             
         MVC   STAKEFDA,DLTSEFNG      YES, BNH IS BECAUSE OF NEGATED            
*                                                                               
ADSTA005 MVI   STARECLN+1,STAELDQ     L(REC) BEFORE ANY ELEMS                   
*                                                                               
         LA    R2,STAFSTEL                                                      
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING STAREPD,R7                                                       
         MVI   STAREPC,STAREPCQ        X'10' - REP ELEMENT                      
         MVI   STAREPLN,STAREPLQ                                                
         MVC   STAREPCR,DLTCRDDS                                                
         DROP  R7                                                               
***  ELEM START IS X'002A' INTO THE RECORD                                      
***  RECORD LEN IS X'0020' INTO THE RECORD                                      
***  MAX LENGTH OF GENFIL RECORDS ARE 2000 BYTES (X'07D0')                      
         GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(R2),=X'002A002007D0'               
         LA    R2,STAREPLQ(R2)     BUMP R2 FOR NEXT INSERTION POINT             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING DATTIMD,R7                                                       
         MVI   DATTIM,DATTIMLQ        X'D1' - DAT/TIME STAMP ELEM               
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    ADSTA010                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
*                                                                               
ADSTA010 GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMCDT)                                
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMCTM                                                    
         MVC   DATTMGDT(L'DATTMCDT+L'DATTMCTM),DATTMCDT                         
         DROP  R7                                                               
         GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(R2),=X'002A002007D0'               
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'STAKEY),STAKEY                                          
*                                                                               
* CHECK IF DSTA ALREADY ADDED IN THIS RUN                                       
*                                                                               
         L     R2,=A(IO4-SPKD02)     LIST OF DSTA'S KEYS ADDED THIS RUN         
         AR    R2,RB                                                            
         L     RF,=A(IO4END-SPKD02)                                             
         AR    RF,RB                                                            
ADSTA012 CR    R2,RF                                                            
         BL    *+6                                                              
         DC    H'0'                MAKE IO4 BIGGER                              
*                                                                               
         OC    0(STAKEFQ,R2),0(R2)                                              
         BZ    ADSTA015                                                         
         CLC   0(STAKEFQ,R2),STAKEY+STAKMEDA-STAKEY   ALREADY ADDED?            
         BE    ADSTA014                                 YES, ERROR              
         AHI   R2,STAKEFQ                                                       
         B     ADSTA012                                                         
*                                                                               
ADSTA014 OI    EMAILFLG,WARNLIN+WARNFLG         DSTA ALREADY ADDED ON           
         MVC   P(11),=C'**WARNING *'             THIS RUN, SEND ERROR           
         MVI   P+9,C'6'                    **WARNING6                           
         MVC   P+12(7),=C'STATION'                                              
         MVC   P+20(5),DLTCRSTA                                                 
         MVC   P+26(5),=C'(EFF:'                                                
         MVI   P+39,C')'                                                        
         ICM   R1,7,STAKEFDA                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+31)                                    
         MVC   P+41(32),=C'PREVIOUSLY ADDED IN THIS UPDATE!'                    
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     NO                                                               
STAKEFQ  EQU   L'STAKEFDA+STAKEFDA-STAKMEDA                                     
*                                                                               
ADSTA015 XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'STAKEY),STAKEY                                          
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   ADSTA020                                                         
         GOTO1 =V(PRTREC),DMCB,(C'E',AREC),(42,32),PRINT,HEXOUT                 
*                                                                               
ADSTA020 DS    0H                     MAKE SURE KEY IS NOT THERE ALRDY          
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),(0,=C'GENDIR'),BIGKEYSV,    X        
               BIGKEY,0                                                         
         CLI   DMCB+8,0               ALREADY THERE                             
         JE    ADSTA027               CAN'T ADD IT AGAIN                        
*                                                                               
ADSTA025 XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'STAKEY),STAKEY                                          
         CLI   QOPT3,C'Y'                                                       
         BE    ADSTA030                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'ADDREC'),(0,=C'GENFIL'),BIGKEY,AREC, X        
               DMWORK,0                                                         
         CLI   DMCB+8,0                                                         
         BE    ADSTA030                                                         
         CLI   DMCB+8,X'20'           DUPLICATE ON ADD?                         
         JNE   *+2                    NO, SOME OTHER HORRIBLE ERROR             
* STATION WITH EFF DATE ALREADY EXISTS                                          
ADSTA027 OI    EMAILFLG,WARNLIN+WARNFLG                                         
         MVC   P(11),=C'**WARNING *'                                            
         MVI   P+9,C'1'                    **WARNING1                           
         MVC   P+12(5),=C'CALLS'                                                
         MVC   P+18(5),DLTCRSTA                                                 
         MVC   P+24(5),=C'(EFF:'                                                
         MVI   P+37,C')'                                                        
         XR    R1,R1                                                            
         ICM   R1,7,DLTSTADD                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+29)                                    
         MVC   P+39(15),=C'ALREADY EXISTS!'                                     
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     NO                  YES, NOTHING TO DO HERE                      
*                                                                               
ADSTA030 MVC   0(STAKEFQ,R2),STAKEY+STAKMEDA-STAKEY                             
*                                                                               
         MVC   P(23),=C'ADDING DSTA RECORD FOR:'                                
         MVC   P+24(L'DLTCRSTA),DLTCRSTA                                        
         MVC   P+30(5),=C'(EFF:'                                                
         MVI   P+43,C')'                                                        
                                                                                
         ICM   R1,7,STAKEFDA                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+35)                                    
***      GOTO1 DATCON,DMCB,(6,DLTSEFJU),(8,P+35)                                
         MVC   P+45(8),=C'DDS REP:'                                             
         MVC   P+54(3),DLTCRDDS                                                 
         OC    DLTREFJU,DLTREFJU                                                
         BZ    ADSTA040                                                         
         MVC   P+58(5),=C'(EFF:'                                                
         MVI   P+71,C')'                                                        
         GOTO1 DATCON,DMCB,(6,DLTREFJU),(8,P+63)                                
ADSTA040 BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE A DDS DSTA RECORD (REP CHANGE)                              
***********************************************************************         
CHGDSTA  NTR1                                                                   
         L     R6,AREC                                                          
         USING STAKEYD,R6                                                       
         LA    R2,STAFSTEL         X'10' - REP ELEMENT IS 1ST ELEM              
         USING STAREPD,R2                                                       
         CLC   STAREPCR,DLTPRDDS   DSTA CURRENT REP SHOULD BE SBS PREV          
         BE    CDSTA010                                                         
         MVC   P(21),=C'REP CHANGE FOR DSTA: '                                  
         MVC   P+21(5),DLTCRSTA                                                 
         MVC   P+27(14),=C'SBS PREV REP: '                                      
         MVC   P+41(3),DLTPRDDS                                                 
         MVC   P+46(25),=C'DOES NOT MATCH DSTA REP: '                           
         MVC   P+71(3),STAREPCR                                                 
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT              REPORT AND PROCESS CHANGE ANYWAY             
*                                                                               
CDSTA010 MVC   STAREPPR,STAREPCR   DSTA PREV IS WHAT WAS CURRENT                
         MVC   DLTPRDDS,STAREPCR    DON'T CARE WHAT SBS HAD FOR PREV            
         MVC   STAREPCR,DLTCRDDS    AND WHAT IS CURRENT IS SBS CURRENT          
         MVC   STAREPED,DLTREFJU                                                
*                                                                               
         CLC   DLTREFJU,JDTTODAY   FLAG TO USER IF DATE < TODAY'S DATE          
         BNL   CDSTA015                                                         
         MVC   P(21),=C'REP CHANGE FOR CALLS:'                                  
         MVC   P+22(5),DLTCRSTA                                                 
         MVC   P+29(9),=C'EFFECTIVE'                                            
         GOTO1 DATCON,DMCB,(6,DLTREFJU),(5,P+39)                                
         MVC   P+48(14),=C'IS IN THE PAST'                                      
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT              REPORT AND PROCESS CHANGE ANYWAY             
*                                                                               
CDSTA015 XC    ELEM,ELEM           BUILD REP CHANGE HISTORY ELEM                
         LA    R7,ELEM                                                          
         USING STARHELD,R7                                                      
         MVI   STARHEL,STARHELQ    X'60'                                        
         MVI   STARHLEN,STARHLNQ                                                
         XR    RF,RF                                                            
         ICM   RF,15,DLTREFJU                                                   
         SRL   RF,4                REMOVE SIGN FROM PACKED JULIAN               
         STCM  RF,7,STARHEDT                                                    
         MVC   STARHGRP,DLTCRDDS                                                
         MVC   STARHLRP,DLTPRDDS                                                
         DROP  R7                                                               
*                                                                               
         XR    RF,RF               KEEPING HISTORY OF CHANGES                   
         LA    R2,STAFSTEL         R2 = A(INSERTION POINT)                      
CDSTA020 CLI   0(R2),0                                                          
         BE    CDSTA030                                                         
         CLI   0(R2),STARHELQ      X'60'                                        
         BNL   CDSTA030                                                         
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     CDSTA020                                                         
*                                                                               
***  ELEM START IS X'002A' INTO THE RECORD                                      
***  RECORD LEN IS X'0020' INTO THE RECORD                                      
***  MAX LENGTH OF GENFIL RECORDS ARE 2000 BYTES (X'07D0')                      
CDSTA030 GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(C'R',(R2)),               X        
               =X'002A002007D0'                                                 
         CLI   DMCB+8,C'R'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING DATTIMD,R7                                                       
         MVI   DATTIM,DATTIMLQ        X'D1' - DAT/TIME STAMP ELEM               
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    CDSTA040                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
*                                                                               
CDSTA040 GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMCDT)                                
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMCTM                                                    
         MVC   DATTMGDT(L'DATTMCDT+L'DATTMCTM),DATTMCDT                         
         DROP  R7                                                               
*                                                                               
         XR    RF,RF               ADDING/CHANGING DATE/TIME STAMP ELEM         
         LA    R2,STAFSTEL         R2 = A(INSERTION POINT)                      
CDSTA050 CLI   0(R2),0                                                          
         BE    CDSTA060                                                         
         CLI   0(R2),DATTIMLQ      X'D1' - DATE/TIME STAMP ELEM                 
         BNL   CDSTA060                                                         
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     CDSTA050                                                         
*                                                                               
CDSTA060 CLI   0(R2),DATTIMLQ      X'D1' - DATE/TIME STAMP ELEM                 
         BNE   CDSTA070                                                         
         USING DATTIMD,R2                                                       
         MVC   DATTMGDT(L'DATTMGDT+L'DATTMGTM),DATTMGDT-DATTIMD+ELEM            
         B     CDSTA080            OVERWRITE ONLY CHANGE INFORMATION            
         DROP  R2                                                               
*                                                                               
CDSTA070 GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(C'R',(R2)),               X        
               =X'002A002007D0'                                                 
         CLI   DMCB+8,C'R'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CDSTA080 XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'STAKEY),STAKEY                                          
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   CDSTA085                                                         
         GOTO1 =V(PRTREC),DMCB,(C'E',AREC),(42,32),PRINT,HEXOUT                 
*                                                                               
CDSTA085 CLI   QOPT3,C'Y'          DO NOT WRITE TO THE FILE?                    
         BE    CDSTA090            ABSOLUTELY NOT!!                             
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),(0,=C'GENFIL'),BIGKEY,AREC, X        
               DMWORK,0                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CDSTA090 MVC   P(22),=C'*** CHANGING DSTA *** '                                 
         MVC   P+24(L'DLTCRSTA),DLTCRSTA                                        
         MVC   P+32(8),=C'DDS REP:'                                             
         MVC   P+41(3),DLTCRDDS                                                 
         MVC   P+46(9),=C'EFFECTIVE'                                            
         GOTO1 DATCON,DMCB,(6,DLTREFJU),(5,P+56)                                
         MVC   P+65(10),=C'PREV REP: '                                          
         MVC   P+75(3),DLTPRDDS                                                 
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT NEW CALL LETTERS EFFECTIVE FOR A DSTA RECORD                   
***********************************************************************         
CALLCHNG NTR1                                                                   
         XC    ELEM,ELEM           BUILD THE NEW STATION ELEMENT                
         LA    R2,ELEM                                                          
         USING STASTAD,R2                                                       
         MVI   STASTAC,STASTACQ    X'20' - NEW STATION EL CODE                  
         MVI   STASTALN,STASTALQ                                                
         MVC   STASTA,DLTCRSTA                                                  
         MVC   STASTADT,DLTSEFJU                                                
         DROP  R2                                                               
*                                                                               
         L     R6,AREC                                                          
         USING STAKEYD,R6                                                       
         XR    R0,R0                                                            
         LA    R2,STAFSTEL         X'10' - REP ELEMENT IS 1ST ELEM              
CCHNG010 CLI   0(R2),0             ANY NEW STATION ELEMENT?                     
         BE    CCHNG015                                                         
         CLI   0(R2),STASTACQ      X'20' - NEW STATION ELEM                     
         BNL   CCHNG015                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CCHNG010                                                         
*                                                                               
CCHNG015 CLI   0(R2),STASTACQ      ADDING OR CHANGING?                          
         BNE   CCHNG020             ADDING ELEM AT (R2)                         
         MVC   0(STASTALQ,R2),ELEM   CHANGING, OVERWRITE OLD ELEMENT            
         B     CCHNG030                                                         
*                                                                               
***  ELEM START IS X'002A' INTO THE RECORD                                      
***  RECORD LEN IS X'0020' INTO THE RECORD                                      
***  MAX LENGTH OF GENFIL RECORDS ARE 2000 BYTES (X'07D0')                      
CCHNG020 GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(C'R',(R2)),               X        
               =X'002A002007D0'                                                 
         CLI   DMCB+8,C'R'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CCHNG030 XC    ELEM,ELEM                                                        
         LA    R7,ELEM                                                          
         USING DATTIMD,R7                                                       
         MVI   DATTIM,DATTIMLQ        X'D1' - DAT/TIME STAMP ELEM               
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    CCHNG040                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
*                                                                               
CCHNG040 GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMCDT)                                
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMCTM                                                    
         MVC   DATTMGDT(L'DATTMCDT+L'DATTMCTM),DATTMCDT                         
         DROP  R7                                                               
*                                                                               
         XR    RF,RF               ADDING/CHANGING DATE/TIME STAMP ELEM         
         LA    R2,STAFSTEL         R2 = A(INSERTION POINT)                      
CCHNG050 CLI   0(R2),0                                                          
         BE    CCHNG060                                                         
         CLI   0(R2),DATTIMLQ      X'D1' - DATE/TIME STAMP ELEM                 
         BNL   CCHNG060                                                         
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     CCHNG050                                                         
*                                                                               
CCHNG060 CLI   0(R2),DATTIMLQ      X'D1' - DATE/TIME STAMP ELEM                 
         BNE   CCHNG070                                                         
         USING DATTIMD,R2                                                       
         MVC   DATTMGDT(L'DATTMGDT+L'DATTMGTM),DATTMGDT-DATTIMD+ELEM            
         B     CCHNG080            OVERWRITE ONLY CHANGE INFORMATION            
         DROP  R2                                                               
*                                                                               
CCHNG070 GOTO1 RECUP,DMCB,(X'FE',AREC),ELEM,(C'R',(R2)),               X        
               =X'002A002007D0'                                                 
         CLI   DMCB+8,C'R'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CCHNG080 XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'STAKEY),STAKEY                                          
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   CCHNG085                                                         
         GOTO1 =V(PRTREC),DMCB,(C'E',AREC),(42,32),PRINT,HEXOUT                 
*                                                                               
CCHNG085 CLI   QOPT3,C'Y'          DO NOT WRITE TO THE FILE?                    
         BE    CCHNG090            DO NOT                                       
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),(0,=C'GENFIL'),BIGKEY,AREC, X        
               DMWORK,0                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CCHNG090 MVC   P(21),=C'*** NEW CALLS FOR ***'                                  
         MVC   P+22(L'DLTPRSTA),DLTPRSTA                                        
         MVC   P+28(4),=C'(EFF'                                                 
         MVI   P+41,C')'                                                        
         ICM   R1,7,STAKEFDA                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,FULL                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(8,P+33)                                    
         MVC   P+43(8),=C'WILL BE:'                                             
         MVC   P+52(L'DLTCRSTA),DLTCRSTA                                        
         MVC   P+58(5),=C'(EFF:'                                                
         MVI   P+71,C')'                                                        
         GOTO1 DATCON,DMCB,(6,DLTSEFJU),(5,P+63)                                
         BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT STATION INFORMATION FROM RECORD READ FROM THE SBS          
* STATION FILE                                                                  
* FORMAT : DLTCRSTA,DLTSEFJU/DLTSEFNG,DLTSTADD,DLTPRSTA,                        
*         DLTCRTRD,DLTCRCMP,DLTREFJU,DLTPRTRD,DLTPRCMP                          
*                                                                               
* NOTE: PROTECT US FROM KATZ, MFS, AND ANY OTHER FROM ADDING STREAMING          
*       RADIO DSTA RECORDS.                                                     
***********************************************************************         
GTSTADLT NTR1                                                                   
         GET   DLTADSTA,IO3                                                     
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   GTDLT010                                                         
         MVC   P(7),=C'DELTA: '                                                 
         MVC   P+7(126),IO3                                                     
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
GTDLT010 XC    IO3HEAD,IO3HEAD     EXTRACT WHAT WE READ FROM FILE               
         LA    RE,IO3                                                           
         LA    RF,132(RE)                                                       
GTDLT013 CLI   0(RF),C' '          SQUASH ALL SPACES FROM END SO WE GET         
         BH    GTDLT016              THE CORRECT E-O-L FOR SCANNER              
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BCT   RF,GTDLT013                                                      
*                                                                               
GTDLT016 LA    RF,1(RF)                                                         
         SR    RF,RE                                                            
         STC   RF,IO3HEAD+5                                                     
*                                                                               
         GOTO1 ASCANNER,DMCB,IO3HEAD,(10,BLOCK)   10 INPUTS                     
*   WE'RE USING IO2 TO STORE ALL THE INFORMATION TEMPORARILY                    
         LA    RE,IO2              CLEAR THE IO AREA                            
         LA    RF,1000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,IO2                                                           
         USING DLTAINFO,R5                                                      
***************                                                                 
* STATION CALL LETTERS SECTION                                                  
***************                                                                 
         LA    R4,BLOCK            MUST HAVE AT LEAST CURRENT CALLS             
         USING SCANBLKD,R4           WITH CALL CHANGE AND/OR REP CHANGE         
         MVC   DLTCRSTA,SC1STFLD   CURRENT CALL LETTERS                         
         CLI   DLTCRSTA+4,C'-'     4 CHARACTER AND BAND?                        
         BNE   *+14                                                             
         MVC   DLTCRSTA+4(1),SC1STFLD+5   YES, DDS FORMAT IS AAAAB              
         B     *+8                                                              
         MVI   DLTCRSTA+3,C' '                                                  
*                                                                               
         CLI   DLTCRSTA+4,C'A'     AM OR FM?                                    
         BE    *+12                                                             
         CLI   DLTCRSTA+4,C'F'                                                  
         BNE   INVLBAND            NO, INVALID RADIO BAND                       
*                                                                               
         LA    R4,L'BLOCK(R4)                                                   
         CLC   SC1STFLD,SPACES     ANY STATION CALLS EFFECTIVE DATE?            
         BH    *+12                YES, GET IT THEN                             
         LA    R4,L'BLOCK(R4)      NONE, SO NO DATE NOR PREV CALLS              
         B     GTDLT024                                                         
*                                                                               
*   FORMAT IS COMING IN AS  YYYY-MM-DD FOR CALL LETTER EFF DATE                 
*            CHANGING IT TO YYYYMMDD                                            
GTDLT020 MVC   SC1STFLD+4(2),SC1STFLD+5                                         
         MVC   SC1STFLD+6(2),SC1STFLD+8                                         
         MVC   SC1STFLD+8(2),SPACES                                             
         GOTO1 DATCON,DMCB,(0,SC1STFLD+2),(15,DLTSEFJU)                         
         GOTO1 (RF),(R1),,(3,DLTSEFNG)                                          
         XR    R1,R1                                                            
         ICM   R1,7,DLTSEFNG       NEGATED AS PER CTGENSTAD                     
         LNR   R1,R1                                                            
         STCM  R1,7,DLTSEFNG                                                    
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
*   FORMAT IS COMING IN AS  YYYY-MM-DD FOR STATION ADD DATE                     
*            CHANGING IT TO YYYYMMDD                                            
GTDLT024 CLC   SC1STFLD,SPACES     ANY STATION CALLS EFFECTIVE DATE?            
         BH    *+12                YES, GET IT THEN                             
         LA    R4,L'BLOCK(R4)      NONE, SO NO ADD DATE                         
         B     GTDLT028                                                         
*                                                                               
         MVC   SC1STFLD+4(2),SC1STFLD+5                                         
         MVC   SC1STFLD+6(2),SC1STFLD+8                                         
         MVC   SC1STFLD+8(2),SPACES                                             
         GOTO1 DATCON,DMCB,(0,SC1STFLD+2),(3,DLTSTADD)                          
         XR    R1,R1                                                            
         ICM   R1,7,DLTSTADD       NEGATED AS PER CTGENSTAD                     
         LNR   R1,R1                                                            
         STCM  R1,7,DLTSTADD                                                    
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
GTDLT028 CLC   SC1STFLD,SPACES     ANY PREVIOUS STATION CALLS?                  
         BH    *+12                YES, GET IT THEN                             
         LA    R4,L'BLOCK(R4)      NONE                                         
         B     GTDLT030                                                         
*                                                                               
         MVC   DLTPRSTA,SC1STFLD   CURRENT CALL LETTERS                         
         CLI   DLTPRSTA+4,C'-'     4 CHARACTER AND BAND?                        
         BNE   *+14                                                             
         MVC   DLTPRSTA+4(1),SC1STFLD+5   YES, DDS FORMAT IS AAAAB              
         B     *+8                                                              
         MVI   DLTPRSTA+3,C' '                                                  
         LA    R4,L'BLOCK(R4)                                                   
***************                                                                 
* REP SECTION                                                                   
***************                                                                 
GTDLT030 CLC   SC1STFLD,SPACES     ANY CURRENT REP?                             
         BNH   GTDLTYES            NO, THAT MEANS NO REP INFO NEEDED            
****        ICM   RE,15,SC1STNUM                                                
****        STCM  RE,3,DLTCRTRD       SAVE CURRENT SBS TRADING COMPANY          
         MVC   DLTCRTRD,SC1STFLD                                                
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
         ICM   RE,15,SC1STNUM                                                   
         STCM  RE,3,DLTCRCMP                                                    
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
         CLC   SC1STFLD,SPACES     ANY REP CHANGE EFFECTIVE DATE?               
         BNH   GTDLT040            NO, ADDED STATION WITH JUST THE REP          
*   FORMAT IS COMING IN AS  YYYY-MM-DD                                          
*            CHANGING IT TO YYYYMMDD                                            
         MVC   SC1STFLD+4(2),SC1STFLD+5                                         
         MVC   SC1STFLD+6(2),SC1STFLD+8                                         
         MVC   SC1STFLD+8(2),SPACES                                             
         GOTO1 DATCON,DMCB,(0,SC1STFLD+2),(15,DLTREFJU)                         
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
****        ICM   RE,15,SC1STNUM                                                
****        STCM  RE,3,DLTPRTRD       SAVE PREVIOUS SBS TRADING COMPANY         
         MVC   DLTPRTRD,SC1STFLD                                                
         LA    R4,L'BLOCK(R4)                                                   
*                                                                               
         ICM   RE,15,SC1STNUM                                                   
         STCM  RE,3,DLTPRCMP                                                    
*                                                                               
GTDLT040 LA    R3,DLTCRTRD                                                      
         BRAS  RE,GTDDSREP                                                      
         BNE   INVLREP                                                          
*                                                                               
         CLC   DLTPRTRD,SPACES     ANY PREVIOUS REP?                            
         BNH   GTDLTYES            NONE, DONE HERE                              
         LA    R3,DLTPRTRD                                                      
         BRAS  RE,GTDDSREP                                                      
         BNE   INVLREP                                                          
*                                                                               
GTDLTYES B     YES                                                              
*                                                                               
INVLREP  MVC   P(31),=C'*** LINE SKIPPED DUE TO INVALID'                        
         MVC   P+32(12),=C'REP:     ***'                                        
         MVC   P+37(3),0(R3)                                                    
         B     SKIPLINE                                                         
*                                                                               
INVLBAND MVC   P(31),=C'*** LINE SKIPPED DUE TO INVALID'                        
         MVC   P+32(12),=C'BAND:    ***'                                        
         MVC   P+38(5),DLTCRSTA                                                 
*                                                                               
SKIPLINE BRAS  RE,EMAILLIN                                                      
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         OI    EMAILFLG,SKIPLIN+SKIPFLG    WE SKIPPED SOME LINES                
GTDLTNO  B     NO                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
JESMAIL  DC    CL8'JESMAIL '                                                    
SMTPTO   DC    CL24'INT_EBIZ@MEDIAOCEAN.COM:'                                   
SMTPCC   DC    0CL69                                                            
         DC    CL45'WALTERHO@MEDIAOCEAN.COM,HWONG@MEDIAOCEAN.COM,'              
         DC    CL40'US-OPERATIONS_SUPPORT_NY@MEDIAOCEAN.COM:'                   
SMTPBCC  DC    CL17'SPKD02@GMAIL.COM:'                                          
SMTPSUBJ DC    C'SBS RADIO DSTA UPDATE'                                         
SMTPLINE DC    CL80' '             SMTP CAN ONLY HANDLE 80 BYTE LINES           
SMTPSUB2 DC    C'SKIPPED LINE(S) IN SBSDELTA RPT'                               
***********************************************************************         
DLTADSTA DCB   DDNAME=DLTADSTA,DSORG=PS,RECFM=FB,LRECL=133,            X        
               BLKSIZE=6118,MACRF=GM,EODAD=MAIN50                               
***********************************************************************         
ADDSAVE  DS    A                                                                
ASCANNER DS    A                                                                
AREPIDS  DS    A                                                                
MYVSMTP  DS    A                                                                
JDTTODAY DS    XL4                 JULIAN DATE FOR TODAY                        
NDTTODAY DS    XL3                 NEGATED BINARY DATE FOR TODAY                
ELCODE   DS    XL1                                                              
SMTPLINS DS    XL1                                                              
SMTPPAGE DS    XL2                 PAGE COUNT                                   
PACKOF4B DS    PL4                                                              
EMAILFLG DS    C                                                                
SKIPFLG  EQU   X'80'               WHETHER WE SKIPPED ANY LINES                 
SKIPLIN  EQU   X'40'               CURRENT LINE SKIPPED                         
WARNFLG  EQU   X'20'               WHETHER WE HAVE A WARNING                    
WARNLIN  EQU   X'10'               CURRENT LINE HAS WARNING                     
*                                                                               
KEYSNAME DC    CL8'**KEYS**'                                                    
BIGKEY   DS    CL48                                                             
BIGKEYSV DS    CL48                                                             
BLCKNAME DC    CL8'**BLOCK*'                                                    
BLOCK    DS    10CL32                                                           
ELEMNAME DC    CL8'**ELEM**'                                                    
ELEM     DS    XL255                                                            
OFFLSTEL DS    XL100               OFFICE LIST ELEMENT                          
IO2NAME  DC    CL8'**I/O2**'                                                    
IO2      DS    1000C                                                            
IO3NAME  DC    CL8'**I/O3**'                                                    
IO3HEAD  DS    XL8                 NOT QSAM AS DATASET IS FB                    
IO3      DS    150C                RECORDS ARE 133 BYTES                        
IONAME   DC    CL7'**I/O**'                                                     
IOQSAM   DS    XL4                                                              
IO       DS    4000C                                                            
IO4NAME  DC    CL8'**I/O4**'                                                    
IO4      DS    500CL(L'STAKEFDA+STAKEFDA-STAKMEDA)                              
IO4END   EQU   *                                                                
*                                                                               
GETSBSRP BASR  R7,RE               RETURNS SBSRTABS IN (R7)                     
SBSRTABS DS    0X                                                               
***********************************************************************         
* SBS LIST OF INTEREP REPS (GEN MEDIA PARTNERS NOW)                             
SBSTABI  DC    CL3'IR ',AL2(SBSTABIX-SBSTABI)                                   
         DC    AL2(056),CL30'GMP - GEN MEDIA PARTNERS'                          
         DC    AL2(063),CL30'TAC - TACHER GMP'                                  
         DC    AL2(066),CL30'RRG - REGIONAL REPS GMP'                           
         DC    AL2(072),CL30'MG  - MCGAVREN GUILD RADIO'                        
         DC    AL2(077),CL30'SBI - SBS/INTEREP'                                 
         DC    AL2(088),CL30'NRR - NON-REP REGIONAL REPS'                       
         DC    AL2(101),CL30'LCF - LOCAL FOCUS'                                 
         DC    AL2(102),CL30'NLF - NON-REP LOCAL FOCUS'                         
         DC    AL2(109),CL30'MGM - MCGAVERN MEDIA'                              
         DC    AL2(110),CL30'LFR - MFS LOCAL FOCUS'                             
*** GEN MEDIA PARTNERS SAID THESE REP COMPANIES BELOW ARE NO LONGER             
*** LEAVING THEM HERE IN CASE OF ISSUES, MARKING REUSED ONES, I.E. 56           
         DC    AL2(062),CL30'CML - CUMULUS RADIO SALES'                         
**REUSED DC    AL2(100),CL30'CIT - ABC/CITADEL'                                 
**REUSED DC    AL2(056),CL30'ABR - ABC RADIO SALES'                             
**REUSED DC    AL2(066),CL30'CBR - CBS RADIO SALES'                             
         DC    X'FF'                                                            
SBSTABIX DS    0H                                                               
* SBS LIST OF KATZ REPS                                                         
SBSTABK  DC    CL3'KRG',AL2(SBSTABKX-SBSTABK)                                   
         DC    AL2(052),CL30'CHR - CHRISTAL'                                    
         DC    AL2(053),CL30'KRD - EASTMAN'                                     
         DC    AL2(055),CL30'KH  - UNIVISION RADIO SALES'                       
         DC    AL2(056),CL30'KR  - KATZ RADIO'                                  
         DC    AL2(058),CL30'CCR - CCM+E MMS'                                   
         DC    AL2(060),CL30'MASTER REP - ADVANTAGE'                            
         DC    AL2(063),CL30'DED - DEDICATED'                                   
         DC    AL2(064),CL30'KNR - NET RADIO SALES'                             
         DC    AL2(065),CL30'KRG - KATZ RADIO GROUP'                            
         DC    AL2(067),CL30'CRD - CCRD'                                        
         DC    AL2(068),CL30'K36 - KATZ360 NETWORK'                             
         DC    AL2(069),CL30'WON - WESTWOOD ONE'                                
         DC    AL2(070),CL30'KNT - KATZ NET'                                    
         DC    X'FF'                                                            
SBSTABKX DS    0H                                                               
* SBS LIST OF RMR REPS                                                          
SBSTABR  DC    CL3'RMR',AL2(SBSTABRX-SBSTABR)                                   
         DC    AL2(089),CL30'RMR - RMR'                                         
         DC    X'FF'                                                            
SBSTABRX DS    0H                                                               
* SBS LIST OF REGIONAL REPS                                                     
SBSTABRR DC    CL3'ROR',AL2(SBSTABRRX-SBSTABRR)                                 
         DC    AL2(001),CL30'RRP - REGIONAL REP'                                
         DC    X'FF'                                                            
SBSTABRRX DS    0H                                                              
* SBS LIST OF NATIONAL PUBLIC MEDIA REPS                                        
SBSTABN  DC    CL3'NPM',AL2(SBSTABNX-SBSTABN)                                   
         DC    AL2(001),CL30'NPM - NPM'                                         
         DC    X'FF'                                                            
SBSTABNX DS    0H                                                               
* SBS LIST OF ENTRAVISION REPS                                                  
SBSTABE  DC    CL3'ENT',AL2(SBSTABEX-SBSTABE)                                   
         DC    AL2(001),CL30'ESL - ENTRAVISION SOLUTIONS'                       
         DC    X'FF'                                                            
SBSTABEX DS    0H                                                               
* SBS LIST OF UNREPPED REPS   9999                                              
SBSTABU  DC    CL3'NOR',AL2(SBSTABUX-SBSTABU)                                   
         DC    AL2(001),CL30'NON - UNREPPED'                                    
         DC    X'FF'                                                            
SBSTABUX DS    0H                                                               
*                                                                               
SBSRTABX DC    X'FFFF'                                                          
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* LOOKUP THE DDS REP CODE BASED ON TRADING PARTNER AND REP COMPANY CODE         
*                                                                               
* ON ENTRY:    (R3)                A(TRADING PARTNER  (AL2)    &                
*                                    SBS COMPANY CODE (AL2)    &                
*                                    DDS REP CODE     (CL3) )                   
***********************************************************************         
GTDDSREP NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETSBSRP         POINT TO THE SBS REP TABLES                  
GTDDSR10 CLC   =X'FFFF',0(R7)      END OF SBS REP TABLES?                       
         JE    GTDDSRNO            YES, DON'T DIE, REPORT MISSING NAME          
*                                                                               
***      CLC   0(2,R7),0(R3)       THIS WAS SUPPOSE TO BE NUMERIC               
         CLC   0(3,R7),0(R3)       MATCH ON THIS TRADING PARTNER?               
         JE    GTDDSR20                                                         
         XR    R0,R0                                                            
***      ICM   R0,3,2(R7)             MATCHED WITH PREV COMMENTED LINE          
         ICM   R0,3,3(R7)          NO, LET'S FIND THE NEXT PARTNER              
         AR    R7,R0                                                            
         J     GTDDSR10                                                         
*                                                                               
***GTDDSR20 LA    R7,4(R7)            MATCHED WITH PREV COMMENTED LINE          
GTDDSR20 LA    R7,5(R7)                                                         
         USING SBSTABD,R7                                                       
GTDDSR25 CLI   0(R7),X'FF'         DID WE HIT THE END?                          
         JE    GTDDSRNO            YES, DON'T DIE, REPORT MISSING NAME          
         CLC   SBREPNUM,DLTCRCMP-DLTCRTRD(R3)                                   
         JE    GTDDSR30                                                         
         LA    R7,SBREPLNQ(R7)                                                  
         J     GTDDSR25                                                         
*                                                                               
GTDDSR30 MVC   DLTCRDDS-DLTCRTRD(L'DLTCRDDS,R3),SBREPNAM  SAVE DDS REP          
         DROP  R7                                                               
*                                                                               
GTDDSRYS J     YES                                                              
*                                                                               
GTDDSRNO J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUTS PRINT LINE INTO EMAIL TEXT BODY                                          
***********************************************************************         
EMAILLIN NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT4,C'Y'          USING EMAIL?                                 
         JNE   EMLLINX                                                          
         CLI   SMTPLINS,190        RUNNING OUT OF ROOM ON THIS EMAIL?           
         JL    EMLLIN50            NOPE, WE'RE GOOD                             
         MVC   SMTPLINE,SPACES                                                  
         MVC   SMTPLINE(17),=C'*** CONTINUED ***'                               
         MVC   SMTPLINE+70(10),=C'PAGE: ????'                                   
         XR    R1,R1                                                            
         ICM   R1,3,SMTPPAGE                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,SMTPPAGE                                                    
*                                                                               
         EDIT  (B2,SMTPPAGE),(4,SMTPLINE+76),FILL=0                             
*                                                                               
         GOTOR MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
         GOTOR MYVSMTP,DMCB,('SMTPASND',0)    SEND OUT THE EMAIL                
*                                                                               
         GOTOR MYVSMTP,DMCB,('SMTPATCS',SMTPTO),(L'SMTPSUBJ,SMTPSUBJ), +        
               (0,SMTPCC),(0,SMTPBCC)                                           
         MVI   SMTPLINS,10                                                      
*                                                                               
EMLLIN50 MVC   SMTPLINE,P          SMTPLINE IS 80 CHARACTERS WIDE               
         GOTOR MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
         ZIC   R1,SMTPLINS                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SMTPLINS                                                      
*                                                                               
EMLLINX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NOTIFY LIST THAT LINE(S) WERE SKIPPED OR WARNGING IN DSTA UPDATE              
***********************************************************************         
EMAILSKP NTR1  BASE=*,LABEL=*                                                   
******************************************                                      
***NOP   TM    EMAILFLG,SKIPFLG+WARNFLG  SKIP OR WARNING?  NOP***               
         TM    EMAILFLG,SKIPFLG          SKIP                                   
         JZ    EMLSKPX             NO                                           
*                                  YES, INFORM EMAIL LIST                       
         GOTOR MYVSMTP,DMCB,('SMTPATCS',SMTPTO),(L'SMTPSUB2,SMTPSUB2), +        
               (0,SMTPCC),(0,SMTPBCC)                                           
         MVI   SMTPLINS,10                                                      
*                                                                               
         MVC   SMTPLINE,=CL80'PLEASE CHECK THE OUTPUT FOR'                      
         GOTOR MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
         ZIC   R1,SMTPLINS                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SMTPLINS                                                      
         TM    EMAILFLG,SKIPFLG                                                 
         JZ    EMLS010                                                          
         MVC   SMTPLINE,=CL80'- LINES SKIPPED'                                  
         GOTOR MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
         ZIC   R1,SMTPLINS                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SMTPLINS                                                      
EMLS010  TM    EMAILFLG,WARNFLG                                                 
         JZ    EMLS020                                                          
         MVC   SMTPLINE,=CL80'- WARNING LINES'                                  
         GOTOR MYVSMTP,DMCB,('SMTPAPTL',SMTPLINE)                               
EMLS020  GOTOR MYVSMTP,DMCB,('SMTPASND',0)    SEND OUT THE EMAIL                
*                                                                               
EMLSKPX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DLTAINFO DSECT                                                                  
DLTCRSTA DS    CL5                  CURRENT CALLS  (AAAA-B OR AAA-B)            
DLTSEFNG DS    XL3                 STA CALLS EFFECTIVE DATE 'NEGATED'           
DLTSEFJU DS    XL4                 STA EFFECTIVE DATE 'FULL JULIAN'             
DLTSTADD DS    XL3                 STA ADDED DATE 'NEGATED' BINARY              
DLTSADJU DS    XL4                 STA ADD DATE 'FULL JULIAN'                   
DLTPRSTA DS    CL5                 PREVIOUS CALLS  (AAAA-B OR AAA-B)            
*                                                                               
****DLTCRTRD DS    AL2                  CURRENT REP TRADING PARTNER             
DLTCRTRD DS    CL3                  CURRENT REP TRADING PARTNER                 
DLTCRCMP DS    AL2                              COMPANY NUMBER                  
DLTCRDDS DS    CL3                              DDS REP CODE                    
DLTREFJU DS    XL4                 REP EFFECTIVE DATE 'FULL JULIAN'             
****DLTPRTRD DS    AL2                  CURRENT REP TRADING PARTNER             
DLTPRTRD DS    CL3                 PREVIOUS REP TRADING PARTNER                 
DLTPRCMP DS    AL2                              COMPANY NUMBER                  
DLTPRDDS DS    CL3                              DDS REP CODE                    
*                                                                               
SBSTABD  DSECT                                                                  
SBREPNUM DS    AL2                 COMPANY NUMBER                               
SBREPNAM DS    CL30                DDS REP NAME                                 
SBREPLNQ EQU   *-SBREPNUM                                                       
*                                                                               
PRTLINED DSECT                                                                  
PRTSCCLL DS    CL9                  +00 CURRENT CALL LETTERS                    
         DS    CL3                  +09                                         
PRTSEFFD DS    CL14                 +12 EFF DATE OF CALLS CHANGE                
         DS    CL3                  +26                                         
PRTSPCLL DS    CL9                  +29 PREV CALL LETTERS                       
         DS    CL12                 +38                                         
PRTCTRNG DS    CL5                  +50 CURRENT REP TRADING PARTNER             
         DS    C                    +55                                         
PRTCCMPN DS    CL5                  +56             COMPANY NUMBER              
         DS    CL3                  +61                                         
PRTREFFD DS    CL12                 +64 EFF DATE OF REP CHANGE                  
         DS    CL3                  +76                                         
PRTPTRNG DS    CL5                  +79 PREVIOUS REP TRADING PARTNER            
         DS    C                    +84                                         
PRTPCMPN DS    CL5                  +85              COMPANY NUMBER             
*                                                                               
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDDARETABD                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE CTGENSTAD                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102SPREPKD02 05/02/19'                                      
         END                                                                    
