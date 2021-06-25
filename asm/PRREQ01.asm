*          DATA SET PRREQ01    AT LEVEL 094 AS OF 07/17/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE T41201A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PRREQ01- REQUEST - CHANGE LOG '                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ------------------------------------------------------------------- *         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* SMUR SPEC-17729  04/14/18 NEW MEDIA D DIGITAL AUDIO (18.3)          *         
* AKAT SPSUG-85    07/08/16 ALLOW AGENCY OU TO REQUEST GT REPORT      *         
* AKAT SPSUG-86    07/08/16 ALLOW AGENCY OO TO REQUEST GT REPORT      *         
* AKAT CSD-477     07/08/16 ALLOW AGENCY UB TO REQUEST GT REPORT      *         
* ------------------------------------------------------------------- *         
* DEIS 01/2016  ALLOW IN (NISSAN INTERFACE) SOON                                
*                                                                               
* BPLA 08/2015  LIMIT GM REQUESTS TO AGENCIES GZ AND UB                         
*                                                                               
* BPLA 07/2015  CHANGES FOR NEW MEDIA CODES                                     
*                                                                               
* BPLA 05/2015  CHANGES FOR AT&T PFIZER INTERFACE                               
*                                                                               
* BPLA 01/15    REMOVE CHECKS FOR OLD AGENCIES                                  
*                                                                               
* BPLA 11/14    ALLOW OO TO REQUEST THE IN (NISSAN INTERFACE)                   
*                                                                               
* BPLA 04/2014  CHANGES FOR PZ PFIZER INTERFACE                                 
*                                                                               
* BPLA 02/2014  CHANGES FOR MEDIA L AND PO# PURGE                               
*                                                                               
* BPLA 01/2013  CHANGES FOR THE IN - NISSAN INTERFACE                           
*               CHANGES FOR THE CH - CHOICE HOTELS                              
*                                                                               
* BPLA 5/2012   ALLOW AGENCIES H0 AND HY TO REQUEST THE JW                      
*                                                                               
* BPLA 3/2012   NEW REPORT WB - WARNER BROS. INTERFACE                          
*                                                                               
* BPLA 1/2012   ALLOW JS TO REQUEST THE JW                                      
*                                                                               
* BPLA 2/2011   NEW REPORT JW - JWT INTERFACE                                   
*                                                                               
* BPLA 1/2011   NEW OPTION FOR THE P92                                          
*                                                                               
* BPLA 11/2010  ALLOW OUTTOA (HY) TO REQUEST THE GT                             
*                                                                               
* SMYE 04/2010  DELETE AGY=MX CODE (MX NO LONGER ON DDS SYSTEM)                 
*                                                                               
* BPLA 09/2009  CHANGES FOR TD (TAB DELIMITED INTERFACE)                        
*                                                                               
* BPLA 05/2009  ALLOW SOON P98 REQ FOR ALL MEDIA (*)                            
*                                                                               
* BPLA 02/2009  CHANGE FOR LO (L'OREAL INTERFACE)                               
*                                                                               
* BPLA 08/08    CHANGE DEFAULT DEST FOR P98 TO SJR INSTEAD OF DDS               
*                                                                               
* BPLA 07/08    ALLOW SOON P41 REQUESTS                                         
*                                                                               
* BPLA 05/08    CHANGES FOR P98 REQUESTS                                        
*                                                                               
* BPLA 05/08    ALLOW WT TO REQUEST P10'S FOR MEDIA *                           
*                                                                               
* BPLA 04/08    REMOVE CHECKS FOR MINSHARE USER ID'S TO ACCESS BILLING          
*               IMDB# 0175672N                                                  
*                                                                               
* BPLA 04/08    ALLOW WT (WITO) TO REQUEST THE 49                               
*                                                                               
* BPLA 02/08    ANOTHER MINDSHARE BILLING ID - MSMES (13625)                    
*                                                                               
* BPLA 12/07    SET DEFAULT R07 TO T FOR 01 PURGE REQUESTS                      
*                                                                               
* BPLA 09/07    ANOTHER MINDSHARE BILLING ID - MSNEO (13386)                    
*                                                                               
* BPLA 11/06    MORE MINDSHARE IDS FOR BILLING                                  
*                                                                               
* BPLA 08/06    GIVE H7 ACCESS TO THE SE                                        
*                                                                               
* SMYE 05/23/06 CHANGE SPECIAL HANDLING (INDICATED BY * IN 1ST POS'N OF         
*                 AGY) TO ALLOW FOR ACTUAL POWER CODES BEGINNING WITH *         
*                                                                               
* SMYE 02/23/06 COMMENT CHANGE IN SOONTAB FOR CM (NO LOGIC CHANGE)              
*                                                                               
* BPLA 2/06   SOON BILLING FOR ALL AGYS                                         
*                                                                               
* BPLA 1/06   SOON BILLING FOR MUWS (M$)                                        
*                                                                               
* SMYE 11/23/05 ADD 74 TO SOONTAB                                               
*                                                                               
* BPLA 10/05 CHANGES FOR PH REPORT                                              
*                                                                               
* BPLA 09/05 CHANGES FOR SE REPORT                                              
*                                                                               
* BPLA 09/05 CI REPORT SOONABLE                                                 
*                                                                               
* SMYE 05/05 CHANGES FOR LT REPORT                                              
*                                                                               
* BPLA 03/05 CHANGES FOR SN REPORT                                              
*            CHANGES FOR CI REPORT                                              
*                                                                               
* SMYE 02/23/05 ALLOW MSNYMAXB TO REQUEST BILLING                               
*                                                                               
* KWAN 10/19/04 ALLOW MSNYMAX TO REQUEST BILLING                                
*                                                                               
* SMYE 10/08/04 ALLOW MSMOGM TO REQUEST BILLING                                 
*                                                                               
* SMYE 10/06/04 SOX "NON-UPDATIVE" IMPLEMENTATION FOR B1, R1, & 79              
*               REQUESTS AT VALNUMXX                                            
*                                                                               
* SMYE 09/21/04 SOX "NON-UPDATIVE" IMPLEMENTATION FOR P72 INSERTION             
*               ORDERS AT VALNUMXX                                              
*                                                                               
* BPLA 08/02/04 ALLOW MSME AND MSGM TO REQUEST BILLING                          
*                                                                               
* BPLA 6/25/04  ALLOW MEDIA * FOR MCCANN (AND SJR+HDTO) FOR P10                 
*                                                                               
* SMYE 12/10/03 ALLOW MSSOHO TO REQUEST BILLING                                 
*                                                                               
* BPLA 04/23/03 ALLOW MSNYJCC AND MSNYBCH TO REQUEST BILLING                    
*                                                                               
* KWAN 02/08/02 ALLOW H9 FOR PM (112) REPORTS                                   
*                                                                               
* KWAN 12/03/01 ADD NV TO SOONTAB                                               
*                                                                               
* KWAN 11/09/01 DISALLOW INPUTS FOR OUTPUT TYPE FLD FOR P92 (CLOSEOUT)          
*                                                                               
* KWAN 09/06/01 ALLOW PRD GRP INPUTS ON PRODUCT CODE FIELD                      
*                                                                               
* KWAN 07/27/01 TRAFFIC LIMIT ACCESS FOR P72,P75,P77,P79                        
*                                                                               
* KWAN 04/06/01 NEW FIELDS FOR CLR/PRD/PUB GROUP RECORD PURGES                  
*                                                                               
* SMYE 12/00    ADD NEW FIELDS TO TWATBL FOR PCM(CLT/PRD MEDIA COPY)            
*                                                                               
* KWAN 10/00    ALLOW UID FILTERING ON NONE DDS TERMINALS                       
*                                                                               
* BPLA 08/00    FIX SPELLING IN 'ALL MEDIAS' - MEDIA                            
*                                                                               
* KWAN 07/00    ADD NEW FIELD FOR PA8: PRD SUMMARY FMT                          
*                                                                               
* KWAN 03/00    ADD NEW COMMENT FOR PURGE OPTION IN P01 AND P02                 
*                                                                               
* KWAN 01/00    UID FILTER (FLTUID) IS EXPANDED FROM CL7 TO CL10                
*                                                                               
* KWAN 12/99    UID=YES BUG FIX (FLTUID DIDN'T GET CLEARED PROPERLY)            
*                                                                               
* KWAN 11/99    ADD COMMENT FOR P41 (I=INACTIVE CLT/PRD)                        
*                                                                               
* KWAN 11/99    CODES FOR UID=YES AND UID=XXX OPTION                            
*                                                                               
* KWAN 10/99    CODES FOR RT02 (PRINTPAK RECORD PURGES DDS ONLY)                
*                                                                               
* KWAN 09/99    CODES FOR RT01 (PRINTPAK RECORD PURGES)                         
*                                                                               
* SMYE 3/99     ADDED COMMENT 255 TO THE COMTBL (PRINT OPTIONS FOR 12)          
*                                                                               
* SMYE 2/99     REACTIVATED "I=WEB SITE" IN COMMENT 154 IN COMTBL               
*                                                                               
* SMYE 1/99     ADD WR TO WESTERN AGENCY LIST                                   
*               *NOP* (NO-OP) "I=WEB SITE" IN COMMENT 154 IN COMTBL             
*                                                                               
* SMYE 11/98    IN COMTBL ADDED "I=WEB SITE" TO COMMENT 154                     
*                                                                               
* BPLA 9/98     ADD MEDIA "I" (INTERACTIVE) TO MEDTBL                           
*                                                                               
* SMYE 8/98     IN VALNUM ALLOW CL ONLY FOR AGY YN (YNR)                        
*                                                                               
* SMYE 8/98     IN VALMED ALLOW MED * (ALL) FOR P10 IF AGY DF (SAATCHI)         
*                                                                               
* SMYE 5/98     ADDED COMMENT 75 TO THE COMTBL (LEVEL OPTION=H,L,B,O,A)         
*               ADDED COMMENT 73 TO THE COMTBL ("CONTRACT END MTH..")           
*                                                                               
* SMYE 4/98     FOR NEW EB REQUEST ADDED 205, 206 & 207 TO THE TWATBL           
*               AND ADDED COMMENT 29 TO THE COMTBL                              
*                                                                               
* SMYE 4/98     IN COMTBL CHANGED COMMENTS 138, 142, AND 154 TO ADD             
*               2 NEW LIST TYPES FOR P48 - P48 HAS MAXIMUM 23 LINES             
*                                                                               
* BPLA 3/98     SJR NO LONGER CONSIDERED A WESTERN STYLE AGENCY                 
*               PAY CONTROLS REACTIVATED GROUP ASSGNMENTS NO-OPED               
*                                                                               
* BPLA 9/97     PUB GROUP ASSIGNMENTS FOR P48 REACTIVATED                       
*               (PAY CONTROLS NO-OPTED - STILL NEEDS UPDATE)                    
* BPLA 7/97     ALLOW WESTERN AGENCIES ACCESS TO R1, RD, RA                     
*                                                                               
* SMYE 5/15/97  DISALLOWED P49 FOR MX,WI,WR,WT AGENCIES (VALNUMX9)              
*                                                                               
* BPLA 3/97     ADD MX WESTERN AGENCY LIST                                      
*                                                                               
* SMYE 2/25/97  ACTIVATED 4-POS'N. CONTRACT NUMBER IN TWATBL                    
*                                                                               
* SMYE 2/97     ACTIVATED "LARGER" COMMENT 154 IN COMTBL FOR P48                
*                                                                               
* SMYE 2/97     LIMITED BELLSOUTH AGENCIES TO P60, P72, AND P77                 
*                                                                               
* SMYE 12/96    MODIFIED COMMENT 154 IN COMTBL FOR P48                          
*                                                                               
* SMYE 7/96     ADDED TO COMMENT 154 FOR P48 (G OPTION - PUB GROUP              
*                 ASSIGNS) - ADD ENTRY FOR SCHEME FOR ABOVE OPTION              
*                                                                               
* SMYE 4/96     NEW PUBLISHER REP                                               
*                                                                               
* BPLA 2/96     NEW STEREO CHECK - FULL VS. LITE                                
*                                                                               
* BPLA 1/96     NEW COMMENT (154) FOR P48                                       
*                                                                               
* BPLA 11/95    CHANGES FOR STEREO REQUESTS                                     
*                                                                               
* BPLA 11/95    ADD ENTRY FOR LANGUAGE (FOR P46 AND P48)                        
*                                                                               
* BPLA 9/95     CHANGE TO COMMENT 153 (X=OMIT ADDED)                            
*                                                                               
* BPLA 8/95     NEW COMMENT ENTRIES FOR P43 AND MAKE SOONABLE                   
*                                                                               
* BPLA 5/95     NEW ENTRY FOR ADCODE RECAP 154 (FOR S2)                         
*               ALSO T=COST OPTION FOR S2                                       
*                                                                               
* BPLA 2/22/95  CHANGES FOR NEW REPORT - Z5 (228)                               
*                                                                               
* BPLA 1/20/95  CHANGE COMMENT 049 - X'31' USED BY P79                          
*                                                                               
* BPLA 12/23/94 IN BUILDER  - DISPLAY AN 'A' IN UNPROTECTED FIELD               
*               OF 'STATUS FILTER' (X'99') ENTRY                                
*                                                                               
* BPLA 11/1/94  NEW FIELDS + COMMENTS FOR TS REPORT                             
*                                                                               
* BPLA 8/29/94  CHANGES FOR FAXING CONTRACTS                                    
*                                                                               
* BPLA 11/18/93 NEW COMMENT (150) FOR P14                                       
*                                                                               
* BPLA 7/22/93  C=COST COMMENT (#57) FOR PRA                                    
*                                                                               
* BPLA 6/11/93  ADD AGENCY FILTER                                               
*                                                                               
* BPLA 3/18/93  CHANGES FOR RFP                                                 
*                                                                               
* BPLA 1/28/93  CHANGES FOR NEW PPAR REPORT (217)                               
*                                                                               
* BPLA 10/28/92 "S" ADDED TO COMMENT 144                                        
*                                                                               
* BPLA 7/2/92   KILL DATE FILTER FOR 48                                         
*                                                                               
* BPLA 6/26/92  CHANGES FOR ECT (INCLUDING TEST BUYS)                           
*                                                                               
* BPLA 4/1/92   CHANGE TO OVERAGE FIELD (097) + NEW COMMENT (049)               
*                                                                               
* BPLA 3/13/92  CHANGES FOR AC AND AU                                           
*                                                                               
* BPLA 2/4/92   ADD '4' SHIP TO COMMENT 138 AND MOVE 'X' PREMS TO 142           
* BPLA 9/12/91  REP FIELD EXPANDED TO ALLOW FOR NAME SEARCHING                  
*                                                                               
* BPLA 8/2/91   IF DEST = DDS SET REQDEST TO X'005A'                            
*                                                                               
* BPLA 6/12/91  RENAMED COMMENT 029 TO 027                                      
*                                                                               
* BPLA 5/23/91  ADD NEW OPTION FOR P48 (CLIENT ACTIVITY FILTER)                 
*                                                                               
* BPLA 4/24/91  ONLY ACCEPT RD FOR FINANCIAL AGENCIES                           
*                                                                               
* BPLA 3/26/91  LOGIC FOR RD - REBATE DRAFT (RT123)                             
*                                                                               
* BPLA 2/27/91  ALTER P49 DEFAULT FIELDS                                        
*                                                                               
* ROSA 2/11/91 ADD P77 MARKET SORT OPTION                          L07          
*                                                                               
* BPLA 2/7/91    ADD P49 FIELDS AND MAKE SOONABLE                               
*                                                                               
* ROSA 6/1/89    ADD P37  TO THE SOON LIST                     L01              
*                                                                               
* BPLA 7/18/89   ADD PJ  TO THE AGENCY THAT CAN REQUEST MEDIA  L02              
*                '*' BILLING                                   L02              
* BPLA 7/19/89   ADD OM TO THE AGENCY THAT CAN REQUEST MEDIA   L03              
*                R1 BILLING                                    L03              
* ROSA 1/16/90   ADD NEW LINE "COMMISSION ONLY" OPTION TO P10      L04          
*                                                                               
* BPLA 3/19/90   PATO ADDED TO MEDIA * B1,D1,E1 LIST           L05              
*                                                                               
* BPLA 3/29/90   COMMENT 145 CHANGED                                            
*                                                                               
* BPLA 5/30/90   ADD PBA TO SOONABLE LIST                       L06             
         TITLE 'PRREQ01- REQUEST - VALIDATE DEFN AND BUILD SCREEN'              
         PRINT NOGEN                                                            
T41201   CSECT                                                                  
         NMOD1 000,T41201,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         LA    RC,T41201+4095                                                   
         LA    RC,1(RC)                                                         
         USING T41201+4096,RC      RC=SECOND BASE REGISTER                      
         EJECT                                                                  
*        INITIALIZE PRINT + PUB KEYS + REQ RECORD                               
*                                                                               
         XC    KRT1,KRT1                                                        
         MVC   KRT2,KRT1                                                        
         XC    KUB1,KUB1                                                        
         XC    REQNUM(18),REQNUM                                                
*                                                                               
         XC    RHDR,RHDR                     INITIALISE REQ REC                 
         MVI   RNUM,C' '                                                        
         MVC   RNUM+1(159),RNUM                                                 
         SPACE 2                                                                
         LR    R2,R3                         R2=A(TWA SCREEN FF)                
         USING T412FFD,R2                                                       
         MVI   FIND,0                                                           
*                                                                               
         MVI   MYFLAG,0                                                         
*                                                                               
         MVC   FERN,=AL2(FF)                                                    
         MVI   USRIDF,0                                                         
         XC    CLISAVE(6),CLISAVE                                               
         XC    ESTDATES,ESTDATES                                                
         XC    CLIPROF,CLIPROF                                                  
*                                                                               
* VALIDATE REQUESTOR NAME & SET FIND X'01' = NAME INPUT                         
*                                                                               
VALNAME  DS    0H                                                               
*                                                                               
         XC    FLTUID,FLTUID                 USER ID FILTERING                  
*                                                                               
         CLI   BVRNAMEH+5,0                                                     
         BE    VALNAMA                                                          
         CLC   BVRNAME(4),=C'MENU '          CHECK FOR MENU REQUEST             
         BNE   *+14                                                             
         MVC   REQNDX1,=X'FFFC'              SET MENU SCREEN ID                 
         B     VALDEF1                                                          
******** CLI   DDS,0               DDS TERMINALS CAN HAVE KEYWORDS              
******** BNE   VALNAM1                                                          
         B     VALNAM1             CHECK KEYWORDS ANYWAY                        
*                                                                               
******** CLI   BVRNAMEH+5,12       USER TERM HAS REQUESTOR NAME ONLY            
******** BH    INVNAME                                                          
******** ZIC   R7,BVRNAMEH+5       MAX LEN IS 12 CHRS                           
******** BCTR  R7,0                                                             
******** EX    R7,*+8                                                           
******** B     *+10                                                             
******** MVC   RNAME(0),BVRNAME                                                 
******** OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
******** B     VALNAMA                                                          
*                                                                               
VALNAM1  LA    R7,TEMP                                                          
         GOTO1 SCANNER,PLIST,BVRNAMEH,(3,(R7))                                  
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5               R5=NUM OF INPUT FIELDS                       
         BZ    INVNAME                                                          
*                                                                               
VALNAM2  CLI   1(R7),0             ORDINARY FIELD IS REQUESTOR NAME             
         BNE   VALN40                                                           
         CLI   0(R7),12            MAX LEN IS 12 CHRS                           
         BH    INVNAME                                                          
         MVC   RNAME,12(R7)                                                     
         OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
         B     VALNAM9                                                          
*                                                                               
VALN40   CLC   =C'UID',12(R7)                                                   
         BNE   VALN50                                                           
         CLI   1(R7),0             ANY INPUT NEXT TO UID= ?                     
         BNH   MISNAME                                                          
         CLC   =C'YES',22(R7)                                                   
         BNE   VALN40H                                                          
         MVC   FLTUID(3),=C'YES'                                                
         B     VALN40X                                                          
*                                                                               
VALN40H  LA    R4,PRTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,22(R7)                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   INVNAME                                                          
         MVC   FLTUID,22(R7)       VALIDATED USER ID FOR FILTERING              
*                                                                               
VALN40X  B     VALNAM9                                                          
         DROP  R4                                                               
*                                                                               
VALN50   DS    0H                  FOR FUTURE USES                              
         CLI   DDS,0                                                            
         BE    INVNAME             FOLLOWING KEYWORDS ARE DDS ONLY              
*                                                                               
VALNAM3  CLI   12(R7),C'U'         U=XXX... FOR USERID INPUT                    
         BNE   VALNAM4                                                          
         CLI   1(R7),3                                                          
         BL    INVNAME                                                          
         BH    VALNAM30                                                         
         CLC   22(3,R7),=C'ALL'    U=ALL CHANGE TO AGY=00                       
         BNE   VALNAM30                                                         
         MVI   1(R7),2                                                          
         MVI   3(R7),X'20'                                                      
         MVC   22(2,R7),=C'00'                                                  
         B     VALNAM5A                                                         
VALNAM30 CLI   1(R7),10                                                         
         BH    INVNAME                                                          
         LA    R4,PRTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,22(R7)                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   INVNAME                                                          
         LA    RE,CTIDATA                                                       
         SR    RF,RF                                                            
*                                                                               
VALNAM3A CLI   0(RE),0             SEARCH FOR SYSTEM ELEMENT                    
         BE    INVNAME                                                          
         CLI   0(RE),X'21'                                                      
         BE    VALNAM3C                                                         
         CLI   0(RE),X'02'                                                      
         BNE   *+14                                                             
         MVC   DUB(2),2(RE)        SAVE USER ID NUM                             
         B     VALNAM3B                                                         
         CLI   0(RE),X'06'                                                      
         BNE   *+10                                                             
         MVC   DUB+6(2),2(RE)      SAVE AGY CODE                                
VALNAM3B IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALNAM3A                                                         
VALNAM3C CLC   SYSNUMOV,CTSYSNUM-CTSYSD(RE)                                     
         BNE   VALNAM3B                                                         
         MVC   DUB+2(1),CTSYSSE-CTSYSD(RE)                                      
         MVC   DUB+3(1),CTSYSAGB-CTSYSD(RE)                                     
**       MVC   DUB+4(2),CTSYSAGA-CTSYSD(RE)                                     
*                                                                               
VALNAM3D CLC   SYSNUM,DUB+2        MUST BE SAME SYSTEM AS CONNECT               
         BNE   INVNAME                                                          
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE U= OR A=                            
         OI    USRIDF,X'80'                                                     
         MVC   AGY,DUB+6           SET NEW AGY CODE                             
         MVC   USRID,DUB           SET NEW USERID NUMBER                        
         B     VALNAM9                                                          
*                                                                               
VALNAM4  CLI   12(R7),C'T'         T=Y TO DEFINE UNKNOWN REQUEST                
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     VALNAM4A                                                         
         CLC   12(3,R7),=C'AGY'        ACCEPT OLD SYNTAX                        
         BE    VALNAM5A                                                         
         CLI   12(R7),C'C'         C=Y TO DEFINE CARD REQUEST                   
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     VALNAM4A                                                         
         CLI   12(R7),C'1'         1=Y TO DEFINE DDS ONLY FLDS INCLUDED         
         BNE   *+12                                                             
         OI    FIND,X'14'                                                       
         B     VALNAM4A                                                         
         CLI   12(R7),C'2'         2=Y TO DEFINE DDS/2UP FLDS DISPLAY           
         BNE   *+12                                                             
         OI    FIND,X'34'                                                       
         B     VALNAM4A                                                         
         B     VALNAM5                                                          
VALNAM4A CLI   1(R7),1                                                          
         BNE   INVNAME                                                          
         CLI   22(R7),C'Y'                                                      
         BNE   INVNAME                                                          
         B     VALNAM9                                                          
*                                                                               
VALNAM5  CLI   12(R7),C'A'         A=X TO DEFINE AGY X (OR 00 FOR ALL)          
         BE    VALNAM5A                                                         
         CLI   12(R7),C'D'         D=X TO DEFINE AGY X AND DDS OFFICE           
         BNE   VALNAM6                                                          
         MVC   REQOFFC,=C'DDS*'                                                 
VALNAM5A MVC   AGY(2),22(R7)                                                    
         CLC   AGY,=C'00'                                                       
         BNE   *+8                                                              
         MVI   AGY,C'*'                                                         
*                                                                               
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE A= OR U=                            
         OI    USRIDF,X'40'                                                     
         B     VALNAM9                                                          
*                                                                               
VALNAM6  B     INVNAME             INVALID KEYWORD LETTER                       
*                                                                               
VALNAM9  LA    R7,32(R7)           BUMP TO NEXT FIELD                           
         BCT   R5,VALNAM2                                                       
         B     VALNAMA                                                          
*                                                                               
*                                                                               
*                                                                               
INVNAME  MVC   FERN,=AL2(FLDINV)                                                
         B     *+10                                                             
MISNAME  MVC   FERN,=AL2(FLDMIS)                                                
         LA    R7,BVRNAMEH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
*              SET AGENCY IN KEYS                                               
*                                                                               
VALNAMA  MVC   RAGY,AGY                                                         
         MVC   KRT1(2),AGY                                                      
         MVC   KRT2(2),AGY                                                      
         MVC   KUB1+7(2),AGY                                                    
*                                                                               
VALNAMX  EQU   *                                                                
         CLC   AGY,=C'WI'          SEE IF WESTERN                               
         BE    VALNAMXW                                                         
         CLC   AGY,=C'WJ'          SEE IF WESTERN  - TEST                       
         BE    VALNAMXW                                                         
         CLC   AGY,=C'WR'          SEE IF WESTERN                               
         BE    VALNAMXW                                                         
         CLC   AGY,=C'WT'          SEE IF WESTERN  - TEST                       
         BE    VALNAMXW                                                         
         B     VALNAMXX                                                         
*                                                                               
VALNAMXW DS    0H                  IF WESTERN OR SJR - ALTER WESTC1             
*                      (SPECIAL STATUS FILTER COMMENT FOR WESTERN)              
         L     RE,=A(COMTBL)                                                    
         A     RE,RELO                                                          
         AH    RE,=Y(WESTC1-COMTBL)                                             
         MVC   3(38,RE),=CL38'A,I,M,R,BLANK=NOT ENTERED,X=ALL'                  
*                                                                               
VALNAMXX DS    0H                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE REQUEST NUMBER & ACTION                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALNUM   CLI   BVRNUMH+5,0                                                      
         BE    MISNUM                                                           
         MVC   IFLDH,BVRNUMH       COPY FIELD INTO IFLD                         
         MVI   IFLD,C' '                                                        
         MVC   IFLD+1(L'IFLD-1),IFLD                                            
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRNUM                                                   
*                                                                               
         CLC   =C'SB',BVRNUM       SEE IF SOON BILLING                          
         BNE   VALNUM0B                                                         
         CLI   IFLDH+5,2           NO ACTION CAN FOLLOW                         
         BNE   INVNUM                                                           
         MVC   IFLD+1(2),=C'B1'      FUDGE TO B1                                
         MVI   IFLD,C'#'                                                        
         OI    MYFLAG,SBREPT       SOON BILLING FLAG                            
         B     VALNUM0P                                                         
*                                                                               
VALNUM0B DS    0H                                                               
         CLI   IFLDH+5,2                                                        
         BL    INVNUM                                                           
         BE    VALNUM0K                                                         
*                                                                               
         CLC   IFLD(3),=C'52T'      MEANS INCLUDE TEST BUYS                     
         BE    VALNUM0D             REQUEST NAME(1) SET TO LOWER CASE           
         CLC   IFLD(3),=C'ECT'      MEANS INCLUDE TEST BUYS                     
         BE    VALNUM0D             REQUEST NAME(1) SET TO LOWER CASE           
*                                                                               
* FOLLOWING REPORTS SET RO7 TO Y                                                
*                                                                               
         CLC   IFLD(3),=C'S2T'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'60T'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'77T'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'L1T'                                                  
         BE    VALNUM0D                                                         
*                                                                               
* FOLLOWING REPORTS SET RO6 TO Y                                                
*                                                                               
         CLC   IFLD(3),=C'18T'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'19T'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'AUT'                                                  
         BE    VALNUM0D                                                         
         CLC   IFLD(3),=C'ART'                                                  
         BE    VALNUM0D                                                         
         B     VALNUM0H                                                         
*                                                                               
VALNUM0D MVC   IFLD+1(2),BVRNUM    PUT INPUT INTO NEW FORMAT                    
         MVI   IFLD,C'#'           INPUT LENGTH STAYS THE SAME                  
         B     VALNUM1                                                          
*                                                                               
VALNUM0H CLI   IFLD+2,C','                                                      
         BNE   VALNUM1                                                          
*                                                                               
VALNUM0K DS    0H                                                               
         EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM                                                 
         MVI   IFLD,C'#'                                                        
*                                                                               
VALNUM0P DS    0H                                                               
         IC    R1,IFLDH+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,IFLDH+5                                                       
*                                                                               
VALNUM1  L     R7,AREQTBL          SEARCH REQUEST TABLE                         
         SR    R8,R8                                                            
         CLC   IFLD(3),=C'ALL'     CHECK FOR ID=ALL IF DDS REQUESTOR            
         BNE   VALNUM1A                                                         
         CLI   DDS,1                                                            
         BNE   INVNUM                                                           
         MVI   REQNUM,255          SET ALL VALUE IN REQNUM                      
         XC    BVRRNAM,BVRRNAM                                                  
         OI    BVRRNAMH+6,X'80'                                                 
         CLI   IFLD+3,C','         ACTION MUST BE DISPLAY                       
         BNE   INVNUM                                                           
         MVC   REQACTN,IFLD+4                                                   
         CLI   REQACTN,C'D'                                                     
         BNE   INVACTN                                                          
         MVI   REQOPTN,C'S'                                                     
         MVC   REQINCR,=H'1'                                                    
         B     VALNUM4                                                          
VALNUM1A CLI   0(R7),0             TEST FOR END OF TABLE                        
         BE    VALNUM1C                                                         
         IC    R8,0(R7)            R8=TABLE ENTRY LENGTH                        
         CLI   IFLD,C'#'                                                        
         BE    VALNUM1B                                                         
         CLC   4(3,R7),IFLD        MATCH ON THREE CHR MNENOMIC                  
         BE    VALNUM1D                                                         
         AR    R7,R8                                                            
         B     VALNUM1A                                                         
VALNUM1B LA    RF,0(R7,R8)         POINT TWO LAST TWO BYTES OF ENTRY            
         SH    RF,=H'2'                                                         
         CLC   0(2,RF),IFLD+1      MATCH ON TWO CHR REQUEST ID                  
         BE    VALNUM1D                                                         
         AR    R7,R8                                                            
         B     VALNUM1A                                                         
*                                                                               
VALNUM1C TM    FIND,X'0C'          REQUEST NOT FOUND                            
         BZ    INVNUM              OK FOR CARD/TEST OPTION                      
         CLI   IFLD,C'#'                                                        
         BNE   INVNUM                                                           
         L     R7,AREQTBL          POINT TO FIRST ENTRY                         
         IC    R8,0(R7)                                                         
         MVC   RNUM,IFLD+1                                                      
         B     VALNUM1E                                                         
*                                                                               
VALNUM1D CLI   1(R7),0             REQUEST FOUND                                
         BE    INVNUM                                                           
         MVC   REQNUM,1(R7)        SAVE INTERNAL BINARY REQUEST NUM             
         MVI   REQNUM+1,0                                                       
         LA    RF,0(R8,R7)                                                      
         SH    RF,=H'2'                                                         
         MVC   RNUM,0(RF)          SAVE REQ ID IN REQ REC                       
VALNUM1E MVC   BVRRNAM(22),4(R7)   DISPLAY REQ NAME AND #ID OR MNEMONIC         
         CLI   IFLD,C'#'                                                        
         BE    *+14                                                             
         MVI   BVRRNAM,C'#'                                                     
         MVC   BVRRNAM+1(2),RNUM                                                
*                                                                               
         OI    BVRRNAMH+6,X'80'                                                 
*                                                                               
         TM    MYFLAG,SBREPT       SOON BILLING REQ?                            
         BO    SBMSG                                                            
         B     VALNUM2                                                          
*                                                                               
SBMSG    MVC   BVRRNAM(22),=CL22'SOON UPDATIVE BILLING'                         
*                                                                               
VALNUM2  MVC   RHDR+10(1),REQNUM   R7=A(REQTBL ENTRY)                           
         MVI   REQACTN,C'N'        SET DEFAULT VALUES                           
         MVI   REQOPTN,C'S'                                                     
         MVC   REQINCR,=H'1'                                                    
         CLI   IFLDH+5,3           ONLY NUM INPUT                               
         BE    VALNUM3A                                                         
         CLI   IFLD+3,C','         NO MUST DELIMIT WITH ,                       
         BNE   INVNUM                                                           
*                                                                               
VALNUM2H MVC   REQACTN,IFLD+4      CHECK ACTION VALUE                           
VALNUM2K CLI   REQACTN,C'A'        AMEND                                        
         BE    VALNUM3                                                          
         CLI   REQACTN,C'D'        DISPLAY                                      
         BE    VALNUM4                                                          
         CLI   REQACTN,C'N'        NEW (DEFAULT)                                
         BE    VALNUM3                                                          
         B     INVACTN                                                          
*                                                                               
VALNUM3  CLI   IFLDH+5,5           NO OPTIONS FOR A OR N                        
         BNE   INVACTN                                                          
****VALNUM3A CLI   AGY,C'*'            AGY MUST BE SPECIFIC FOR A OR N          
VALNUM3A CLC   AGY(2),=C'*0'           AGY MUST BE SPECIFIC FOR A OR N          
         BE    INVNAME                                                          
         TM    USRIDF,X'40'                                                     
         BO    INVNAME                                                          
*                                                                               
         CLI   REQACTN,C'N'        NEW REQUEST                                  
         BNE   VALNUM3X                                                         
         OC    FLTUID,FLTUID       UID FILTER CANNOT BE ENTERED                 
         BNZ   INVNAME                                                          
*                                                                               
VALNUM3X B     VALNUMX                                                          
*                                                                               
VALNUM4  CLI   IFLDH+5,5                                                        
         BE    VALNUMX             USE DEFAULT OPTION                           
         CLI   IFLD+5,C','                                                      
         BNE   INVACTN                                                          
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         SH    R5,=H'7'            R5=L'OPTION-1                                
         BM    INVACTN                                                          
         MVC   REQOPTN,IFLD+6      SAVE OPTION                                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'TOTAL'                                              
         BNE   *+16                                                             
         CLI   REQNUM,255          TOTAL OPTION ONLY VALID FOR ALL              
         BNE   INVOPTN                                                          
         B     VALNUMX                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'NEXT'                                               
         BE    VALNUMX                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'LAST'                                               
         BE    VALNUMX                                                          
         MVC   TEMP(5),=C'00000'   N THRU NNNNN OK                              
         LA    R6,4                                                             
         SR    R6,R5                                                            
         BM    INVOPTN                                                          
         LA    R6,TEMP(R6)                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),IFLD+6                                                   
         MVC   TEMP+5(5),=C'00000'                                              
         MVZ   TEMP+5(5),TEMP                                                   
         CLC   TEMP+5(5),=C'00000'                                              
         BNE   INVOPTN                                                          
         PACK  DUB,TEMP(5)                                                      
         CVB   R6,DUB                                                           
         LTR   R6,R6                                                            
         BZ    INVOPTN             LOWEST SEQUENCE NUM IS ONE                   
         STH   R6,REQINCR                                                       
         MVI   REQOPTN,C'S'        SET SEQUENCE NUM OPTION                      
*                                                                               
VALNUMX  CLC   RNUM(2),=C'31'                                                   
         BE    VALCHKS                                                          
         B     VALNUMX2                                                         
*                                                                               
VALCHKS  LA    R6,NOCKTAB                                                       
VALC0    CLI   0(R6),0             END OF TABLE                                 
         BE    INVNUM                                                           
         CLC   0(2,R6),14(R3)                                                   
         BE    VALNUMXX                                                         
         LA    R6,L'NOCKTAB(R6)                                                 
         B     VALC0                                                            
*                                                                               
NOCKTAB  DS    0CL2                TABLE OF AGYS WHO CAN WRITE CHECKS           
         DC    C'T1'               TEACH 1                                      
         DC    XL4'00'                                                          
         DS    0H                                                               
*                                                                               
VALNUMX2 CLC   RNUM(2),=C'66'      OLD SPECIAL FOR DUPONT                       
         BNE   VLX3A2                                                           
         B     INVNUM                                                           
*                                                                               
VLX3A2   DS    0H                                                               
*                                  19 AND 37 FOR EVERYONE NOW                   
VALNUMX3 CLC   RNUM(2),=C'81'                                                   
         BNE   VALNUMX4                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMX4 DS    0H                                                               
         CLC   RNUM(2),=C'R1'      REBATE FINANCIAL BILLING                     
         BE    EXCLUDS                                                          
         CLC   RNUM(2),=C'RD'      REBATE FINANCIAL BILLING - DRAFT             
         BE    EXCLUDS                                                          
         CLC   RNUM(2),=C'E1'                                                   
         BNE   VALNUMX5                                                         
*                                                                               
EXCLUDS  CLC   RAGY(2),=C'DM'      ONLY DM/OM/SJ/XD FOR R1,RD                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'OM'      ONLY DM/OM/SJ/XD FOR R1,RD                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'XD'      ONLY DM/OM/SJ/XD FOR R1,RD                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'WI'      OR WESTERN                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'WJ'      OR WESTERN                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'WR'      OR WESTERN                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'WT'      OR WESTERN                                   
         BE    VALNUMXX                                                         
SJRONLY  CLC   RAGY(2),=C'SJ'                                                   
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMX5 DS    0H                                                               
         CLC   RNUM(2),=C'B1'                                                   
         BNE   VALNUMX6                                                         
         CLC   TWAUSRID,=H'9203'   STAR?                                        
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9067'   STACHO?                                      
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9332'   STARPG?                                      
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9341'   START?                                       
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9638'   STARPMT?                                     
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'10277'  STARLA?                                      
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9210'   MVNY?                                        
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9533'   MVLA?                                        
         BE    INVNUM                                                           
         CLC   TWAUSRID,=H'9486'   BRSA?                                        
         BE    INVNUM                                                           
*                                                                               
         B     VALNUMX6                                                         
*                                                                               
*                 CHECKS FOR MINSHARE ID'S REMOVED 4/28/08                      
*                                                                               
* ONLY 60, 72 & 77 FOR BELLSOUTH AGENCIES                                       
*                                                                               
VALNUMX6 DS    0H                                                               
         CLC   TWAUSRID,=X'1833'   BELLSOUTH AGENCY ID# ?                       
         BL    VALNUMX9            NO                                           
         CLC   TWAUSRID,=X'1837'   BELLSOUTH AGENCY ID# ?                       
         BH    VALNUMX9            NO                                           
         CLC   RNUM(2),=C'60'                                                   
         BE    VALNUMXX                                                         
         CLC   RNUM(2),=C'72'                                                   
         BE    VALNUMXX                                                         
         CLC   RNUM(2),=C'77'                                                   
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMX9 CLC   RNUM(2),=C'PM'                                                   
         BNE   VALNUMXA                                                         
         CLC   RAGY(2),=C'YN'      YN?                                          
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'BS'      BS?                                          
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'H9'      H9?                                          
         BNE   INVNUM                                                           
         B     VALNUMXX                                                         
*                                                                               
VALNUMXA CLC   RNUM(2),=C'CL'                                                   
         BNE   VALNUMXB                                                         
         CLC   RAGY(2),=C'SJ'      TEMP -FOR TEST                               
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'YN'      CL ONLY FOR YN                               
         BNE   INVNUM                                                           
         B     VALNUMXX                                                         
*                                                                               
VALNUMXB CLC   RNUM(2),=C'49'                                                   
         BNE   VALNUMXC                                                         
         CLC   RAGY(2),=C'WI'                                                   
         BE    INVNUM                                                           
         CLC   RAGY(2),=C'WR'                                                   
         BE    INVNUM                                                           
*                                  WT NOW HAS ACCESS  (4/9/08)                  
         B     VALNUMXX                                                         
*                                                                               
VALNUMXC CLC   RNUM(2),=C'GT'                                                   
         BNE   VALNUMXD                                                         
         CLC   RAGY(2),=C'H7'      GROUPM                                       
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'O0'      GT ONLY O0, H9, DR ,TC                       
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'H9'                                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'HY'      OUTTOA                                       
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'DR'                                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'TC'                                                   
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'HD'      HDTO                                         
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'T1'      TCH1                                         
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'UB'      CARAT                                        
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'OO'      OMG                                          
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'OU'      OMG CANADA                                   
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXD CLC   RNUM(2),=C'SN'      SONY INTERFACE                               
         BNE   VALNUMXE                                                         
         CLC   TWAUSRID,=H'12173'   MUST BE ID MCPIX                            
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXE CLC   RNUM(2),=C'CI'      CONTINENTAL                                  
         BNE   VALNUMXF                                                         
         CLC   RAGY,=C'H9'         MUST BE STARCOM                              
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXF CLC   RNUM(2),=C'LT'                                                   
         BNE   VALNUMXG                                                         
         CLC   RAGY(2),=C'HD'      ***TESTING***                                
         BE    VALNUMXX                                                         
         CLC   RAGY(2),=C'U#'      LT ONLY U# (M2 UNIVERSAL)                    
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXG CLC   RNUM(2),=C'SE'      SPRINT                                       
         BNE   VALNUMXH                                                         
         CLC   RAGY,=C'H7'         MUST BE MINDSHARE                            
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'H9'         OR STARCOM  (LOST ACCOUNT)                   
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXH CLC   RNUM(2),=C'PH'      PHILIP MORRIS                                
         BNE   VALNUMXI                                                         
         CLC   RAGY,=C'H9'         MUST BE STARCOM                              
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXI CLC   RNUM(2),=C'LO'      L'OREAL INTERFACE                            
         BNE   VALNUMXJ                                                         
         CLC   RAGY,=C'MC'         MUST BE MCCANN                               
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXJ CLC   RNUM(2),=C'TD'      TAB DELIMITED INTERFACE                      
         BNE   VALNUMXK                                                         
         CLC   RAGY,=C'M2'         MUST BE MEG2                                 
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXK CLC   RNUM(2),=C'WB'      WARNER BROS.  INTERFACE                      
         BNE   VALNUMXL                                                         
         CLC   RAGY,=C'OO'         MUST BE OMG                                  
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'OU'         OR OMDTOA                                    
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'SJ'         OR SJR - TESTING                             
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXL CLC   RNUM(2),=C'JW'      JWT INTERFACE                                
         BNE   VALNUMXM                                                         
         CLC   RAGY,=C'FR'                                                      
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'H7'                                                      
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'O$'                                                      
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'SJ'         SJR FOR TESTING                              
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'JS'                                                      
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'H0'        MSHTOA                                        
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'HY'        OUTTO                                         
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXM CLC   RNUM(2),=C'IN'      NISSAN INTERFACE                             
         BNE   VALNUMXP                                                         
         CLC   RAGY,=C'OU'         FOR OMDTOA                                   
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'OO'         FOR OMDUSEC (USA)                            
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXP CLC   RNUM(2),=C'CH'      CHOICE HOTELS                                
         BNE   VALNUMXQ                                                         
         CLC   RAGY,=C'FM'         FOR FMNY                                     
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXQ CLC   RNUM(2),=C'PZ'      PFIZER INTERFACE                             
         BNE   VALNUMXR                                                         
         CLC   RAGY,=C'UB'         FOR CARNY                                    
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXR CLC   RNUM(2),=C'AI'      AT&T INTERFACE                               
         BNE   VALNUMXS                                                         
         CLC   RAGY,=C'H7'         GROUP M                                      
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXS CLC   RNUM(2),=C'GM'      GM INTERFACE                                 
         BNE   VALNUMXX                                                         
         CLC   RAGY,=C'GZ'                                                      
         BE    VALNUMXX                                                         
         CLC   RAGY,=C'UB'                                                      
         BE    VALNUMXX                                                         
         B     INVNUM                                                           
*                                                                               
VALNUMXX DS    0H                  ABOUT TO VALIDATE MEDIA FIELD                
         BRAS  RE,CKTRAFF          CHECK FOR TRAFFIC LIMIT ACCESS               
         BNE   INVNUM                                                           
*                                                                               
         CLC   RNUM(2),=C'72'      INSERTION ORDERS                             
         BE    VALNUMXY                                                         
         CLC   RNUM(2),=C'79'      SHIPPING LIST                                
         BE    VALNUMXY                                                         
         CLC   RNUM(2),=C'B1'      NEW BILLING                                  
         BE    VALNUMXY                                                         
         CLC   RNUM(2),=C'R1'      REBATE BILLING                               
         BE    VALNUMXY                                                         
         B     VALMED              DONE WITH REQ NUMB FLD, DO MEDIA FLD         
*                                                                               
VALNUMXY DS    0H                  TEST IF USER IS "NON-UPDATIVE"               
         L     RF,ACOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,X'E0'       READ ONLY OR WRONG FACPAK ?                  
         BZ    VALMED              NO - OK - GO DO MEDIA FLD                    
*                                                                               
         MVC   FERN,=AL2(NOTUPD)   UPDATES NOT ALLOWED ERROR                    
         XC    BVRRNAM,BVRRNAM                                                  
         B     DISPERR                                                          
*                                                                               
MISNUM   MVC   FERN,=AL2(FLDMIS)   MISSING ERROR                                
         XC    BVRRNAM,BVRRNAM                                                  
         B     DISPERR                                                          
*                                                                               
INVNUM   XC    BVRRNAM,BVRRNAM                                                  
         MVC   FERN,=AL2(NUMINV)   INV REQ NUM                                  
DISPERR  FOUT  BVRRNAMH                                                         
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
INVACTN  MVC   FERN,=AL2(ACTINV)   INV ACTION                                   
         B     DISPERR                                                          
INVOPTN  MVC   FERN,=AL2(OPTINV)   INV INPUT                                    
         B     DISPERR                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE MEDIA & SET FIND X'02' = MEDIA INPUT                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALMED   CLI   BVRMEDH+5,0                                                      
         BNE   VALMEDA                                                          
         XC    BVRMEDN(10),BVRMEDN                                              
         B     VALDEST                                                          
*                                                                               
VALMEDA  MVC   KRT1+2(1),BVRMED                                                 
         CLI   BVRMED,C'*'         ALL MEDIA                                    
         BE    VALMEDB             SKIP AGYHDR READ                             
         CLI   BVRMED,C'C'                                                      
         BE    VALMEDB                                                          
*****    CLI   AGY,C'*'                                                         
         CLC   AGY(2),=C'*0'                                                    
         BE    VALMEDB             CAN'T READ AGY HEADER                        
         MVI   KRT1+3,X'01'        AGY REC CODE                                 
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FF)                                                    
         BNE   INVMED                                                           
*                                                                               
VALMEDB  MVC   REQMED,BVRMED                                                    
         MVC   RMED,BVRMED                                                      
VALMEDG  MVC   KRT1+2(1),BVRMED                                                 
         MVC   KRT2+2(1),BVRMED                                                 
         MVC   KUB1+0(1),BVRMED                                                 
         OI    FIND,X'02'         MEDIA INPUT                                   
         LA    R4,MEDTBL                                                        
VALMED1  CLI   0(R4),0                                                          
         BE    INVMED                                                           
         CLC   0(1,R4),BVRMED                                                   
         BE    VALMED2                                                          
         LA    R4,L'MEDTBL(R4)                                                  
         B     VALMED1                                                          
VALMED2  MVC   REQMED,0(R4)                  SAVE MEDIA VALUE                   
         MVC   RMED,0(R4)                    MEDIA TO REQ REC                   
         MVC   REQMED1,1(R4)                 SAVE MEDIA BIT-MASK                
         OI    FIND,X'02'                                                       
         CLI   BVRMED,C'*'         ALL MEDIA                                    
         BNE   VMED2C                                                           
*                                                                               
         TM    MYFLAG,SBREPT       DISALLOW FOR SOON BILLING                    
         BO    INVMED              LOCKING ISSUES FOR NOW                       
*                                                                               
         MVC   PRTREC+140(10),=CL10'ALL MEDIA '                                 
         CLC   RNUM,=C'10'                                                      
         BNE   VALMED2B                                                         
         CLC   RAGY,=C'SJ'      SJR ALSO                                        
         BE    VALMED4                                                          
         CLC   RAGY,=C'HD'      OR HDTO                                         
         BE    VALMED4                                                          
         CLC   RAGY,=C'MC'      MCCANN OR DF CAN DO MEDIA * P10                 
         BE    VALMED4                                                          
         CLC   RAGY,=C'U#'      M2TOA CAN DO MEDIA * P10                        
         BE    VALMED4                                                          
         CLC   RAGY,=C'WT'      WITO CAN DO MEDIA * P10                         
         BE    VALMED4                                                          
         CLC   RAGY,=C'DF'      10 ALLMEDIA ONLY FOR DF AGY (SAATCHI)           
         BNE   INVMED                                                           
*                                                                               
VALMED2B CLC   RNUM,=C'L1'                                                      
         BE    VALMED4                                                          
         CLC   RNUM,=C'LB'                                                      
         BE    VALMED4                                                          
         CLC   RNUM,=C'RA'                                                      
         BE    VMED2A                                                           
         CLC   RNUM,=C'D1'                    B1/D1/E1 ALLMEDIA ONLY            
         BE    VMED2A                            FOR SJR/DO AGYS                
         CLC   RNUM,=C'B1'                                                      
         BE    VMED2A                                                           
         CLC   RNUM,=C'E1'                                                      
         BNE   VALMED4                                                          
*                                                                               
*                              FOR MEDIA * BI BILLING TO WORK                   
*                              PROPERLY THE B1 PROFILE SHOULD BE                
*                              SET TO READ ACROSS MEDIA FOR NEXT INV.           
*                                                                               
VMED2A   CLC   RAGY,=C'SJ'                                                      
         BE    VALMED4                                                          
         CLC   RAGY,=C'PJ'                                          L02         
         BE    VALMED4                                              L02         
         CLC   RAGY,=C'PA'      PATO ADDED 3/19/90                  L05         
         BE    VALMED4                                              L05         
         CLC   RAGY,=C'DM'                                                      
         BNE   INVMED                                                           
         B     VALMED4                                                          
*                                                                               
VMED2C   CLI   BVRMED,C'C'                                                      
         BNE   VALMED4                                                          
         CLC   RNUM,=C'L1'                                                      
         BE    VMED2E                                                           
         CLC   RNUM,=C'LB'                                                      
         BNE   INVMED                                                           
VMED2E   MVC   PRTREC+140(10),=C'COMBINED  '                                    
*                                                                               
VALMED4  FOUT  BVRMEDNH,PRTREC+140,10        MEDIA NAME                         
         B     VALDEST                                                          
*                                                                               
MISMED   MVC   FERN,=AL2(FLDMIS)             MISSING MEDIA                      
         B     *+10                                                             
INVMED   MVC   FERN,=AL2(FLDINV)             INVALID MEDIA                      
         LA    R7,BVRMEDH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
MEDTBL   DS    0CL2                          MEDIA VALUE,BIT-MASK,KEY           
         DC    C'N',B'10000000'                                                 
         DC    C'M',B'01000000'                                                 
         DC    C'S',B'01000000'                                                 
         DC    C'T',B'01000000'                                                 
         DC    C'I',B'01000000'           INTERACTIVE                           
         DC    C'L',B'01000000'           SOCIAL                                
         DC    C'B',B'01000000'           MOBILE                                
         DC    C'D',B'01000000'           DIGITAL AUDIO                         
         DC    C'V',B'01000000'           NAT. VIDEO                            
         DC    C'W',B'01000000'           LOC. VIDEO                            
         DC    C'O',B'00100000'           OUTDOOR                               
         DC    C'*',B'00010000'           ALL MEDIA                             
         DC    C'C',B'00010000'           COMBINE MEDIA                         
MEDTBLX  DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*        VALIDATE DESTINATION ID NAME                                           
*                                                                               
VALDEST  EQU   *                                                                
         MVC   REQORIG,USRID                                                    
*                                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALDESTX                                                         
*                                                                               
         CLI   BVRDESTH+5,0                                                     
         BNE   VDESTA              DEST INPUT?                                  
         CLC   RNUM,=C'98'         P99 REQUEST AND DEST MISSING                 
         BNE   VALDESTX            IF SO,FORCE TO SJR                           
         MVC   BVRDEST(3),=C'SJR'                                               
         MVI   BVRDESTH+5,3                                                     
*                                                                               
VDESTA   SR    R1,R1                                                            
         IC    R1,BVRDESTH+5                                                    
         BCTR  R1,0                                                             
         MVI   IFLD,C' '           SET DEST ID NAME IN IFLD                     
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRDEST                                                  
*                                                                               
         CLC   RNUM,=C'98'          P98 REQUEST?                                
         BNE   VDESTC                                                           
         CLC   IFLD(10),=CL10'DDS'  DEST MUST BE DDS OR SJR                     
         BE    VDESTC                                                           
         CLC   IFLD(10),=CL10'SJR'                                              
         BE    VDESTC                                                           
         B     INVDEST                                                          
*                                                                               
*                                                                               
VDESTC   CLC   IFLD(10),=CL10'DDS'                                              
         BNE   VDEST0                                                           
         MVC   REQDEST(2),=X'005A'                                              
         B     VALDESTX                                                         
*                                                                               
VDEST0   DS    0H                                                               
         LA    R4,PRTREC           READ ORIGIN ID REC                           
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),REQORIG                                              
         CLC   IFLD(10),=CL10'DDS'                                              
         BE    VDEST1                                                           
         CLC   IFLD(10),=CL10'SJR'                                              
         BE    VDEST1                                                           
         B     VDEST2                                                           
*                                                                               
VDEST1   MVC   CTIKID(10),IFLD                                                  
VDEST2   GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DATAMGR                                                       
         GOTO1 GETIDS,PLIST,(C'D',PRTREC),0,(R5)                                
         CLI   PLIST,0                                                          
         BE    INVDEST             NO DESTS FOUND                               
         CLI   PLIST,X'FF'                                                      
         BNE   *+6                 DISK ERROR                                   
         DC    H'0'                                                             
         ZIC   R5,PLIST             NUMBER OF DESTS                             
         L     R6,PLIST+4           ADDR OF BLOCK OF DESTS                      
*                                                                               
VDEST4   CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    INVDEST                                                          
         CLC   IFLD(10),0(R6)                                                   
         BNE   VDEST5                                                           
         MVC   REQDEST,10(R6)      DEST ID NUMBER                               
         B     VALDESTX                                                         
*                                                                               
VDEST5   LA    R6,12(R6)                                                        
         BCT   R5,VDEST4                                                        
         B     INVDEST                                                          
*                                                                               
*                                                                               
INVDEST  MVC   FERN,=AL2(FLDINV)                                                
         LA    R7,BVRDESTH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALDESTX EQU   *                                                                
         EJECT                                                                  
*        VALIDATE OUTPUT TYPE                                                   
*                                                                               
VALOUT   EQU   *                                                                
*                                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALOUTX                                                          
*                                                                               
         CLI   BVROUTH+5,0                                                      
         BNE   VALOUT1             NO OUTPUT TYPE INPUT                         
*                                                                               
         TM    MYFLAG,SBREPT       SOON BILLING?                                
         BO    INVOUT              MUST ENTER SOON                              
         B     VALOUTX                                                          
*                                                                               
VALOUT1  DS    0H                                                               
         CLC   RNUM,=C'92'         P92, CLOSEOUT?                               
         BE    INVOUT              NO OUTPUT TYPE IS ALLOWED FOR P92            
*                                                                               
         CLC   BVROUT(4),=C'SOON'                                               
         BE    VALOUT7                                                          
*                                                                               
         TM    MYFLAG,SBREPT       SOON BILLING?                                
         BO    INVOUT              MUST ENTER SOON                              
*                                                                               
         CLI   BVROUTH+5,6                                                      
         BH    INVOUT                                                           
         SR    R1,R1                                                            
         IC    R1,BVROUTH+5                                                     
         BCTR  R1,0                                                             
         MVI   IFLD,C' '           SET OUTPUT TYPE IN IFLD                      
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVROUT                                                   
         CLC   IFLD(7),=C'DIRECT ' ALLOW DIRECT FOR BILLING                     
         BNE   VALOUTA                                                          
         B     VALOUT2                                                          
         SPACE                                                                  
VALOUTA  CLC   RNUM,=C'04'         NO OUTPUT TYPE ALLOWED FOR BILLING           
         BE    INVOUT                                                           
         CLC   RNUM,=C'06'         NO OUTPUT TYPE ALLOWED FOR BILLING           
         BE    INVOUT                                                           
         CLC   RNUM,=C'B1'    NO OUTPUT TYPE ALLOWED FOR NEW BILLING            
         BE    INVOUT                                                           
*                                                                               
*                                                                               
VALOUT2  DS    0H                                                               
         LA    R4,PRTREC           READ OUTPUT TYPE RECORD                      
         USING CTOREC,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID,IFLD                                                      
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   INVOUT                                                           
         LA    R6,CTOREC+28                                                     
VALOUT3  CLI   0(R6),X'38'                                                      
         BE    VALOUT4                                                          
         CLI   0(R6),0             END OF REC                                   
         BE    INVOUT                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VALOUT3                                                          
*                                                                               
VALOUT4  DS    0H                                                               
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      SEE IF REQUESTABLE OUTPUT TYPE               
         BZ    INVOUT              NO                                           
         MVC   REQOUT,IFLD         SET OUTPUT TYPE IN REQ REC HDR               
         B     VALOUTX                                                          
*                                                                               
VALOUT7  DS    0H                                                               
         CLI   BVROUTH+5,5         MUST BE SOON,NN                              
         BNH   INVOUT                                                           
**       TM    3(R7),X'10'         THIS BYTE IS USED                            
**       BZ    INVOUT                                                           
**       B     VALOUTX                                                          
         LA    R1,SOONTAB                                                       
VALOUT7C CLI   0(R1),X'FF'                                                      
         BE    INVOUT                                                           
         CLC   RNUM(2),0(R1)                                                    
         BE    VALOUTS                                                          
         LA    R1,2(R1)                                                         
         B     VALOUT7C                                                         
*                                                                               
* SOONABLE REPORTS TABLE                                                        
*                                                                               
SOONTAB  DS    0H                                                               
         DC    C'07'                                                            
         DC    C'12'                                                            
         DC    C'14'                                                            
         DC    C'16'               TEST RUNS ONLY                               
         DC    C'18'                                                            
         DC    C'19'                                                            
         DC    C'27'                                                            
         DC    C'28'                                                            
         DC    C'37'                                                            
         DC    C'41'                                                            
         DC    C'43'                                                            
         DC    C'46'                                                            
         DC    C'49'                                                            
         DC    C'52'                                                            
         DC    C'60'                                                            
         DC    C'74'                                                            
         DC    C'77'                                                            
         DC    C'98'               STATISTICS REPORT                            
         DC    C'AI'               AT&T INTERFACE - TEST RUNS ONLY              
         DC    C'AC'               ADV CONTRACTS                                
         DC    C'AR'               ADV CONTRACT REPORT                          
         DC    C'AU'               ADV UNTILZATION                              
         DC    C'BA'               TEST RUNS ONLY                               
         DC    C'B1'               UPDATIVE SOON BILLING                        
         DC    C'CI'               CONTINENTAL AIR LINES INTERFACE              
         DC    C'CM'               COPY MEDIA (CLT, PRD, ??)                    
         DC    C'E1'                                                            
         DC    C'EC'                                                            
         DC    C'D1'                                                            
         DC    C'L1'                                                            
         DC    C'L2'                                                            
         DC    C'RD'               REBATE DRAFT                                 
         DC    C'S2'                                                            
         DC    C'TS'               TEARSHEET REPORT                             
         DC    C'NT'               NVTEXT LISTING                               
         DC    C'IC'               I/OCOM LISTING                               
         DC    C'CC'               CONCOM LISTING                               
         DC    C'NV'               NVR-NV REPORT/LETTERS                        
         DC    C'GT'                                                            
         DC    C'LT'               LABATT EXTRACT                               
         DC    C'PH'               PHILIP MORRIS INTERFACE                      
         DC    C'SE'               SPRINT INTERFACE                             
         DC    C'LO'               L'OREAL INTERFACE                            
         DC    C'TD'               TAB DELIMITED INTERFACE                      
         DC    C'WB'               WARNER BROS. INTERFACE                       
         DC    C'IN'               NISSAN INTERFACE                             
         DC    X'FFFF'                                                          
*                                                                               
INVOUT   MVC   FERN,=AL2(FLDINV)                                                
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALOUTS  CLC   RNUM,=C'B1'        BILLING?                                      
         BNE   VALOUTS5                                                         
*                                                                               
         TM    MYFLAG,SBREPT     MUST BE SOON BILLING (ENTERED AS SB)           
         BZ    INVOUT                                                           
*                                                                               
*        BYPASS THIS CHECK - ALLOWS SOON BILLING FOR ALL AGENCIES               
*                                                                               
         B     VALOUTX       CODE BELOW LEFT SO THAT I CAN PATCH                
*                                                                               
         LA    R1,SB1AGYT    TABLE OF AGENCIES ALLOWED SOON BILLING             
VALOUTS3 DS    0H                                                               
         CLI   0(R1),X'FF'   END OF TABLE                                       
         BE    INVOUT                                                           
         CLC   0(2,R1),RAGY                                                     
         BE    VALOUTX                                                          
         LA    R1,2(R1)                                                         
         B     VALOUTS3                                                         
*                                                                               
SB1AGYT  DC    C'SJ'         ALLOW SOON BILLING FOR THESE AGENCIES              
         DC    C'HD'         HDTO                                               
         DC    C'MC'         MCCANN                                             
         DC    C'WD'         WIEDEN & KENNEDY                                   
         DC    C'O$'         OGILVY/MONE                                        
         DC    C'M$'         MUWS                                               
         DC    C'  '         SPARE THAT COULD BE PATCHED                        
         DC    C'  '         ANOTHER                                            
         DC    C'  '         AND ANOTHER                                        
         DC    X'FFFF'       END OF TABLE                                       
*                                                                               
VALOUTS5 CLC   RNUM,=C'98'   DON'T ALLOW SOON 98 REQUESTS FOR MEDIA *           
         B     VALOUTX                                                          
**       BNE   VALOUTX                                                          
**       CLI   BVRMED,C'*'                                                      
**       BE    INVMED                                                           
*                                                                               
VALOUTX  EQU   *                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* CHECK IF REQUESTOR IS COMPATIBLE WITH REQUEST NUMBER                          
*                                                                               
VALREQ   TM    3(R7),X'04'                   FOR DDS ONLY                       
         BZ    *+12                                                             
         CLI   DDS,1                         YES MUST BE DDS TERM               
         BNE   INVNUM                                                           
*                                                                               
         CLI   REQACTN,C'D'                                                     
         BNE   VALREQ0                                                          
         MVC   REQNDX1(2),=X'FFFE'           SET ENQ SCR REQUIRED               
         CLC   =C'YES',FLTUID                                                   
         BNE   *+10                                                             
         MVC   REQNDX1(2),=X'FFEF'           SET USR SCR REQUIR                 
*                                                                               
         CLI   REQOPTN,C'T'                                                     
         BNE   VALREQ7                                                          
         MVC   REQNDX1(2),=X'FFFC'   MENU SCREEN FOR TOTAL OPTION               
         B     VALREQ7                                                          
VALREQ0  TM    FIND,X'08'                    CARD REQUEST                       
         BZ    VALREQ1                                                          
         MVC   REQNDX1(2),=X'FFFD'           SET CARD SCR REQUIRED              
         B     VALREQ7                                                          
VALREQ1  TM    3(R7),X'08'                   ONLY AVAIL AS CARD REQ             
         BO    INVNAME                                                          
         TM    3(R7),X'01'                   IS REQUESTOR REQUIRED              
         BZ    VALREQ2                       NO                                 
         TM    FIND,X'01'                    WAS REQUESTOR INPUT                
         BZ    MISNAME                       NO ERROR                           
VALREQ2  TM    3(R7),X'02'                   IS MEDIA REQUIRED                  
         BO    VALREQ3                       YES                                
         TM    FIND,X'02'                    WAS MEDIA INPUT                    
         BO    VALREQ3                       YES                                
         LA    R4,MEDTBL                     FIND DEFAULT MEDIA                 
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),1(R4)                                                     
         NC    DUB(1),29(R7)                                                    
         BNZ   *+12                                                             
         LA    R4,L'MEDTBL(R4)                                                  
         B     *-30                                                             
         MVC   BVRMED,0(R4)                                                     
         MVC   BVRMEDH+4(2),=X'C001'                                            
         B     VALMED                                                           
*                                                                               
VALREQ3  TM    FIND,X'02'                    WAS MEDIA INPUT                    
         BZ    MISMED                        NO ERROR                           
         MVC   DUB(1),REQMED1                YES CHECK WITH REQ VALUES          
         NC    DUB(1),3(R7)                                                     
         BZ    INVMED                                                           
*                                                                               
VALREQ4  LA    R8,28(R7)                     FIND FLD LIST FOR MEDIA            
         SR    R6,R6                                                            
VALREQ5  CLI   0(R8),0                       R8=A(REQTBL MEDIA ENTRY)           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),1(R8)                                                     
         NC    DUB(1),REQMED1                                                   
         BNZ   VALREQ6                                                          
         IC    R6,0(R8)                                                         
         AR    R8,R6                                                            
         B     VALREQ5                                                          
*                                                                               
VALREQ6  L     R6,AREQTBL                                                       
         SR    R8,R6                         SET SCR LIST REQUIRED = ..         
         STH   R8,REQNDX1                    SAVE INDEX TO REQTBL               
         AR    R8,R6                                                            
*                                                                               
VALREQ7  L     R6,AREQTBL                                                       
         SR    R7,R6                                                            
         STH   R7,REQNDX                     SAVE INDEX TO REQTBL               
         AR    R7,R6                                                            
         MVC   REQFMT,FIND                                                      
         EJECT                                                                  
*        CHECK IF THIS REQUEST IS COMPATIBLE WITH THE PREVIOUS REQUEST          
*                                                                               
VALDEFN  CLI   REQACTN,C'A'                                                     
         BNE   VALDEF1                                                          
         CLI   PREQACTN,C'N'                 AMEND ONLY VALID AFTER NEW         
         BNE   INVACTN                                                          
         CLC   REQNDX1(2),PREQNDX1           SAME SCREEN REQUIRED               
         BNE   INVACTN                       NO CANT AMEND                      
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    INVACTN                                                          
         B     VALIPT                        OK TO AMEND                        
*                                                                               
VALDEF1  CLI   REQNDX1,X'FF'                 ENQ OR CARD SCR REQUIRED           
         BNE   VALIPT                        NO                                 
         CLC   PREQNDX1(2),REQNDX1           YES IS IT ALREADY LOADED           
         BNE   *+12                                                             
         CLI   REQNDX1+1,X'FC'                                                  
         BNE   VALDEF2                                                          
         XC    DISPFLDS(2),DISPFLDS          SET NO REQUESTS DISPLAYED          
         MVC   PLIST+4(4),=X'D90412FF'                                          
         MVC   PLIST+7(1),REQNDX1+1                                             
         GOTO1 CALLOV,PLIST,BVRFRSTH                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PREQNDX1(2),REQNDX1           SAVE SCR LOADED                    
*                                                                               
         LA    R6,BVRFRSTH         RETRANSMIT HDR FIELDS                        
         SR    R7,R7                                                            
         LA    R8,64(R3)                                                        
         OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   *-12                                                             
*                                                                               
         CLI   REQNDX1+1,X'FD'     BUILD REQMAP FOR CARD REQ SCREEN             
         BNE   VALDEF2                                                          
         LA    R5,BVRFRSTH         FIND 1ST UNPROT DATA FIELD                   
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         SR    R5,R3                                                            
         STH   R5,DUB                                                           
         MVI   LREQMAP,126                                                      
         MVC   LREQMAP+1(2),DUB                                                 
         MVI   LREQMAP+3,127                                                    
         MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         B     DEFAULT                                                          
*                                                                               
VALDEF2  CLI   REQNDX1+1,X'FE'                                                  
         BE    VALDEF2A                                                         
         CLI   REQNDX1+1,X'EF'     USR SCR                                      
         BNE   VALDEF3                                                          
*                                                                               
VALDEF2A MVI   STATUS,3                      SET ENQ/CANC STATUS                
         MVC   REQFLTR,FIND                  SAVE ENQ/CANC FILTERS              
         B     SAVEDATA                                                         
VALDEF3  CLI   REQNDX1+1,X'FD'               CARD REQUEST                       
         BE    VALIPT1                       YES                                
         MVI   STATUS,4                      SET MENU DISPLAY STATUS            
         CLI   REQOPTN,C'T'                                                     
         BE    VALDEF2A                                                         
         B     SAVEDATA                                                         
         EJECT                                                                  
*        VALID TO INPUT BEYOND HEADR FOR STATUS=0 ONLY IF A NEW SCREEN          
*        IS NOT REQUIRED FOR NEW REQUEST DEFINITION                             
VALIPT   CLC   PREQNDX1(2),REQNDX1           SCR FLD LIST CHANGED               
         BNE   BUILDER                       YES MUST BUILD SCR                 
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    BUILDER                                                          
*                                                                               
VALIPT1  LA    R5,BVRFRSTH                   FIND 1ST UNPROT DATA FLD           
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         CLI   LREQMAP,127                   ZERO INPUT REQUEST                 
         BE    *+12                          YES                                
         C     R5,ALASTF                     ANY INPUT IN DATA AREA             
         BH    VALIPT3                       NO                                 
VALIPT2  MVI   STATUS,2                      SET REQUEST DATA INPUT             
         ST    R5,AFIRSTF                                                       
         B     DEFAULT                                                          
*                                                                               
VALIPT3  CLI   REQNDX1,X'FF'                                                    
         BNE   VALIPT4                                                          
         MVI   STATUS,1            CARD REQUEST                                 
         B     VALIPT2                                                          
VALIPT4  MVI   STATUS,1            DATA REQUEST                                 
         B     VALIPT2                                                          
         EJECT                                                                  
*        BUILD A NEW SCREEN FROM REQTBL - R7=A(REQTBL ENTRY)                    
*                                                                               
*          DATA SET SPREQ01    AT LEVEL 025 AS OF 08/28/95                      
BUILDER  DS    0H                                                               
* - STEREO NEEDS SOME HELP - WHEN SWITCHING FROM DISPLAY BACK TO                
*   REQ SCREEN, SCREEN NUMBER IN SERVICE REQUEST FIELD DOES NOT CHANGE          
*   SO TELL SYSTEM WE ARE PASSING OWN SCREEN NUMBER                             
*                                                                               
         BAS   RE,CHKSTREO        GO SEE IF FULL STEREO                         
         BNO   BLD5                                                             
*                                                                               
         CLC   =C'12',BVRSRV+2    IF STEREO                                     
         BNE   BLD5                                                             
         L     R1,APARM            GET FATIOB                                   
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN                                                
         MVI   TIOBCNT,X'FF'                                                    
         DROP  R1                                                               
*                                                                               
BLD5     MVI   STATUS,1                      SET REQUEST DATA REQUIRED          
         LA    RA,LREQMAP                    RA=A(REQMAP ENTRY)                 
         LR    R4,R8                         R4=A(REQTBL ENTRY)                 
         LA    R4,2(R4)                                                         
         LA    R8,BVRFRSTH                   R8=A(NEXT TWA BYTE)                
         LA    R5,6                          R5=LAST TWA LINE NUMBER            
         SR    R6,R6                                                            
         MVC   HALF,=H'40'                                                      
         B     REQLOO5B                                                         
*                                                                               
REQLOOP  L     R7,=A(TWATBL)                    R7=A(TWATBL ENTRY)              
         A     R7,RELO                                                          
*                                                                               
TWALOOP  IC    R6,1(R7)                      FIND ENTRY IN TWATBL               
         LTR   R6,R6                                                            
         BNZ   TWALOOP1                                                         
         LR    RF,RA                         FLD MUST BE A SUB FLD              
         SH    RF,=H'3'                                                         
         MVC   0(1,RA),0(R4)                 SET FLD NUM                        
         MVC   1(2,RA),1(RF)                 SET FLD ADR TO PREVIOUS            
         LA    RA,3(RA)                      BUMP REQ MAP ENTRY                 
         B     REQLOOP5                                                         
TWALOOP1 CLC   0(1,R7),0(R4)                                                    
         BE    REQLOOP1                                                         
         LA    R7,3(R7,R6)                                                      
         B     TWALOOP                                                          
*                                                                               
REQLOOP1 TM    REQFMT,X'04'                                                     
         B     REQLOOP2                                                         
*        IF THE ABOVE INSTRUCTION IS CHANGED TO BZ THEN THE FOLLOWING           
*        CODE WILL CAUSE THE DDS OPTION TO OUTPUT TWO FIELDS PER LINE           
*        AND TO IGNORE ALL COMMENT FIELDS.                                      
         CLI   COMSW,1                                                          
         BNE   RECLP1A                                                          
         SR    R6,R6                         IGNORE ALL COMMENTS                
         B     REQLOOP5                                                         
*                                                                               
RECLP1A  TM    0(R4),X'80'                   DDS OPTION                         
         BZ    *+10                                                             
         SR    R6,R6                         IGNORE ALL COMMENTS                
         B     REQLOOP5                                                         
         OC    HALF,HALF                                                        
         BNZ   REQLOO25                                                         
         MVC   HALF,=H'40'                   SET TO 2ND HALF                    
         B     REQLOOP3                                                         
*                                                                               
REQLOOP2 DS    0H                                                               
         CLI   COMSW,1                                                          
         BNE   REQLOO25                                                         
         TM    0(R4),X'01'                                                      
         BO    REQLOO28                                                         
*                                                                               
REQLOO25 XC    HALF,HALF                     BUMP TO NEW LINE                   
         LA    R5,1(R5)                                                         
REQLOO28 CH    R5,=H'24'                                                        
         BNH   *+6                                                              
         DC    H'0'                          TOO MANY LINES                     
*                                                                               
REQLOOP3 MVC   0(8,R8),=X'1A20010200008012'  MOVE STD PROT FLD TO TWA           
         CLI   COMSW,1                                                          
         BE    REQLOO3B                                                         
*                                                                               
         CLI   0(R7),127                                                        
***      BL    REQLOO3A                                                         
***      BH    *+14                                                             
         BNE   REQLOO3A                                                         
         MVC   0(8,R8),=X'0920000200008001'  MOVE TAB PROT FLD TO TWA           
         B     REQLOO3A                                                         
REQLOO3B STC   R6,7(R8)                      SET LENGTH FOR COMMENTS            
         LA    RF,8(R6)                                                         
         STC   RF,0(R8)                                                         
         MVC   2(2,R8),=X'002A'              SET COL#42 FOR COMMENTS            
*                                                                               
REQLOO3A SR    RF,RF                         SET PROT FLD TEXT                  
         IC    RF,7(R8)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R8),8(R8)                                                    
*                                                                               
         BCTR  R6,0                                                             
*                                                                               
         CLI   COMSW,1                SEE IF DOING A COMMENT                    
         BE    REQLOO3F                                                         
         CLI   0(R4),127             OR END OF TABLE                            
         BE    REQLOO3F                                                         
*                                                                               
         BAS   RE,CHKSTREO           FIRST SEE IF FULL STEREO                   
         BNO   REQLOO3F                                                         
*                                                                               
         CLC   =C'12',BVRSRV+2       SEE IF STEREO                              
         BE    DOSTEREO                                                         
         CLI   BVRSRV+2,X'40'    COULD BE BLANK IN STEREO IF SWITCHING          
         BH    REQLOO3F                                                         
*                                                                               
DOSTEREO DS    0H                                                               
         BAS   RE,STEREOID          RETURN SPECIAL DATA FOR STEREO              
         B     REQLOO3G                                                         
*                                                                               
REQLOO3F DS    0H                                                               
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),3(R7)                                                    
*                                                                               
REQLOO3G DS    0H                                                               
         LA    R6,8(RF)                      R6=TOT PROT TWA LEN                
         IC    RF,2(R7)                                                         
         LTR   RF,RF                                                            
         BZ    REQLOO3X                                                         
         LA    RE,0(R8,R6)                                                      
         MVC   0(8,RE),=X'0000001500008000'  SET UNPROT FLD HDR                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                 NULL TEXT                          
*                                                                               
         CLI   0(R7),X'99'     SEE IF DOING STATUS FILTER (153)                 
         BNE   REQLOO3S                                                         
         MVI   8(RE),C'A'          DISPLAY 'A' IN FIELD                         
         MVI   5(RE),1             SET INPUT LENGTH TO ONE                      
*                                                                               
REQLOO3S DS    0H                                                               
         STC   RF,7(RE)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(RE)                                                         
REQLOO3X AR    R6,RF                         SET R6=TOTAL TWA LENGTH            
*                                                                               
         SR    R0,R0                         CALC ABS SCREEN ADDR               
         LR    R7,R8                         R7=A(FLD IN TWA)                   
REQLOOP4 CLI   0(R7),0                                                          
         BE    REQLOOP5                                                         
         MVC   DUB(2),2(R7)                  DUB=X'XXCC' CC=COL NUM             
***      CLI   0(R4),127                                                        
***      BNL   REQLOO45                                                         
         CLI   COMSW,1                                                          
         BE    REQLOO45                                                         
*                                                                               
         CLI   DUB,0                         SPECIAL ATTRIBUTE REQD             
         BE    REQLOO45                      NO                                 
         TM    1(R4),X'01'                   OPTIONAL INPUT FLD                 
         BO    REQLOO45                      YES                                
         OI    1(R7),X'08'                   SET HIGH INTENSITY                 
         NI    1(R7),X'FB'                                                      
         IC    R0,0(R7)                      SET FLD REQUIRED INDIC             
         LR    RF,R7                                                            
         AR    RF,R0                                                            
         BCTR  RF,R0                                                            
         MVI   0(RF),C'='                                                       
REQLOO45 MVI   DUB,0                         DUB=X'00CC'                        
         LH    RE,DUB                                                           
         AH    RE,HALF                                                          
         BCTR  RE,R0                                                            
         LR    RF,R5                                                            
         BCTR  RF,R0                                                            
         MH    RF,=H'80'                                                        
         AR    RF,RE                                                            
         STH   RF,DUB                        DUB=X'AAAA'=ABS SCR ADR            
         MVC   2(2,R7),DUB                                                      
         TM    1(R7),X'20'                   IS FLD PROTECTED                   
         BO    REQLOO47                      YES                                
         LR    RF,R7                         NO GET RELATIVE TWA ADR            
         SR    RF,R3                                                            
         MVC   0(1,RA),0(R4)                 SET FLD NUM                        
         STC   RF,2(RA)                      SET FLD ADR TO REL TWA ADR         
         SRL   RF,8                                                             
         STC   RF,1(RA)                                                         
         LA    RA,3(RA)                      BUMP REQ MAP ENTRY                 
REQLOO47 IC    R0,0(R7)                                                         
         AR    R7,R0                                                            
         B     REQLOOP4                                                         
*                                                                               
REQLOOP5 AR    R8,R6                         UPDATE NEXT TWA ADR                
REQLOO5A CLI   0(R4),127                     BUMP REQ TBL ENTRY                 
         BE    REQLOOPX                      LAST ENTRY WAS TAB                 
***      BL    *+12                                                             
         CLI   COMSW,1                                                          
         BNE   *+12                                                             
*                                                                               
         LA    R4,1(R4)                      LAST ENTRY WAS COMMENT             
         B     *+8                                                              
         LA    R4,3(R4)                      LAST ENTRY WAS DATA                
REQLOO5B DS    0H                                                               
         MVI   COMSW,0                                                          
         CLI   0(R4),0                                                          
         BE    REQLOOP6                                                         
***      CLI   0(R4),127                                                        
***      BH    REQLOOP                                                          
         CLI   0(R4),X'01'                                                      
         BNE   REQLOO5D                                                         
         LA    R4,1(R4)                                                         
         L     R7,=A(COMTBL)                                                    
         A     R7,RELO                                                          
         MVI   COMSW,1                                                          
         B     TWALOOP                                                          
*                                                                               
REQLOO5D DS    0H                                                               
         CLC   RNUM(2),=C'92'                CLOSE-OUT REQ?                     
         BNE   REQLOOP                                                          
         CLI   DDS,0                         DDS ID?                            
         BNE   REQLOOP                                                          
         CLC   0(2,R4),=X'E505'              SKIPONLY FIELD?                    
         BE    REQLOO5A                      OMIT FOR NON-DDS                   
         B     REQLOOP                                                          
*                                                                               
**QLOO5D B     REQLOOP                                             L07          
******5D TM    2(R4),X'80'                   DDS ENTRY                          
******   BZ    REQLOOP                       NO OK FOR ALL SCREENS              
******   TM    REQFMT,X'04'                  YES ONLY FOR DDS FORMAT            
******   BZ    REQLOO5A ********** NO LONGER USED                  L07          
******   B     REQLOOP                                                          
*                                                                               
REQLOOP6 LA    R4,=X'7F0000'                 SET FOR TAB LINE                   
         B     REQLOOP                                                          
*                                                                               
REQLOOPX MVC   0(3,R8),=X'000100'            SET B,A=CLEAR,NOTHING              
         LA    R8,64(R3)           RETRANSMIT REQ DEFN SCR                      
         SR    R7,R7                                                            
         LA    R6,BVRFRSTH                                                      
REQLXL   OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   REQLXL                                                           
*                                                                               
         MVC   PREQNDX1(2),REQNDX1           SET FLD LIST SCR LOADED            
         CLI   LREQMAP,127                   IS 1ST FLD TAB LINE                
         BNE   DEFAULT                       NO                                 
         MVI   STATUS,2                      YES SET REQ DATA INPUT             
         MVC   HALF,LREQMAP+1                                                   
         LH    R7,HALF                                                          
         AR    R7,R3                                                            
         ST    R7,AFIRSTF                    SIMULATE INPUT                     
         EJECT                                                                  
*        SET DEFAULT VALUES IN REQUEST RECORD (IF ANY)                          
*                                                                               
DEFAULT  L     R7,=A(REQROUTS)                                                  
         A     R7,RELO                                                          
DEFAULT1 CLI   0(R7),0                                                          
         BE    SAVEDATA                      NOT IN TBL NO DEFAULTS             
         CLC   0(2,R7),REQNUM                                                   
         BE    DEFAULT2                                                         
         LA    R7,L'REQROUTS(R7)                                                
         B     DEFAULT1                                                         
DEFAULT2 MVC   DUB+1(3),2(R7)                                                   
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO                       RF=A(DEFAULT ROUTINE)              
         BASR  RE,RF                         SET DEFAULT VALUES                 
         SPACE 2                                                                
*        SAVE INITIALISED DATA IN TWA                                           
*                                                                               
SAVEDATA MVC   LREQNUM(18),REQNUM                                               
         MVC   LKRT1,KRT1                                                       
         MVC   LKRT2,KRT2                                                       
         MVC   LKUB1,KUB1                                                       
         MVC   LREQREC(L'REQREC),REQREC                                         
         TM    FIND,X'08'                                                       
         BZ    EXIT                                                             
         CLI   REQACTN,C'N'                                                     
         BNE   EXIT                                                             
         CLI   STATUS,2                                                         
         BE    EXIT                                                             
         MVC   DUB(2),LREQMAP+1              DISPLAY NEW CARD DEFAULTS          
         LH    R5,DUB                                                           
         AR    R5,R3                                                            
         FOUT  (R5),RAGY,78                                                     
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        ROUTINES TO FILL IN DEFAULT VALUES IN REQUEST RECORD                   
         SPACE 2                                                                
REQR01   MVI   RO7,C'T'      DEFAULT TO TEST RUN                                
         BR    RE                                                               
         SPACE 2                                                                
REQR04   MVI   RBPD,C'B'                                                        
         BR    RE                                                               
         SPACE 2                                                                
REQR07   MVI   RBPD,C'B'                                                        
*NOOP*   MVC   RSTRD,=C'900101'                                                 
*NOOP*   MVC   RENDD,=C'101231'                                                 
         MVI   RO6,C'S'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR12   MVC   RPRO,=C'ALL'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR27   MVC   RSORT(2),=C'NO'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR36   MVC   RSORT(2),=C'02'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR46   MVC   RCLI,=C'ALL'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR49   DS    0H                                                               
         MVC   RPAY(4),=C'1211'      MONTHS BACK,FORWARD                        
         MVI   RO1-1,C'K'            SET TO ALWAYS READ BUCKETS                 
         MVI   RO2,C'R'              SO P49 WILL KNOW IT WAS REQUESTED          
         MVI   RO5,C'S'              SET FOR SPECIAL TOTALS                     
         BR    RE                                                               
         SPACE 2                                                                
REQR55   MVC   RPRO,=C'ALL'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR66   MVC   RSORT(2),=C'20'                                                  
         MVC   KUB1+7(2),=C'BD'     RESET PUB KEY TO BBD+O                      
         BR    RE                                                               
         SPACE 2                                                                
REQR77   MVC   RSORT(2),=C'20'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR79   MVI   RO1,C'N'                                                         
         MVC   RSORT(2),=C'20'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR81   MVC   RSORT(2),=C'20'                                                  
         MVI   RBPD,C'S'                                                        
         BR    RE                                                               
         SPACE 2                                                                
REQR92   DS    0H                                                               
         MVI   RO2,C'T'          TEST RUN                                       
         BR    RE                                                               
         SPACE 2                                                                
REQR98   DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         MVC   REST,=C'ALL'                                                     
         MVI   RO2,C'N'          NO TOTALS                                      
         MVI   RO5,C'W'          WEB I/O'S                                      
         MVI   RO6,C'X'          NO EXPANDED ANALYSIS                           
         MVC   RSORT(2),=C'NO'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR108  MVC   RREG,=C'ALL'                                                     
         MVC   RDIS,=C'ALL'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR109  DS    0H                                                               
         MVC   RSORT(2),=C'20'      FOR USER SORT                               
         BR    RE                                                               
         SPACE 2                                                                
REQR121  MVC   RSORT(2),=C'NO'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR124  MVI   RO1,C'R'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR132  DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'ALL***'                                               
         MVI   RO5,C'Y'                                                         
         BR    RE                                                               
*                                                                               
REQR133  DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(3),=C'ALL'                                                  
         BR    RE                                                               
*                                                                               
REQR135  DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'ALL***'                                               
         MVI   RO1,C'I'                                                         
         BR    RE                                                               
*                                                                               
REQR136  DS    0H                                                               
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'ALL***'                                               
         MVI   RO1,C'I'                                                         
         MVI   RO5,C'Y'                                                         
         BR    RE                                                               
*                                                                               
REQR228  MVC   RCLI,=C'ALL'                                                     
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(2),=C'NO'                                                   
         BR    RE                                                               
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
COMSW    DS    CL1                                                              
*                                                                               
MYFLAG   DS    X                                                                
SBREPT   EQU   X'40'              SOON BILLING                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* SEE IF FULL STEREO                                                            
*                                                                               
CHKSTREO      NTR1                                                              
*                                                                               
* CHECK FOR STEREO NOW THAT THERE ARE MORE THAN ONE                             
*                                                                               
         L     RF,APARM                                                         
         L     RF,16(RF)           A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(X'80',0),F#UTLD                                   
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
*                                                                               
* SET CONDITION IF STEREO                                                       
*                                                                               
         TM    F@TSTAT6,TST6STRO+TST6STFU                                       
         XIT1                                                                   
         DROP  RF,R1                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* RETURNS DATA STEREO NEEDS TO PRE-VALIDATE REQUEST FIELDS                      
* - R4 POINTS TO REQUEST TABLE ENTRY / MULTNUM IS WORKAREA                      
*                                                                               
* - ROUTINE RETURNS HEX AS CHARACTER / X'01' AS C'0',C'1'                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
STEREOID NTR1                                                                   
         LA    R2,2                                                             
STD10    BAS   R5,SPLITBYT         SPLIT REQ TABLE BYTE                         
         LA    R8,2(R8)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,STD10                                                         
         LA    R2,2                                                             
STD20    LA    R4,1(R4)            SKIP REQ CARD POSITION BYTE IN TBL           
         CLI   0(R4),X'01'         OPTIONAL COMMENT FIELD                       
         BNE   STDX                NO                                           
         LA    R4,1(R4)            YES                                          
         BAS   R5,SPLITBYT                                                      
         LA    R8,2(R8)            BUMP OUTAREA                                 
         BCT   R2,STD20            AND SEE IF ANY MORE COMMENTS                 
STDX     B     EXIT                                                             
*                                                                               
* X'1C' GOES OUT AS C'1',C'C'                                                   
*                                                                               
SPLITBYT DS    0H                                                               
         MVC   MULTNUM,0(R4)       GET REQ TABLE BYTE                           
         ZIC   R1,MULTNUM                                                       
         SRA   R1,4                DEAL WITH FIRST HALF OF BYTE                 
         STC   R1,MULTNUM                                                       
         C     R1,=F'9'            IF OVER 9, SEND  C'A,B' ETC                  
         BNH   SPLT10                                                           
         LA    R1,1(R1)            IF > 9, MUST SEND C'A' ETC                   
         S     R1,=F'10'                                                        
         STC   R1,MULTNUM                                                       
         OI    MULTNUM,X'C0'                                                    
         B     *+8                                                              
SPLT10   OI    MULTNUM,X'F0'                                                    
         MVC   8(1,R8),MULTNUM     RETURN IT                                    
*                                                                               
         MVC   MULTNUM,0(R4)       DEAL WITH SECOND HALF OF BYTE                
         OI    MULTNUM,X'F0'                                                    
         CLI   MULTNUM,X'F9'       ..IF GREATER THAN 9, SEND C'A' ETC           
         BNH   SPLT20                                                           
         NI    MULTNUM,X'0F'       ..CLEAR FIRST HALF OF BYTRE                  
         ZIC   R1,MULTNUM                                                       
         LA    R1,1(R1)                                                         
         S     R1,=F'10'                                                        
         STC   R1,MULTNUM                                                       
         OI    MULTNUM,X'C0'                                                    
SPLT20   MVC   9(1,R8),MULTNUM     RETURN IT                                    
         BR    R5                                                               
*                                                                               
MULTNUM  DS    CL1                WORK BYTE FOR STEREOID                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
NUMINV   EQU   10                            REQUEST NUMBER INVALID             
ACTINV   EQU   12                            REQUEST ACTION INVALID             
OPTINV   EQU   02                            REQUEST OPTION INVALID             
NOTUPD   EQU   400     UPDATES NOT AVAILABLE IN THIS ONLINE APPLICATION         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TABLE OF ROUTINE ADDRESS FOR EACH REQ THAT REQUIRES DEFAULT VALUES            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REQROUTS DS    0CL5                                                             
         DC    AL1(01,0),AL3(REQR01)                                            
         DC    AL1(04,0),AL3(REQR04)                                            
         DC    AL1(06,0),AL3(REQR04)                                            
         DC    AL1(07,0),AL3(REQR07)                                            
         DC    AL1(12,0),AL3(REQR12)                                            
         DC    AL1(14,0),AL3(REQR12)        SAME AS 12                          
         DC    AL1(16,0),AL3(REQR12)        SAME AS 12                          
         DC    AL1(18,0),AL3(REQR12)        SAME AS 12                          
         DC    AL1(19,0),AL3(REQR12)        SAME AS 12                          
         DC    AL1(27,0),AL3(REQR27)                                            
         DC    AL1(28,0),AL3(REQR27)        SAME AS 27                          
         DC    AL1(36,0),AL3(REQR36)                                            
         DC    AL1(37,0),AL3(REQR36)        SAME AS 36                          
         DC    AL1(46,0),AL3(REQR46)                                            
         DC    AL1(49,0),AL3(REQR49)                                            
         DC    AL1(55,0),AL3(REQR55)                                            
         DC    AL1(66,0),AL3(REQR66)                                            
         DC    AL1(72,0),AL3(REQR27)         SAME AS 27                         
         DC    AL1(73,0),AL3(REQR27)         SAME AS 27                         
         DC    AL1(75,0),AL3(REQR27)         SAME AS 27                         
         DC    AL1(77,0),AL3(REQR77)                                            
         DC    AL1(79,0),AL3(REQR79)                                            
         DC    AL1(81,0),AL3(REQR81)                                            
         DC    AL1(91,0),AL3(REQR27)         SAME AS 27                         
         DC    AL1(92,0),AL3(REQR92)                                            
         DC    AL1(98,0),AL3(REQR98)                                            
         DC    AL1(108,0),AL3(REQR108)                                          
         DC    AL1(109,0),AL3(REQR109)                                          
         DC    AL1(110,0),AL3(REQR04)        B1 SAME AS 04                      
         DC    AL1(112,0),AL3(REQR27)        PM SAME AS 27                      
         DC    AL1(115,0),AL3(REQR04)        D1 SAME AS B1 - 04                 
         DC    AL1(116,0),AL3(REQR04)        E1 SAME AS B1 - 04                 
         DC    AL1(119,0),AL3(REQR04)        R1 SAME AS B1 - 04                 
         DC    AL1(120,0),AL3(REQR27)        RA SAME AS 27                      
         DC    AL1(121,0),AL3(REQR121)                                          
         DC    AL1(123,0),AL3(REQR04)        RD SAME AS B1 - 04                 
         DC    AL1(124,0),AL3(REQR124)                                          
         DC    AL1(129,0),AL3(REQR77)        LB SAME AS L1 SAME AS 77           
         DC    AL1(132,0),AL3(REQR132)       SN                                 
         DC    AL1(133,0),AL3(REQR133)       CI                                 
         DC    AL1(135,0),AL3(REQR135)       SE                                 
         DC    AL1(136,0),AL3(REQR136)       PH                                 
         DC    AL1(138,0),AL3(REQR133)       TD SAME AS CI                      
         DC    AL1(142,0),AL3(REQR135)       CH SAME AS SE                      
         DC    AL1(212,0),AL3(REQR12)        AC SAME AS 12                      
         DC    AL1(217,0),AL3(REQR12)        AR SAME AS 12                      
         DC    AL1(219,0),AL3(REQR12)        AU SAME AS 12                      
         DC    AL1(228,0),AL3(REQR228)                                          
REQROUTX DC    AL1(00,0)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* EACH ENTRY IN THIS TABLE DEFINES TEXT OF A PROTECTED DATA FIELD               
* ENTRY CAN ALSO DEFINE LENGTH OF AN ASSOCIATED UNPROTECTED INPUT FIELD         
* ENTRY FORMAT IS:                                                              
*        AL1   ENTRY NUMBER                                                     
*        AL1   PROTECTED FIELD LENGTH (=P)                                      
*        AL1   UNPROTECTED FIELD LENGTH                                         
*        CLP   PROTECTED FIELD DATA                                             
*                                                                               
* TWATBL COMPRISED OF ALL ENTRIES WITH CORRESPONDING INPUT FIELD                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TWATBL   DS    0C                                                               
*                                                                               
         DC    AL1(002,11,07),C'CLIENT CODE'                                    
         DC    AL1(004,13,03),C'DIVISION CODE'                                  
         DC    AL1(006,12,09),C'PRODUCT CODE'                                   
         DC    AL1(007,07,06),C'AD CODE'                                        
         DC    AL1(008,11,03),C'REGION CODE'                                    
         DC    AL1(010,13,03),C'DISTRICT CODE'                                  
         DC    AL1(012,12,10),C'ESTIMATE NUM'                                   
         DC    AL1(014,11,16),C'PUBLICATION'                                    
         DC    AL1(016,15,17),C'START,END DATES'                                
         DC    AL1(020,08,01),C'BILL/PAY'                                       
         DC    AL1(022,09,02),C'SORT MENU'                                      
         DC    AL1(026,13,01),C'PRIOR MONTHS?'                                  
         DC    AL1(028,06,04),C'REPORT'                                         
         DC    AL1(029,06,12),C'FILTER'                                         
         DC    AL1(030,09,01),C'FREQUENCY'                                      
         DC    AL1(038,16,09),C'MONTH OF SERVICE'                               
         DC    AL1(040,12,09),C'INVOICE DATE'                                   
         DC    AL1(042,11,09),C'PERIOD THRU'                                    
         DC    AL1(044,13,09),C'CURRENT MONTH'                                  
         DC    AL1(046,08,17),C'REP CODE'                                       
         DC    AL1(048,12,17),C'MANUAL GROSS'                                   
         DC    AL1(050,16,17),C'MANUAL CASH DISC'                               
         DC    AL1(052,14,06),C'COMMENT NUMBER'                                 
         DC    AL1(054,12,09),C'CONTROL DATE'                                   
         DC    AL1(056,12,01),C'PRINT OPTION'                                   
         DC    AL1(057,09,01),C'TEST RUN?'                                      
         DC    AL1(058,13,01),C'TYPE OF ITEMS'                                  
         DC    AL1(059,14,01),C'FAX CONTRACTS?'                                 
         DC    AL1(060,17,01),C'ONE PUB PER PAGE?'                              
         DC    AL1(059,14,01),C'LIST UNPOSTED?'                                 
         DC    AL1(061,17,01),C'LIST PREV POSTED?'                              
         DC    AL1(062,12,01),C'TYPE OF PUBS'                                   
         DC    AL1(064,12,01),C'TYPE OF LIST'                                   
         DC    AL1(068,15,01),C'OMIT AUTH DATA?'                                
         DC    AL1(070,16,09),C'CHG CONTROL DATE'                               
         DC    AL1(072,12,01),C'TYPE OF BUYS'                                   
         DC    AL1(074,18,01),C'SUMMARY MTH OPTION'                             
         DC    AL1(075,14,01),C'TOTALS OPTION '                                 
         DC    AL1(076,14,01),C'SUMMARY OPTION'                                 
         DC    AL1(078,15,04),C'CONTRACT NUMBER'                                
         DC    AL1(080,08,02),C'DUE DAYS'                                       
         DC    AL1(082,14,17),C'BILLING PERIOD'                                 
         DC    AL1(084,10,12),C'AS OF DATE'                                     
         DC    AL1(086,13,01),C'$ ON SCHEDULE'                                  
         DC    AL1(088,11,01),C'REPORT TYPE'                                    
         DC    AL1(090,11,01),C'DATE OPTION'                                    
         DC    AL1(092,09,03),C'LIST CODE'                                      
         DC    AL1(093,09,01),C'SORT CODE'                                      
         DC    AL1(094,12,01),C'GRID FORMAT?'                                   
         DC    AL1(095,13,01),C'CHANGE OPTION'                                  
         DC    AL1(096,11,01),C'AD # OPTION'                                    
         DC    AL1(097,12,03),C'OVERAGE PCT.'                                   
         DC    AL1(098,18,01),C'SHOW MKTS/VENDORS?'                             
         DC    AL1(099,13,01),C'DETAIL OPTION'                                  
         DC    AL1(100,12,01),C'SPACE OPTION'                                   
         DC    AL1(101,12,01),C'WEEKS OPTION'                                   
         DC    AL1(102,08,01),C'$ OPTION'                                       
         DC    AL1(103,11,16),C'PUB OR ASPO'                                    
         DC    AL1(104,11,01),C'DATA OPTION'                                    
         DC    AL1(105,11,01),C'ADFILE INFO'                                    
         DC    AL1(106,12,01),C'PAYING ADDR?'                                   
         DC    AL1(107,15,01),C'DOUBLE SPACING?'                                
         DC    AL1(108,13,01),C'SCHEDULE SORT'                                  
         DC    AL1(109,16,02),C'$ COLS. OVERRIDE'                               
         DC    AL1(110,12,09),C'CUT-OFF DATE'                                   
         DC    AL1(111,15,02),C'ESTIMATE OPTION'                                
         DC    AL1(112,14,02),C'PRDS TOGETHER?'                                 
         DC    AL1(113,13,02),C'PRODUCE FILE?'                                  
         DC    AL1(114,16,02),C'SUPPRESS LABELS?'                               
         DC    AL1(115,15,06),C'USER REPORT NUM'                                
         DC    AL1(116,15,02),C'SUPPRESS BOXES?'                                
         DC    AL1(117,18,01),C'BILLED ITEMS ONLY?'                             
         DC    AL1(118,12,01),C'BILLING TYPE'                                   
         DC    AL1(119,18,01),C'BLLABLE ITMS ONLY?'                             
         DC    AL1(120,14,01),C'PRINT OPTION 2'                                 
         DC    AL1(121,14,01),C'PRINT OPTION 3'                                 
         DC    AL1(122,14,01),C'PRINT OPTION 4'                                 
         DC    AL1(124,18,01),C'DATE TYPE OVERRIDE'                             
         DC    AL1(125,13,17),C'PAYABLE DATES'                                  
         DC    AL1(127,01,01),C' '                                              
         DC    AL1(128,13,01),C'LETTER OPTION'                                  
         DC    AL1(129,18,01),C'REBTABLE ITM ONLY?'                             
         DC    AL1(130,18,01),C'REBATED ITMS ONLY?'                             
         DC    AL1(131,18,01),C'PAYABLE ITMS ONLY?'                             
*                                                                               
         DC    AL1(132,16,02),C'52/EC REQUESTED?'                               
         DC    AL1(133,18,01),C'SUPPRESS INACTIVE?'                             
         DC    AL1(134,16,01),C'TRAFFIC ADDRESS?'                               
         DC    AL1(135,18,01),C'OTHER LEVEL OPTION'                             
         DC    AL1(136,11,01),C'TEST OPTION'                                    
         DC    AL1(137,18,01),C'SUPPRESS NET COL.?'                             
         DC    AL1(138,10,01),C'AOR OPTION'                                     
         DC    AL1(139,11,01),C'BILL OPTION'                                    
         DC    AL1(140,07,60),C'OPTIONS'                                        
         DC    AL1(141,09,01),C'CD OPTION'                                      
         DC    AL1(142,14,01),C'SORT BY MARKET'                                 
         DC    AL1(143,17,01),C'CLT ACTVTY FILTER'                              
         DC    AL1(144,12,03),C'BILLING TYPE'                                   
         DC    AL1(145,04,09),C'DATE'                                           
         DC    AL1(146,09,01),C'DATE TYPE'                                      
         DC    AL1(147,14,09),C'INTERFACE DATE'                                 
         DC    AL1(148,14,13),C'BILL NUMBER(S)'                                 
         DC    AL1(149,04,01),C'NET?'                                           
         DC    AL1(150,16,09),C'KILL DATE FILTER'                               
         DC    AL1(151,17,01),C'LEVEL/TEST OPTION'                              
         DC    AL1(152,13,06),C'AGENCY FILTER'                                  
         DC    AL1(153,13,01),C'STATUS FILTER'                                  
         DC    AL1(154,13,01),C'ADCODE RECAP?'                                  
         DC    AL1(155,15,01),C'LANGUAGE FILTER'                                
         DC    AL1(198,14,01),C'LIST PREV CONV'                                 
         DC    AL1(199,16,01),C'REPLACE INVOICES'                               
         DC    AL1(200,11,01),C'AUTO MATCH?'                                    
         DC    AL1(201,06,01),C'      '          HIDDEN FIELD                   
         DC    AL1(202,12,01),C'POST INVOICE'                                   
         DC    AL1(203,09,04),C'PUBLISHER'                                      
         DC    AL1(204,11,01),C'SCHEME (ID)'                                    
         DC    AL1(205,15,04),C'START INVOICE #'                                
         DC    AL1(206,13,04),C'END INVOICE #'                                  
         DC    AL1(207,05,01),C'RERUN'                                          
         DC    AL1(208,11,01),C'RECORD TYPE'                                    
         DC    AL1(209,16,01),C'DOWNLOAD FORMAT?'                               
         DC    AL1(210,12,01),C'PURGE OPTION'                                   
         DC    AL1(212,17,01),C'CLT/PUB PURGE OPT'                              
         DC    AL1(214,17,01),C'COMMENT PURGE OPT'                              
         DC    AL1(216,17,02),C'FOREIGN BANK CODE'                              
         DC    AL1(218,16,01),C'PRD SUMMARY FMT?'                               
         DC    AL1(220,13,01),C'COPY TO MEDIA'                                  
         DC    AL1(221,14,01),C'COPY USER DEF?'                                 
         DC    AL1(222,15,01),C'COPY BILL FORM?'                                
         DC    AL1(223,15,01),C'COPY DIVISIONS?'                                
         DC    AL1(224,14,01),C'ID/SCHEME CODE'                                 
         DC    AL1(225,15,13),C'REVERSED NUMBER'                                
         DC    AL1(226,06,17),C'AMOUNT'                                         
         DC    AL1(227,14,17),C'DETAILS AMOUNT'                                 
         DC    AL1(228,14,01),C'PLANNED COSTS?'                                 
         DC    AL1(229,09,01),C'SKIPONLY?'                                      
         DC    AL1(230,12,09),C'PO# END DATE'                                   
*                                                                               
TWATBLX  DC    X'0000'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVIDTYPE,0          TYPE OF USER ID                              
*                                                                               
         LA    R4,PRTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,TWAUSRID                                                 
         GOTO1 DATAMGR,DMCB,(0,TRDMREAD),TRCTFILE,(R4),(R4)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
*                                                                               
         MVC   SVIDTYPE,CTAGYIDT                                                
*                                                                               
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRAFFX                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRA30  CLC   RNUM(2),=C'72'                                                   
         BE    CKTRAFFX                                                         
         CLC   RNUM(2),=C'75'                                                   
         BE    CKTRAFFX                                                         
         CLC   RNUM(2),=C'77'                                                   
         BE    CKTRAFFX                                                         
         CLC   RNUM(2),=C'79'                                                   
         BE    CKTRAFFX                                                         
*                                                                               
         B     CKTRAERR                                                         
*                                                                               
CKTRAFFX CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRAERR LTR   RB,RB               NOT EQUAL (ERROR)                            
         XIT1                                                                   
*                                                                               
TRDMREAD DC    CL8'DMREAD'                                                      
TRCTFILE DC    CL8'CTFILE'                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TABLE FOR ALL ENTRIES THAT ARE COMMENTS ONLY                                  
* ALL ODD NUMBERS ARE SAME LINE COMMENTS                                        
* ALL EVEN NUMBERS ARE NEXT LINE COMMENTS                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COMTBL   CSECT                                                                  
         DC    AL1(003,19,00),C'Y=SUPPRESS PROFILES'                            
         DC    AL1(005,27,00),C'Y=SUPPRESS BILLING FORMULAS'                    
         DC    AL1(007,29,00),C'Y=SUPPRESS INACTIVE CLTS/PRDS'                  
         DC    AL1(008,20,00),C'I=INACTIVE CLTS/PRDS'                           
         DC    AL1(009,20,00),C'Y=SUPPRESS ADDRESSES'                           
         DC    AL1(011,07,00),C'Y,T,$,B'                                        
         DC    AL1(013,29,00),C'CAN NOT USE WITH SPECIFIC PUB'                  
         DC    AL1(015,38,00),C'BLANK=INSRTN,B=BILLING,P=PAYING,C=CLOS'         
         DC    AL1(017,30,00),C'B=BILLING,P=PAYING,D=INSERTION'                 
         DC    AL1(019,23,00),C'H=HIGHER,L=LOWER,B=BOTH'                        
         DC    AL1(021,13,00),C'N=LIVE,Y=TEST'                                  
         DC    AL1(022,38,00),C'H=TEST USING HIGHER,L=TEST USING LOWER'         
         DC    AL1(023,32,00),C'ONLY FOR USE WITH SORTS 08 OR 09'               
         DC    AL1(025,10,00),C'Y=SUPPRESS'                                     
         DC    AL1(027,34,00),C'A=AOR ONLY,X=NO AOR,C=COMMISS ONLY'             
         DC    AL1(028,17,00),C'N=NO COMMISS ONLY'                              
         DC    AL1(029,07,00),C'R=RERUN'                                        
         DC    AL1(031,19,00),C'T=INCLUDE TEST BUYS'                            
         DC    AL1(033,31,00),C'INCLUDE P=PRIOR,S=SUBSEQ,B=BOTH'                
         DC    AL1(035,21,00),C'C=INCLUDE IN BILL,PAY'                          
         DC    AL1(037,12,00),C'N,Y, F=FIRST'                                   
         DC    AL1(039,27,00),C'B=BUYS,C=CONTRACTS,Y=EITHER'                    
         DC    AL1(041,19,00),C'ALL,REG,AOR,RET,FIN'                            
         DC    AL1(043,18,00),C'R=RUN ON,I=INVOICE'                             
         DC    AL1(045,13,00),C'MMM/YY-MMM/YY'                                  
         DC    AL1(047,10,00),C'S=SHIPPING'                                     
         DC    AL1(049,37,00),C'Y=SPACE REC OVERAGE LOOKUP OR 10 PCT.'          
         DC    AL1(051,20,00),C'SKIP IF ON OR BEFORE'                           
         DC    AL1(053,34,00),C'L=LISTING,A=ANALYSIS,R=RATE CHANGE'             
         DC    AL1(054,36,00),C'FOR R PUB,CON,TEST OPTN ARE REQUIRED'           
         DC    AL1(055,30,00),C'FOR RPT TYPES A+L: LEVEL=H,L,B'                 
         DC    AL1(056,28,00),C'FOR RPT TYPE R: TEST=N,Y,H,L'                   
         DC    AL1(057,06,00),C'C=COST'                                         
         DC    AL1(058,38,00),C'* OPTIONS BELOW FOR RPT TYPES A OR L *'         
         DC    AL1(059,33,00),C'1=COPY,2=+CAPTION,A=AD,B=+CAPTION'              
WESTC1   DC    AL1(061,38,00),C'A=APPRVD,N=NOT,BLANK=NOT ENTERED,X=ALL'         
         DC    AL1(063,29,00),C'B=BILLED,U=UNBILLED,BLANK=ALL'                  
         DC    AL1(065,35,00),C'Y=FLAG BILLED/PAID/TRAFFICKED ITEMS'            
         DC    AL1(067,33,00),C'P=PUBLISHERS ONLY,X=NO PUBLISHERS'              
         DC    AL1(068,20,00),C'L=PUBLISHER PUB LIST'                           
         DC    AL1(069,18,00),C'E=ENGLISH,F=FRENCH'                             
         DC    AL1(071,26,00),C'FOR LIST TYPES G OR U ONLY'                     
         DC    AL1(073,36,00),C'Y=SHOW ONLY IF END MTH = REQ END MTH'           
         DC    AL1(075,36,00),C'H=HIGHER,L=LOWER,B=BOTH,O=OPEN,A=ALL'           
*                                                                               
         DC    AL1(077,36,00),C'H=CLT,R=REP,J=JOB,C=COM,P=PUB,L=PUBL'           
         DC    AL1(078,35,00),C'D=CLT GRP,E=PRD GRP,F=PUB GRP,#=PO#'            
         DC    AL1(079,38,00),C'P=MARKED *PURGE ONLY,A=ALL UNUSED RECS'         
         DC    AL1(080,32,00),C'G=ALL RECORDS THAT CAN BE PURGED'               
         DC    AL1(081,19,00),C'Y=PRINT RECORD KEYS'                            
         DC    AL1(083,27,00),C'Y=PURGE CLT REC OR PUB LIST'                    
         DC    AL1(085,35,00),C'DEL COM ELEMS FOR C=CON,J=JOB,P=PRD'            
         DC    AL1(086,37,00),C'E=EST,B=BUY,R=AOR,U=USERP,L=PUB,A=ALL'          
         DC    AL1(087,10,00),C'Y=YES,N=NO'                                     
         DC    AL1(089,17,00),C'T=TEST WITH TRACE'                              
         DC    AL1(091,28,00),C'B=NORMAL,M=MANUAL,R=REVERSAL'                   
         DC    AL1(093,05,00),C'GROSS'                                          
         DC    AL1(095,22,00),C'REQUIRED FOR REVERSALS'                         
         DC    AL1(097,22,00),C'I=INVOICES,E=ESTIMATES'                         
         DC    AL1(099,16,00),C'BILLING RUN DATE'                               
*                                                                               
         DC    AL1(101,22,00),C'P=REPORT PLANNED COSTS'                         
         DC    AL1(103,21,00),C'DATES INSERTION ADDED'                          
         DC    AL1(105,17,00),C'S=SOON BILLS ONLY'                              
         DC    AL1(129,24,00),C'                        '                       
         DC    AL1(130,01,00),C' '        FOR SPACING                           
         DC    AL1(131,26,00),C'02=NUMERIC,03=ALPHA,07=MKT'                     
         DC    AL1(133,27,00),C'05=MAGAZINES IN ALPHA ORDER'                    
         DC    AL1(134,30,00),C'07=NEWS + OUTDOOR IN MKT ORDER'                 
         DC    AL1(135,36,00),C'N=NAME,Z=+ZONE,A=+ADDR,P=PROD,C=CIRC'           
         DC    AL1(136,38,00),C'L=CLES,S=SAU RATES,D=REGS+DSTS,$=RATES'         
         DC    AL1(137,29,00),C'C=CASH DISC.,N=NON-CASH DISC.'                  
         DC    AL1(138,38,00),C'R=REPS,1=PAY,2=TRA,3=CON,4=SHIP,X=PREM'         
         DC    AL1(139,34,00),C'02=NUMERIC,03=ALPHA,04=DIST,07=MKT'             
         DC    AL1(140,36,00),C'B=BUY WORKSHEET,W=WORKSHEET,K=RCODES'           
         DC    AL1(141,25,00),C'P=PAID,U=UNPAID,BLANK=ALL'                      
         DC    AL1(142,37,00),C'M=STND COMMS,V=AOR-ADV,Y=PAY CONTROLS'          
         DC    AL1(143,34,00),C'B=BILLING,P=PAYING,BLANK=INSERTION'             
         DC    AL1(144,38,00),C'C=CLOSING,I=INSRTN ORD,M=MAT.CLOS,S=OS'         
         DC    AL1(145,36,00),C'C=CONTRACT ONLY,S=SUPPRESS COMMMENTS'           
         DC    AL1(146,36,00),C'S=ON SALE,M=MAT.CLOS,I=BILLED,A=PAID'           
         DC    AL1(147,20,00),C'P=INCLUDE PRIOR MTHS'                           
         DC    AL1(148,32,00),C'B=CONTRACT,SCHEDULE,AND COMMENTS'               
         DC    AL1(149,38,00),C'O=INSRTN ORDER,N=NO INSRTN ORDER,A=ALL'         
         DC    AL1(150,37,00),C'08=DIV/REG/DST - USE DIV/REG/DST FLDS'          
         DC    AL1(151,24,00),CL24'MUST BE ALL'                                 
         DC    AL1(152,31,00),C'09=ALPHA/MKT WITHIN DIV/REG/DST'                
         DC    AL1(153,24,00),C'N=NET,BLANK=GROSS,X=OMIT'                       
         DC    AL1(154,23,00),C'G=GRP ASSGNS,I=WEB SITE'                        
         DC    AL1(155,34,00),C'R=SPACE RESERVATION,BLANK=CONTRACT'             
         DC    AL1(157,28,00),C'D=BILL MONTH+DAY,BLANK=MONTH'                   
         DC    AL1(159,27,00),C'06=MAGAZINES IN ALPHA ORDER'                    
         DC    AL1(161,27,00),C'$=SHOW GROSS $,S=SPACE DESC'                    
         DC    AL1(163,24,00),C'1=COPY # ONLY,2=+CAPTION'                       
         DC    AL1(164,22,00),C'A=AD # ONLY,B=+CAPTION'                         
         DC    AL1(165,21,00),C'A=SORT ON VENDOR NAME'                          
         DC    AL1(167,31,00),C'F=FLAG SCH REVS,C=SCH REVS ONLY'                
         DC    AL1(169,32,00),C'A=SHIPPING ADDR,BLANK=VENDOR NO.'               
         DC    AL1(171,36,00),C'P=PUB,M=MKT,D=DST,R=REG,E=EST,B=PROD'           
         DC    AL1(173,31,00),C'S=SHOW SPACE DESC.,C=+INS. COST'                
         DC    AL1(175,35,00),C'SHOW W=WEEKS,D=DAYS INSTEAD OF MTHS'            
         DC    AL1(177,20,00),C'S=SUPPRESS $ COLUMNS'                           
         DC    AL1(179,38,00),C'05=MAGS IN ALPHA,07=NEWS + OUTD IN MKT'         
         DC    AL1(181,38,00),C'C=SHOW,T=TOTAL ONLY CHGS,NEW BUYS,DELS'         
         DC    AL1(183,32,00),C'S=SUM.ONLY,P=SEP.PAGING,R=NO R/D'               
         DC    AL1(185,27,00),C'Y=SHOW ONLY CONTRACTS WHOSE'                    
         DC    AL1(186,33,00),C'END MTH MATCHES REQUESTED END MTH'              
         DC    AL1(187,18,00),C'A=AUDIT,F=FORECAST'                             
         DC    AL1(189,32,00),C'P=PRD,J=JOB,X=X,BLANK=DAY OF MTH'               
         DC    AL1(190,33,00),C'B=PRD+DAY OF MTH,A=JOB+DAY OF MTH'              
         DC    AL1(191,29,00),C'D=DATE,S=SPACE,1=PRD,A=+SPACE'                  
         DC    AL1(192,35,00),C'2=CUST,B=+SPACE,3=PRD+CUST,C=+SPACE'            
         DC    AL1(193,37,00),C'Y=FLAG BILLED AND/OR TRAFFICKED ITEMS'          
         DC    AL1(195,38,00),C'B=BILL,P=PAY,1=CLR,C=CLO,S=OS,BLNK=INS'         
         DC    AL1(197,19,00),C'B=BILLING,P=PAYABLE'                            
         DC    AL1(199,26,00),C'Y=PRINT FREE FORM COMMENTS'                     
         DC    AL1(201,24,00),C'Y=CHANGED CONTRACTS ONLY'                       
         DC    AL1(203,22,00),C'Y=''NEEDED'' ORDERS ONLY'                       
         DC    AL1(205,33,00),C'B=BILL,P=PAY,C=CLO,S=OS,BLANK=INS'              
         DC    AL1(207,26,00),C'P=PRODUCT,D=INSERTION DATE'                     
         DC    AL1(209,38,00),C'06=MAGS IN ALPHA,07=NEWS + OUTD IN MKT'         
         DC    AL1(210,31,00),C'08=REP CODE,09=PAYING ADDR NAME'                
         DC    AL1(211,16,00),C'DETAIL - SUMMARY'                               
         DC    AL1(213,25,00),C'S=SUMMARIES + RECAPS ONLY'                      
         DC    AL1(215,32,00),C'SUPPRESS C=CLIENT,P=PUB BREAKOUT'               
         DC    AL1(217,24,00),C'2=SHOW ESTIMATES,N=DON''T'                      
         DC    AL1(219,21,00),C'M=OMIT MONTHS,N=DON''T'                         
         DC    AL1(221,31,00),C'Y=LIST CURRENT INVOICES,N=DON''T'               
         DC    AL1(223,36,00),C'5=MAGS IN ALPHA,7=NEWS + OUTD IN MKT'           
         DC    AL1(225,09,00),C'BLANK=YES'                                      
         DC    AL1(227,25,00),C'M=SUPPRESS MONTH BREAKOUT'                      
         DC    AL1(229,23,00),C'B=FILTER ON DATE BILLED'                        
         DC    AL1(231,14,00),C'BILL RUN DATES'                                 
         DC    AL1(233,17,00),C'D=DUPLICATES ONLY'                              
         DC    AL1(235,22,00),C'N=NET,G=GROSS(DEFAULT)'                         
         DC    AL1(237,16,00),C'I=INCHES,L=LINES'                               
         DC    AL1(239,17,00),C'MMMDD/YY-MMMDD/YY'                              
         DC    AL1(241,20,00),C'BILLING TYPE=4,5,6,7'                           
         DC    AL1(242,26,00),C'1=GROSS-CD,2=NET-CD,T=COST'                     
         DC    AL1(243,25,00),C'G=GROSS,N=NET,C=CASH DISC'                      
         DC    AL1(245,33,00),C'FILTER ON EFFECTIVE DATE - MMM/YY'              
         DC    AL1(247,21,00),C'SHOW ZERO DIFFERENCES'                          
         DC    AL1(249,20,00),C'SUPRESS PROFILES Y/N'                           
         DC    AL1(251,28,00),C'R=REPORT ONLY,L=LETTERS ONLY'                   
         DC    AL1(252,34,00),C'F=FAX LETTERS,S=NO RPT+FAX LETTERS'             
         DC    AL1(253,30,00),C'I=INSERTION,B=BILLING,P=PAYING'                 
         DC    AL1(255,38,00),C'C=CON ONLY,S=NO CMNTS,B=CON+SCHD+CMNTS'         
COMTBLX  DC    X'0000'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
       ++INCLUDE PRREQFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
*        INCLUDE CTGENFILE                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
* XTRAINFD DSECT                                                                
       ++INCLUDE FAXTRAINF                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094PRREQ01   07/17/18'                                      
         END                                                                    
