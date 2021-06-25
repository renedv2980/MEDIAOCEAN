*          DATA SET PRREQ03S   AT LEVEL 050 AS OF 05/01/02                      
*          DATA SET PRREQ03    AT LEVEL 116 AS OF 01/29/93                      
*PHASE T41203C,+0                  NOTE "C" APPENDED TO PHASE NAME              
*INCLUDE PUBVAL                                                                 
*INCLUDE SRCHCALL                                                               
         TITLE 'PRREQ03  NEW REQUEST  VALIDATE FIELDS PART -1'                  
*        CHANGE LOG                                                             
*                                                                               
* SMYE 11/99    IN CLIVAL ALLOW ONLY ONE SPECIFIC CLIENT FOR B1, D1,            
*               R1, & RD REQUESTS IF MEDIA * (ALL MEDIA)                        
*                                                                               
* SMYE 5/98     IN CLIVAL ACCEPT *N (OFFICE) FOR 48 FOR LIST-TYPE 1-4           
*                                                                               
* SMYE 9/97     IN PROVAL DO NOT READ FOR PRODUCT IF MASTER CLIENT AND          
*               REPORT REQUEST FOR 12, 14, 16 OR 18                             
*                                                                               
* SMYE 6/97     IN ESTVAL ACCEPT "L" FOR LAST AND "H" FOR HIGH AND              
*               DISPLAY ESTIMATE NUMBER SELECTED (REPLACE LAST & HIGH)          
*                                                                               
* SMYE 1/97     IN CLIVAL INCORPORATE CLIENT GROUP SECURITY                     
*                                                                               
* SMYE 1/97     IN ESTVAL ACCEPT "LAST" TO LOOK FOR "LATEST" START DATE         
*               AND "HIGH" TO LOOK FOR THE HIGHEST ESTIMATE NUMBER              
*                                                                               
* BPLA 8/26/94  IN CLIVAL SAVE DRD OVERRIDE CLT (FROM PCLTDRO X'30'             
*               ELEMENT) IN CLIPROF+20(3). IT WILL BE CHECKED IN                
*               PRREQ02 (BILLING POST VALIDATION)                               
*                                                                               
* BPLA 1/18/94  IN CLIVAL IF P48 AND OPTION 1 = V                               
*               THEY SHOULD ENTER THE ADV SO DON'T TRY TO READ AS A             
*               CLIENT.                                                         
*                                                                               
* BPLA 3/19/93  CHANGES FOR RFP                                                 
*                                                                               
* BPLA 1/29/93  CHANGES IN CLIVAL FOR AR                                        
*                                                                               
* BPLA 1/25/93  CODE TO CHECK LIMIT ACCESS FOR CLIENT GROUP REQS                
*                                                                               
* BPLA 3/5/92   CHANGES IN CLIVAL FOR AC AND AU                                 
*                                                                               
* BPLA 10/22/91 ADD 60,77,L1,LB,S2 TO LIST OF REPORTS FOR WHICH                 
*               START AND END ARE NOT REQUIRED TO BE IN ESTIMATE                
*               PERIOD (MAY BE USING BILLABLE,PAYABLE ETC.)                     
         PRINT NOGEN                                                            
T41203   CSECT                                                                  
         NMOD1 000,T41203,R2,RR=R9          R2 NEW 2ND BASE REG 10/6/87         
*                                                                               
**********************************************************************          
*  ADDING ONE TO FIELD VALIDATION CODES CHANGES THEM FROM REQUIRED   *          
*  FIELDS TO OPTIONAL FIELDS                                         *          
**********************************************************************          
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                       R3=A(TWA)                          
         EJECT                                                                  
         L     R1,FLDHADR                    R1=A(FLD HDR TO BE VALED)          
         SR    RF,RF                                                            
         IC    RF,ROUTNUM                    RF=ROUTINE NUM REQD                
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO                       RF=A(ROUTINE REQ)                  
         BASR  RE,RF                         PASS CONTROL TO ROUTINE            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        VALIDATE CLIENT AND SET FIND FORMAT BITS                               
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*        X'08' =*XX          OFFICE CODE                                        
*        X'10' =XXX-XXX           SECOND XXX IS REG/DST OVERRIDE CLT            
*        X'20' =XXX-XXX           SECOND XXX IS SLAVE FOR FIRST                 
*                                  FOR CONTRACTS                                
*        X'40' =$N                 OFFICE LISTS OR ALL*=ALL OFFICES             
*        X'80' =&N                 CLIENT GROUP                                 
*                                                                               
CLIVAL   NTR1                                                                   
         MVI   ROUTSUB,0                                                        
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         SPACE                                                                  
         CLC   RNUM,=C'L1'                                                      
         BE    CLV0                                                             
         CLC   RNUM,=C'LB'                                                      
         BNE   CLV                                                              
CLV0     CLC   RAGY,=C'DP'   ALLOW CLIENT=ALL FOR DUPONT IN L1 OR LB            
         BNE   CLV                                                              
*                                                                               
         CLC   =C'ALL ',IFLD                                                    
         BNE   CLV                                                              
         OI    FIND,X'04'                                                       
         B     CLIVO                                                            
*                                                                               
CLV      CLC   =C'ALL*',IFLD                                                    
         BNE   CLVB                                                             
         OC    6(2,R3),6(R3)     CHECK FOR LIMIT ACCESS                         
         BNZ   CACCERR                                                          
         OI    FIND,X'40'                                                       
         MVC   RCLI(2),=C'$*'                                                   
         MVC   NAME(15),=C'* ALL OFFICES *'                                     
         B     CLIVOX                                                           
*                                                                               
CLVB     CLC   RNUM(2),=C'81'                                                   
         BNE   CLVC                                                             
         CLC   8(2,R4),=C'RI'      ONLY CLT RI FOR REQ 81                       
         BNE   CLIVE                                                            
*                                                                               
*                   MUST BE ONE ONLY CLIENT FOR MEDIA * BILLING REQ'S           
CLVC     DS    0H                                                               
         CLI   RMED,C'*'           "ALL" MEDIA ?                                
         BNE   CLVD                NO                                           
         CLC   =C'B1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'D1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'R1',RNUM                                                      
         BE    CLVCC               GO TEST CLIENT                               
         CLC   =C'RD',RNUM                                                      
         BNE   CLVD                NOT "BILLING" REQUEST                        
CLVCC    DS    0H                  TEST FOR "SPECIFIC" CLIENT                   
         CLI   IFLDH+5,3           MORE THAN 3 CHARACTERS ?                     
         BH    CLIVE               YES - ERROR                                  
         CLC   =C'ALL',IFLD        ALL CLIENTS ?                                
         BE    CLIVE               YES - ERROR                                  
         CLI   IFLD,C'*'           OFFICE ?                                     
         BE    CLIVE               YES - ERROR                                  
         CLI   IFLD,C'$'           OFFICE LIST ?                                
         BE    CLIVE               YES - ERROR                                  
         CLI   IFLD,C'&&'          GROUP ?                                      
         BE    CLIVE               YES - ERROR                                  
*                                                                               
CLVD     XC    CLIPROF,CLIPROF                                                  
         XC    CLISAVE(6),CLISAVE  CLEARS CLT,PRD,EST,PUB,S-E                   
*                                                                               
         CLC   RNUM,=C'48'         SEE IF PUB LISTING - AOR ADV                 
         BNE   CLVD5                                                            
         CLI   RO1,C'V'       SEE IF AOR ADV LISTING                            
         BNE   CLVD5                                                            
         OI    FIND,X'04'                                                       
         B     CLIVO     DON'T VALIDATE AS A CLIENT SINCE IT IS                 
*                        THE AOR ADV                                            
CLVD5    DS    0H                                                               
         TM    FIND,2              SEE IF ALL                                   
         BZ    CLIV                                                             
         CLC   RNUM,=C'47'         BYPASS CLT SECURITY FOR 47                   
         BE    CLIVO                                                            
*                                                                               
         MVI   CLIPROF+9,C'0'    FOR BILLING - TO REQUIRE EST=ALL,NNN           
         OC    6(2,R3),6(R3)       CHK LIMIT ACCESS                             
         BZ    CLIVO                                                            
         B     CACCERR                                                          
*                                                                               
CLIV     CLI   FIND,1                                                           
         BNE   CLIVO             CLIENT MISSING                                 
         CLI   IFLD,C'$'                                                        
         BNE   CLIV0                                                            
         CLI   IFLDH+5,2                                                        
         BH    CLIVE                                                            
         MVC   NAME(13),=C'OFFICE LIST X'                                       
         MVC   NAME+12(1),IFLD+1                                                
         OI    FIND,X'40'           OFFICE LIST                                 
         OC    6(2,R3),6(R3)        CHK FOR LIMIT ACCESS                        
         BZ    CLIVO                                                            
         CLI   6(R3),C'$'           OFFICE LIST LOCKOUT                         
         BNE   CACCERR              NO                                          
         CLC   7(1,R3),IFLD+1       OFFICE LIST MUST MATCH                      
         BNE   CACCERR                                                          
         B     CLIVO                                                            
*                                                                               
CLIV0    CLI   IFLD,C'&&'           CLIENT GROUP                                
         BNE   CLIV00                                                           
         CLI   IFLDH+5,2                                                        
         BH    CLIVE                                                            
         CLI   RMED,C'*'            DISALLOW FOR MEDIA *                        
         BE    CLIVE                                                            
**                                                                              
CLIV000  MVC   NAME(7),=C'GROUP  '                                              
         MVC   NAME+6(1),IFLD+1                                                 
         OI    FIND,X'80'           CLIENT GROUP                                
         OC    6(2,R3),6(R3)         CHK FOR LIMIT ACCESS TERMINAL              
         BZ    CLIVO                 NO                                         
         CLI   6(R3),C'*'            OFFICE LOCKOUT                             
         BE    CLIV005                                                          
         CLI   6(R3),C'$'            OR OFFICE LIST LOCKOUT                     
         BE    CLIV005                                                          
         B     CACCERR       MUST BE SINGLE CLIENT - NO GROUPS                  
*                                                                               
CLIV005  BAS   RE,CHKGRP                                                        
         B     CLIVO                                                            
*                                                                               
CLIV00   CLC   IFLD(2),=C'*-'        BYPASS DELIMITER                           
         BE    CLIV1                                                            
         CLI   IFLDH+5,3                                                        
         BNH   CLIV1               TO ALLOW X-X                                 
         MVI   ROUTSUB,1                                                        
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
CLIV1    BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCLI(0),IFLD                  CLIENT TO REQ REC                  
         CLI   IFLDH+5,2                                                        
         BL    CLIVE                                                            
         CLI   IFLDH+5,3                                                        
         BH    CLIVE                                                            
         CLI   IFLD,C'*'                                                        
         BNE   CLIV1X                                                           
         CLC   RNUM,=C'48'         PUB LISTING ?                                
         BNE   CLIV1A              NO                                           
         CLI   RO1,C'1'            LIST TYPE 1 - 4 ?                            
         BL    CLIVE               NO - ERROR                                   
         CLI   RO1,C'4'                                                         
         BH    CLIVE                                                            
CLIV1A   OC    6(2,R3),6(R3)       CHK LIMIT ACCESS                             
         BZ    CLIV1C              NO LIMIT                                     
         CLI   IFLDH+5,3           NO *NN OR *-N FOR LMT ACC TERMS              
         BE    CACCERR                                                          
**NEW 1/19/90                                                                   
         CLI   6(R3),C'$'          OFFICE LIST LIMIT ACCESS                     
         BNE   CLIV1B                                                           
         BAS   RE,CLIV100                                                       
         B     CLIV1C                                                           
*                                                                               
**NEW 1/19/90                                                                   
CLIV1B   CLI   6(R3),C'*'          CHK LMT BY OFFICE                            
         BNE   CACCERR                                                          
         CLC   IFLD+1(1),7(R3)     MATCH OFFICE CODES                           
         BNE   CACCERR                                                          
*                                                                               
CLIV1C   OI    FIND,X'08'         OFFICE INPUT                                  
         MVI   CLIPROF+9,C'0'    FOR BILLING - TO REQUIRE EST=ALL,NNN           
         CLC   IFLD(2),=C'*-'                                                   
         BE    CLIVO                                                            
         B     CLIV9                                                            
*                                                                               
CLIV1X   MVC   KRT1+4(3),RCLI                CLIENT TO KEY1                     
         MVC   KRT2+4(3),RCLI                CLIENT TO KEY2                     
CLIV2    MVI   KRT1+3,X'02'                  READ CLIENT                        
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         LA    R5,PRTREC                                                        
         USING PCLTRECD,R5                                                      
         CLC   FERN,=AL2(FE)                                                    
         BL    CLIVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(CLINOF)                                                
         B     CLIVO                                                            
CLIV2A   OC    6(2,R3),6(R3)       CHK LIMIT ACCESS                             
         BZ    CLIV2G              NO RESTRICTIONS                              
         CLI   6(R3),C'*'          LIMIT FOR OFFICE ?                           
         BNE   CLIV2A2             NO                                           
         CLI   7(R3),C'A'          SEE IF ALPHA                                 
         BL    CLIV2B              NO - MUST BE OFFICE                          
         CLI   7(R3),C'Z'                                                       
         BH    CLIV2B                                                           
         CLI   8(R3),C'0'          SEE IF 3RD BYTE IS NUMERIC                   
         BL    CLIV2B                                                           
         CLI   8(R3),C'9'                                                       
         BH    CLIV2B                                                           
         BAS   RE,VALGROUP         CLIENT GROUP SECURITY                        
         BNE   CACCERR             ACCESS DENIED                                
         B     CLIV2G              ACCESS OK                                    
*                                                                               
**NEW 1/19/90                                                                   
CLIV2A2  CLI   6(R3),C'$'          LMT FOR OFFICE LIST                          
         BNE   CLIV2A5             YES                                          
         BAS   RE,CLIV100                                                       
         B     CLIV2G                                                           
*                                                                               
**NEW 1/19/90                                                                   
CLIV2A5  CLC   6(3,R3),PCLTKCLT    CLTS MUST MATCH  4/19/90                     
         BE    CLIV2G                                                           
         B     CACCERR                                                          
*                                                                               
CLIV2B   CLI   PCLTOFF+2,C'*'      IGNORE LIMIT                                 
         BE    CLIV2G                                                           
         CLC   7(1,R3),PCLTOFF     MATCH OFFICE CODES                           
         BNE   CACCERR                                                          
*                                                                               
CLIV2G   DS    0H                                                               
*                                                                               
         CLC   RNUM(2),=C'AC'      ADV CONTRACT                                 
         BE    CLIV2I                                                           
         CLC   RNUM(2),=C'AR'      AOR CON RPT                                  
         BE    CLIV2I                                                           
         CLC   RNUM(2),=C'AU'      ADV UTILIZATION                              
         BNE   CLIV2X                                                           
*                                                                               
CLIV2I   LA    RE,PCLTREC+33                                                    
         USING PCLTADVE,RE                                                      
CLIV2K   CLI   0(RE),X'15'                                                      
         BE    CLCV8                                                            
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CLIVE                                                            
         B     CLIV2K                                                           
*                                                                               
CLCV8    CLC   RNUM,=C'AR'                                                      
         BNE   CLCV12                                                           
         TM    PCLTACON,X'20'   MUST BE SET FOR NO BUYING AGY CONS              
         BNO   CLIVE                                                            
         CLC   RAGY,PCLTAOR     ALSO I CAN'T BE THE AOR                         
         BE    CLIVE                                                            
         B     CLIV2X                                                           
*                                                                               
CLCV12   CLC   PCLTKCLT,PCLTADV      MUST BE ADVERTISER CLIENT                  
         BNE   CLIVE                                                            
*                                                                               
         DROP  RE                                                               
*                                                                               
CLIV2X   DS    0H                                                               
         CLC   RNUM,=C'12'     NO SLAVE CLTS FOR CONTRACTS                      
         BE    CLIV3                                                            
         CLC   RNUM,=C'14'                                                      
         BNE   CLIV4                                                            
CLIV3    CLI   PCLTPROF+5,C'2'       SLAVE                                      
         BNE   CLIV8                                                            
         MVC   FERN,=AL2(NOSLAV)                                                
         B     CLIVX                                                            
*                                                                               
CLIV4    CLC   RNUM,=C'06'                                                      
         BNE   CLIV8                                                            
         CLI   PCLTPROF+13,C'N'                                                 
         BNE   CLIV8                                                            
         MVC   FERN,=AL2(NUMINV)     NO DETAIL BILLING FOR THIS CLT             
         B     CLIVX                                                            
CLIV8    OI    FIND,X'04'                                                       
         MVC   NAME(20),PCLTNAME                                                
         MVC   CLIPROF,PCLTPROF                                                 
*                                                                               
*                               SEARCH FOR PCLTDRO (X'30') ELEMENT              
*                               AND SAVE DRD OVERRIDE CLIENT IN                 
*                               CLIPROF+20(3)                                   
         XC    CLIPROF+20(3),CLIPROF+20                                         
*                                                                               
         LA    RE,PCLTREC+33                                                    
CLIV8C   CLI   0(RE),X'30'                                                      
         BE    CLIV8F                                                           
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    CLIV8X                                                           
         B     CLIV8C                                                           
*                                                                               
CLIV8F   MVC   CLIPROF+20(3),2(RE)                                              
         OC    CLIPROF+20(3),SPACES                                             
*                                                                               
CLIV8X   DS    0H                                                               
*                              OVERLAY LAST 3 BYTES WITH OFFICE CODE            
         MVC   CLIPROF+27(3),PCLTOFF                                            
CLIV9    FOUT  (R6),NAME                                                        
*                                                                               
         CLI   ROUTSUB,0           ONE FIELD INPUT                              
         BE    CLIVO                                                            
         DROP  R5                                                               
*                                                                               
         LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BE    CLIVX                                                            
         TM    FIND,X'04'                                                       
         BZ    CLIVE           FIRST CLT CAN'T BE ALL OR *NN                    
         CLC   IFLD(3),=C'ALL'     SECOND CLT CAN'T BE ALL                      
         BE    CLIVE                                                            
         CLI   IFLDH+5,2                                                        
         BL    CLIVE                                                            
         CLI   IFLDH+5,3                                                        
         BH    CLIVE                                                            
         MVC   KRT1+4(3),IFLD      READ SECOND CLT                              
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    CLIVO         DISK ERROR                                         
         BH    *+14                                                             
         MVC   FERN,=AL2(CLINOF)  NOT FOUND                                     
         B     CLIVO                                                            
         CLC   RNUM,=C'12'         CONTRACTS - SECOND CLT                       
*                                  MUST BE SLAVE FOR FIRST CLIENT               
         BNE   CLIV12                                                           
         LA    R5,PRTREC                                                        
         USING PCLTRECD,R5                                                      
         CLI   PCLTPROF+5,C'2'      MUST BE SLAVE                               
         BNE   CLIVE                                                            
         CLC   PCLTPROF+6(3),RCLI  MUST BE FOR REQUESTED MASTER                 
         BNE   CLIVE                                                            
         MVI   FIND,X'21'          SET FOR MASTER/SLAVE                         
*                                                                               
         MVC   KRT1+4(3),RCLI      NEEDED FOR CONTRACT READ                     
         MVC   KRT2+4(3),IFLD      SLAVE-FOR PRODUCT READ                       
         MVC   RDIV(3),IFLD                                                     
         B     CLIVX                                                            
*                                                                               
CLIV12   MVI   FIND,X'11'          CLI=XXX-XXX                                  
         MVC   KRT1+4(3),RCLI      RESET OT FIRST CLT                           
         MVC   RPUB+1(3),=C'RD='                                                
         MVC   RPUB+4(3),IFLD                                                   
         B     CLIVX                                                            
*                                                                               
CACCERR  MVC   FERN,=AL2(ACCERR)                                                
         B     CLIVO                                                            
CLIVE    MVC   FERN,=AL2(FLDINV)                                                
CLIVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
CLIVOX   FOUT  (R6),NAME                                                        
CLIVX    OC    CLISAVE,FIND                                                     
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
CLIV100  NTR1                   * TEST OFFICE LIST SECURITY *                   
         SPACE 1                                                                
         LA    R7,PRTREC                                                        
         USING PCLTRECD,R7                                                      
         XC    PLIST(8),PLIST                                                   
         MVC   PLIST+4(4),=X'D9000A38' GET OFFICER ADDRESS                      
         GOTO1 CALLOV,PLIST                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC,PCLTOFF                                                   
         CLI   IFLD,C'*'           SEE IF OFFICE REQ                            
         BNE   *+10                                                             
         MVC   OFCOFC,IFLD+1       AND MOVE IN REQ OFFICE                       
         DROP  R1                                                               
*                                                                               
         L     RF,PLIST                                                         
         GOTO1 (RF),PLIST,DUB,ACOMFACS                                          
         CLI   0(R1),0                                                          
         BNE   CACCERR                                                          
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
CHKGRP   NTR1                   * TEST CLIENT GROUP SECURITY *                  
         SPACE 1                                                                
         XC    PRTREC(10),PRTREC                                                
         XC    AOFFICER,AOFFICER                                                
         LA    R7,PRTREC                                                        
         USING PCLTRECD,R7                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(25),KRT1                                                     
         XC    KEY+4(21),KEY+4                                                  
         MVI   KEY+3,X'02'                                                      
         BAS   R8,PRTHIGH                                                       
         B     CKGP5                                                            
*                                                                               
CKGP3    BAS   R8,PRTSEQ                                                        
*                                                                               
CKGP5    CLC   KEY(4),KEYSAVE                                                   
         BNE   CKGPX              HAVE DONE ALL CLINETS                         
         BAS   R8,PRTGET                                                        
         CLC   PCLTBLGP,IFLD+1        SEE IF RIGHT GROUP                        
         BNE   CKGP3                                                            
         CLI   6(R3),C'*'             CHECK OFFICE LIMIT ACCESS                 
         BNE   CKGP8                                                            
         CLC   PCLTOFF(1),7(R3)                                                 
         BE    CKGP3                                                            
         B     CACCERR                                                          
*                                                                               
*        TO GET HERE MUST BE OFFICE LIST LIMIT ACCESS                           
*                                                                               
CKGP8    XC    PLIST(8),PLIST                                                   
         OC    AOFFICER,AOFFICER        SEE IF I ALREADY HAVE                   
         BNZ   CKGP10                                                           
         MVC   PLIST+4(4),=X'D9000A38' GET OFFICER ADDRESS                      
         GOTO1 CALLOV,PLIST                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AOFFICER,PLIST                                                   
CKGP10   XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'         SYSTEM ID                                    
         MVC   OFCAUTH,6(R3)       ID AUTH VALUE                                
         MVC   OFCAGY,RAGY                                                      
         MVC   OFCOFC,PCLTOFF                                                   
         CLI   IFLD,C'*'           SEE IF OFFICE REQ                            
         BNE   *+10                                                             
         MVC   OFCOFC,IFLD+1       AND MOVE IN REQ OFFICE                       
         DROP  R1                                                               
*                                                                               
         GOTO1 AOFFICER,PLIST,DUB,ACOMFACS                                      
         CLI   0(R1),0                                                          
         BNE   CACCERR                                                          
*                                 CHK ACCESS TO THIS CLIENT                     
         B     CKGP3                                                            
*                                                                               
PRTHIGH  LA    R4,=C'DMRDHI'                                                    
         MVC   KEYSAVE(25),KEY                                                  
         B     PRTREAD                                                          
*                                                                               
PRTSEQ   LA    R4,=C'DMRSEQ'                                                    
PRTREAD  LA    R5,=C'PRTDIR'                                                    
*                                                                               
         ST    R6,FULL                                                          
         LA    R6,KEY                                                           
         LA    R3,KEY                                                           
         B     PRTFILE                                                          
*                                                                               
PRTGET   LA    R4,=C'GETREC'                                                    
         LA    R5,=C'PRTFILE'                                                   
         LA    R6,KEY+27         DISK ADDR                                      
         LA    R3,PRTREC                                                        
*                                                                               
PRTFILE  GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),(R3)                                 
         L     R3,ASAVE       MUST RESTORE R3                                   
         L     R6,FULL                                                          
         CLI   DMCB+8,0        CHK FOR DISK ERROR                               
         BE    *+14                                                             
         MVC   FERN,=AL2(0)                                                     
         B     CLIVOX                                                           
         BR    R8                                                               
*                                                                               
CKGPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*       *************************                                               
******  TEST CLIENT GROUP SECURITY  ROUTINE                                     
*                                                                               
VALGROUP NTR1                                                                   
*                                                                               
         LA    R7,PRTREC          STILL SHOULD POINT TO CLIENT                  
         USING PCLTRECD,R7                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPRECD,R4          CLIENT GROUP PASSIVE POINTER                 
*                                                                               
         MVI   GRPPTYP,GRPPCGQ     RECORD TYPE                                  
         MVC   GRPPAGY(3),PCLTKAGY   AGENCY & MEDIA                             
*****    MVC   GRPPMED(1),PCLTKMED                                              
         MVC   GRPPVAL(3),PCLTKCLT   CLIENT                                     
         OC    GRPPVAL,=6C' '      SPACE PADDED                                 
         MVC   GRPPID(1),7(R3)       GROUP ID                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(14),KEYSAVE     CHECK THROUGH ID                             
         BNE   VALGR10                                                          
*                                                                               
         MVC   FULL,=6C' '         (6 TO SAVE USING ANOTHER LITERAL)            
         MVC   FULL(2),8(R3)       GROUP CODE                                   
         OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRL   R0,4                GET RID OF SIGN NIBBLE                       
         STCM  R0,3,HALF           LEFT-JUSTIFIED, PWOS                         
*                                                                               
         CLC   HALF,GRPPCODE       GROUP CODE MATCH?                            
         BE    VALGR20             YES                                          
*                                                                               
VALGR10  LTR   RE,RE               ACCESS DENIED EXIT (NOT EQUAL)               
         B     VALGXIT                                                          
*                                                                               
VALGR20  CR    RE,RE               ACCESS OK EXIT (EQUAL)                       
*                                                                               
VALGXIT  XIT1                                                                   
*                                                                               
         DROP  R4,R7                                                            
*                                                                               
*******************************************************                         
*                                                                               
         EJECT                                                                  
*        VALIDATE DIVISION AND SET FIND FORMAT BITS                             
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*                                                                               
DIVVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    DIVVO                                                            
         CLI   FIND,1                                                           
         BE    DIVVA5                                                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    DIVVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            IF NOT EQUAL BUT OFFICE SPECIFIED           
         BE    DIVVO                GO ON TO FURTHER VALIDATION                 
         CLI   RCLI,C'&&'           IF CLIENT GROUP REQUEST                     
         BE    DIVVO                GO ON TO FURTHER VALIDATION                 
         CLC   RCLI,=C'ALL'         CLIENT ALL REQUEST                          
         BE    DIVVO                                                            
*                                                                               
         XC    KRT1+7(18),KRT1+7    CLEAR END OF KEY                            
         MVI   KRT1+3,X'03'         MOVE IN DIV CODE                            
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVVA3                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
DIVVA3   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVVA4                                                           
         CLC   KEYSAVE+7(3),=C'000'                                             
         BNE   DIVINV                                                           
**                                                                              
DIVVA4   CLC   KEYSAVE(7),KRT1        IF EQUAL, DIVISION FOUND                  
         BE    DIVVO                                                            
         B     DIVINV                 ELSE DIVISION NOT FOUND                   
*                                                                               
DIVVA5   CLI   RCLI,C'*'              IF ALL OFFICE CODE SPECIFIED              
         BNE   DIVVA8                                                           
         MVC   FERN,=AL2(FF)                                                    
         B     DIVVO                                                            
*                                                                               
DIVVA8   GOTO1 ARJN                   RIGHT JUSTIFY ROUTINE                     
*                                                                               
         CLC   TEMP+2(3),=C'000'                                                
         BNE   DIVV10                                                           
         MVC   FERN,=AL2(FF)                                                    
         B     DIVV10                                                           
*                                                                               
DIVV10   CLC   FERN,=AL2(FF)                                                    
         BL    DIVINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+7(3),TEMP+2                                                 
         UNPK  RDIV(3),DUB+6(2)                                                 
         MVC   KRT1+7(3),RDIV                DIVISION TO KEY1                   
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DIVV20                                                           
         CLC   RDIV,=C'000'                                                     
         BNE   DIVINV                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
DIVV20   OC    KRT1+4(3),KRT1+4              WAS CLIENT SPECIFIC                
         BZ    DIVINV                        NO                                 
         MVI   KRT1+3,03                     READ DIVISION                      
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    DIVVO                         DISK ERROR                         
         BH    DIVV30                                                           
         MVC   FERN,=AL2(DIVNOF)             DIVISION NOT ON FILE               
         B     DIVVO                                                            
DIVV30   OI    FIND,X'04'                                                       
DIVV40   MVC   NAME(20),PRTREC+35                                               
         B     DIVVO                                                            
*                                                                               
DIVINV   MVC   FERN,=AL2(DIVERR)             DIVISION INVALID                   
DIVVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
DIVVX    XIT1                                                                   
         EJECT                                                                  
*        VALIDATE PRODUCT AND SET FIND FORMAT BITS                              
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*        X'10' =*XX OTHER AGY PRDS                                              
*                                                                               
PROVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BNE   PROVO               PRODUCT =ALL OR MISSING                      
PROV1    CLC   RNUM,=C'04'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'06'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'B1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'D1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'E1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'R1'                                                      
         BE    CKZZZ                                                            
         CLC   RNUM,=C'RD'         REBATE DRAFT                                 
         BE    CKZZZ                                                            
         CLC   RNUM,=C'66'         DUPONT REPORT                                
         BE    DUPON1                                                           
         CLC   RNUM,=C'L1'         OR L1 FOR DUPONT                             
         BE    DUPON0                                                           
         CLC   RNUM,=C'LB'         OR LB FOR DUPONT                             
         BNE   PROV1A                                                           
DUPON0   CLC   RAGY,=C'DP'                                                      
         BNE   PROV1A                                                           
DUPON1   MVC   KRT2(2),KRT2+4        MOVE CLT (AGY) TO AGY                      
         MVC   KRT2+4(3),=C'DP '    MUST SET CLIENT TO DP                       
PROV1A   MVC   KRT2+7(3),IFLD                                                   
         OC    KRT2+4(3),KRT2+4              WAS CLIENT SPECIFIC                
         BZ    PROINV                        NO                                 
*                                                                               
PROV1D   CLI   CLIPROF+5,C'1'      MASTER CLIENT ?                              
         BNE   PROV2               NO                                           
         CLC   RNUM,=C'12'         CONTRACTS ?                                  
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'14'         CONTRACTS LISTING ?                          
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'16'         AUTOMATIC RATE CHG ?                         
         BE    PROV1G              YES                                          
         CLC   RNUM,=C'18'         CONTRACT ANALYSIS ?                          
         BNE   PROV2               NO - NOT 12, 14, 16, OR 18                   
PROV1G   DS    0H                  DO NOT READ PRODUCT - MASTER CLIENT          
         OI    FIND,X'04'          JUST SET PRODUCT VALID =XXX                  
         XC    NAME(20),NAME       CLEAR NAME                                   
         B     PROVO               AND FINISH PRODUCT                           
*                                                                               
PROV2    MVI   KRT2+3,06                     READ PRODUCT                       
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    PROVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(PRDNOF)             PRODUCT NOT ON FILE                
         B     PROVO                                                            
         OI    FIND,X'04'                    PRODUCT VALID =XXX                 
         MVC   NAME(20),PRTREC+35                                               
         CLI   IFLD,C'*'           CHK FOR OTHER AGY PRD                        
         BNE   PROV4                                                            
         MVI   FIND,X'11'          SET VALID *XX PRD                            
         B     PROV4                                                            
*                                                                               
PROV4    CLC   RNUM,=C'B1'                   NEW BILLING                        
         BE    PROV5                                                            
         CLC   RNUM,=C'D1'                   NEW DRAFT BILLING                  
         BE    PROV5                                                            
         CLC   RNUM,=C'R1'                   NEW REBATE BILLING                 
         BE    PROV5                                                            
         CLC   RNUM,=C'RD'                   NEW REBATE BILLING DRAFT           
         BE    PROV5                                                            
         B     PROVO                                                            
*                                                                               
PROV5    CLC   PRTREC+159(3),=C'   '         CHK FOR DIVISION                   
         BNH   PROVO                                                            
         CLC   RDIV,=C'ALL'                  SEE IF ALL IN DIVISION             
         BNE   PROVO                                                            
         MVC   RDIV,PRTREC+159               SET TO PROPER DIVISION             
         B     PROVO                                                            
*                                                                               
PROINV   MVC   FERN,=AL2(PRDERR)             PRODUCT INVALID                    
PROVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         TM    FIND,X'02'          PRD=ALL                                      
         BZ    PROVO5                                                           
         CLC   RNUM(2),=C'27'      PAYER'S LIST                                 
         BNE   PROVO5                                                           
         MVC   NAME(21),=C'** PRDS SEPARATELY **'                               
*                                                                               
PROVO5   DS    0H                                                               
         FOUT  (R6),NAME                                                        
PROVX    OC    PROSAVE,FIND                                                     
         XIT1                                                                   
*                                                                               
*                                                                               
CKZZZ    CLC   IFLD(3),=C'ZZZ'         NO ZZZ BILLING ALLOWED                   
         BNE   PROV1A                                                           
         MVC   FERN,=AL2(NOZZZ)                                                 
         B     PROVO                                                            
         EJECT                                                                  
*        VALIDATE REGION AND SET FIND FORMAT BITS                               
*        X'02' =ALL                                                             
*        X'04' = XXX                                                            
*                                                                               
REGVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    REGVO               REG=ALL OR MISSING                           
         CLI   FIND,1                                                           
         BE    REGVA20             REG=ALL OR MISSING                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    REGVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            SEE IF OFFICE REQUEST                       
         BE    REGVO                YES THEN NO CHK                             
         CLI   RCLI,C'&&'           SEE IF CLIENT GROUP REQUEST                 
         BE    REGVO                YES THEN NO CHK                             
         CLC   RCLI,=C'ALL'                                                     
         BE    REGVO                                                            
*                                                                               
         XC    KRT1+10(15),KRT1+10                                              
         MVI   KRT1+3,X'04'                                                     
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   REGVA5                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
REGVA5   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         OC    KRT1+7(3),KRT1+7                                                 
         BZ    REGVA10                                                          
         CLC   KRT1(10),KEYSAVE                                                 
         BNE   REGINV                                                           
         B     REGVO                                                            
*                                                                               
REGVA10  CLC   KRT1(7),KEYSAVE                                                  
         BNE   REGINV                                                           
         B     REGVO                                                            
*                                                                               
REGVA20  GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    REGINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+10(3),TEMP+2             REGION TO KEY1                     
         CLC   RPUB+1(3),=C'RD='    SEE IF USING CLT R/D OVERRIDE               
         BNE   REGV1                                                            
         MVC   KRT1+4(3),RPUB+4                                                 
         MVC   KRT1+7(3),=C'000'       MUST BE DIV 000                          
*                                                                               
REGV1    OC    KRT1+7(3),KRT1+7              WAS DIVISION SPECIFIC              
         BZ    REGINV                        NO                                 
         CLC   TEMP+2(3),=C'999'                                                
         BNE   REGV2                                                            
         OI    FIND,X'04'                                                       
         B     REGVO         OMIT FILE READ FOR REG 999                         
REGV2    MVI   KRT1+3,04                     READ REGION                        
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    REGVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(REGNOF)             REGION NOT ON FILE                 
         B     REGVO                                                            
         OI    FIND,X'04'                    REGION VALID =XXX                  
         MVC   NAME(20),PRTREC+35                                               
         B     REGVO                                                            
*                                                                               
REGINV   MVC   FERN,=AL2(REGERR)             REGION INVALID                     
REGVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
REGVX    XIT1                                                                   
         EJECT                                                                  
*        VALIDATE DISTRICT AND SET FIND FORMAT BITS                             
*        X'02' =ALL                                                             
*        X'04' =XXX                                                             
*                                                                               
DISVAL   NTR1                                                                   
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         CLI   FIND,1                                                           
         BL    DISVO                                                            
         CLI   FIND,1                                                           
         BE    DISVA5                                                           
*                                                                               
         CLI   RMED,C'*'            SEE IF ALL MEDIA REQ                        
         BE    DISVO                YES THEN NO CHK                             
         CLI   RCLI,C'*'            SEE IF OFFICE REQUEST                       
         BE    DISVO                YES THEN NO CHK                             
         CLI   RCLI,C'&&'           SEE IF CLIENT GROUP REQUEST                 
         BE    DISVO                YES THEN NO CHK                             
         CLC   RCLI,=C'ALL'                                                     
         BE    DISVO                                                            
*                                                                               
*                                                                               
         XC    KRT1+13(12),KRT1+13                                              
         MVI   KRT1+3,X'05'                                                     
**                                                                              
         CLC   RPUB+1(3),=C'RD='                                                
         BNE   DISVAA                                                           
         MVC   KRT1+4(3),RPUB+4                                                 
**                                                                              
DISVAA   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT1,KEYSAVE                  
         OC    KRT1+10(3),KRT1+10                                               
         BZ    DISVA1                                                           
         CLC   KRT1(13),KEYSAVE                                                 
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA1   OC    KRT1+7(3),KRT1+7                                                 
         BZ    DISVA3                                                           
         CLC   KRT1(10),KEYSAVE                                                 
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA3   CLC   KRT1(7),KEYSAVE                                                  
         BNE   DISINV                                                           
         B     DISVO                                                            
*                                                                               
DISVA5   GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    DISINV                                                           
         MVC   IFLD(3),TEMP+2                                                   
         MVC   KRT1+13(3),TEMP+2                                                
         OC    KRT1+10(3),KRT1+10            WAS REGION SPECIFIC                
         BZ    DISINV                        NO                                 
         CLC   TEMP+2(3),=C'999'                                                
         BNE   DISV2                                                            
         OI    FIND,X'04'                                                       
         B     DISVO        OMIT FILE READ FOR DIS 999                          
DISV2    MVI   KRT1+3,05                     READ DISTRICT                      
         GOTO1 AREAD,PLIST,C'PRT1'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    DISVO                         DISK ERROR                         
         BH    *+14                                                             
         MVC   FERN,=AL2(DISERR)             DISTRICT NOT ON FILE               
         B     DISVO                                                            
         OI    FIND,X'04'         DIS=XXX                                       
         MVC   NAME(20),PRTREC+35                                               
         B     DISVO                                                            
*                                                                               
DISINV   MVC   FERN,=AL2(DISERR)             DISTRICT INVALID                   
DISVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
DISVX    XIT1                                                                   
         EJECT                                                                  
*        VALIDATE ESTIMATE(S) AND SET FIND FORMAT BITS                          
*        X'02' = ALL                                                            
*        X'04' =NNN                                                             
*        X'08' =NNN-NNN                                                         
*        X'10' = ALL,XXX   FILTERS                                              
*        X'20' =NNN  + NON SPECIFIC PROD                                        
*        X'40' = NNN-NNN  + NON SPECIFIC PROD                                   
*        X'80' =NO,XXX             FILTERS                                      
*                                                                               
ESTVAL   NTR1                                                                   
         MVI   ROUTSUB,1           TEST FIRST FOR "HIGH" AND "LAST"             
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         XC    ESTDATES,ESTDATES                                                
         CLC   IFLD(4),=C'HIGH'    SEARCH FOR HIGHEST NUMBER USED               
         BE    EVAL2                                                            
         CLC   IFLD(4),=C'LAST'  SEARCH FOR EST WITH HIGHEST START DATE         
         BE    EVAL2                                                            
         CLI   IFLDH+5,1                                                        
         BNE   EVALX               EST NOT HIGH OR LAST OR H OR L               
         CLI   IFLD,C'H'                                                        
         BE    EVAL2                                                            
         CLI   IFLD,C'L'                                                        
         BNE   EVALX               EST NOT HIGH OR LAST OR H OR L               
EVAL2    DS    0H                                                               
         TM    PROSAVE,X'0C'       IS PRODUCT SPECIFIC ?                        
         BZ    ESTVE1              NO - HIGH AND LAST ARE INVALID               
         XC    SVESDT,SVESDT       CLEAR SEARCH EST START DATE                  
         XC    SVLEST,SVLEST       CLEAR ESTIMATE NUMBER                        
         XC    KRT2+10(15),KRT2+10    CLEAR KEY FOLLOWING PROD                  
         MVI   KRT2+3,X'07'        READ ESTIMATE                                
         XC    KEYSAVE,KEYSAVE                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PRTDIR',KRT2,KEYSAVE                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EVAL4B                                                           
*                                                                               
EVAL4    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PRTDIR',KEYSAVE,KEYSAVE               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EVAL4B   CLC   KRT2(10),KEYSAVE    CHECK THRU PRODUCT                           
         BNE   EVAL4T                                                           
         MVC   FULL,KEYSAVE+27     SAVE REC ADDRESS                             
         CLI   IFLD,C'H'           SEE IF LOOKING FOR HIGHEST EST               
         BNE   EVAL4D                                                           
         MVC   SVLEST,KEYSAVE+10   SAVE NUMBER AND KEEP SEARCHING               
         B     EVAL4                                                            
*                                                                               
EVAL4D   DS    0H                  READ THE RECORD                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'PRTFIL',FULL,PRTREC,DMWORK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*****    CLC   IFLD(4),=C'HIGH'    FINISHING "HIGH" PROCESSING ?                
         CLI   IFLD,C'H'           FINISHING "HIGH" PROCESSING ?                
         BE    EVAL4TD             YES - GO DO IT                               
         CLC   SVESDT,PRTREC+55    EST DATE IN RECORD HIGH ?                    
         BH    EVAL4               NO - KEEP SEARCHING                          
         MVC   SVESDT,PRTREC+55    YES - SAVE DATES                             
         MVC   ESTDATES(12),PRTREC+55                                           
         MVC   SVLEST,PRTREC+10          AND EST NUM                            
         MVC   NAME(20),PRTREC+35        AND NAME                               
         B     EVAL4               LOOK FOR HIGHER DATE                         
*                                                                               
EVAL4T   DS    0H                  LAST ESTIMATE READ                           
         OC    SVLEST,SVLEST       SEE IF ANY FOUND                             
         BZ    ESTVE1              NO - INVALID EST ERROR EXIT                  
         OI    FIND,X'04'          EST=NNN                                      
*****    CLC   IFLD(4),=C'LAST'    LOOKING FOR HIGHEST START DATE ?             
         CLI   IFLD,C'L'           LOOKING FOR HIGHEST START DATE ?             
         BE    EVAL4TX             YES - FINISH UP                              
         B     EVAL4D            FOR "HIGH" GO READ REC FOR DATA BELOW          
EVAL4TD  MVC   ESTDATES(12),PRTREC+55    SAVE DATES                             
         MVC   NAME(20),PRTREC+35        AND NAME                               
EVAL4TX  LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         EDIT  (B2,SVLEST),(3,0(R7)),0,ALIGN=LEFT,FILL=0,ZERO=NOBLANK           
         XC    8(4,R4),8(R4)                                                    
         FOUT  (R4),0(R7),3                                                     
         FOUT  (R6),NAME                                                        
         B     ESTVX               DONE WITH "LAST" AND "HIGH"                  
*                                                                               
EVALX    DS    0H                  EST NOT "LAST" OR "HIGH"                     
         CLI   FIND,1                                                           
         BNE   ESTVO               EST = ALL OR MISSING                         
         CLI   IFLDH+5,3                                                        
         BH    ESTVE                                                            
         CLC   IFLD(3),=C'NO '                                                  
         BNE   ESTV1                                                            
         OI    FIND,X'80'                                                       
         CLC   5(1,R4),IFLDH+5                                                  
         BE    ESTVE1              'NO' MUST HAVE FILTERS                       
         B     ESTVO                                                            
*                                                                               
ESTV1    GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE                                                            
         MVC   IFLD(3),TEMP+2                                                   
         TM    PROSAVE,X'0C'                                                    
         BNZ   ESTV1A                                                           
         OI    FIND,X'20'      EST=NNN + NON SPECIFIC PROD                      
         B     ESTVO                                                            
*                                                                               
ESTV1A   MVC   KRT2+10(2),TEMP                                                  
         MVI   KRT2+3,X'07'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    ESTVO               DISK ERROR                                   
         BH    *+14                                                             
         MVC   FERN,=AL2(ESTNOF)   NOT ON FILE                                  
         B     ESTVX                                                            
         MVC   ESTDATES(12),PRTREC+55                                           
         MVC   NAME(20),PRTREC+35                                               
         OI    FIND,X'04'          EST=NNN                                      
         B     ESTVO                                                            
*                                                                               
ESTVE    MVC   FERN,=AL2(ESTINV)                                                
ESTVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(3,R7),IFLD                                                     
         FOUT  (R6),NAME                                                        
*                                                                               
ESTV3    CLC   5(1,R4),IFLDH+5     ONLY ONE FIELD                               
         BE    ESTVX                                                            
ESTV3A   TM    FIND,X'A6'     WAS FIRST FLD NNN,NO,ALL                          
         BZ    ESTVE1              NO - CAN'T HAVE SECOND EST                   
         NI    FIND,X'59'                                                       
*                                                                               
*              CHK FOR NO,XXX   FILTERS                                         
*                                  OR ALL,XXX                                   
*                                                                               
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTV3B                                                           
         CLC   0(3,R7),=C'ALL'                                                  
         BE    ESTV3B                                                           
         B     ESTV3K                                                           
*                                                                               
ESTV3B   DS    0H                                                               
         MVI   ROUTSUB,0           SET TO REEDIT FIELD                          
         GOTO1 AINITV                                                           
*                                  CHECK FILTERS                                
         CLC   0(2,R7),=C'NO'                                                   
         BE    ESTV3B4                                                          
         CLI   IFLDH+5,7                                                        
         BL    ESTVE1                                                           
         CLI   IFLDH+5,10                                                       
         BH    ESTVE1                                                           
         ZIC   R1,IFLDH+5                                                       
         SH    R1,=H'4'       ADJUST FOR ALL,                                   
         LA    R4,IFLD+4                                                        
         B     ESTV3B6                                                          
*                                                                               
ESTV3B4  DS    0H                                                               
         CLI   IFLDH+5,6                                                        
         BL    ESTVE1                                                           
         CLI   IFLDH+5,9                                                        
         BH    ESTVE1              MAX IS -X-X-X                                
         ZIC   R1,IFLDH+5          SAVE INPUT LENGHT                            
         SH    R1,=H'3'            ADJUST FOR NO,                               
         LA    R4,IFLD+3                                                        
ESTV3B6  LA    R5,3                FOR BCT                                      
         XC    TEMP(5),TEMP                                                     
         LA    R6,TEMP+2                                                        
ESTV3C   EQU   *                                                                
         CLI   0(R4),C'-'          SEE IF NEGATIVE FILTER                       
         BNE   ESTV3E              NO                                           
         MVI   TEMP,1                                                           
         LA    R4,1(R4)            PAST -                                       
         BCTR  R1,0                DECREMENT COUNTER                            
ESTV3E   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   ESTV3F                                                           
         CLI   TEMP,0                                                           
         BNE   ESTVE1              -* IS INVALID                                
         B     ESTV3G                                                           
*                                                                               
ESTV3F   CLI   0(R6),C'A'                                                       
         BL    ESTVE1                                                           
         CLI   0(R6),C'9'                                                       
         BH    ESTVE1                                                           
         CLI   TEMP,1            SEE IF NEGATIVE FILETR                         
         BNE   *+8                                                              
         NI    0(R6),X'BF'         SET OFF X'40'                                
ESTV3G   LA    R6,1(R6)                                                         
         MVI   TEMP,0            ZERO NEGATIVE INDICATOR                        
         LA    R4,1(R4)                                                         
         BCTR  R1,0                                                             
         BCT   R5,ESTV3C                                                        
         LTR   R1,R1                                                            
         BNZ   ESTVE1              SHOULD HAVE NO MORE INPUT                    
         CLC   0(3,R7),=C'ALL'                                                  
         BNE   ESTV3H                                                           
         MVI   FIND,X'11'          ALL,XXX                                      
         B     ESTV5                                                            
*                                                                               
ESTV3H   DS    0H                                                               
         MVI   FIND,X'81'          NO,XXX                                       
         B     ESTV5                                                            
*                                                                               
ESTV3K   EQU   *                                                                
         MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BNE   ESTVE1                                                           
         GOTO1 ARJN                                                             
         CLC   FERN,=AL2(FF)                                                    
         BL    ESTVE1                                                           
         TM    PROSAVE,X'0C'                                                    
         BNZ   *+18                                                             
         OI    FIND,X'40'          EST=NNN-NNN + NON SPECIFIC PRD               
         XC    ESTDATES,ESTDATES                                                
         B     *+8                                                              
         OI    FIND,X'08'          EST=NNN-NNN                                  
ESTV5    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   3(3,R7),TEMP+2                                                   
         CLC   0(2,R7),=C'NO'                                                   
         BNE   ESTV6                                                            
         MVC   0(3,R7),=C'   '     SET TO BLANKS                                
         B     ESTVX                                                            
*                                                                               
ESTV6    DS    0H                                                               
         CLC   0(3,R7),=C'ALL'                                                  
         BE    ESTVX                                                            
         CLC   0(3,R7),3(R7)                                                    
         BL    ESTVX                                                            
         MVC   FERN,=AL2(EST1G2)                                                
         B     ESTVX                                                            
*                                                                               
ESTVE1   MVC   FERN,=AL2(ESTINV)                                                
ESTVX    MVC   ESTSAVE,FIND                                                     
         XIT1                                                                   
         EJECT                                                                  
*        VALIDATE PUBLICATION,ZONE,EDITION AND SET FIND FORMAT BITS             
*        X'02' =ALL   SET OT BLANKS IN REQ REC                                  
*        X'04' NNNNNNNN                                                         
*        X'08' NNNNNNNN,XX,X                                                    
*        X'10' JNNNNNN           JOB NUMBER                                     
*        X'20' NUM,Z,ALL                                                        
*        X'40' NUM,ALL                                                          
*                                                                               
PZEVAL   NTR1                                                                   
*                                                                               
**       PUB NAME SEARCHING                                                     
**                                                                              
*          DATA SET PPMBC00N   AT LEVEL 113 AS OF 08/23/91                      
**                                                                              
**       NAME SEARCH CALL                                                       
**                                                                              
         L     R4,FLDHADR                                                       
         SR    R4,R3               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R5,TEMP                                                          
         USING DSPARM,R5                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,RMED                                                    
*                                                                               
         GOTO1 =V(SRCHCALL),PLIST,(3,(R4)),(X'80',(R3)),ACOMFACS,      X        
               ('DSPARML',TEMP),(1,=CL8'PUB'),0,RR=RELO                         
         DROP  R5                                                               
*                                                                               
         GOTO1 AINITV                        SET R4=A(HDR) & R5=L'DATA          
         TM    FIND,X'02'     CHK FOR PUB=ALL                                   
         BZ    PZEV1      MOVE ALL TO REQ REC UNLESS RPUB USED                  
         CLC   RPUB(4),=4C' '                                                   
         BE    PZEV0                                                            
         B     PZEVX         RPUB USED ALREADY SO LEAVE ALONE                   
PZEV0    MVC   RPUB(3),=C'ALL'                                                  
         B     PZEVX                                                            
*                                                                               
PZEV1    CLI   FIND,1              PUB= MISSING - EXIT                          
         BNE   PZEVX                                                            
         CLC   RPUB(4),=4C' '                                                   
         BE    EDTJOB                                                           
         MVC   FERN,=AL2(NOPUB)  RPUB USED ALREADY - ERROR                      
         B     PZEVX                                                            
*                                                                               
EDTJOB   CLI   IFLD,C'J'                                                        
         BNE   EDTPUB                                                           
         CLI   IFLDH+5,7                                                        
         BH    EJOBINV                                                          
*                                                                               
         MVC   KRT2+10(6),IFLD+1                                                
         OC    KRT2+7(3),KRT2+7                                                 
         BZ    EJOBINV         PRODUCT MUST BE SPECIFIED                        
         MVI   KRT2+3,X'15'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    EJOBVO                                                           
         BH    *+14                                                             
         MVC   FERN,=AL2(JOBNOF)                                                
         B     EJOBVO                                                           
         OI    FIND,X'10'            VALID JOB INPUT                            
         B     EJOBVO                                                           
*                                                                               
EJOBINV  MVC   FERN,=AL2(JOBERR)                                                
EJOBVO   MVI   RPUB+1,C'J'                                                      
         MVC   RPUB+2(6),KRT2+10                                                
         B     PZEVXX                                                           
*                                                                               
*                                                                               
EDTPUB   MVC   TEMP(20),IFLD           SAVE ALL OF INPUT                        
         LH    R7,=H'4'                                                         
         MVI   ROUTSUB,1                                                        
         GOTO1 AINITV                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BE    PZEINV                                                           
         CLI   IFLDH+5,0                                                        
         BE    PZEINV                                                           
         LR    R8,R5                                                            
EDTP1    MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BE    EDTP1E                                                           
         CLI   IFLDH+5,0                                                        
         BE    EDTP1D                                                           
         AR    R8,R5                                                            
         LA    R8,1(R8)                                                         
         LH    R7,=H'5'                                                         
         CLI   IFLD,C'0'       NOT NUMERIC SO NO 3RD FLD ALLOWED                
         BL    EDTP1D                                                           
         MVI   ROUTSUB,3                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BNE   EDTP1A                                                           
         LH    R7,=H'6'                                                         
         B     EDTP2                                                            
*                                                                               
EDTP1A   CLC   IFLD(3),=C'ALL'                                                  
         BNE   EDTP1B                                                           
         B     EDTP2                                                            
*                                                                               
EDTP1B   AR    R8,R5                                                            
         LA    R8,1(R8)                                                         
         LH    R7,=H'6'                                                         
         B     EDTP2                                                            
*                                                                               
EDTP1D   LH    R7,=H'6'                                                         
EDTP1E   MVI   ROUTSUB,3                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0                                                        
         BNE   PZEINV                                                           
*                                                                               
EDTP2    GOTO1 =V(PUBVAL),PLIST,((R8),TEMP),(1,RPUB),RR=RELO                    
         CLI   0(R1),X'FF'                   PUBLICATION TO REQ REC             
         BE    PZEINV                                                           
         GOTO1 (RF),(R1),,(0,KUB1+1)         PUBLICATION TO KEY#1               
*                                                                               
*        READ AGYHEADER FOR DEFAULT PROFILE BYTE                                
*                                                                               
         MVC   TEMP(25),KRT2       SAVE KRT2 KEY                                
         XC    KRT2+4(21),KRT2+4                                                
         MVI   KRT2+3,X'01'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    PZEVX         DISK ERROR                                         
         BH    *+6                                                              
         DC    H'0'         AGY HEADER NOT FOUND                                
         MVC   DFSW,PRTREC+117            SRDS DEFAULT PROFILE BYTE             
*                                                                               
         MVI   KUB1+9,X'81'                                                     
         MVC   KRT2(25),TEMP       RESTORE KRT2 KEY                             
         B     PZEV2                                                            
*                                                                               
*                                                                               
HIGH     LA    R4,=C'DMRDHI'                                                    
         B     READ                                                             
SEQ      LA    R4,=C'DMRSEQ'                                                    
READ     LA    R5,=C'PUBDIR'                                                    
*****                                                                           
         ST    R6,FULL                                                          
         LA    R6,KUB1                                                          
         B     FILE                                                             
GET      LA    R4,=C'GETREC'                                                    
         LA    R5,=C'PUBFILE'                                                   
         LA    R6,PRTREC+27                                                     
FILE     GOTO1 DATAMGR,DMCB,(R4),(R5),(R6),PRTREC                               
*****                                                                           
         L     R6,FULL                                                          
         CLI   DMCB+8,0        DISK ERROR                                       
         BE    *+14                                                             
         MVC   FERN,=AL2(0)                                                     
         B     PZEVX                                                            
         BR    R8                                                               
*                                                                               
*                                                                               
PZEV2    BAS   R8,HIGH                                                          
         B     *+8                                                              
PZEV2A   BAS   R8,SEQ                                                           
         EX    R7,*+8            R7 WAS SET FOR LENGHT TO COMPARE               
         B     *+10                                                             
         CLC   PRTREC(0),KUB1                                                   
         BNE   NOTFND                                                           
         CLC   PRTREC+7(3),KUB1+7                                               
         BE    PZEV5              FOUND                                         
         CLI   DFSW,C'0'                                                        
         BE    PZEV2A                                                           
         CLC   PRTREC+7(2),=C'ZZ'                                               
         BE    PZEV5        ZZ FOUND                                            
         B     PZEV2A            GO READ SEQ                                    
*                                                                               
*                                                                               
PZEV5    BAS   R8,GET                                                           
         MVC   NAME(20),PRTREC+35                                               
         CH    R7,=H'6'                                                         
         BNE   PZEV6                                                            
         OC    KUB1+5(2),KUB1+5                                                 
         BNZ   PZEV5A                                                           
         OI    FIND,X'04'          NUM                                          
         B     PZEVX                                                            
PZEV5A   OI    FIND,X'08'         NUM,Z,E                                       
         B     PZEVX                                                            
*                                                                               
PZEV6    CH    R7,=H'4'                                                         
         BNE   PZEV6A                                                           
         OI    FIND,X'40'         NUM,ALL,ALL                                   
         MVC   RPUB+8(3),=C'ZZZ'                                                
         B     PZEVX                                                            
PZEV6A   OI    FIND,X'20'         NUM,Z,ALL                                     
         MVI   RPUB+10,C'Z'                                                     
         B     PZEVX                                                            
*                                                                               
DFSW     DC    X'00'            PAGYPROF+16     DEFAULT PUB READ BYTE           
*                                                                               
NOTFND   MVC   FERN,=AL2(PUBNOF)                                                
         B     PZEVX                                                            
*                                                                               
*                                                                               
*                                                                               
PZEINV   MVC   FERN,=AL2(PUBERR)             PUBLICATION INVALID                
PZEVX    FOUT  (R6),NAME                                                        
PZEVXX   MVC   PUBSAVE,FIND                                                     
         XIT1                                                                   
         EJECT                                                                  
*        VALIDATE START DATE AND SET FIND FORMAT BITS                           
*        X'04' =YYMM                                                            
*        X'08' =YYMMDD                                                          
*        X'10' = ES                                                             
*        X'20' = ES  +EST =ALL OR ALL+FILTERS (FOR 52/EC WITH 'BILL'            
*                    IN CONTROL DATE)                                           
*                                                                               
SEDVAL   NTR1                                START,END DATES -FIND BITS         
         MVI   ROUTSUB,1                     04=YYMM 08=YYMMDD 10=ES            
         GOTO1 AINITV                                                           
         CLI   5(R4),0                                                          
         BE    SEDVO                         START,END NOT INPUT                
         CLI   IFLDH,1                                                          
         BNE   SEDVE                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SEDV1                                                            
         CLC   IFLD(2),=C'ES'                                                   
         BNE   SEDVE                                                            
         OI    FIND,X'10'                    START = ES                         
         MVC   RSTRD(2),IFLD                                                    
         B     SEDV2                                                            
SEDV1    GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+12                                                             
         OI    FIND,X'08'                    START = YYMMDD                     
         B     SEDV2                                                            
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
         OI    FIND,X'04'                    START = YYMM                       
SEDV2    MVI   ROUTSUB,2                                                        
         GOTO1 AINITV                                                           
         CLI   IFLDH,1                                                          
         BL    SEDV3                         END NOT INPUT                      
         BH    SEDVE                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SEDV4                                                            
         CLC   IFLD(2),=C'ES'                ALLOW ES OR ES,ES                  
         BNE   SEDVE                                                            
SEDV3    TM    FIND,X'10'                                                       
         BZ    SEDVE                         END DATE MISSING                   
         TM    ESTSAVE,X'24'                                                    
         BNZ   SEDVO                         ES ONLY FOR SPECIFIC EST           
         TM    ESTSAVE,X'12'      THEN MUST BE 'ALL' OR ALL+FILTERS             
         BZ    SEDVE              ERROR                                         
         NI    FIND,X'EF'         SET OFF X'10'                                 
         OI    FIND,X'20'         ES AND EST = 'ALL' OR ALL+FILTERS             
         B     SEDVO                                                            
*                                                                               
SEDV4    TM    FIND,X'0C'                                                       
         BZ    SEDVE                                                            
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    *+16                                                             
         TM    FIND,X'08'                                                       
         BZ    SEDVE                                                            
         B     SEDV5                         STR = END = YYMMDD                 
         GOTO1 (RF),(R1),(2,IFLD)                                               
         OC    PLIST(4),PLIST                                                   
         BE    SEDVE                                                            
         TM    FIND,X'04'                                                       
         BZ    SEDVE                         STR = END = YYMM                   
SEDV5    CLC   RSTRD,RENDD                                                      
         BNH   SEDV6                                                            
         MVC   FERN,=AL2(SEDSGE)             START GT END                       
         B     SEDVO                                                            
SEDV6    CLI   ESTDATES,0                                                       
         BE    SEDVO                                                            
*                     BILLING AND PAYING DATES NEED NOT FALL IN EST PD          
*                     RTABLE IS A LIST OF REPORTS FOR WHICH                     
*                     BILLING OR PAYING DATES MAY BE USED                       
*****                                                                           
         ST    R6,FULL                                                          
         LA    R6,RTABLE                                                        
SEDV7    CLC   RNUM(2),0(R6)                                                    
         BE    SEDVO                                                            
         CLI   0(R6),X'FF'           END OF TABLE                               
         BE    SEDV7X                                                           
         LA    R6,2(R6)                                                         
         B     SEDV7                                                            
*                                                                               
SEDV7X   CLC   ESTDATES(6),RENDD                                                
         BH    *+14                                                             
         CLC   ESTDATES+6(6),RSTRD                                              
         BNL   SEDVO                                                            
         MVC   FERN,=AL2(SEDNIE)             ERROR DATES NOT WITHIN EST         
         B     SEDVO                                                            
SEDVE    MVC   FERN,=AL2(SEDINV)             DATES INVALID                      
SEDVO    EQU   *                                                                
*****                                                                           
         L     R6,FULL                                                          
         MVC   SEDSAVE,FIND                                                     
SEDVX    DS    0H                                                               
         CLI   FIND,0              IF NO INPUT                                  
         BE    SEDVXX              EXIT                                         
         CLC   FERN,=AL2(FF)       IF NO ERROR                                  
         BE    SEDVXX              EXIT                                         
         TM    RFPSTAT,RFPINUSE          SEE IF $RFP IN USE                     
         BZ    SEDVXX                                                           
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         MVI   FIND,X'01'                                                       
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   SEDVXX                                                           
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPST)    START DATE (YYMMDD) + NO END            
         BNE   *+12                                                             
         OI    FIND,X'80'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPSM)    START DATE (YYMM) + NO END              
         BNE   *+12                                                             
         OI    FIND,X'40'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPRD)    START-END (YYMMDD-YYMMDD)               
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     START400                                                         
*                                                                               
         CLC   QRFPDICT,=Y(PP#RFPRM)    START-END (YYMM-YYMM)                   
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     START400                                                         
*                                                                               
         B     SEDVXX                                                           
*                                                                               
START400 MVC   RSTRD,SPACES             STORE ESCAPE SEQ IN REQCARD             
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12                                                       
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
SEDVXX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
RTABLE   DC    C'102728363752607792ECL1LBS2XT',X'FFFF'                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE JOD CODE AND SET FIND BITS                                    
*        X'02'=ALL                                                              
*        X'04'=XXXXX                                                            
*                                                                               
JOBVAL   NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   JOBVO            JOB=ALL OR MISSING                              
         MVC   KRT2+10(6),IFLD                                                  
         OC    KRT2+7(3),KRT2+7                                                 
         BZ    JOBINV            PRODUCT NOT SPECIFIED                          
         MVI   KRT2+3,X'15'                                                     
         GOTO1 AREAD,PLIST,C'PRT2'                                              
         CLC   FERN,=AL2(FE)                                                    
         BL    JOBVO         DISK ERROR                                         
         BH    *+14                                                             
         MVC   FERN,=AL2(JOBNOF)       NOT FOUND                                
         B     JOBVO                                                            
         OI    FIND,X'04'         JOB=XXXXX                                     
         B     JOBVO                                                            
*                                                                               
JOBINV   MVC   FERN,=AL2(JOBERR)                                                
*                                                                               
JOBVO    LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(6,R7),IFLD                                                     
JOBVX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              VALIDATE ASPO                                                    
*              X'80' = ASPO=ALL OR XXXXXXXXX                                    
*                                                                               
ASPOVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BNE   ASPOVO                                                           
         CLC   IFLD(5),=C'ASPO='                                                
         BNE   EDTPUB              GO USE PUBEDIT IN PZEVAL                     
         CLI   IFLDH+5,5                                                        
         BNH   ASPOVE                                                           
         CLI   IFLDH+5,14                                                       
         BH    ASPOVE              TOO LONG                                     
         CLC   IFLD+5(3),=C'ALL'                                                
         BE    ASPOV2                                                           
         TM    CLISAVE,X'02'       CLT CAN'T BE ALL FOR ONE ASPO                
         BNZ   ASPOVE                                                           
*                                                                               
ASPOV2   DS    0H                                                               
         OI    FIND,X'80'                                                       
         B     ASPOVO                                                           
*                                                                               
ASPOVE   MVC   FERN,=AL2(FLDINV)                                                
         B     ASPOVX                                                           
*                                                                               
ASPOVO   LH    R7,COLNUM                                                        
         LA    R7,RNUM(R7)                                                      
         MVC   0(9,R7),IFLD+5                                                   
ASPOVX   XIT1                                                                   
*                                                                               
KEY      DS    CL32                                                             
KEYSAVE  DS    CL32                                                             
SVESDT   DS    CL6                 ESTIMATE DATE (YYMMDD)                       
SVLEST   DS    H                   ESTIMATE NUMBER (BINARY)                     
DMWORK   DS    12D                                                              
WORK     DS    CL17                                                             
AOFFICER DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM                            
*                                                                               
ROUTADRT DC    F'0'                          00                                 
         DC    A(CLIVAL)                     01 - CLIENT                        
         DC    A(DIVVAL)                     02 - DIVISION                      
         DC    A(PROVAL)                     03 - PRODUCT                       
         DC    A(REGVAL)                     04 - REGION                        
         DC    A(DISVAL)                     05 - DISTRICT                      
         DC    A(ESTVAL)                     06 - ESTIMATE(S)                   
         DC    A(PZEVAL)                     07 - PUBLICATION,ZONE,EDN          
         DC    A(SEDVAL)           08 - START,END DATES                         
         DC    A(JOBVAL)                09- JOB                                 
         DC    A(ASPOVAL)          10 - PUB OR ASPO                             
*                                                                               
FLDINV   EQU   2                                                                
CLINOF   EQU   40                                                               
NOSLAV   EQU   189                                                              
DIVNOF   EQU   45                                                               
DIVERR   EQU   22                                                               
PRDNOF   EQU   41                                                               
PRDERR   EQU   15                                                               
NOZZZ    EQU   178                                                              
REGNOF   EQU   46                                                               
REGERR   EQU   23                                                               
DISNOF   EQU   47                                                               
DISERR   EQU   24                                                               
ESTINV   EQU   16                                                               
EST1G2   EQU   16                                                               
ESTNOF   EQU   42                                                               
PUBNOF   EQU   44                                                               
PUBERR   EQU   18                                                               
SEDSGE   EQU   80                                                               
SEDNIE   EQU   81                                                               
SEDINV   EQU   20                                                               
JOBNOF   EQU   53                                                               
JOBERR   EQU   02                                                               
NUMINV   EQU   90         NO DETAIL BILLING FOR THIS CLT                        
NOPUB    EQU   249       CAN'T USE PUB FLD WITH CLT OVERRIDE                    
ACCERR   EQU   55                  LIMIT ACCESS ERROR                           
         EJECT                                                                  
       ++INCLUDE PRREQSAVE                                                      
       ++INCLUDE PRREQTEMP                                                      
         SPACE 2                                                                
       ++INCLUDE FLDIND                                                         
*                                                                               
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
*                                                                               
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
       ++INCLUDE PPDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PRREQ03S  05/01/02'                                      
         END                                                                    
