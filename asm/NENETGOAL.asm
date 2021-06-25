*          DATA SET NENETGOAL  AT LEVEL 122 AS OF 06/22/09                      
*PHASE T00A35A                                                                  
         TITLE 'T00A35 - GOAL READING MODULE FOR NETWORK'                       
NETGOAL  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYDLNQ,**NTGL**                                                  
         USING MYD,RC                                                           
         MVC   USERRD,4(RD)                                                     
         L     R9,0(R1)                                                         
         USING NETGOALD,R9                                                      
         ST    R9,ANGBLOCK                                                      
         L     RA,NGANTBLK                                                      
         USING NETBLOCK,RA                                                      
         ST    RA,ANBLOCK                                                       
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
MAIN10   MVI   SYSTFLAG,C'X'                                                    
*                                                                               
         SPACE 3                                                                
         ZIC   R1,NGEXTOPT         FIGURE OUT ENTRY WIDTH                       
         SLL   R1,1                                                             
****     LA    R1,8(R1)            =(N'EXTENTIONS*2)+8                          
         LA    R1,LISTDEQU(R1)                                                  
         ST    R1,ENTWID                                                        
         CLI   NGMAXPRD,0                                                       
         BNE   *+8                                                              
         MVI   NGMAXPRD,99         DEFAULT IS 99 PRODUCTS                       
         CLC   =C'ALL',NBSELCLI    IF ALL CLIENTS                               
         BNE   CLIBLDX                                                          
         XC    NBEFFCLI,NBEFFCLI   CLEAR CLIENT                                 
         BAS   RE,GETCLT           GET CLIENT REC                               
         BE    CLIBLDX                                                          
         B     XIT                                                              
CLIBLDX  EQU   *                                                                
         L     RE,NGAPLIST         CLEAR PROD LIST                              
***      LA    RF,1600                                                          
         SR    R0,R0               NEWRI20 NOW TAKES 2000                       
         ZIC   R1,NGMAXPRD                                                      
         M     R0,=F'8'                                                         
         LR    RF,R1                                                            
         XCEF                                                                   
         BAS   RE,PRDBILD                                                       
         BAS   RE,GRPBILD                                                       
         CLI   NGFUNCT,NGBUILD     OPTION JUST TO BUILD LIST                    
         BE    XIT                                                              
         BAS   RE,GOAL2                                                         
         CLC   =C'ALL',NBSELCLI     IF ALL CLIENTS                              
         BNE   XIT                                                              
         BAS   RE,GETCLT                                                        
         BE    CLIBLDX             GET NEXT CLIENT                              
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
GETCLT   NTR1                                                                   
*                                                                               
NXTCLI   LH    R1,NBEFFCLI         PROCESS CLIENT = ALL                         
         LA    R1,1(R1)            NBEFFCLI IS CURRENT CLIENT                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING CLTHDR,R4                                                        
         MVC   CKEYAM,NBACTAM                                                   
         STCM  R1,3,CKEYCLT                                                     
         BRAS  RE,HIGH             GET NEXT CLIENT RECORD                       
         CLC   KEY(2),KEYSAVE                                                   
         BNE   CLIPROF                                                          
         XC    NBUSER,NBUSER       ENSURE PROFILES AGAIN                        
         MVC   NBEFFCLI,CKEYCLT                                                 
         MVC   NBACTCLI,NBEFFCLI                                                
         MVC   FULL,NBAIO                                                       
         MVC   NBAIO,NBACLI        READ CLIENT REC INTO NBACLI                  
         BRAS  RE,GETREC           GET CLIENT RECORD                            
         MVC   NBAIO,FULL          RESTORE NBAIO                                
         L     R4,NBACLI                                                        
* - CHECK CLIENT GROUP FILTERING                                                
         CLI   NBSELCGR,0                                                       
         BE    VCL9                                                             
         LA    R0,5                                                             
         LA    RF,CGRP1                                                         
         CLC   NBSELCGR(1),0(RF)       CHECK SCHEME LETTER                      
         BE    VCL5                                                             
VCL4     LA    RF,3(RF)                                                         
         BCT   R0,*-14                                                          
         B     NXTCLI                                                           
VCL5     UNPK  DUB(5),1(3,RF)      UNPK PWOS                                    
         LA    R3,DUB                                                           
         LA    RE,NBSELCGR+1                                                    
         LA    R1,4                                                             
VCL6     CLI   0(RE),X'C1'         IF LETTER OR NUMBER                          
         BL    VCL7                                                             
         CLC   0(1,RE),0(R3)       MUST MATCH                                   
         BNE   VCL4                IF NO MATCH,TEST AGAINST NXT CGRP            
VCL7     LA    RE,1(RE)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,VCL6                                                          
         MVC   NBACTCGR(2),1(RF)   SET CLIENT GROUP CODE  PWOS                  
         MVC   NBEFFOFF,COFFICE                                                 
         SPACE 1                                                                
* ENSURE THAT EACH CLIENT IS AUTHORIZED FOR AGENCY WHEN NBSELCLI=ALL            
VCL9     DS    0H                                                               
         OC    NBTWAACC(2),NBTWAACC  LOCKOUT CHECKS                             
         BZ    VCL10                                                            
         CLC   =C'ALL',NBSELCLI                                                 
         BNE   VCL10                                                            
         TM    NBUNTSW,X'20'      SKIP OFFICE CHECKS                            
         BO    VCL10                                                            
         MVC   NBEFFOFF,COFFICE                                                 
         CLI   NBTWAACC,C'*'        IF *                                        
         BNE   VCL9B                                                            
*                                   IF *AN THEN CLIENT GROUP LIMIT              
*                                   ACCESS AND NOT OFFICE LIMIT                 
                                                                                
         CLI   NBTWAACC+1,C'A'        CHECK IF ALPHA                            
         BL    VCL9A                                                            
         CLI   NBTWAACC+1,C'Z'                                                  
         BH    VCL9A                                                            
         CLI   NBTWAACC+2,C'0'         CHECK IF NUMERIC                         
         BL    VCL9A                                                            
         CLI   NBTWAACC+2,C'9'                                                  
         BH    VCL9A                                                            
         B     VCL10                PASSED TESTS SO ITS CLIENT GROUP            
*                                   LIMIT ACCESS / HANDLED IN NEMEDGEN          
                                                                                
*                                                                               
VCL9A    MVC   NBSELOFF,NBTWAACC+1     SET OFFICE                               
         B     VCL10                                                            
*                                                                               
VCL9B    CLI   NBTWAACC,C'$'        IF $                                        
         BNE   VCL9D                                                            
         MVC   NBSELOFF,NBTWAACC+1                                              
         OI    NBOFFTYP,X'80'                                                   
         B     VCL10                                                            
*                                                                               
VCL9D    CLI   NBTWAACC,C'-'        IF NOT * OR - THEN CLIENT LOCKOUT           
         BE    VCL9F                                                            
         CLC   NBACTCLI,NBTWAACC                                                
         BNE   NXTCLI                                                           
         B     VCL10                                                            
*                                                                               
VCL9F    CLI   NBTWAACC+1,C'*'      -* MEANS ALL BUT THIS OFFICE                
         BNE   VCL9H                                                            
         CLC   NBTWAACC+2(1),NBEFFOFF                                           
         BE    NXTCLI                                                           
         B     VCL10                                                            
*                                                                               
VCL9H    CLC   NBTWAACC+1(2),NBACTCLI   ALL BUT THIS CLIENT                     
         BE    NXTCLI                                                           
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
VCL10    TM    NBUNTSW,X'20'       SKIP OFFICE CHECKS                           
         BO    PROCCLI             YES                                          
         MVC   NBEFFOFF,COFFICE    NOTE OFFICE                                  
         CLI   NBSELOFF,0          IF OFFICE FILTERING                          
         BE    PROCCLI                                                          
         TM    NBOFFTYP,X'80'      CHECK OFFICE LIST                            
         BO    OFFLIST                                                          
         TM    NBOFFTYP,X'40'      CHECK NEGATIVE FILTER                        
         BO    NEGOFF                                                           
         CLC   NBEFFOFF,NBSELOFF                                                
         BNE   NXTCLI                                                           
         B     PROCCLI                                                          
         SPACE 1                                                                
OFFLIST  XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 NBCALLOV,DMCB                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         XC    DUB(8),DUB                                                       
***-->   MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVI   OFCAUTH,C'$'                                                     
         MVC   OFCAUTH+1(1),NBSELOFF                                            
         OI    OFCAUTH+1,X'C0'                                                  
         MVC   OFCAGY,NBEFFAGY                                                  
         MVC   OFCOFC,NBEFFOFF                                                  
         DROP  R1                                                               
         SPACE 1                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,NBACOM                                             
         TM    NBOFFTYP,X'40'      MAY BE NEGATIVE LIST                         
         BO    NEGLIST                                                          
         CLI   0(R1),0             IS OFFICE IN LIST                            
         BNE   NXTCLI                                                           
         B     PROCCLI                                                          
         SPACE 1                                                                
NEGLIST  CLI   0(R1),0             CHECK FOR NEGATIVE LIST                      
         BE    NXTCLI                                                           
         B     PROCCLI                                                          
         SPACE 1                                                                
NEGOFF   CLC   NBEFFOFF,NBSELOFF                                                
         BE    NXTCLI                                                           
         B     PROCCLI                                                          
*                                                                               
PROCCLI  GOTO1 NBCLUNPK,DMCB,(CPROF+6,NBACTCLI),NBCLICOD                        
         SR    RE,RE                                                            
         LTR   RE,RE      SET CC= IF OK                                         
*                                                                               
*                                  GET USER PROFILES                            
CLIPROF  DS    0H          ---->   ASSUME WE DON'T NEED USER PROFS              
         B     XIT                 FOR GOALS                                    
         EJECT                                                                  
*              ROUTINE BUILDS LIST OF BRANDS WITH MATCHING ESTIMATE             
         SPACE 3                                                                
PRDBILD  NTR1                                                                   
         USING LISTD,R3                                                         
         L     R3,NGAPLIST                                                      
         LA    R4,KEY                                                           
         SR    R5,R5                                                            
         USING ESTHDR,R4                                                        
         XC    EKEY,EKEY                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
PB2      LA    R4,KEY                                                           
         MVC   KEY,KEYSAVE                                                      
         ZIC   R1,EKEYPRD+2                                                     
         LA    R1,1(R1)                                                         
         STC   R1,EKEYPRD+2                                                     
         XC    EKEYEST(6),EKEYEST                                               
         BRAS  RE,HIGH             BUMP TO NEXT PRODUCT                         
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PBEND                                                            
         MVC   KEYSAVE,KEY                                                      
         MVC   LISTPRD,EKEYPRD                                                  
         L     R2,NBACLI                                                        
         LTR   R2,R2                                                            
         BNZ   PB3                                                              
PB2C     BRAS  RE,GETREC           USER DID NOT PROVIDE CLIENT RECORD           
         L     R4,NBAIO            SO WE HAVE TO READ PRODUCT RECORD            
         USING PRDHDR,R4                                                        
         MVC   LISTNO,PCODE+1      TO FIND THE PRODUCT NUMBER                   
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         B     PB6                                                              
         SPACE 1                                                                
PB3      L     R2,NBACLI                                                        
         USING CLTHDR,R2           POINT R2 TO CLIST                            
         LA    R2,CLIST                                                         
         LA    R0,220                                                           
         SPACE 1                                                                
PB3A     MVC   LISTNO,3(R2)                                                     
         CLC   LISTPRD,0(R2)       LOOK UP NUMBER                               
         BE    PB6                                                              
         LA    R2,4(R2)                                                         
         BCT   R0,PB3A                                                          
*                                                                               
         L     R2,NBACLI                                                        
         LA    R2,CLIST2                                                        
         LA    R0,32                                                            
         SPACE 1                                                                
PB4A     MVC   LISTNO,3(R2)                                                     
         CLC   LISTPRD,0(R2)       LOOK UP NUMBER                               
         BE    PB6                                                              
         LA    R2,4(R2)                                                         
         BCT   R0,PB4A                                                          
***      DC    H'0'                ,,INSTEAD OF THIS                            
         B     PB2C                ,,LET'S TRY PRODUCT REC                      
         DROP  R2                                                               
*                                  STUPID IDEA - MEANS PROD REC READ            
*                                  FOR EVERY UNIT - NEED TO CHANGE              
*                                  NETGOAL TO HANDLE CLIENT GROUPS              
         SPACE 2                                                                
PB6      CLI   LISTNO,255          TAKE POOL                                    
         BE    PB7                                                              
***      ZIC   R1,LISTNO           DO WE WANT THIS PRODUCT                      
***      LA    R2,NBPRDMSK                                                      
***      BRAS  RE,TESTMASK                                                      
         CLI   NBSELPGR,0          PRODUCT GROUPS?                              
         BE    PB6D                                                             
         XC    WORK(4),WORK                                                     
         MVC   WORK(1),LISTNO                                                   
         MVC   WORK+1(3),LISTPRD                                                
         BAS   RE,TSTOPMSK                                                      
         BNE   PB2                                                              
         B     PB7                                                              
*                                                                               
PB6D     CLI   NBPRDALL,X'FF'      ALL PRODS?                                   
         BE    PB7                                                              
         CLC   NBSELPRD,LISTPRD                                                 
         BNE   PB2                                                              
         SPACE 1                                                                
PB7      MVC   EKEYEST,NBSELEST    SET UP FOR ESTIMATE                          
         XC    EKEYEST+1(5),EKEYEST+1                                           
         CLI   EKEYEST,0                                                        
         BNE   *+8                                                              
         MVI   EKEYEST,1                                                        
         SPACE 1                                                                
PB8      BRAS  RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PBEND                                                            
         CLC   KEY(7),KEYSAVE                                                   
         BNE   PB2                                                              
         BRAS  RE,GETREC                                                        
         L     R4,NBAIO                                                         
         BRAS  RE,CHKFILT                                                       
         BNE   PB10                                                             
         CLC   EEND,NBSELSTR       ESTIMATE MUST FIT PERIOD                     
         BL    PB10                                                             
         CLC   ESTART,NBSELEND                                                  
         BH    PB10                                                             
         MVC   LISTTARG,EDEMOS+2                                                
**       BAS   RE,ADDEXT           OPTIONAL EXTENSIONS                          
         BRAS  RE,ADDEXT           OPTIONAL EXTENSIONS                          
         CLI   LISTNO,X'FF'                                                     
         BNE   *+8                                                              
         MVI   LISTNO,0                                                         
         LA    R5,1(R5)                                                         
         STC   R5,LISTUSER                                                      
         ST    R5,NGNPRDS                                                       
         A     R3,ENTWID                                                        
         CLC   NGNPRDS+3(1),NGMAXPRD                                            
         BL    PB2                                                              
         B     PBEND                                                            
         SPACE 1                                                                
PB10     LA    R4,KEY                                                           
         CLI   NBSELEST,0          IF FOR 1 EST DONT TRY OTHERS                 
         BE    PB12                                                             
         CLI   NBSELESE,0          IS IT FOR RANGE                              
         BE    PB2                                                              
PB12     CLI   EKEYEST,255         IF EST=255 THIS IS LAST                      
         BE    PB2                                                              
         AI    EKEYEST,1                                                        
         CLI   NBSELESE,0          IF FOR RANGE                                 
         BE    PB14                                                             
         CLC   NBSELESE,EKEYEST    CHECK/IS IT OVER RANGE                       
         BL    PB2                                                              
PB14     XC    EKEYEST+1(5),EKEYEST+1                                           
         B     PB8                 GO BACK FOR NEXT ESTIMATE                    
         SPACE 1                                                                
PBEND    GOTO1 NBCALLOV,DMCB,0,X'D9000A12'                                      
         L     RF,DMCB             PICK UP A(XSORT)                             
         L     R2,NGAPLIST                                                      
         MVC   DMCB+4(4),NGNPRDS   PASS XSORT N'PRODUCTS                        
         MVC   DMCB+8(4),ENTWID    AND WIDTH OF EACH ENTRY                      
         GOTO1 (RF),DMCB,(R2),,,3,1                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO PUT PRODUCT GROUPS INTO LIST                          
         SPACE 3                                                                
GRPBILD  NTR1                                                                   
         CLI   NGSELSCH,0                                                       
         BE    XIT                                                              
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING PRGKEY,R4                                                        
         MVC   PRGPTYP(2),=X'0D81'                                              
         MVC   PRGPAGMD(3),NBACTAM                                              
         MVC   PRGPID,NGSELSCH                                                  
         BRAS  RE,HIGH                                                          
         B     GB4                                                              
GB2      BRAS  RE,SEQ                                                           
         SPACE 2                                                                
GB4      CLC   KEY(8),KEYSAVE      BROWSE THRU DIRECTORY                        
         BNE   GB10                                                             
         L     R3,NGAPLIST                                                      
         USING LISTD,R3                                                         
         L     R4,NGNPRDS                                                       
         SPACE 2                                                                
GB6      CLC   LISTPRD,PRGPPRD     LOOK FOR A MATCH IN PRODUCT LIST             
         BE    GB8                                                              
         A     R3,ENTWID                                                        
         BCT   R4,GB6                                                           
         B     GB2                                                              
         SPACE 2                                                                
GB8      MVC   LISTDIV,PRGPGRP     AND POP IN GROUP CODE                        
         B     GB2                                                              
         SPACE 2                                                                
GB10     L     R3,NGAPLIST                                                      
         L     R4,NGNPRDS                                                       
         SPACE 2                                                                
GB12     OC    LISTDIV,LISTDIV     NOW ENSURE EACH PRODUCT HAS A GROUP          
         BNZ   *+10                                                             
         MVC   LISTDIV,=X'9990'    IF NOT, PUT INTO 999                         
         A     R3,ENTWID                                                        
         BCT   R4,GB12                                                          
         LA    R4,KEY                                                           
         XC    KEY,KEY             FINALLY READ SCHEME RECORD                   
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD(3),NBACTAM                                              
         MVC   PRGKID,NGSELSCH                                                  
         BRAS  RE,READ                                                          
         BRAS  RE,GETREC                                                        
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING PRGEL01,R6                                                       
         MVC   NGOALBK1,PRGBK1     SAVE BREAK NAMES/LENGTHS                     
         MVC   NGOALBK2,PRGBK2                                                  
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL READING OF GOALS                                         
         SPACE 3                                                                
GOAL2    NTR1                                                                   
         L     R3,NGAPLIST                                                      
         USING LISTD,R3                                                         
         L     R0,NGNPRDS                                                       
         LTR   R0,R0               TEST NGNPRDS                                 
         BZ    XIT                                                              
         CLI   NBSELDP,0                                                        
         BE    GOAL4                                                            
         CLI   NGSELDP,0                                                        
         BNE   GOAL4                                                            
         MVC   NGSELDP,NBSELDP                                                  
         SPACE 1                                                                
GOAL4    BAS   RE,GOAL6            HANDLE A PRODUCT                             
         A     R3,ENTWID                                                        
         BCT   R0,GOAL4                                                         
         MVI   NGOALIOS,0                                                       
GOALXX   B     XIT                                                              
         SPACE 1                                                                
         USING LISTD,R3                                                         
GOAL6    NTR1                                                                   
         OC    LISTPRD,LISTPRD                                                  
         BZ    XIT                                                              
*                                                                               
         CLI   SYSTFLAG,C'X'                                                    
         BNE   *+12                                                             
         BRAS  RE,XGOAL                                                         
         B     XIT                                                              
*                                                                               
*!!!     CLI   LISTNO,0                                                         
*!!!     BE    XIT                                                              
         MVC   NGOALPRD,LISTPRD    PASS BACK PRODUCT DETAILS                    
         MVC   NGOALPNO,LISTNO                                                  
         MVC   NGOALPGR,LISTDIV                                                 
         MVC   NGOALTRG,LISTTARG                                                
         MVC   NGOALTR2,LISTDEMO+3       TARGET 2 DEMO                          
         MVC   NGOALTR3,LISTDEMO+5       TARGET 3 DEMO                          
         MVC   NGOALUSR,LISTUSER                                                
*&&DO                                                                           
         NI    TRGFLAG,X'FF'-TRGSAME                                            
         TM    LISTFLAG,LISTTGSM                                                
         BZ    *+8                                                              
         OI    TRGFLAG,TRGSAME                                                  
*&&                                                                             
         LA    R4,KEY              BUILD KEY FOR THIS PRODUCT                   
         XC    KEY,KEY                                                          
         USING GOALREC,R4                                                       
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM(3),NBACTAM                                                
         MVC   GKEYPRD,LISTNO                                                   
         MVC   GKEYMKT,NGSELMKT                                                 
         MVC   GKEYEST,NGSELEST                                                 
         MVC   GKEYDPT,NGSELDP                                                  
         BRAS  RE,HIGH                                                          
         B     GOAL10                                                           
         SPACE 2                                                                
GOAL8    LA    R4,KEY                                                           
         CLI   NGOALIOS,2          (2=SEQUENCE HAS BEEN DISTURBED)              
         BNE   *+8                                                              
         BRAS  RE,HIGH             (SO - RESTORE SEQUENCE)                      
         BRAS  RE,SEQ                                                           
         SPACE 2                                                                
GOAL10   MVI   NGOALIOS,1          (1=ACTIVELY READING GOALS)                   
         CLC   KEY(5),KEYSAVE      CHECK MATCH ON BRAND                         
         BNE   XIT                                                              
         OC    NGSELMKT,NGSELMKT                                                
         BZ    GOAL12                                                           
         CLC   KEY(7),KEYSAVE      OR MARKET IF SPECIFIED                       
         BNE   XIT                                                              
         SPACE 1                                                                
GOAL12   ZIC   R1,GKEYEST          CHECK ESTIMATE MATCH                         
         LA    R2,NBESTMSK                                                      
         BRAS  RE,TESTMASK                                                      
         BNE   GOAL8                                                            
         CLI   NGSELDP,0           CHECK DP FILTER                              
         BE    GOAL14                                                           
         CLC   NGSELDP,GKEYDPT                                                  
         BNE   GOAL8                                                            
         SPACE 1                                                                
GOAL14   CLI   NGSELSL,0           CHECK SL FILTER                              
         BE    GOAL16                                                           
         CLC   NGSELSL,GKEYSLN                                                  
         BNE   GOAL8                                                            
         SPACE 1                                                                
GOAL16   CLI   NGSELPKG,0          CHECK PACKAGE FILTER                         
         BE    GOAL18                                                           
         TM    GKEYAGY,X'40'       CHECK FOR PACKAGE IN KEY BIT                 
         BZ    GOAL8                                                            
         CLC   NGSELPKG,GKEYPRD2                                                
         BNE   GOAL8                                                            
         SPACE 1                                                                
GOAL18   BAS   RE,GOAL20                                                        
         B     GOAL8                                                            
         EJECT                                                                  
*              CONTROL PROCESSING OF GOALS                                      
         SPACE 3                                                                
GOAL20   NTR1                                                                   
         MVI   NBSTATYP,C'N'                                                    
         CLC   GKEYMKT,=X'0309'    NETWORK GOALS?                               
         BE    GOAL20A                                                          
*                                                                               
         MVI   NBSTATYP,C'S'                                                    
         CLC   GKEYMKT,=X'0306'    SYNDICATION GOALS?                           
         BE    GOAL20A                                                          
*                                                                               
         MVI   NBSTATYP,C'C'                                                    
         CLC   GKEYMKT,=X'0307'    CABLE GOALS?                                 
         BE    GOAL20A                                                          
         MVI   NBSTATYP,C'N'                                                    
*                                                                               
GOAL20A  MVC   NGOALEST,GKEYEST    SOME VALUES FROM THE KEY                     
         MVC   NGOALMKT,GKEYMKT                                                 
         MVC   NGOALDP,GKEYDPT                                                  
         XC    NBDPNAM,NBDPNAM         MATCH NETVALUE+SPEED                     
         XC    NBDPNAM2,NBDPNAM2                                                
         GOTOR PROCDP,DMCB,NBDPNAM,NGOALDP                                      
*!!!     EXPDP NBDPNAM,NGOALDP                                                  
         MVI   NGOALDP2+1,X'40'                                                 
         MVC   NGOALDP2(1),NGOALDP                                              
         CLC   =C'UNKNOWN',NBDPNAM                                              
         BNE   GOAL20X                                                          
* - GET 2 CHAR DAYPART                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),=XL2'0D07'                                               
         MVC   WORK+2(1),NBACTAM                                                
         CLI   NGOALDP,127            CHECK CLIENT LEVEL DAYPART                
         BH    *+10                                                             
         MVC   WORK+3(2),NBACTCLI                                               
         MVC   WORK+5(1),NGOALDP                                                
         MVC   WORK+50(10),WORK                                                 
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'UNTDIR ',WORK,WORK,0                     
         CLC   WORK(6),WORK+50                                                  
         BE    *+6                                                              
         DC    H'0'                NEW DPT CRASH!                               
         LA    R1,WORK                                                          
         USING NDPTHDR,R1                                                       
         MVC   NGOALDP2,NDPTDPTA    PASS BACK 2 CHAR DAYPART                    
* MOVE DESCRIPTION TO NETBLOCK HERE AND NOT IN EXDP                             
* SINCE I DON'T WANT TO USE REST OF SPARE IN NETGOALD                           
         MVC   NBDPNAM,NDPTDES      DESCRIPTION                                 
         MVC   NBDPNAM2,NDPTDES+8   REST OF DESCRIPTION                         
         BRAS  RE,HIGH             RESTORE GOAL SEQUENCE                        
         DROP  R1                                                               
*                                                                               
GOAL20X  MVC   NGOALSL,GKEYSLN                                                  
         MVI   NGOALPAK,0                                                       
         TM    GKEYAGY,X'40'       IS SECOND PRODUCT A GOAL                     
         BZ    *+10                                                             
         MVC   NGOALPAK,GKEYPRD2                                                
         BRAS  RE,GETREC                                                        
*                                                                               
* CHECK FOR GOAL HISTORY RECORD                                                 
*                                                                               
         TM    GKEYAGY,X'20'                                                    
         BNO   ENDHIST                                                          
         TM    NBINDS5,NBI5NOGL    DON'T READ HISTORY                           
         BO    ENDHIST                                                          
* SET UP NETBLOCK FOR GOAL HISTORY RECORDS                                      
         MVC   NBSPLPRN,GKEYPRD    PRODUCT                                      
         MVC   NBPRD,GKEYPRD                                                    
         MVI   NBPRD2,0                                                         
         MVC   NBMARKET,GKEYMKT    MARKET                                       
         MVC   NBACTEST,GKEYEST    ESTIMATE                                     
         MVC   NBACTDP,GKEYDPT     DAYPART                                      
         MVI   NBPACK,0            (PACKAGE)                                    
         MVC   NBLEN,GKEYSLN       SPOT LENGTH                                  
*                                                                               
         XC    NBACTPRG,NBACTPRG   CLEAR PROGRAM                                
         XC    NBPROGNM,NBPROGNM                                                
         XC    NBACTDAT,NBACTDAT   CLEAR DATE                                   
         MVI   NBDAY,0             CLEAR DAY                                    
         XC    NBTIME,NBTIME       (TIME)                                       
         MVI   NBUNITST,0                                                       
         DROP  R4                                                               
*                                                                               
         BRAS  RE,GOHOOK                                                        
         B     XIT                                                              
ENDHIST  DS    0H                                                               
         NI    TRGFLAG,X'FF'-TRGSAME                                            
*                                                                               
         CLC   =C'SJ',NBSELAGY          ***IF SJR                               
         BE    EH02                                                             
         CLC   =C'*B',NBSELAGY          ***IF DDSB                              
         BE    EH02                                                             
         CLC   =C'MC',NBSELAGY          ***IF MCCANN                            
         BNE   GOAL21                                                           
*                                                                               
EH02     L     RF,NBAIO                                                         
         MVC   SVGKEY,0(RF)             SAVE AWAY GOAL KEY                      
         USING GOALREC,RF                                                       
*                                                                               
         MVC   BYTE,GKEYPRD                                                     
         BRAS  RE,GPRDA                 GET PRODUCT ALPHA                       
*                                                                               
         L     RF,NBAIO                                                         
         XC    KEY,KEY                  GET CORRECT EST RECORD TO CHECK         
         LA    R4,KEY                   TARGET DEMOS                            
         USING ESTHDR,R4                                                        
*                                                                               
         MVC   EKEYAM,GKEYAM            AGY/MED                                 
         MVC   EKEYCLT,GKEYCLT          CLIENT                                  
         MVC   EKEYPRD,PRDA             PRODUCT                                 
         MVC   EKEYEST,GKEYEST          ESTIMATE                                
         DROP  RF                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EH10                                                             
         BRAS  RE,GETREC                                                        
*                                                                               
         L     R4,NBAIO                                                         
         MVC   NGOALTRG,EDEMOS+2                                                
         MVC   NGOALTR2,EDEMLST+5                                               
         MVC   NGOALTR3,EDEMLST+8                                               
*                                                                               
         OC    ETRGLST,ETRGLST          ANY TARGETS?                            
         BZ    EH10                                                             
         MVC   NGOALTRG,ETRGLST+2                                               
         MVC   NGOALTR2,ETRGLST+5                                               
         OC    ETRGLST+3(3),ETRGLST+3   IS THERE A 2ND TARGET?                  
         BZ    EH10                                                             
         CLC   ETRGLST(3),ETRGLST+3                                             
         BNE   *+8                                                              
         OI    TRGFLAG,TRGSAME          1ST AND 2ND TARGET ARE SAME             
         DROP  R4                                                               
*                                                                               
EH10     DS    0H                                                               
         XC    KEY,KEY                  RESTORE GOAL RECORD                     
         MVC   KEY(13),SVGKEY                                                   
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         BRAS  RE,GETREC                                                        
*                                                                               
*--GET STATION NAME                                                             
GOAL21   DS    0H                                                               
         XC    NGOALNET,NGOALNET                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                20 ELEMENT IS REQUIRED                       
         USING GDELEM,R6                                                        
         CLI   GDNETWK,X'40'                                                    
         BNH   *+10                                                             
         MVC   NGOALNET,GDNETWK                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         B     GOAL24                                                           
         SPACE 2                                                                
GOAL22   BAS   RE,NEXTEL                                                        
         SPACE 2                                                                
GOAL24   BNE   XIT                                                              
         USING GLEMENT,R6                                                       
         SPACE                                                                  
         OC    NBCMPSTR,NBCMPSTR   TEST GOALWEEK WITHIN NBSTRT/END              
         BZ    GOAL24A                                                          
         CLC   GLWEEK,NBCMPSTR                                                  
         BL    GOAL22                                                           
GOAL24A  OC    NBCMPEND,NBCMPEND                                                
         BZ    GOAL24B                                                          
         CLC   GLWEEK,NBCMPEND                                                  
         BH    GOAL22                                                           
         SPACE                                                                  
GOAL24B  MVC   NGOALWK,GLWEEK                                                   
         XC    NGOALDOL,NGOALDOL                                                
*                                                                               
         TM    NBINDS5,NBI5MTN                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'N'                                                    
         BNE   GOAL24B9                                                         
*                                                                               
         TM    NBINDS5,NBI5MTS                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'S'                                                    
         BNE   GOAL24B9                                                         
*                                                                               
         TM    NBINDS5,NBI5MTC                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'C'                                                    
         BNE   GOAL24B9                                                         
*                                                                               
         L     R0,GLBUDGET         CONVERT TO DOLLARS                           
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,NGOALDOL                                                      
GOAL24B9 XC    NGOALG3,NGOALG3     CLEAR/NOT ALWAYS 3                           
         XC    NGOALID,NGOALID     CLEAR ID FIELD                               
         MVC   NGOALGRP,GLGRP      POINTS FOR TARGET 1                          
         CLI   GLEN,16                                                          
         BL    GOAL24C                                                          
*                                                                               
         MVC   NGOALG2,GLGRP2             AND TARGET 2                          
         CLC   =C'SJ',NBSELAGY          ***IF SJR                               
         BE    *+14                                                             
         CLC   =C'MC',NBSELAGY          ***IF MCCANN                            
         BNE   GOAL24C                                                          
*                                                                               
         TM    TRGFLAG,TRGSAME          1ST AND 2ND TARGETS SAME?               
         BZ    GOAL24C                                                          
         MVC   NGOALG2,GLGRP              SAME AS TARGET 1                      
*                                                                               
GOAL24C  CLI   GLEN,21                                                          
         BL    *+10                                                             
         MVC   NGOALG3,GLGRP3             AND TARGET 3                          
         CLI   GLEN,33                                                          
         BL    *+18                                                             
         CLI   GLOTTYP,C'W'                                                     
         BNE   *+10                                                             
         MVC   NGOALID,GLOTFLD    GOAL IDENTIFIER                               
         TM    NBINDS3,NBI3A2DC    IF 2 DEC AGENCY                              
         BO    GOAL24D                                                          
         CLI   NBPREOPT,C'Y'                                                    
         BNE   GOAL25                                                           
GOAL24D  L     R0,NGOALGRP         PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALGRP                                                      
*                                                                               
         L     R0,NGOALG2          PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALG2                                                       
*                                                                               
         L     R0,NGOALG3          PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALG3                                                       
GOAL25   L     R1,NGAWLIST         LOOK UP WEEK (OPTIONAL)                      
         ZIC   R0,NGNWEEKS                                                      
         LA    RF,1                                                             
         LTR   R1,R1                                                            
         BZ    GOALGO                                                           
         SPACE 1                                                                
GOAL26   CLC   GLWEEK,0(R1)        DOES GOAL FIT IN PERIOD                      
         BL    GOAL27                                                           
         CLC   GLWEEK,2(R1)                                                     
         BNH   GOAL28                                                           
         SPACE 1                                                                
GOAL27   LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GOAL26                                                        
         B     GOAL22                                                           
         SPACE 1                                                                
GOAL28   STC   RF,NGOALWKN                                                      
         SPACE 1                                                                
GOALGO   BRAS  RE,EXNB             OPTIONAL NETBLOCK EXPANSION                  
         BRAS  RE,GOHOOK           PASS WEEK DATA BACK TO USER                  
         B     GOAL22                                                           
         EJECT                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TEST MASK                                             
         SPACE 3                                                                
         SPACE 1                                                                
RELO     DS    A                                                                
         GETEL (R6),NBDTADSP,ELCODE                                             
*                                                                               
*                                                                               
* TEST PROD CODE AGAINST OP PROD AREA                                           
* INPUT  WORK BYTE 1 = BINARY CODE                                              
* INPUT  WORK CL3 = 3 CHAR PROD CODE                                            
* PROD TABEL = (1 BINARY + 3 CHAR) X 200 PRODS                                  
TSTOPMSK NTR1                                                                   
         ICM   R2,15,NBADRPRG         ADDRESS OF PROGR GROUP?                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,500              HARD CODED FOR 500 OVERFLOW PRDMSK           
TS20     CLC   1(3,R2),WORK+1                                                   
         BE    TSX                                                              
         CLI   WORK+1,0            ARE WE DEALING WITH BINARY PROD              
         BNE   TS22                NO                                           
         CLC   0(1,R2),WORK        YES                                          
         BE    TSX                                                              
TS22     LA    R2,4(R2)                                                         
         BCT   RE,TS20                                                          
         LTR   R2,R2               SET CC -> NOT EQUAL                          
TSX      XIT1                                                                   
*                                                                               
         LTORG                                                                  
*****************************************************************               
*              ROUTINE TO ADD DEMO EXTENSIONS TO LIST                           
         SPACE 3                                                                
ADDEXT   NTR1  BASE=*,LABEL=*                                                   
         USING LISTD,R3                                                         
         USING ESTHDR,R4                                                        
         CLI   NGEXTOPT,0                                                       
         BE    ADDEXT3                                                          
         LA    R2,EDEMLST                                                       
         LA    R5,LISTDEMO                                                      
         ZIC   R0,NGEXTOPT                                                      
*                                                                               
ADDEXT2  MVC   0(2,R5),1(R2)                                                    
         LA    R2,3(R2)                                                         
         LA    R5,2(R5)                                                         
         BCT   R0,ADDEXT2                                                       
*                                                                               
ADDEXT3  DS    0H                                                               
         CLC   =C'SJ',NBSELAGY          ***IF SJR                               
         BE    *+14                                                             
         CLC   =C'MC',NBSELAGY          ***IF MCCANN                            
         BNE   ADDEXTX                                                          
*                                                                               
         OC    ETRGLST,ETRGLST          ***AND IF ETRGLST                       
         BZ    ADDEXTX                                                          
         MVC   LISTTARG,ETRGLST+2                                               
         LA    R5,LISTDEMO                                                      
         MVC   0(2,R5),ETRGLST+1        SET THESE TO LISTDEMO                   
         MVC   2(2,R5),ETRGLST+4                                                
         OC    ETRGLST+3(3),ETRGLST+3   IS THERE A 2ND TARGET?                  
         BZ    ADDEXTX                                                          
         CLC   ETRGLST(3),ETRGLST+3                                             
         BNE   *+8                                                              
         OI    LISTFLAG,LISTTGSM        1ST AND 2ND TARGET ARE SAME             
ADDEXTX  XIT1                                                                   
                                                                                
         EJECT                                                                  
                                                                                
GPRDA    NTR1  BASE=*,LABEL=*                                                   
GP1      L     R2,NBACLI                                                        
         USING CLTHDR,R2           POINT R2 TO CLIST                            
         LA    R2,CLIST                                                         
         LA    R0,220                                                           
*                                                                               
GP2      DS    0H                                                               
         CLC   BYTE,3(R2)          LOOK UP NUMBER                               
         BE    GP5                                                              
         LA    R2,4(R2)                                                         
         BCT   R0,GP2                                                           
*                                                                               
         L     R2,NBACLI                                                        
         LA    R2,CLIST2                                                        
         LA    R0,32                                                            
*                                                                               
GP3      DS    0H                                                               
         CLC   BYTE,3(R2)          LOOK UP NUMBER                               
         BE    GP5                                                              
         LA    R2,4(R2)                                                         
         BCT   R0,GP3                                                           
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
GP5      DS    0H                                                               
         MVC   PRDA,0(R2)                                                       
         OC    PRDA,=C'   '                                                     
*                                                                               
GPRDAX   DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
*****************************************************************               
XGOAL    NTR1  BASE=*,LABEL=*                                                   
         USING LISTD,R3                                                         
         L     R9,ANGBLOCK                                                      
         L     RA,ANBLOCK                                                       
*                                                                               
         MVC   NGOALPRD,LISTPRD    PASS BACK PRODUCT DETAILS                    
         MVC   NGOALPNO,LISTNO                                                  
         MVC   NGOALPGR,LISTDIV                                                 
         MVC   NGOALTRG,LISTTARG                                                
         MVC   NGOALTR2,LISTDEMO+3       TARGET 2 DEMO                          
         MVC   NGOALTR3,LISTDEMO+5       TARGET 3 DEMO                          
         MVC   NGOALUSR,LISTUSER                                                
*                                                                               
         LA    R4,KEY              BUILD KEY FOR THIS PRODUCT                   
         XC    KEY,KEY                                                          
         USING NGOLXD,R4                                                        
*                                                                               
         MVI   GXKEYTYP,X'02'                                                   
         MVC   GXKEYAM(3),NBACTAM                                               
         MVC   GXKEYPRD,LISTNO                                                  
         MVC   GXKEYMKT,NGSELMKT                                                
         MVC   GXKEYEST,NGSELEST                                                
         MVC   GXKEYDPT,NGSELDP                                                 
         BAS   RE,XHIGH                                                         
         B     XGOAL10                                                          
*                                                                               
XGOAL8   LA    R4,KEY                                                           
         CLI   NGOALIOS,2          (2=SEQUENCE HAS BEEN DISTURBED)              
         BNE   *+8                                                              
         BAS   RE,XHIGH            (SO - RESTORE SEQUENCE)                      
         BAS   RE,XSEQ                                                          
         L     R1,NBAIO                                                         
         MVC   KEY(32),0(R1)                                                    
*                                                                               
XGOAL10  MVI   NGOALIOS,1          (1=ACTIVELY READING GOALS)                   
*                                                                               
         TM    NGSELOPT,NGSELBOT+NGSELPLN    ANY FILTER?                        
         BNZ   *+16                                                             
         TM    GXKEYAGY,GXKEYTAR   DEFAULT IS TO SKIP PLANNED GOALS             
         BO    XGOAL8                                                           
         B     XGOAL11                                                          
*                                                                               
         TM    NGSELOPT,NGSELBOT   GET BOTH PLAN AND REGULAR GOALS?             
         BO    XGOAL11                                                          
         TM    GXKEYAGY,GXKEYTAR   IS THIS A PLANNED GOAL?                      
         BZ    XGOAL8                                                           
*                                                                               
XGOAL11  CLC   KEY(4),KEYSAVE      CHECK MATCH ON BRAND                         
         JNE   XIT                                                              
         CLC   GXKPRDA,LISTPRD      CHECK ALPHA PRODUCT                         
         BNE   XGOAL8                                                           
*                                                                               
         OC    NGSELMKT,NGSELMKT                                                
         BZ    XGOAL12                                                          
         CLC   KEY(7),KEYSAVE      OR MARKET IF SPECIFIED                       
         JNE   XIT                                                              
*                                                                               
XGOAL12  ZIC   R1,GXKEYEST          CHECK ESTIMATE MATCH                        
         LA    R2,NBESTMSK                                                      
         BRAS  RE,TESTMASK                                                      
         BNE   XGOAL8                                                           
         CLI   NGSELDP,0           CHECK DP FILTER                              
         BE    XGOAL14                                                          
         CLC   NGSELDP,GXKEYDPT                                                 
         BNE   XGOAL8                                                           
*                                                                               
XGOAL14  CLI   NGSELSL,0           CHECK SL FILTER                              
         BE    XGOAL16                                                          
         CLC   NGSELSL,GXKEYSLN                                                 
         BNE   XGOAL8                                                           
*                                                                               
XGOAL16  CLI   NGSELPKG,0          CHECK PACKAGE FILTER                         
         BE    XGOAL18                                                          
         TM    GXKEYAGY,X'40'       CHECK FOR PACKAGE IN KEY BIT                
         BZ    XGOAL8                                                           
         CLC   NGSELPKG,GXKEYPR2                                                
         BNE   XGOAL8                                                           
*                                                                               
XGOAL18  DS    0H                   CONTROL PROCESSING OF GOALS                 
         MVI   NBSTATYP,C'N'                                                    
         CLC   GXKEYMKT,=X'0309'    NETWORK GOALS?                              
         BE    XGOAL20A                                                         
*                                                                               
         MVI   NBSTATYP,C'S'                                                    
         CLC   GXKEYMKT,=X'0306'    SYNDICATION GOALS?                          
         BE    XGOAL20A                                                         
*                                                                               
         MVI   NBSTATYP,C'C'                                                    
         CLC   GXKEYMKT,=X'0307'    CABLE GOALS?                                
         BE    XGOAL20A                                                         
         MVI   NBSTATYP,C'N'                                                    
*                                                                               
XGOAL20A MVC   NGOALEST,GXKEYEST    SOME VALUES FROM THE KEY                    
         MVC   NGOALMKT,GXKEYMKT                                                
         MVC   NGOALDP,GXKEYDPT                                                 
         XC    NBDPNAM,NBDPNAM         MATCH NETVALUE+SPEED                     
         XC    NBDPNAM2,NBDPNAM2                                                
         GOTOR PROCDP,DMCB,NBDPNAM,NGOALDP                                      
*!!!     EXPDP NBDPNAM,NGOALDP                                                  
         MVI   NGOALDP2+1,X'40'                                                 
         MVC   NGOALDP2(1),NGOALDP                                              
         CLC   =C'UNKNOWN',NBDPNAM                                              
         BNE   XGOAL20X                                                         
* - GET 2 CHAR DAYPART                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),=XL2'0D07'                                               
         MVC   WORK+2(1),NBACTAM                                                
         CLI   NGOALDP,127            CHECK CLIENT LEVEL DAYPART                
         BH    *+10                                                             
         MVC   WORK+3(2),NBACTCLI                                               
         MVC   WORK+5(1),NGOALDP                                                
         MVC   WORK+50(10),WORK                                                 
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'UNTDIR ',WORK,WORK,0                     
         CLC   WORK(6),WORK+50                                                  
         BE    *+6                                                              
         DC    H'0'                NEW DPT CRASH!                               
         LA    R1,WORK                                                          
         USING NDPTHDR,R1                                                       
         MVC   NGOALDP2,NDPTDPTA    PASS BACK 2 CHAR DAYPART                    
* MOVE DESCRIPTION TO NETBLOCK HERE AND NOT IN EXDP                             
* SINCE I DON'T WANT TO USE REST OF SPARE IN NETGOALD                           
         MVC   NBDPNAM,NDPTDES      DESCRIPTION                                 
         MVC   NBDPNAM2,NDPTDES+8   REST OF DESCRIPTION                         
         BAS   RE,XHIGH             RESTORE GOAL SEQUENCE                       
         DROP  R1                                                               
*                                                                               
XGOAL20X MVC   NGOALSL,GXKEYSLN                                                 
         MVI   NGOALPAK,0                                                       
         TM    GXKEYAGY,X'40'       IS SECOND PRODUCT A GOAL                    
         BZ    *+10                                                             
         MVC   NGOALPAK,GXKEYPR2                                                
         BAS   RE,XGETREC                                                       
*                                                                               
         NI    NGOALIND,X'FF'-NGOALPLN                                          
         TM    GXKEYAGY,GXKEYTAR    PLANNED GOAL?                               
         BZ    *+8                                                              
         OI    NGOALIND,NGOALPLN    YES                                         
*                                                                               
* CHECK FOR GOAL HISTORY RECORD                                                 
*                                                                               
         TM    GXKEYAGY,X'20'                                                   
         BNO   XENDHIST                                                         
* SET UP NETBLOCK FOR GOAL HISTORY RECORDS                                      
         MVC   NBSPLPRN,GXKEYPRD    PRODUCT                                     
         MVC   NBPRD,GXKEYPRD                                                   
         MVC   NBPR1CL3,GXKPRDA                                                 
         MVI   NBPRD2,0                                                         
         MVC   NBMARKET,GXKEYMKT    MARKET                                      
         MVC   NBACTEST,GXKEYEST    ESTIMATE                                    
         MVC   NBACTDP,GXKEYDPT     DAYPART                                     
         MVI   NBPACK,0            (PACKAGE)                                    
         MVC   NBLEN,GXKEYSLN       SPOT LENGTH                                 
*                                                                               
         XC    NBACTPRG,NBACTPRG   CLEAR PROGRAM                                
         XC    NBPROGNM,NBPROGNM                                                
         XC    NBACTDAT,NBACTDAT   CLEAR DATE                                   
         MVI   NBDAY,0             CLEAR DAY                                    
         XC    NBTIME,NBTIME       (TIME)                                       
         MVI   NBUNITST,0                                                       
         DROP  R4                                                               
*                                                                               
         B     XGOAL8                                                           
*                                                                               
XGOALX   DS    0H                                                               
         BRAS  RE,GOHOOK                                                        
         J     XIT                                                              
*                                                                               
XENDHIST DS    0H                                                               
         NI    TRGFLAG,X'FF'-TRGSAME                                            
*                                                                               
         CLC   =C'SJ',NBSELAGY          ***IF SJR                               
         BE    XEH02                                                            
         CLC   =C'*B',NBSELAGY          ***IF DDSB                              
         BE    XEH02                                                            
         CLC   =C'MC',NBSELAGY          ***IF MCCANN                            
         BNE   XGOAL21                                                          
*                                                                               
XEH02    L     R1,NBAIO                                                         
         MVC   SVXGKEY,0(R1)             SAVE AWAY GOAL KEY                     
         USING NGOLXD,R1                                                        
*                                                                               
         MVC   BYTE,GXKEYPRD                                                    
         MVC   PRDA,GXKPRDA                                                     
*                                                                               
         L     R1,NBAIO                                                         
         XC    KEY,KEY                  GET CORRECT EST RECORD TO CHECK         
         LA    R4,KEY                   TARGET DEMOS                            
         USING ESTHDR,R4                                                        
*                                                                               
         MVC   EKEYAM,GXKEYAM            AGY/MED                                
         MVC   EKEYCLT,GXKEYCLT          CLIENT                                 
         MVC   EKEYPRD,PRDA              PRODUCT                                
         MVC   EKEYEST,GXKEYEST          ESTIMATE                               
         DROP  R1                                                               
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XEH10                                                            
         BAS   RE,GETREC                                                        
*                                                                               
         L     R4,NBAIO                                                         
         MVC   NGOALTRG,EDEMOS+2                                                
         MVC   NGOALTR2,EDEMLST+5                                               
         MVC   NGOALTR3,EDEMLST+8                                               
*                                                                               
         OC    ETRGLST,ETRGLST          ANY TARGETS?                            
         BZ    XEH10                                                            
         MVC   NGOALTRG,ETRGLST+2                                               
         MVC   NGOALTR2,ETRGLST+5                                               
         OC    ETRGLST+3(3),ETRGLST+3   IS THERE A 2ND TARGET?                  
         BZ    XEH10                                                            
         CLC   ETRGLST(3),ETRGLST+3                                             
         BNE   *+8                                                              
         OI    TRGFLAG,TRGSAME          1ST AND 2ND TARGET ARE SAME             
         DROP  R4                                                               
*                                                                               
XEH10    DS    0H                                                               
         XC    KEY,KEY                  RESTORE GOAL RECORD                     
         MVC   KEY(32),SVXGKEY                                                  
         BAS   RE,XHIGH                                                         
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,XGETREC                                                       
*                                                                               
*--GET STATION NAME                                                             
XGOAL21  DS    0H                                                               
         XC    NGOALNET,NGOALNET                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                20 ELEMENT IS REQUIRED                       
         USING GDELEM,R6                                                        
         CLI   GDNETWK,X'40'                                                    
         BNH   *+10                                                             
         MVC   NGOALNET,GDNETWK                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL2                                                        
         B     XGOAL24                                                          
*                                                                               
XGOAL22  BAS   RE,NEXTEL2                                                       
*                                                                               
XGOAL24  DS    0H                                                               
         BNE   XGOAL8                                                           
*!!!     JNE   XIT                                                              
         USING GLEMENT,R6                                                       
*                                                                               
         OC    NBCMPSTR,NBCMPSTR   TEST GOALWEEK WITHIN NBSTRT/END              
         BZ    XGOAL24A                                                         
         CLC   GLWEEK,NBCMPSTR                                                  
         BL    XGOAL22                                                          
XGOAL24A OC    NBCMPEND,NBCMPEND                                                
         BZ    XGOAL24B                                                         
         CLC   GLWEEK,NBCMPEND                                                  
         BH    XGOAL22                                                          
*                                                                               
XGOAL24B MVC   NGOALWK,GLWEEK                                                   
         XC    NGOALDOL,NGOALDOL                                                
*                                                                               
         TM    NBINDS5,NBI5MTN                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'N'                                                    
         BNE   XG24B9                                                           
*                                                                               
         TM    NBINDS5,NBI5MTS                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'S'                                                    
         BNE   XG24B9                                                           
*                                                                               
         TM    NBINDS5,NBI5MTC                                                  
         BZ    *+12                                                             
         CLI   NBSTATYP,C'C'                                                    
         BNE   XG24B9                                                           
*                                                                               
         L     R0,GLBUDGET         CONVERT TO DOLLARS                           
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,NGOALDOL                                                      
XG24B9   XC    NGOALG3,NGOALG3     CLEAR/NOT ALWAYS 3                           
         XC    NGOALID,NGOALID     CLEAR ID FIELD                               
         MVC   NGOALGRP,GLGRP      POINTS FOR TARGET 1                          
         CLI   GLEN,16                                                          
         BL    XGOAL24C                                                         
*                                                                               
         MVC   NGOALG2,GLGRP2             AND TARGET 2                          
         CLC   =C'SJ',NBSELAGY          ***IF SJR                               
         BE    XG24B10                                                          
         CLC   =C'*B',NBSELAGY          ***IF DDSB                              
         BE    XG24B10                                                          
         CLC   =C'MC',NBSELAGY          ***IF MCCANN                            
         BNE   XGOAL24C                                                         
*                                                                               
XG24B10  TM    TRGFLAG,TRGSAME          1ST AND 2ND TARGETS SAME?               
         BZ    XGOAL24C                                                         
         MVC   NGOALG2,GLGRP              SAME AS TARGET 1                      
*                                                                               
XGOAL24C CLI   GLEN,21                                                          
         BL    *+10                                                             
         MVC   NGOALG3,GLGRP3             AND TARGET 3                          
         CLI   GLEN,33                                                          
         BL    *+18                                                             
         CLI   GLOTTYP,C'W'                                                     
         BNE   *+10                                                             
         MVC   NGOALID,GLOTFLD    GOAL IDENTIFIER                               
         TM    NBINDS3,NBI3A2DC    IF 2 DEC AGENCY                              
         BO    XGOAL24D                                                         
         CLI   NBPREOPT,C'Y'                                                    
         BNE   XGOAL25                                                          
XGOAL24D L     R0,NGOALGRP         PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALGRP                                                      
*                                                                               
         L     R0,NGOALG2          PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALG2                                                       
*                                                                               
         L     R0,NGOALG3          PUT IN CABLE PRECISSION                      
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         ST    R1,NGOALG3                                                       
XGOAL25  L     R1,NGAWLIST         LOOK UP WEEK (OPTIONAL)                      
         ZIC   R0,NGNWEEKS                                                      
         LA    R5,1                                                             
         LTR   R1,R1                                                            
         BZ    XGOALGO                                                          
*                                                                               
XGOAL26  CLC   GLWEEK,0(R1)        DOES GOAL FIT IN PERIOD                      
         BL    XGOAL27                                                          
         CLC   GLWEEK,2(R1)                                                     
         BNH   XGOAL28                                                          
*                                                                               
XGOAL27  LA    R1,4(R1)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,XGOAL26                                                       
         B     XGOAL22                                                          
*                                                                               
XGOAL28  STC   R5,NGOALWKN                                                      
*                                                                               
XGOALGO  BRAS  RE,EXNB             OPTIONAL NETBLOCK EXPANSION                  
         BRAS  RE,GOHOOK           PASS WEEK DATA BACK TO USER                  
         B     XGOAL22                                                          
         EJECT                                                                  
         J     XIT                                                              
         LTORG                                                                  
***************************************************************                 
*              DATAMGR INTERFACE                                                
***************************************************************                 
XHIGH    NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         MVC   FILE(8),=C'XSPDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    XHIGH10                                                          
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
XHIGH10  MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
XSEQ     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         MVC   FILE(8),=C'XSPDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    XSEQ10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
XSEQ10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
XREAD    NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         MVC   FILE(8),=C'XSPDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    XRED10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
XRED10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
XGETREC  NTR1                                                                   
         LA    R3,KEY+36                                                        
         MVC   FILE(8),=C'XSPFILE '     FILE                                    
         MVC   NBDTADSP,=H'42'                                                  
*                                                                               
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,(X'80',=C'GETREC'),FILE,(R3),(R2),DMWORK               
         CLI   8(R1),0                                                          
         BE    XGET10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
XGET10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
XDMCHECK CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    XNO                                                              
         DC    H'0'                                                             
*                                                                               
XYES     SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
XNO      LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
         GETEL2 R6,42,ELCODE                                                    
*                                                                               
PROCDP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         EXPDP DUB,0(R3)                                                        
*                                                                               
         MVC   0(8,R2),DUB                                                      
         J     XIT                                                              
         LTORG                                                                  
*****************************************************************               
CHKFILT  NTR1  BASE=*,LABEL=*                                                   
         L     R4,NBAIO            MEETS FILTER                                 
         USING ESTHDR,R4                                                        
         LA    R3,NBSELEFL                                                      
         LA    R5,EPROF                                                         
         LA    R0,3                                                             
         SPACE 1                                                                
QI46     CLI   0(R3),C'*'          WILD                                         
         BE    QI48                                                             
         CLI   0(R3),0                                                          
         BE    QI48                                                             
         TM    0(R3),X'40'                                                      
         BZ    QI47                                                             
         CLC   0(1,R3),0(R5)       FILTER                                       
         BNE   QINO                                                             
         B     QI48                                                             
         SPACE 1                                                                
QI47     MVC   HOLDEFLT,0(R5)                                                   
         NI    HOLDEFLT,X'FF'-X'40'   TURN OFF X'40' BIT                        
         CLC   0(1,R3),HOLDEFLT    NEGATIVE FILTER                              
         BE    QINO                                                             
         SPACE 1                                                                
QI48     LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,QI46                                                          
QYES     SR    R5,R5               PASSED FILTERS                               
QINO     LTR   R5,R5                                                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
*&&DO                                                                           
SETSPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   LKEY,=H'13'         SET VALUES FOR                               
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
SETXSP   NTR1  BASE=*,LABEL=*                                                   
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         J     XIT                                                              
         LTORG                                                                  
*&&                                                                             
*              NETBLOCK EXPANSION                                               
*                                                                               
EXNB     NTR1  BASE=*,LABEL=*                                                   
         CLI   NGEXBOPT,C'Y'       OPTION TO EXPAND NETBLOCK                    
         JNE   XIT                                                              
         MVC   NBSPLPRN,NGOALPNO   PRODUCT                                      
         MVC   NBPRD,NGOALPNO                                                   
         MVC   NBSPLPR3,NGOALPRD   ALPHA PRODUCT                                
         MVC   NBPR1CL3,NGOALPRD   ALPHA PRODUCT                                
         MVI   NBPRD2,0                                                         
         XC    NBPR2CL3,NBPR2CL3                                                
         MVC   NBMARKET,NGOALMKT   MARKET                                       
         MVC   NBACTEST,NGOALEST   ESTIMATE                                     
         MVC   NBACTNET,NGOALNET   ESTIMATE                                     
*                                                                               
* NEED POSTING TYPE FOR NETWORK - GET FROM NETLIST                              
         L     R1,NBANBUFF         GET ADDR OF NETLIST                          
         LTR   R1,R1                                                            
         BZ    EXNB10              NOT THERE/SKIP                               
         LA    R1,5(R1)            JUMP OVER CLIENT CODE AT START               
EXNB05   CLI   0(R1),0                                                          
         BE    EXNB10              NO MATCH/SKIP                                
         CLC   0(4,R1),NBACTNET    MATCH                                        
         BE    *+12                                                             
         LA    R1,5(R1)                                                         
         B     EXNB05                                                           
         MVC   NBPOSTYP,4(R1)      SET POST TYPE                                
*                                                                               
EXNB10   MVC   NBACTDP,NGOALDP     DAYPART                                      
         MVC   NBACTNDP,NGOALDP2   2 CHAR DAYPART                               
         MVC   NBPACK,NGOALPAK     (PACKAGE)                                    
         MVC   NBLEN,NGOALSL       SPOT LENGTH                                  
         XC    NBACTPRG,NBACTPRG   PROGRAM                                      
         XC    NBPROGNM,NBPROGNM                                                
         MVC   NBACTDAT,NGOALWK    DATE (WEEK COMMENCING)                       
         MVI   NBDAY,X'7F'         DAY (M-S)                                    
         XC    NBTIME,NBTIME       (TIME)                                       
         MVI   NBUNITST,0                                                       
         J     XIT                                                              
*                                                                               
GOHOOK   NTR1  BASE=*,LABEL=*                                                   
         L     RF,NGAHOOK                                                       
         L     RE,USERRD           USERS RD                                     
         LM    R0,RC,20(RE)        RESTORE USERS R0-RC                          
         BASR  RE,RF               RF CONTAINED A(HOOK ROUTINE)                 
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*              DATAMGR INTERFACE                                                
*                                                                               
HIGH     NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         MVC   FILE(8),=C'SPTDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    HIGH10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
HIGH10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
SEQ      NTR1  BASE=*,LABEL=*                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         MVC   FILE(8),=C'SPTDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    SEQ10                                                            
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
SEQ10    MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
READ     NTR1  BASE=*,LABEL=*                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         MVC   FILE(8),=C'SPTDIR  '         DIRECTORIES                         
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,COMMAND,FILE,KEY,(R2),0                                
         BAS   RE,DMCHECK                                                       
         CLI   8(R1),0                                                          
         BE    READ10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
READ10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
GETREC   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY+14                                                        
         MVC   FILE(8),=C'SPTFILE '     FILE                                    
         MVC   NBDTADSP,=H'24'                                                  
         L     R2,NBAIO                                                         
         GOTO1 NBDM,DMCB,(X'80',=C'GETREC'),FILE,(R3),(R2),DMWORK               
         CLI   8(R1),0                                                          
         BE    GETR10                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
GETR10   MVC   KEY,0(R2)                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
*                                                                               
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         J     XIT                                                              
*                                                                               
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
TESTMASK NTR1  BASE=*,LABEL=*                                                   
*                                  R1=TEST NUMBER                               
*                                  R2=A(MASK)                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   BITTEST,0(R2)                                                    
         NC    BITTEST,0(R1)                                                    
         CLI   BITTEST,0                                                        
         JE    NO                                                               
         J     YES                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
         SPACE 1                                                                
*              DSECTS ETC                                                       
         SPACE 3                                                                
*                                                                               
MYD      DSECT                                                                  
         DS    0D                                                               
KEY      DS    CL40                                                             
KEYSAVE  DS    CL40                                                             
WORK     DS    CL64                                                             
DUB      DS    D                                                                
PARAS    DS    0F                                                               
DMCB     DS    6F                                                               
USERRD   DS    F                                                                
FULL     DS    F                                                                
HOLDEFLT DS    CL1                                                              
ELCODE   DS    CL1                                                              
BITTEST  DS    CL1                                                              
COMMAND  DS    CL8                                                              
FILE     DS    CL8                                                              
DMWORK   DS    CL96                                                             
ENTWID   DS    F                                                                
*                                                                               
TRGFLAG  DS    XL1                 TARGET DEMO FLAGS                            
TRGSAME  EQU   X'01'               T1 AND T2 ARE THE SAME                       
*                                                                               
SYSTFLAG DS    CL1                 (S)POT (X)SPFILE                             
*                                                                               
BYTE     DS    XL1                                                              
*                                                                               
PRDA     DS    CL3                                                              
*                                                                               
ANGBLOCK DS    A                                                                
ANBLOCK  DS    A                                                                
*                                                                               
SVGKEY   DS    XL13                SAVED GOAL KEY                               
SVXGKEY  DS    XL32              SAVED GOAL KEY                                 
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
         SPACE 3                                                                
LISTD    DSECT                     PRODUCT LIST ENTRY                           
LISTNO   DS    XL1                 PRODUCT NUMBER                               
LISTFLAG DS    XL1                 DEMO FLAG                                    
LISTTGSM EQU   X'01'               TARGET1 = TARGET 2                           
LISTDIV  DS    XL2                 DIVISION NUMBER                              
LISTPRD  DS    CL3                 PRODUCT ALPHA                                
LISTTARG DS    XL1                 TARGET DEMO NUMBER                           
LISTUSER DS    XL1                 USER NUMBER                                  
LISTDEQU EQU   *-LISTNO                                                         
LISTDEMO DS    CL16                UP TO 8 ADDITIONAL DEMOS                     
         EJECT                                                                  
       ++INCLUDE NETBLOCKN                                                      
         EJECT                                                                  
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122NENETGOAL 06/22/09'                                      
         END                                                                    
