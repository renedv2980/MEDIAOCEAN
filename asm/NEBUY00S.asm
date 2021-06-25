*          DATA SET NEBUY00S   AT LEVEL 216 AS OF 05/01/02                      
*PHASE T31100A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM BASE - T31100'                               
T31100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BUYWRKX-BUYWRKD,**BUY00,RA,RR=RE,CLEAR=YES                       
         LA    R7,2048(RA)                                                      
         LA    R7,2048(R7)                                                      
         USING T31100+8192,R7       THIRD BASE REGISTER                         
         LR    R9,RC               R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         ST    RE,BRELO            SAVE BASE RELOCATION                         
         ST    RB,BASE1            SAVE BASE REGISTERS                          
         ST    RA,BASE2                                                         
         ST    R7,BASE3                                                         
         ST    RD,AWORK                                                         
*                                                                               
         ST    R1,APARM            PARAMETER LIST FROM MONITOR                  
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
BUY01    L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,YES                                                          
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAUSRID                                                  
         MVC   AGYALPH,TWAAGY                                                   
*                                                                               
BUY02    L     RE,ACOMFACS         OBTAIN COMFACS MODULE ADDRESSES              
         USING COMFACSD,RE                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VDEMOUT,CDEMOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGLOBBER,CGLOBBER                                                
         DROP  RE                                                               
*                                                                               
BUY03    LA    R2,CORETAB          OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,VCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
BUY04    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,BUY04                                                         
*                                                                               
         MVI   DMCB+7,QMSPACK      GET A(MSPACK)                                
         GOTO1 (RF),(R1),0                                                      
         MVC   FULL2,DMCB          FULL2=A(MSPACK)                              
*                                                                               
BUY06    LA    R1,BASETAB          SET UP BASE FACILITIES                       
         LA    R0,BASETABC         COUNTER                                      
         LA    RE,BASEFACS         POINTER TO WORKING STORAGE                   
BUY07    L     RF,0(R1)            ADDRESS OF BASE FACILITY                     
         A     RF,BRELO            RELOCATE IT                                  
         ST    RF,0(RE)                                                         
         LA    R1,4(R1)            NEXT ADDRESS                                 
         LA    RE,4(RE)            NEXT OUTPUT AREA                             
         BCT   R0,BUY07                                                         
*                                                                               
         MVC   VMSPACK,FULL2       SET A(MSPACK)                                
*                                                                               
BUY10    LA    R1,COMMIN           LINKAGE TO COMMON ROUTINES                   
         SR    R2,R2                                                            
         LA    R3,BASECOM          OUTPUT ADDRESSES                             
         LA    R0,COMMONS          COUNTER                                      
*                                                                               
BUY11    ST    R1,0(R3)                                                         
         STC   R2,0(R3)                                                         
         LA    R2,4(R2)            BUMP BRANCH INDEX                            
         LA    R3,4(R3)            NEXT OUTPUT AREA                             
         BCT   R0,BUY11                                                         
         SPACE 1                                                                
*                                                                               
BUY12    LA    RE,IOAREA1          SET ADCONS FOR EXTENDED ADDRESSING           
         LA    R0,4                COUNTER - 4 I/O AREAS                        
         LA    R1,AIOAREA1         OUTPUT AREA                                  
         ST    RE,0(R1)                                                         
         LA    RE,IOLEN(RE)                                                     
         LA    R1,4(R1)                                                         
         BCT   R0,*-12                                                          
         L     R1,=AL4(PROGREC-BUYWRKD)                                         
         LA    R1,BUYWRKD(R1)      SET ADCON FOR PROGRAM RECORD                 
         ST    R1,APROGREC                                                      
         L     R1,=AL4(OVWORK-BUYWRKD)                                          
         LA    R1,BUYWRKD(R1)      SET ADCON TO OVERLAY LOCAL STORAGE           
         ST    R1,AOVWORK                                                       
*                                                                               
         MVI   SPACES,C' '         SET OTHER WORKING STORAGE VALUES             
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYC)                                    
         MVC   XTRA,SPACES                                                      
         MVI   DEMFRST,YES         SET FIRST TIME DEMO LOOKUP SWITCH            
         MVC   BUYMSG,SPACES                                                    
         OI    BUYMSGH+6,X'80'                                                  
*                                                                               
         L     R5,ATIA             R5 POINTS TO BUYVALS                         
         USING BUYVALD,R5                                                       
         ST    R5,ABUYVALS                                                      
         LR    RE,R5               CLEAR BUY VALUES AREA                        
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         EJECT                                                                  
* ROUTINE TO TEST WHETHER WE'VE BEEN CALLED FROM $MAD FOR UPLOAD                
*                                                                               
TESTUP   ICM   RF,15,VGLOBBER                                                   
         BZ    VALAGY                                                           
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,GLEGNF                                                    
         BE    VALAGY                                                           
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                               DELETE 'OLD' TRANSFER ELEM                      
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BUYACTH,72          LOOK UP UPLOAD                               
         MVI   BUYACTH+5,6                                                      
         MVC   BUYACT,SPACES                                                    
         MVC   BUYACT(6),=CL6'UPLOAD'                                           
         B     VALACT                                                           
         EJECT                                                                  
****************************************************************                
* VALIDATION CODE REGISTER CONVENTIONS                         *                
* R3 POINTS TO ACTION TABLE ENTRY  R4 POINTS TO RECORD         *                
* R5 POINTS TO BUY VALUES          R6 POINTS TO NETBLOCK       *                
****************************************************************                
         SPACE 2                                                                
* START BY GETTING AGENCY VALUES                                                
*                                                                               
VALAGY   MVC   NBSELAGY,AGYALPH                                                 
         MVI   NBSELMED,C'N'                                                    
         MVI   NBSELMOD,NBVALAGY   READ AGENCY RECORD                           
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         MVC   NBAUTH,TWAAUTH                                                   
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AGENCY,AGYALPH                                                   
         MVC   AGYMED,NBACTAM                                                   
         L     R4,NBAIO                                                         
         USING AGYHDRD,R4                                                       
         MVC   AGYPRO,AGYPROF      EXTRACT PROFILE                              
         B     VALACT                                                           
         DROP  R4                                                               
         EJECT                                                                  
* FIRST VALIDATE ACTION                                                         
*                                                                               
VALACT   L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         USING ACTTABD,R3                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FTERM,FTERM                                                      
         MVI   FTERM,C','                                                       
         MVI   FERN,MISERR                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VALACTR                                                          
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,L'ACTCODE                                                 
         BH    VALACTR             LONGER THAN A VALID ACTION CODE              
         CLI   FLDH+5,1                                                         
         BNE   VALACT2                                                          
         CLI   FLD,C'?'            TEST FOR QUESTION MARK                       
         BNE   VALACT2                                                          
         L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         LA    R3,HELPENT-ACTTAB(R3) POINT TO HELP ENTRY                        
         B     VALACT4                                                          
         SPACE 1                                                                
VALACT2  CLI   ACTCODE,X'FF'       TEST FOR END-OF-TABLE                        
         BE    VALACTR                                                          
         CLC   ACTCODE,FLD         TEST FOR VALID ACTION                        
         BE    VALACT4             FOUND ONE                                    
         CLC   FLDH+5(1),ACTMINL   TEST ACTUAL LEN VS. MIN                      
         BL    VALACT3             TOO SMALL FOR VARIABLE COMPARE               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),ACTCODE                                                   
         BE    VALACT4             OK                                           
         SPACE 1                                                                
VALACT3  LA    R3,ACTNTRL(R3)      NEXT TABLE ENTRY                             
         B     VALACT2                                                          
         SPACE 1                                                                
VALACT4  CLI   DDS,YES             DDS, NO SECURITY CHECK NEEDED                
         BE    VALACT5                                                          
         TM    ACTIND,DDSONLY      TEST FOR DDS ONLY ACTION                     
         BNZ   VALACTR             YES, ERROR                                   
         TM    ACTIND,READONLY     IS THIS READ ONLY ACTION                     
         BO    VALACT5             YES, NO SECURITY CHECK NEEDED                
         CLC   ACTCODE,=CL6'UPLOAD'                                             
         BE    VALACT5             BYPASS AUTH CODE CHECK FOR UPLOADS           
         TM    AUTH,X'08'          IS AUTHORIZED TO DO UPDATES                  
         BO    VALACTR             NO, SECURITY ERROR                           
VALACT5  ST    R3,AACTNTRY         SAVE A(ACTION ENTRY)                         
         B     VALACTX                                                          
         SPACE 1                                                                
VALACTR  MVC   XTRA(6),=C'ACTION'  SET ADDITIONAL ERROR MSG                     
         B     ERROR                                                            
         SPACE 1                                                                
VALACTX  CLI   ACTN,HELP           TEST FOR HELP                                
         BE    OV                  SKIP ALL FIELD VALIDATIONS                   
         B     VALCLI                                                           
         EJECT                                                                  
* VALIDATE CLIENT                                                               
*                                                                               
VALCLI   CLC   ACTCODE,=CL6'UPLOAD'   IF UPLOAD ACTION                          
         BE    OV                  GO DIRECTLY TO OVERLAY                       
*                                                                               
         LA    R2,BUYCLIH                                                       
         MVI   FERN,MISERR         CLIENT IS ALWAYS COMPULSORY                  
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BNE   VALCLI1A                                                         
         TM    ACTCLI,OPTIONAL     TEST IF FIELD IS OPTIONAL                    
         BZ    ERROR               NO                                           
         B     VALEST                                                           
*                                                                               
VALCLI1A MVI   FERN,INVERR                                                      
         GOTO1 VCLPACK,DMCB,FLD,CLIPK                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
* CHECK IF PROFILES HAVE TO BE RE-READ                                          
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SVAREA,RE                                                        
*                                                                               
         CLC   LSTAGY,AGYALPH                                                   
         BNE   VALCLI1C                                                         
         CLC   LSTCLI,FLD                                                       
         BNE   VALCLI1C                                                         
                                                                                
* MOVE PROFILES INTO THE BLOCK                                                  
         MVC   NBUSER,SVPROF3                                                   
         MVC   NBUSER1,SVPROF1                                                  
         MVC   NBUSER2,SVPROF2                                                  
         DROP  RE                                                               
*                                                                               
VALCLI1C MVC   NBSELCL2,CLIPK                                                   
         MVI   NBSELMOD,NBVALCLI                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD      TEST FOR ERROR                               
         BE    VALCLI1                                                          
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
         SPACE 1                                                                
VALCLI1  MVC   CLI,FLD             ALPHA CLIENT                                 
         CLC   CLIENT,CLIPK        TEST FOR CHANGE IN CLIENT                    
         BE    *+8                                                              
         MVI   MODE,DISPLAY        FORCE DISPLAY ON CHANGE                      
         MVC   CLIENT,CLIPK                                                     
*                                                                               
         L     R4,NBAIO                                                         
         USING CLTHDRD,R4                                                       
         MVC   BUYCLIN,CNAME       MOVE CLIENT NAME TO SCREEN                   
         OI    BUYCLINH+6,X'80'                                                 
*                                                                               
         OC    TWAACCS(2),TWAACCS  TEST FOR ANY SECURITY LIMITS                 
         BZ    VALCLI4                                                          
         CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKOUT                      
         BE    VALCLI4             NO CHECK                                     
         MVI   FERN,SCTYERR                                                     
         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIMIT                        
         BE    VALCLI2                                                          
         CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                        
         BE    VALCLI3                                                          
         CLC   TWAACCS(2),CLIPK    TEST FOR FILTERED CLIENT                     
         BE    VALCLI4             OK                                           
         B     ERROR                                                            
         SPACE 1                                                                
*                                                                               
VALCLI2  DS    0H               * TEST OFFICE LIST SECURITY *                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,VCALLOV                                                       
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                         
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
         B     VALCLI4                                                          
*                                                                               
VALCLI3  CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                    
         BNE   ERROR                                                            
         SPACE 1                                                                
VALCLI4  MVC   CLIPRO,CPROF                                                     
         MVC   CLIEXTRA,CEXTRA                                                  
         MVC   CLIOPT2,COPT2                                                    
         MVC   CLICOST2,CCOST2                                                  
         MVC   CLIFPGRP,CRFPGRP                                                 
         LA    RE,CLILIST                                                       
         LA    RF,880                                                           
         LR    R1,RF                                                            
         LA    R0,CLIST            PRODUCT LIST                                 
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
         SPACE 1                                                                
*                                                                               
VALCLI6  L     R2,ATWA                                                          
         A     R2,=A(BASETWA)                                                   
         USING SVAREA,R2                                                        
*                                                                               
         MVC   SVPROF3,NBUSER                                                   
         MVC   SVPROF1,NBUSER1                                                  
         MVC   SVPROF2,NBUSER2                                                  
*                                                                               
         MVC   BUYPROF,SVPROF1                                                  
         MVC   BUYPROF2,SVPROF2                                                 
         MVC   NETPROF,SVPROF3                                                  
*                                                                               
         MVC   LSTAGY,AGYALPH                                                   
         MVC   LSTCLI,CLI                                                       
         MVC   LSTOFF,COFFICE                                                   
*                                                                               
VALCLIX  B     VALEST                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
* VALIDATE ESTIMATE                                                             
*                                                                               
VALEST   LA    R2,BUYESTH                                                       
         ST    R2,FADDR                                                         
         MVI   FNDX,0                                                           
         XC    FTERM,FTERM                                                      
         XC    FLAST,FLAST         EDIT FROM START OF FIELD                     
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    VALEST2                                                          
         TM    FLDH+4,X'04'        TEST FOR ALPHA INPUT                         
         BZ    VALEST4             NO                                           
         MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
         BNE   ERROR                                                            
         CLC   =C'ALL',FLD         TEST FOR ALL                                 
         BNE   ERROR                                                            
*                                                                               
VALEST1  MVI   EST,1               SET RANGE OF 1-255 FOR ALL                   
         MVI   EST+1,255                                                        
         MVI   ESTTYP,IALL                                                      
         TM    ACTEST,IALL         TEST IF 'ALL' VALID                          
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST2  MVI   FERN,MISERR         HANDLE CASE OF NO INPUT                      
         TM    ACTEST,OPTIONAL     TEST IF FIELD OPTIONAL                       
         BZ    ERROR               NO                                           
         TM    ACTEST,DEFAULT      TEST FOR DEFAULT SETTING                     
         BZ    *+14                                                             
         MVC   EST(L'ACTESTD),ACTESTD                                           
         MVI   ESTTYP,DEFAULT                                                   
         B     VALEST10                                                         
         SPACE 1                                                                
VALEST4  TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    VALEST6                                                          
         BAS   RE,ESTNUM                                                        
         STC   R0,EST                                                           
         STC   R0,EST+1                                                         
         MVI   ESTTYP,ISINGLE                                                   
         TM    ACTEST,ISINGLE                                                   
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST6  XC    FLAST,FLAST         RANGE VALIDATION                             
         MVC   FTERM(2),=C',='     SEARCH FOR COMMA/EQUALS                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA STOPPED IT                     
         BNE   VALEST8                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIRST FIELD                 
         BZ    ERROR                                                            
         BAS   RE,ESTNUM                                                        
         STC   R0,EST                                                           
*                                                                               
VALEST7  XC    FTERM,FTERM         NOW NO TERMINATORS                           
         GOTO1 AFVAL,0                                                          
         TM    FLDH+4,X'08'        TEST IF SECOND FIELD NUMERIC                 
         BZ    ERROR                                                            
         BAS   RE,ESTNUM                                                        
         CLM   R0,1,EST            TEST IF SECOND NUMBER G.T. FIRST             
         BNH   ERROR                                                            
         STC   R0,EST+1                                                         
         MVI   ESTTYP,IRANGE                                                    
         TM    ACTEST,IRANGE                                                    
         BO    VALEST10                                                         
         B     ERROR                                                            
         SPACE 1                                                                
VALEST8  CLI   FSTOP,C'='          FILTER VALIDATION F=(3 BYTES)                
         BNE   ERROR                                                            
         CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         CLI   FLD,C'F'            TEST FOR FILTER PREFIX                       
         BNE   ERROR                                                            
         XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,3            NO MORE THAN 3 CHARACTERS                    
         BH    ERROR                                                            
         MVI   ESTTYP,IFILTER                                                   
         TM    ACTEST,IFILTER      TEST IF FILTER VALID FOR ACTION              
         BZ    ERROR                                                            
         MVC   EST,FLD             SET FILTER VALUES                            
         B     VALEST10                                                         
         SPACE                                                                  
VALEST10 CLC   ESTIMATE,EST        TEST FOR CHANGED ESTIMATE                    
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   ESTIMATE,EST                                                     
         XC    BUYESTD,BUYESTD                                                  
         OI    BUYESTDH+6,X'80'                                                 
         CLI   ESTTYP,ISINGLE      TEST FOR SINGLE NUMBER                       
         BNE   VALESTX                                                          
         SPACE 1                                                                
VALEST12 MVC   NBSELEST,EST                                                     
         MVI   NBSELMOD,NBVALEST                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+14                                                             
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
*                                  OUTPUT ESTIMATE VALUES ON SCREEN             
VALEST13 L     R4,NBAIO                                                         
         USING ESTHDRD,R4                                                       
         MVI   FERN,ELOCKERR       TEST FOR LOCKED ESTIMATE                     
         TM    ECNTRL,X'08'                                                     
         BO    ERROR                                                            
         MVC   BUYESTD(20),EDESC   ESTIMATE NAME                                
         MVC   ESTSTART,ESTART     ESTIMATE START (YYMMDD)                      
         MVC   ESTEND,EEND         ESTIMATE END (YYMMDD)                        
         GOTO1 VDATCON,DMCB,ESTART,(5,BUYESTD+22)                               
         MVI   BUYESTD+30,C'-'                                                  
         GOTO1 (RF),(R1),EEND,(5,BUYESTD+31)                                    
         GOTO1 (RF),(R1),ESTART,(2,ESTS)                                        
         GOTO1 (RF),(R1),EEND,(2,ESTE)                                          
         MVC   ESTBOOK,EBOOK       HUT BOOK                                     
         MVC   BUYESTD+43(3),=C'HUT'                                            
         ZIC   R0,ESTBOOK          HUT YEAR FIRST                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUYESTD+47(2),DUB+6(2)                                           
         MVC   BUYESTD+49(3),=C'(9)'  NOW DO PERIOD                             
         MVC   BUYESTD+50(1),ESTBOOK+1                                          
         OI    BUYESTD+50,X'F0'    AND MAKE IT NUMERIC                          
*                                                                               
VALEST14 MVC   ESTDEMS,EDEMLST                                                  
         MVC   ESTWLST,EWGTLST                                                  
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   ESTWNAM,EWGTNM                                                   
         MVC   ESTFILT,EPROF                                                    
         MVC   ESTSREP,EREP                                                     
         MVC   ESTRATE,ERATE                                                    
         MVC   ESTRATEC,ERATECST                                                
         MVC   ESTCOST2,ECOST2                                                  
         MVC   ESTFLAG1,EFLAG1                                                  
*                                                                               
         LA    R0,20               MAXIMUM DEMOS                                
         SR    R1,R1               COUNTER                                      
         LA    R2,ESTDEMS                                                       
VALEST15 OC    0(3,R2),0(R2)                                                    
         BZ    *+16                                                             
         LA    R1,1(R1)            INCREMENT DEMO COUNT                         
         LA    R2,3(R2)            NEXT DEMO                                    
         BCT   R0,VALEST15                                                      
         STC   R1,ESTNDEMS                                                      
         B     VALESTX                                                          
         SPACE 1                                                                
VALESTX  B     VALNET                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO PROCESS ESTIMATE NUMBER (AT ENTRY DUB HAS NUMBER)              
         SPACE 1                                                                
ESTNUM   CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE NETWORK                                                              
*                                                                               
VALNET   LA    R2,BUYNETH                                                       
         ST    R2,FADDR                                                         
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   VALNET2             FIELD HAS INPUT                              
*                                                                               
         MVI   FERN,MISERR                                                      
         TM    ACTNET,OPTIONAL     TEST IF FIELD IS OPTIONAL                    
         BZ    ERROR               NO                                           
         TM    ACTNET,DEFAULT      TEST FOR SETTING DEFAULT VALUE               
         BZ    *+14                                                             
         MVC   NET,ACTNETD         YES-MOVE IN THE DEFAULT VALUE                
         MVI   NETTYP,DEFAULT                                                   
         B     VALNET6                                                          
         SPACE 1                                                                
VALNET2  MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
         BNE   VALNET4             NO                                           
         CLC   =C'ALL',FLD         TEST FOR 'ALL'                               
         BNE   VALNET4             NO                                           
         MVI   NETTYP,IALL                                                      
         TM    ACTNET,IALL         TEST IF 'ALL' IS VALID INPUT                 
         BO    VALNET6             YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
VALNET4  MVC   NET,FLD             EXTRACT INPUT                                
         TM    FLDH+4,X'04'        MSPACK REQUIRES ALPHA                        
         BZ    ERROR                                                            
         MVI   NETTYP,ISINGLE      SET SWITCH FOR ONE NETWORK                   
         SPACE 1                                                                
VALNET6  CLC   NETWORK,NET         TEST FOR CHANGE                              
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   NETWORK,NET                                                      
         CLI   NETTYP,ISINGLE      TEST FOR ONE NETWORK INPUT                   
         BNE   VALNETX             NO-SKIP STATION RECORD READ                  
         SPACE 1                                                                
VALNET8  MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NET                                                  
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGYALPH                                                  
         MVC   STAKCLT,CLI                                                      
         GOTO1 AIO,DMCB,STA+READ+FILE,AIOAREA1                                  
*                                                                               
         L     R4,AIOAREA1                                                      
         MVC   FULL,SMKT           EXTRACT MARKET NUMBER                        
         GOTO1 VMSPACK,DMCB,FULL,STAKCALL,DUB                                   
         MVC   NETMARK,DUB                                                      
         MVC   NBSELNET,NET        SET NETWORK IN NETBLOCK                      
         CLI   STYPE,C'O'                                                       
         BE    *+10                                                             
         MVC   NBSTATYP,STYPE                                                   
         MVC   NBSTSTAT,STYPE                                                   
         MVC   NBPOSTYP,SPTYPE                                                  
         MVC   NBSTPSTT,SPTYPE                                                  
         MVC   NBSUBMED,SUBMEDIA                                                
         CLI   SNTISTA,X'40'                                                    
         BNH   VALNET10                                                         
         MVC   NBNTISTA,SNTISTA    IF EXISTS SET OVERRIDE NTI STATION           
*                                                                               
VALNET10 CLI   NBPOSTYP,C'C'                                                    
         BE    *+12                                                             
         CLI   NBPOSTYP,C'O'                                                    
         BNE   *+18                                                             
         MVC   DEMPREC(14),COPREC                                               
         MVI   NBPREOPT,C'Y'                                                    
         B     VALNETX                                                          
         MVC   DEMPREC(14),NSPREC                                               
         MVI   NBPREOPT,C'N'                                                    
*                                                                               
VALNETX  B     VALPAK                                                           
*                                                                               
*--DEMO OVERRIDE PRECISION TABLES                                               
COPREC   DC    C'R',X'82',C'S',X'81',C'P',X'82',C'T',X'42',C'H',X'42'           
         DC    C'U',X'43',C'V',X'40'                                            
NSPREC   DC    C'R',X'81',C'S',X'81',C'P',X'81',C'T',X'43',C'H',X'43'           
         DC    C'U',X'43',C'V',X'40'                                            
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PACKAGE                                                              
*                                                                               
VALPAK   LA    R2,BUYPAKH                                                       
         ST    R2,FADDR                                                         
         XC    BUYPAKD,BUYPAKD     CLEAR PACKAGE DATA ON SCREEN                 
         OI    BUYPAKDH+6,X'80'                                                 
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAK1                                                          
         TM    ACTOP,PUPTRNS                                                    
         BO    *+12                                                             
         TM    ACTPACK,NOEDIT                                                   
         BZ    VALPAK1                                                          
         MVC   BUYPAK,SPACES                                                    
         OI    BUYPAKH+6,X'80'                                                  
         B     VALPAKX                                                          
         SPACE 1                                                                
VALPAK1  GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   VALPAK2                                                          
         MVI   FERN,MISERR                                                      
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    ERROR                                                            
         TM    ACTPACK,OPTIONAL                                                 
         BZ    ERROR               FIELD IS COMPULSORY                          
         TM    ACTPACK,DEFAULT                                                  
         BZ    *+14                NO DEFAULT SETTING                           
         MVC   PACK,ACTPACKD                                                    
         MVI   PACKTYP,DEFAULT     SET TYPE OF INPUT                            
         B     VALPAK6                                                          
         SPACE 1                                                                
VALPAK2  MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    VALPAK4             NO                                           
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,PACK                                                          
         MVI   PACKTYP,ISINGLE                                                  
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAK6                                                          
         TM    ACTPACK,ISINGLE     TEST IF PACKAGE NUMBER IS VALID              
         BO    VALPAK6             YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
VALPAK4  CLI   FLDH+5,3            LOOK FOR 'ALL'                               
         BNE   ERROR                                                            
         CLC   =C'ALL',FLD                                                      
         BNE   ERROR                                                            
         MVI   PACKTYP,IALL                                                     
         TM    ACTPACK,IALL        TEST IF 'ALL' ALLOWED FOR ACTION             
         BO    VALPAK6             YES                                          
         B     ERROR                                                            
         SPACE 1                                                                
VALPAK6  CLC   PACKAGE,PACK        TEST FOR CHANGE IN PACKAGE                   
         BE    *+8                 NO                                           
         MVI   MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   PACKAGE,PACK                                                     
         CLI   PACKTYP,ISINGLE     TEST FOR ONE NUMBER                          
         BNE   VALPAKX                                                          
         CLI   ESTTYP,ISINGLE      MUST ALSO HAVE SINGLE                        
         BNE   VALPAKX             ESTIMATE AND NETWORK                         
         CLI   NETTYP,ISINGLE                                                   
         BNE   VALPAKX                                                          
         SPACE 1                                                                
VALPAK7  MVC   NBSELPAK,PACK       NOW READ THE PACKAGE                         
         MVI   NBSELMOD,NBPROCPK                                                
         MVI   NBSELPST,C'B'                                                    
         MVI   NBDATA,C'P'                                                      
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBMODE,NBREQLST     'REQLST' MEANS PACKAGE WAS                   
         BNE   VALPAK8             NOT FOUND                                    
         TM    ACTOP,PASSEQ                                                     
         BZ    *+12                                                             
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BE    VALPAKX                                                          
         MVI   FERN,PAKERR                                                      
         B     ERROR                                                            
         SPACE 1                                                                
VALPAK8  TM    ACTOP,PASSEQ                                                     
         BZ    VALPAK8A                                                         
         CLI   BUYPROF+11,YES      SELF ASSIGN PACKAGE #                        
         BNE   VALPAK8A                                                         
         MVI   FERN,DUPPACK                                                     
         B     ERROR                                                            
VALPAK8A L     R4,NBAIO            DISPLAY PACKAGE DATA ON SCREEN               
         USING NPRECD,R4                                                        
         TM    ACTOP,VPHBASE                                                    
         BZ    VALPAK8C                                                         
         TM    NPAKCNTL,X'40'      CHECK IMPR. BASED PACKAGE                    
         BZ    VALPAK8C                                                         
         LA    R3,ACTNTRL(R3)      GET NEXT TABLE ENTRY                         
         ST    R3,AACTNTRY                                                      
*                                                                               
VALPAK8C MVC   BUYPAKD,SPACES                                                   
         MVC   BUYPAKD(L'NBPAKNAM),NBPAKNAM                                     
         MVC   BUYPAKD+22(8),NBDPNAM DAYPART NAME                               
         LA    R2,BUYPAKD+31                                                    
         EDIT  (B4,NBPAKCST),(10,(R2)),COMMAS=YES,FLOAT=$,ALIGN=LEFT            
         MVC   BUYPAKD+50(2),=C'48' SET HUT WEEK CALENDAR                       
         TM    NPAKHUTL,X'40'      TEST FOR 52 WEEK CALENDAR                    
         BZ    *+10                                                             
         MVC   BUYPAKD+50(2),=C'52'                                             
*                                                                               
         TM    NPAKSTAT,X'02'      TEST FOR AUDIT                               
         BZ    *+10                                                             
         MVC   BUYPAKD+18(3),=C'AUD'                                            
*                                                                               
         MVC   BUYPAKD+43(4),=C'OPEN'                                           
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN                              
         BZ    *+14                                                             
         MVC   BUYPAKD+43(6),=C'FROZEN'                                         
         B     VALPAK10                                                         
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED                              
         BZ    VALPAK10                                                         
         MVC   BUYPAKD+43(6),=C'LOCKED'                                         
         SPACE 1                                                                
VALPAK10 LA    RE,PACKREC                                                       
         ST    RE,APACKREC         SET POINTER TO PACKAGE                       
         LR    R0,R4                                                            
         LH    R1,NPKRLEN          MOVE IT TO DESIGNATED AREA                   
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',PACKREC),(1,=C'K')             
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   APAKFEL,12(R1)                                                   
         SPACE                                                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',PACKREC),0                     
         CLI   12(R1),0                                                         
         BNE   VALPAKX                                                          
         MVC   APAKAEL,12(R1)                                                   
         SPACE 1                                                                
VALPAKX  B     VALPRG                                                           
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VALPRG   LA    R2,BUYPRGH                                                       
         ST    R2,FADDR                                                         
         XC    BUYPRGD,BUYPRGD                                                  
         OI    BUYPRGDH+6,X'80'                                                 
         TM    ACTPROG,NOEDIT      TEST FOR SKIPPING EDIT                       
         BZ    VALPRG1             NO                                           
         MVC   BUYPRG,SPACES       YES-SEND BACK CLEARED FIELD                  
         OI    BUYPRGH+6,X'80'                                                  
         B     VALPRGX                                                          
         SPACE                                                                  
VALPRG1  XC    KEY,KEY                                                          
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   VALPRG2             YES                                          
         MVI   FERN,MISERR                                                      
         TM    ACTPROG,OPTIONAL                                                 
         BZ    ERROR               FIELD IS COMPULSORY                          
         TM    ACTPROG,DEFAULT                                                  
         BZ    *+10                                                             
         MVC   PROG,ACTPROGD       SET DEFAULT                                  
         MVI   PROGTYP,DEFAULT                                                  
         B     VALPRG10                                                         
         SPACE 1                                                                
VALPRG2  MVI   FERN,INVERR                                                      
         CLI   FLDH+5,3            TEST FOR 'ALL'                               
         BNE   VALPRG4                                                          
         CLC   =C'ALL',FLD                                                      
         BNE   VALPRG4                                                          
         MVI   PROGTYP,IALL                                                     
         TM    ACTPROG,IALL        IS 'ALL' VALID FOR ACTION                    
         BO    VALPRG10            YES                                          
         B     ERROR               NO                                           
         SPACE 1                                                                
*                                  SEARCH FOR 'F=FILTERS' EXPRESSION            
VALPRG4  GOTO1 VSCANNER,DMCB,FLDH,(1,AIOAREA1),0                                
         CLI   4(R1),0                                                          
         BE    VALPRG6                                                          
         L     R1,AIOAREA1                                                      
         CLI   1(R1),0             TEST FOR SPLIT FIELD                         
         BE    VALPRG6             NO                                           
         CLI   0(R1),1                                                          
         BNE   VALPRG6                                                          
         CLI   12(R1),C'F'         TEST FOR KEYWORD OF 'F'                      
         BNE   VALPRG6                                                          
         MVC   PROG,SPACES                                                      
         MVI   PROGTYP,IFILTER                                                  
         MVC   PROG,22(R1)         FILTER VALUES (POSITIONAL)                   
         TM    ACTPROG,IFILTER     TEST IF FILTERS ARE OK FOR ACTION            
         BZ    ERROR                                                            
         B     VALPRG10                                                         
         SPACE 1                                                                
VALPRG6  TM    ACTPROG,ISINGLE                                                  
         BZ    ERROR                                                            
*                                                                               
         MVC   PROG,FLD            SET INPUT DATA                               
         MVI   PROGTYP,ISINGLE                                                  
         CLI   ESTTYP,ISINGLE      MUST HAVE SINGLE ESTIMATE                    
         BNE   VALPRG10            AND NETWORK FOR PROGRAM VALIDATION           
         CLI   NETTYP,ISINGLE                                                   
         BNE   VALPRG10                                                         
*                                                                               
         XC    KEY,KEY             NOW PRE-VALIDATE PROGRAM BY LOOKING          
         LA    R4,KEY              FOR ANY PROGRAMS APPLICABLE TO               
         USING NPGRECD,R4          ESTIMATE                                     
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,AGYMED                                                    
         MVC   NPGKNET,NETMARK     NETWORK MARKET NUMBER                        
         MVC   NPGKPROG,PROG       INPUT DATA                                   
         MVC   NPGKEND,ESTS        PROGRAMS ENDING ON OR AFTER EST ST           
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(NPGKEND-NPGKEY),KEYSAVE                                      
         BE    VALPRG10            PROGRAM IS VALID                             
         MVI   FERN,PROGERR                                                     
         B     ERROR                                                            
         DROP  R4                                                               
         SPACE 1                                                                
VALPRG10 CLC   PROGRAM,PROG        TEST FOR CHANGE IN PROGRAM                   
         BE    *+8                                                              
         MVI   MODE,DISPLAY                                                     
         MVC   PROGRAM,PROG                                                     
*--READ NAD DEFINITION RECORD THROUGH THE PROGRAM RECORD                        
*--AND FILTER OUT ALL NAD DEMOS THAT ARE NOT DEFINED ON                         
*--THE NAD DEFINITION RECORD                                                    
* SUB-ROUTINE TO READ NADDEF RECORD AND RETRIEVE THE NAD DEMOS                  
*                                                                               
*        CLC   KEY(2),=XL2'0D20'   WAS PROGRAM KEY READ                         
*        BNE   VALPRGX             NO EXIT                                      
*        GOTO1 VGETPROG,DMCB,KEY+11                                             
*        USING NPGRECD,R7                                                       
*        L     R7,APROGREC                                                      
*                                                                               
*        USING NPGEL03,R2                                                       
*        LA    R2,NPGMAINL                                                      
*        ZIC   RE,1(R2)            TEST IF NAD ELEMENT FOUND                    
*        AR    R2,RE               NO                                           
*        CLI   0(R2),X'03'                                                      
*        BNE   VALPRGX                                                          
*        OC    NPGNADDM,NPGNADDM                                                
*        BZ    VALPRGX                                                          
*                                                                               
*        XC    KEY,KEY                                                          
*        MVC   KEY(2),=XL2'0D16'                                                
*        MVC   KEY+2(1),AGYMED                                                  
*        MVC   KEY+3(6),NPGNADDM   NAD DEFINITION CODE                          
*        MVC   KEYSAVE(13),KEY                                                  
*        DROP  R2,R7                                                            
*                                                                               
*        GOTO1 AIO,DMCB,SPT+DIR+HIGH  RE-READ RECORD AND REPLACE                
*        CLC   KEY(13),KEYSAVE                                                  
*        BNE   VALPRGX                                                          
*        GOTO1 (RF),(R1),SPT+FILE+GET,AIOAREA4                                  
*                                                                               
*        USING NNDRECD,R7                                                       
*        L     R7,AIOAREA4                                                      
*        LA    R2,NNDMAINL                                                      
*        XC    HUTBLK,HUTBLK                                                    
*        LA    RE,HUTBLK                                                        
*                                                                               
*ALPRG20 ZIC   RF,1(R2)                                                         
*        AR    R2,RF                                                            
*        CLI   0(R2),0                                                          
*        BE    VALPRG25                                                         
*        CLI   0(R2),X'DD'                                                      
*        BNE   VALPRG20                                                         
*        MVC   0(3,RE),3(R2)                                                    
*        LA    RE,3(RE)                                                         
*        B     VALPRG20                                                         
*                                                                               
*ALPRG25 LA    RE,ESTDEMS                                                       
*        ZIC   RF,ESTNDEMS                                                      
*        LA    R4,FLD                                                           
*        SR    R1,R1                                                            
*                                                                               
*ALPRG30 CLI   0(RE),0             IS DEMO NAD                                  
*        BE    VALPRG50            NO BYPASS                                    
*        LA    R2,HUTBLK                                                        
*        LA    R7,40                                                            
*ALPRG35 OC    0(3,R2),0(R2)                                                    
*        BZ    VALPRG55                                                         
*        CLC   0(1,RE),0(R2)                                                    
*        BNE   VALPRG40                                                         
*        CLC   2(1,RE),2(R2)                                                    
*        BE    VALPRG50                                                         
*ALPRG40 LA    R2,3(R2)                                                         
*        BCT   R7,VALPRG35                                                      
*        B     VALPRG55                                                         
*ALPRG50 MVC   0(3,R4),0(RE)                                                    
*        LA    R4,3(R4)                                                         
*        LA    R1,1(R1)                                                         
*ALPRG55 LA    RE,3(RE)            GET NEXT ESTLIST DEMO                        
*        BCT   RF,VALPRG30                                                      
*        STC   R1,ESTNDEMS                                                      
*                                                                               
*        XC    ESTDEMS,ESTDEMS                                                  
*        ZIC   RF,ESTNDEMS                                                      
*        LTR   RF,RF               ANY DEMOS QUALIFY                            
*        BZ    VALPRGX             NO EXIT                                      
*        LA    R1,ESTDEMS                                                       
*        LA    R2,FLD                                                           
*                                                                               
*ALPRG60 MVC   0(3,R1),0(R2)                                                    
*        LA    R1,3(R1)                                                         
*        LA    R2,3(R2)                                                         
*        BCT   RF,VALPRG60                                                      
*                                                                               
         SPACE 1                                                                
VALPRGX  XC    HUTBLK,HUTBLK                                                    
         B     VLOC                                                             
*        DROP  R7                                                               
         EJECT                                                                  
* COMMON ROUTINE TO LOCK RECS BY TYPE, OR CHECK TO SEE IF LOCKED *              
         SPACE                                                                  
VLOC     TM    ACTOP,LOKCHEK                                                    
         BZ    VLOCEX                                                           
*                                                                               
         L     R4,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(,R4)                                        
         GOTO1 (RF),DMCB,0                                                      
*                                                                               
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         LA    R4,WORK                                                          
         USING LKKEYD,R4                                                        
         XC    WORK,WORK                                                        
*                                                                               
         MVC   LOCKSE,FASYS                                                     
         MVC   SECAGY,FATAGYSC     SAVE SECURITY AGENCY                         
         TM    FATFLAG,X'08'                                                    
         BZ    *+10                                                             
         MVC   SVPASSWD,FAPASSWD   SAVE PERSONAL ID                             
         DROP  RE                                                               
         SPACE                                                                  
         MVC   LOCKAGY,AGENCY                                                   
         MVC   LOCKRTY,=CL2'UN'                                                 
         MVC   LOCKKEY(3),CLI                                                   
         OI    LOCKKEY+2,X'40'                                                  
         XC    LOCKKEY+3(7),LOCKKEY+3                                           
         DROP  R4                                                               
         SPACE                                                                  
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QLOCKET                                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         SPACE                                                                  
         PRINT GEN                                                              
         L     R4,ACOMFACS                                                      
         GOTO1 (R2),(R1),(C'T',WORK),(R4)                                       
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   DMCB+4,0            ANY ERRORS                                   
         BNE   VLOCERR                                                          
*                                                                               
VLOCEX   B     OV                                                               
*--LOCK  ERROR                                                                  
VLOCERR  MVI   FERN,CLLOKERR                                                    
         B     ERROR                                                            
         EJECT                                                                  
* INTERFACE TO OVERLAY                                                          
*                                                                               
OV       MVC   ACTION,ACTN         ACTION NUMBER                                
         SPACE 1                                                                
OV2      CLC   ACTSCR,SCREEN       TEST IF SCREEN ALREADY LOADED                
         BE    OV4                 YES                                          
         OI    MODE,FIRST          SET FIRST TIME FOR SCREEN                    
         CLI   ACTSCR,0            TEST FOR SELF-GENERATING SCREEN              
         BE    OV3                                                              
         MVC   DMCB+4(3),=X'D90311'                                             
         MVC   DMCB+7(1),ACTSCR    SCREEN NUMBER                                
         GOTO1 VCALLOV,DMCB,BUYLAST                                             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
OV3      MVC   SCREEN,ACTSCR                                                    
         TM    ACTIND,FSTCURS      TEST FOR SETTING CURSOR/EXIT                 
         BZ    OV4                 NO                                           
         SR    R0,R0                                                            
         LA    R2,BUYACTH          POINT TO ACTION FIELD                        
         IC    R0,0(R2)            AND FIND THE FIRST UNPROTECTED FIELD         
         AR    R2,R0               THAT FOLLOWS IT                              
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         ST    R2,FADDR            CURSOR ADDRESS                               
         MVC   BUYMSG(13),=C'ENTER DETAILS'                                     
         B     OVX                                                              
         SPACE 1                                                                
OV4      CLI   ACTN,HELP           TEST FOR HELP                                
         BE    ACTHELP             DIVERT TO HELP ROUTINE                       
*                                                                               
         PRINT GEN                                                              
         GOTO1 VCALLOV,DMCB,(ACTOV,0),ATWA                                      
         PRINT NOGEN                                                            
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             OVERLAY ADDRESS                              
         GOTO1 (RF),DMCB,BUYWRKD                                                
         SPACE 1                                                                
OVX      NI    MODE,X'FF'-FIRST    MAKE SURE FIRST IS OFF                       
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PRODUCE ACTION HELP DISPLAY                                        
*                                                                               
ACTHELP  GOTO1 VCLEARF,DMCB,(1,HELHED1H),HELLAST                                
         L     R3,=A(ACTTAB)                                                    
         A     R3,BRELO                                                         
*        LA    R3,ACTTAB                                                        
         MVC   HELHED1(6),=C'ACTION'                                            
         MVC   HELHED1+16(3),=C'USE'                                            
         MVC   HELHED1+40(6),=C'ACTION'                                         
         MVC   HELHED1+56(3),=C'USE'                                            
         MVI   BYTE,NO             SET RIGHT HAND SIDE SWITCH                   
         LA    R2,HELLIN1                                                       
         SPACE 1                                                                
ACTHELP1 LA    R0,LINES                                                         
         SPACE 1                                                                
ACTHELP2 CLI   ACTCODE,X'FF'       TEST FOR E-O-T                               
         BE    ACTHELPX                                                         
         CLI   ACTN,HELP                                                        
         BE    *+20                SKIP ITS ENTRY                               
         TM    ACTIND,DDSONLY      TEST FOR DDS ONLY ACTION                     
         BZ    ACTHELP3            NO                                           
         CLI   DDS,YES             YES-NOW CHECK FOR DDS TERMINAL               
         BE    ACTHELP3            YES-SHOW IT IN LIST                          
         LA    R3,ACTNTRL(R3)                                                   
         B     ACTHELP2                                                         
         SPACE 1                                                                
ACTHELP3 LA    R1,L'ACTCODE        FIND LENGTH OF ACTION CODE                   
         LA    RE,ACTCODE+L'ACTCODE-1                                           
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         MVC   WORK,SPACES         PRE-CLEAR SCREEN OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,HELPMOVE         MOVE OUT ENTIRE CODE                         
         LA    R1,1(R1)            RESTORE ACTUAL LENGTH                        
         CLM   R1,1,ACTMINL        TEST ACTUAL LEN SAME AS MINIMUM              
         BE    ACTHELP5            YES                                          
         LR    R4,R1               SAVE ACTUAL LENGTH                           
         IC    R1,ACTMINL                                                       
         BCTR  R1,0                                                             
         EX    R1,HELPMOVE         EXTRACT THE MINIMUM CHARACTERS               
         LA    R1,1(R1)            RESTORE LENGTH                               
         SPACE 1                                                                
ACTHELP4 SR    R4,R1               FIND LENGTH OF OPTIONAL CHARACTERS           
         LA    RE,WORK(R1)                                                      
         MVI   0(RE),C'('          ENCLOSE OPTIONAL CHARS IN PARENS             
         LA    RF,ACTCODE(R1)      POINT PAST MIN CHARS IN ACTCODE              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),0(RF)       EXTRACT OPTIONAL CHARACTERS                  
         LA    RE,2(R4,RE)         POINT TO END OF STRING                       
         MVI   0(RE),C')'                                                       
         LA    R1,3(R4,R1)         TOTAL LENGTH SO FAR                          
         SPACE 1                                                                
ACTHELP5 LA    R4,WORK(R1)         POINT PAST ACTION CODE AND                   
         CLC   ACTINDES,SPACES     TEST FOR ACTION INPUT DESCRIPTN              
         BE    *+14                NONE                                         
         MVI   0(R4),COMMA         ATTACH ACTION INPUT DESCRIPTION              
         MVC   1(L'ACTINDES,R4),ACTINDES                                        
         MVC   0(14,R2),WORK       WHOLE EXPRESSION TO SCREEN                   
         MVC   16(L'ACTDESC,R2),ACTDESC                                         
         SPACE 1                                                                
ACTHELP6 LA    R3,ACTNTRL(R3)      NEXT TABLE ENTRY                             
         LA    R2,LINELEN(R2)      NEXT SCREEN LINE                             
         BCT   R0,ACTHELP2                                                      
*                                                                               
         CLI   BYTE,YES            TEST IF RIGHT SIDE DONE                      
         BE    ACTHELPX            YES                                          
         MVI   BYTE,YES            NO-SET SWITCH AND DO IT                      
         LA    R2,HELLIN1+40       POSITION TO START ON 1ST LINE                
         B     ACTHELP1                                                         
         SPACE 1                                                                
ACTHELPX MVI   FERN,HELPMSG                                                     
         B     ERROR                                                            
         SPACE 2                                                                
HELPMOVE MVC   WORK(0),ACTCODE                                                  
         DROP  R3                                                               
         EJECT                                                                  
* LINKAGE TO COMMON SUB-ROUTINES                                                
*                                                                               
COMMIN   NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,BASE3                                                         
         SRL   RF,24               SHIFT BRANCH INDEX TO L.O.B.                 
         B     COMTAB(RF)                                                       
         SPACE 1                                                                
COMTAB   B     GETFLD                                                           
         B     ERROR                                                            
         B     EXIT                                                             
         B     CLEARF                                                           
         B     GETPROG                                                          
         B     DISPROG                                                          
         B     BLDFLD                                                           
         B     CENFLD                                                           
         B     GETNADS                                                          
         B     CKREASN                                                          
         B     BLDRQST                                                          
         DC    5A(0)                                                            
COMMONS  EQU   (*-COMTAB)/4                                                     
         EJECT                                                                  
* GETFLD - EXTRACT DATA FROM SCREEN FIELD                                       
*                                                                               
* ON ENTRY R2 POINTS TO FIELD HEADER                                            
* ON EXIT                                                                       
*        FADDR = A(FIELD HEADER)                                                
*        FLDH  CONTAINS FIELD HEADER                                            
*        FLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED                       
*        R0    CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                
*                                                                               
GETFLD   ST    R2,FADDR                                                         
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GETFLDX                                                          
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    GETFLDX                                                          
         CLI   FLDH+5,15           NO MORE THAN 15 DIGITS                       
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'  TURN OFF NUMERIC BIT                         
         B     GETFLDX                                                          
         BCTR  R1,0                                                             
         EX    R1,FLDPACK                                                       
         CVB   R0,DUB                                                           
         SPACE 1                                                                
GETFLDX  XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
FLDPACK  PACK  DUB,FLD(0)                                                       
         EJECT                                                                  
* ERROR - SET ERROR MESSAGE AND EXIT                                            
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER OF FIELD IN ERROR)                              
*        FERN  = SYSTEM ERROR NUMBER OR X'FF' FOR USER MESSAGE                  
*        FNDX  = OPTIONALLY SET FIELD INDEX FOR MULTI-FIELD TWA FIELD           
*        XTRA  = OPTIONALLY CONTAINS EXTRA MESSAGE CONCATENATED TO              
*                SYSTEM ERROR MESSAGE                                           
*                                                                               
ERROR    CLI   FERN,USERERR        TEST FOR USER MESSAGE                        
         BE    ERROR6                                                           
         GOTO1 VGETMSG,DMCB+12,(FERN,BUYMSG),(X'FF',DMCB),(7,0)                 
         LA    R2,BUYMSG+L'BUYMSG-1 R2 POINTS TO END OF MSG FLD                 
         LA    R3,L'BUYMSG         CALCULATE MESSAGE LENGTH                     
         CLI   0(R2),C' '          TEST FOR BLANK                               
         BNE   *+10                                                             
         BCTR  R2,0                BACK UP POINTER                              
         BCT   R3,*-10                                                          
         LA    R2,1(R2)            POINT TO BLANK AFTER LAST CHAR               
*                                                                               
ERROR2   CLI   FNDX,0              TEST FOR INDEX NUMBER                        
         BE    ERROR4                                                           
         LA    R0,L'BUYMSG                                                      
         LA    R3,8(R3)            ADD ON LENGTH OF INDEX MSG +1                
         CR    R3,R0               TEST FOR FIT IN FIELD                        
         BH    ERROR6              NO - EXIT                                    
         LA    R2,BUYMSG-7(R3)                                                  
         MVC   0(7,R2),=C'- FLD#N'                                              
         EDIT  (B1,FNDX),(1,6(R2))                                              
         LA    R2,7(R2)            POINT TO BLANK AFTER INDEX MSG               
*                                                                               
ERROR4   CLC   XTRA,SPACES         TEST FOR ANY EXTRA MESSAGE                   
         BE    ERROR6                                                           
         LA    RE,XTRA+L'XTRA-1                                                 
         LA    R1,L'XTRA           CALCULATE LENGTH OF EXTRA MESSAGE            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LA    R0,L'BUYMSG                                                      
         LA    R3,1(R1,R3)         COMPUTE TOTAL MESSAGE LENGTH                 
         CR    R3,R0               TEST FOR FIT                                 
         BH    ERROR6                                                           
         BCTR  R1,0                LESS ONE FOR EXECUTE                         
         EX    R1,*+8              MOVE XTRA TO MESSAGE FIELD                   
         B     ERROR6                                                           
         MVC   1(0,R2),XTRA        EXECUTED                                     
*                                                                               
ERROR6   B     EXIT                                                             
         EJECT                                                                  
* EXIT - SET CURSOR AND EXIT DIRECTLY TO USER                                   
*                                                                               
EXIT     L     R2,FADDR            SET CURSOR                                   
         OI    6(R2),X'C0'         CURSOR AND TRANSMIT BITS                     
         L     RD,AWORK            RESTORE ROOT'S RD                            
         B     EXXMOD                                                           
         EJECT                                                                  
* CLEARF - CLEAR AND FOUT FIELDS                                                
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLEARF   LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     EXXMOD              YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
* GETPROG - GET APPLICABLE PROGRAM RECORD IF NECESSARY                          
*                                                                               
* AT ENTRY P1 CONTAINS A(COMPRESSED DATE FOR READ)                              
*                                                                               
GETPROG  L     RE,0(R1)            POINT TO DATE                                
         ICM   R0,3,0(RE)          DATE                                         
         STH   R0,HALF                                                          
         XC    KEY,KEY                                                          
         MVI   FERN,0              CLEAR ERROR BYTE ON ENTRANCE                 
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,AGYMED                                                    
         MVC   NPGKNET,NETMARK                                                  
         MVC   NPGKPROG,PROG                                                    
         MVC   NPGKEND,HALF                                                     
         L     R2,APROGREC                                                      
         CLC   NPGKEY,0(R2)        TEST IF KEY CHANGED                          
         BE    GETPROGX            NO-ALREADY HAVE RIGHT RECORD                 
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
*                                                                               
*        CLC   KEY(L'NPGKEY),0(R2) TEST IF SAME RECORD APPLIES                  
*        BE    GETPROGX            YES                                          
GETPRG1  CLC   KEY(NPGKEND-NPGKEY),KEYSAVE TEST IF SAME PROGRAM                 
         BE    GETPRG3                                                          
*                                                                               
         MVI   FERN,PROGERR                                                     
         B     GETPROGR                                                         
*                                                                               
GETPRG2  GOTO1 AIO,DMCB,SPT+DIR+SEQ                                             
         B     GETPRG1                                                          
         SPACE                                                                  
GETPRG3  GOTO1 AIO,DMCB,SPT+FILE+GET,(R2)                                       
         SPACE                                                                  
         MVI   ELCODE,X'93'        BOOK ELEMENT SEARCH                          
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BNE   GETPRG4                                                          
         ICM   RE,15,12(R1)                                                     
         USING NPG2ELEM,RE                                                      
         OC    NPG2STD,NPG2STD                                                  
         BZ    GETPRG4                                                          
         CLC   HALF,NPG2STD      MAKE SURE INPUT DATE > PRG START DATE          
         BL    GETPRG2                                                          
         DROP  RE                                                               
*                                                                               
GETPRG4  MVI   ELCODE,X'5D'        BOOK ELEMENT SEARCH                          
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABOOKEL,12(R1)      SAVE ELEMENT ADCON                           
*                                                                               
         MVI   ELCODE,X'92'        PROGRAM ELEMENT SEARCH                       
         BAS   RE,GTPRGEL                                                       
         CLI   12(R1),0            TEST FOR SUCCESSFUL SEARCH                   
         BE    *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
         MVC   APROGEL,12(R1)      SAVE ELEMENT ADCON                           
         B     GETPROGX                                                         
         SPACE                                                                  
GETPROGR L     R3,AACTNTRY                                                      
         USING ACTTABD,R3                                                       
         TM    ACTIND,SETPROGR     TEST FOR HANDLING PROGRAM ERROR              
         BZ    GETPROGX            NO                                           
         LA    R2,BUYPRGH          YES-SET CURSOR POSITION AND EXIT             
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
GETPROGX B     EXXMOD                                                           
         DROP  R3,R4                                                            
*                                                                               
*  GET PROGRAM RECORD ELEMENTS                                                  
GTPRGEL  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(ELCODE,(R2)),0                       
         B     EXXMOD                                                           
         EJECT                                                                  
* DISPROG - DISPLAY PROGRAM RECORD DATA ON BASE SCREEN                          
*                                                                               
* ON ENTRY - APROGEL POINTS TO PROGRAM ELEMENT                                  
*                                                                               
DISPROG  OC    BUYPRGD,BUYPRGD     TEST IF FIELD HAS BEEN DONE                  
         BNZ   DISPROGX            YES                                          
         SPACE 1                                                                
DISPROG2 L     R4,APROGEL          ELEMENT ADDRESS                              
         USING NPGELEM,R4                                                       
         MVC   BUYPRGD,SPACES      PRE-CLEAR FIELD TO SPACES                    
         MVC   BUYPRGD(L'NPGNAME),NPGNAME                                       
         MVC   DUB,SPACES          GET DAY(S)                                   
         GOTO1 VUNDAY,DMCB,NPGDAY,DUB                                           
         MVC   BUYPRGD+22(8),DUB                                                
         MVC   WORK(11),SPACES     GET START/END TIME                           
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         MVC   BUYPRGD+31(11),WORK                                              
         LA    R2,BUYPRGD+43       POINT TO START OF R1/S1 OUTPUT               
         TM    NPGSTAT,X'80'       TEST FOR HOMES RATING                        
         BZ    *+12                NO                                           
         MVI   0(R2),C'R'          YES-PUT RATING PREFIX OUT                    
         LA    R2,1(R2)                                                         
         EDIT  (B2,NPGSHARE),(4,0(R2)),1,ALIGN=LEFT                             
         SPACE 1                                                                
DISPROGX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* BLDFLD - DYNAMICALLY BUILD TWA FIELDS                                         
*                                                                               
* ON ENTRY -                                                                    
*                                                                               
* BLDROW HAS START ROW, BLDCOL HAS START COLUMN, BLDTWAX POINTS TO              
* END OF AVAILABLE TWA, BLDTWAS CAN BE OPTIONALLY SET TO START POINT            
* FOR BUILDING FIELDS IN TWA - THE DEFAULT IS BUYLAST                           
*                                                                               
* EACH BLDLIST ENTRY IS 8 BYTES.                                                
*  BYTE 0 = NUMBER OF LEADING SPACES                                            
*       1 = DATA LENGTH                                                         
*       2 = ATTRIBUTE BYTE (PROT/UNP)                                           
*       3 = NUMBER OF TRAILING SPACES                                           
*       4 = SPARE                                                               
*     5-7 = DATA ADDRESS (OR 0)                                                 
*                                                                               
*  ALL FIELDS MUST FIT THIS LINE OR USE NEXT LINE.                              
* XL4'00' IS E-O-L                                                              
*                                                                               
* ON EXIT CC IS = IF FIELD(S) ADDED TO TWA, NEQ IF UNSUCCESSFUL                 
* AND R2 POINTS TO END OF SCREEN                                                
*                                                                               
BLDFLD   OC    BLDROW,BLDROW       TEST FOR START AT DEFAULT POSITION           
         BNZ   BLDF1                                                            
         LA    R2,BUYLAST          POSITION TO END OF BASE SCREEN               
         MVC   BLDROW,=H'9'                                                     
         XC    BLDCOL,BLDCOL                                                    
         B     BLDF2                                                            
*                                                                               
BLDF1    SR    R0,R0                                                            
         LA    R2,BUYLAST                                                       
         OC    BLDTWAS,BLDTWAS     TEST FOR OPTIONAL USER START                 
         BZ    *+8                                                              
         L     R2,BLDTWAS                                                       
         CLI   0(R2),0             FIND THE CURRENT E-O-S                       
         BE    BLDF2                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-14                                                             
         SPACE                                                                  
* CALCULATE LENGTH NEEDED FOR FIELDS AND CHECK IF IT FITS ON                    
* CURRENT SCREEN LINE                                                           
*                                                                               
BLDF2    SR    R0,R0                                                            
         SR    RE,RE                                                            
         LA    R5,BLDLIST                                                       
*                                                                               
BLDF3    IC    R0,0(R5)            LEADING SPACES                               
         AR    RE,R0                                                            
         IC    R0,1(R5)            DATA LEN                                     
         AR    RE,R0                                                            
         LA    R0,2                UNPROTECTED FIELDS NEED 2 BYTES 0/H          
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0                                                            
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   BLDF3               NO                                           
*                                                                               
         LH    R0,BLDCOL                                                        
         AR    R0,RE                                                            
         CH    R0,=H'80'           TEST IF FITS ON LINE                         
         BL    BLDF4               YES                                          
         SPACE                                                                  
* TRY IGNORING LAST TRAILING SPACES                                             
         SH    R5,=H'8'                                                         
         ZIC   R6,3(R5)                                                         
         SR    R0,R6                                                            
         CH    R0,=H'80'                                                        
         BNH   BLDF4               FITS ON LINE                                 
         SPACE                                                                  
* WILL NOT FIT THIS ROW - TRY NEXT                                              
         XC    BLDCOL,BLDCOL                                                    
         LH    R6,BLDROW                                                        
         LA    R6,1(R6)                                                         
         STH   R6,BLDROW                                                        
         CH    R6,=H'24'           TEST IF PAST E-O-S                           
         BNL   BLDFNEX             YES                                          
         SPACE                                                                  
* MAKE SURE DATA FITS IN TWA. RE HAS DATA LEN                                   
*                                                                               
BLDF4    LA    R5,BLDLIST                                                       
         LA    RE,8(RE)            ADD 8 FOR EACH FLDHDR                        
         LA    R5,8(R5)                                                         
         OC    0(4,R5),0(R5)                                                    
         BNZ   *-14                                                             
         LA    RE,3(RE)            ADD 3 FOR GENCER INDICS                      
         AR    RE,R2                                                            
         C     RE,BLDTWAX          TEST AGAINST END OF TWA                      
         BNL   BLDFNEX             WILL NOT FIT                                 
         SPACE                                                                  
* NOW START BUILDING FIELDS FROM LIST ENTRIES                                   
*                                                                               
         LA    R5,BLDLIST          INITIALIZE LIST POINTER                      
*                                                                               
BLDF6    XC    0(8,R2),0(R2)       CLEAR FIELD HEADER AREA                      
         OI    6(R2),X'80'         SET XMIT BIT                                 
         OI    4(R2),X'20'         SET 'EDITED' FLAG                            
         ZIC   RE,1(R5)            GET FIELD LEN                                
         STC   RE,7(R2)            SET IT                                       
         OI    7(R2),X'80'         SET SPECIAL FLAG FOR XLTR                    
         BCTR  RE,0                                                             
         L     RF,4(R5)            GET DATA ADDR                                
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,SPACES                                                        
         EX    RE,BLDFMVC                                                       
         EX    RE,BLDFOC                                                        
         SPACE                                                                  
BLDF8    LA    RE,9(RE)            RESTORE TO LEN+8                             
         STC   RE,0(R2)            SET LEN+8                                    
         SPACE                                                                  
* BUILD OUTPUT FIELD ADDRESS                                                    
         LH    RE,BLDROW                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'80'                                                        
         AH    RE,BLDCOL                                                        
         ZIC   R0,0(R5)            ADD LEADING SPACES                           
         AR    RE,R0                                                            
         TM    2(R5),X'20'         TEST PROTECTED                               
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STH   RE,2(R2)                                                         
         MVC   1(1,R2),2(R5)       SET PRO/UNP IND                              
         TM    2(R5),X'20'         TEST PROT                                    
         BO    *+8                                                              
         OI    1(R2),X'08'         SET HIGH INTENSITY FOR UNP FIELDS            
         ZIC   RE,0(R2)            GET FIELD LEN                                
         AR    R2,RE               POINT TO NEW END OF SCREEN                   
         SPACE                                                                  
* UPDATE COLUMN                                                                 
         ZIC   RE,0(R5)            LEADING SPACES                               
         ZIC   R0,1(R5)            DATA LENGTH                                  
         AR    RE,R0                                                            
         LA    R0,2                                                             
         TM    2(R5),X'20'         TEST PROTECTED                               
         BZ    *+6                                                              
         SR    R0,R0                                                            
         AR    RE,R0               + OVERHEAD                                   
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         AH    RE,BLDCOL           + PREVIOUS COL                               
         STH   RE,BLDCOL                                                        
*                                                                               
         LA    R5,8(R5)            NEXT FIELD ENTRY                             
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   BLDF6               YES                                          
         XC    0(3,R2),0(R2)       SET E-O-S FLAG                               
         CR    RB,RB               SET CC =                                     
         B     BLDFX                                                            
         SPACE                                                                  
BLDFNEX  LTR   RB,RB               FORCE CC TO NOT =                            
         SPACE                                                                  
BLDFX    XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
BLDFMVC  MVC   8(0,R2),0(RF)                                                    
BLDFOC   OC    8(0,R2),SPACES                                                   
         EJECT                                                                  
* CENFLD - CENTRALIZE FIELDS ON SCREEN LINE                                     
*                                                                               
* ROUTINE ASSUMES THAT ONE OR MORE ITERATIONS OF FIELDS DESCRIBED               
* IN BLDLIST WILL COMPOSE SCREEN LINE.  LEADING AND TRAILING SPACES             
* BETWEEN EACH FIELD OR FIELD PAIR WILL BE CALCULATED TO SPREAD THE             
* FIELD(S) OUT EVENLY OVER LINE                                                 
*                                                                               
* ON EXIT, CC IS = IF FIELDS DO NOT FIT ON LINE, NEQ IF FIELDS FIT              
*                                                                               
CENFLD   SR    R0,R0                                                            
         SR    RE,RE               CLEAR ACCUM FOR TOTAL LEN NEEDED             
         SR    R3,R3               CLEAR ACCUM OF NUM OF ITERATIONS             
         LA    R5,BLDLIST                                                       
         SPACE                                                                  
* CALCULATE TOTAL LENGTH NEEDED FOR BLDLIST FIELDS                              
*                                                                               
CENFLD1  IC    R0,0(R5)            LEADING SPACES                               
         AR    RE,R0                                                            
         IC    R0,1(R5)            DATA LENGTH                                  
         AR    RE,R0                                                            
         LA    R0,2                2 BYTES OVERHEAD FOR UNPROTECTED             
         TM    2(R5),X'20'         TEST IF PROTECTED                            
         BZ    *+6                 NO                                           
         SR    R0,R0               YES                                          
         AR    RE,R0                                                            
         IC    R0,3(R5)            TRAILING SPACES                              
         AR    RE,R0                                                            
         LA    R5,8(R5)            NEXT BLDLIST ENTRY                           
         OC    0(4,R5),0(R5)       TEST FOR E-O-L                               
         BNZ   CENFLD1             NO                                           
         SPACE                                                                  
CENFLD2  CH    RE,=H'80'           TEST IF FITS ON LINE                         
         BL    CENFLD4             YES                                          
         SH    R5,=H'8'            BACK UP TO LAST ENTRY                        
         IC    R0,3(R5)            AND TRY REMOVING TRAILING SPACES             
         SR    RE,R0                                                            
         CH    RE,=H'80'                                                        
         BH    CENFLDX             WILL NOT FIT ON LINE                         
         SPACE                                                                  
* FIND NUM OF ITERATIONS OF BLDLIST ON LINE - THEN EXCESS                       
*                                                                               
CENFLD4  SR    R2,R2               PREPARE DIVIDEND                             
         LA    R3,80               LENGTH OF LINE                               
         DR    R2,RE               LEN OF LINE/LEN OF FIELDS                    
         LTR   R5,R2               EXCESS ON LINE                               
         BZ    CENFLDX             NONE                                         
         SR    R4,R4               PREPARE DIVIDEND                             
         DR    R4,R3               EXCESS BYTES/NUMBER OF ITERATIONS            
         LR    R4,R5               R5 CONTAINS EXCESS PER FIELD                 
         SRL   R4,1                R4 IS BYTES BEFORE FIELD(S)                  
         SR    R5,R4               REST IS BYTES AFTER FIELD(S)                 
         SPACE                                                                  
* UPDATE BLDLIST ENTRIES INSERTING EXTRA LEADING AND TRAILING SPACES            
*                                                                               
CENFLD6  LA    R1,BLDLIST                                                       
         IC    R0,0(R1)            LEADING SPACES                               
         AR    R0,R4               ADD IN BLANKS TO SPACE IT OUT                
         STC   R0,0(R1)                                                         
         LA    R1,8(R1)            NEXT FIELD                                   
         OC    0(4,R1),0(R1)       TEST FOR E-O-L                               
         BNZ   *-10                NO                                           
         SH    R1,=H'8'                                                         
         IC    R0,3(R1)            TRAILING SPACES                              
         AR    R0,R5                                                            
         STC   R0,3(R1)                                                         
         SPACE                                                                  
CENFLDX  LTR   R3,R3               SET CC FOR EXIT                              
         B     EXXMOD                                                           
         EJECT                                                                  
*--READ NAD DEFINITION RECORD THROUGH THE PROGRAM RECORD                        
*--AND FILTER OUT ALL NAD DEMOS THAT ARE NOT DEFINED ON                         
*--THE NAD DEFINITION RECORD                                                    
* SUB-ROUTINE TO READ NADDEF RECORD AND RETRIEVE THE NAD DEMOS                  
*                                                                               
GETNADS  L     R3,APROGREC                                                      
         USING NPGRECD,R3                                                       
*                                                                               
         XC    HUTBLK,HUTBLK                                                    
         CLC   NPGKTYP(2),=XL2'0D20'   WAS PROGRAM KEY READ                     
         BNE   GETNDEX             NO EXIT                                      
*                                                                               
         USING NPGEL03,R2                                                       
         LA    R2,NPGMAINL                                                      
         ZIC   RE,1(R2)            TEST IF NAD ELEMENT FOUND                    
         AR    R2,RE               NO                                           
         CLI   0(R2),X'03'                                                      
         BNE   GETND25                                                          
         OC    NPGNADDM,NPGNADDM   NO NAD DEF RECORD                            
         BZ    GETND25             BYPASS ALL NADS ON ESTIMATE                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0D16'                                                
         MVC   KEY+2(1),AGYMED                                                  
         MVC   KEY+3(6),NPGNADDM   NAD DEFINITION CODE                          
         MVC   KEYSAVE(13),KEY                                                  
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH    RE-READ AND REPLACE                     
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETNDEX                                                          
         GOTO1 (RF),(R1),SPT+FILE+GET,AIOAREA4                                  
*                                                                               
         USING NNDRECD,R3                                                       
         L     R3,AIOAREA4                                                      
         LA    R2,NNDMAINL                                                      
         XC    HUTBLK,HUTBLK                                                    
         LA    RE,HUTBLK                                                        
*                                                                               
GETND20  ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BE    GETND25                                                          
         CLI   0(R2),X'DD'                                                      
         BNE   GETND20                                                          
         MVC   0(3,RE),3(R2)                                                    
         LA    RE,3(RE)                                                         
         B     GETND20                                                          
*                                                                               
GETND25  LA    RE,ESTDEMS                                                       
         ZIC   RF,ESTNDEMS                                                      
         LA    R4,FLD                                                           
         SR    R1,R1                                                            
*                                                                               
GETND30  CLI   0(RE),0             IS DEMO NAD                                  
         BE    GETND50             NO BYPASS                                    
         CLI   0(RE),171           IS DEMO TVQ                                  
         BE    GETND50             YES BYPASS NAD CHECK                         
         LA    R2,HUTBLK                                                        
         LA    R3,40                                                            
GETND35  OC    0(3,R2),0(R2)                                                    
         BZ    GETND55                                                          
         CLC   0(1,RE),0(R2)                                                    
         BNE   GETND40                                                          
         CLC   2(1,RE),2(R2)                                                    
         BE    GETND50                                                          
GETND40  LA    R2,3(R2)                                                         
         BCT   R3,GETND35                                                       
         B     GETND55                                                          
GETND50  MVC   0(3,R4),0(RE)                                                    
         LA    R4,3(R4)                                                         
         LA    R1,1(R1)                                                         
GETND55  LA    RE,3(RE)            GET NEXT ESTLIST DEMO                        
         BCT   RF,GETND30                                                       
         STC   R1,ESTNDEMS                                                      
*                                                                               
         XC    ESTDEMS,ESTDEMS                                                  
         ZIC   RF,ESTNDEMS                                                      
         LTR   RF,RF               ANY DEMOS QUALIFY                            
         BZ    GETNDEX             NO EXIT                                      
         LA    R1,ESTDEMS                                                       
         LA    R2,FLD                                                           
*                                                                               
GETND60  MVC   0(3,R1),0(R2)                                                    
         LA    R1,3(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   RF,GETND60                                                       
*                                                                               
         SPACE 1                                                                
GETNDEX  XC    HUTBLK,HUTBLK                                                    
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* CKREASN - CHECKS THAT REASON CODE IS VALID                                    
*                                                                               
* AT ENTRY P1 CONTAINS A(4 BYTE REASON CODE)                                    
*                                                                               
CKREASN  L     RE,0(R1)            POINT TO DATE                                
         ICM   R0,15,0(RE)                                                      
         LA    R4,KEY                                                           
         USING RSNRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=X'0D77'                                                
         MVC   RSNKAGY,AGENCY                                                   
         MVI   RSNKMED,C'N'                                                     
         STCM  R0,15,RSNKCODE                                                   
         OC    RSNKCODE,SPACES                                                  
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
                                                                                
         CLC   KEY(9),KEYSAVE      TEST IF SAME RECORD APPLIES                  
         BE    CKREASNX            YES                                          
         MVI   FERN,INVREASN                                                    
         B     ERROR                                                            
CKREASNX B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* GENERATE A TUNAROUND REQUEST ON BU CHANGES                                    
*                                                                               
*                                                                               
BLDRQST  CLC   CLIFPGRP,SPACES                                                  
         BNH   BLDRQEX                                                          
         CLC   =C'G7',AGYALPH                                                   
         BNE   BLDRQ4                                                           
         TM    ESTFLAG1,EF1REQ     TEST ESTIMATE REQUESTABLE                    
         BZ    BLDRQEX                                                          
*                                                                               
BLDRQ4   XC    HUTBLK(26),HUTBLK                                                
         MVC   HUTBLK+26(80),SPACES                                             
         MVI   HUTBLK+14,106                                                    
         LA    R4,HUTBLK+26                                                     
         MVC   0(2,R4),=C'RF'                                                   
         MVC   2(2,R4),AGYALPH                                                  
         MVI   4(R4),C'N'                                                       
         MVC   5(3,R4),CLI                                                      
         MVC   11(3,R4),=C'POL'                                                 
         MVC   18(4,R4),NET                                                     
         MVI   22(R4),C'N'                                                      
         EDIT  (1,EST),(3,23(R4)),FILL=0                                        
         MVC   49(8,R4),CLIFPGRP                                                
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',HUTBLK,HUTBLK                
*                                                                               
BLDRQEX  B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FMAX  = MAXIMUM SCAN LENGTH (OPTIONALLY SET BY USER)                   
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS PACKED VALUE OF DATA STRING FOR NUMERIC FIELD         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
*        R1    = 0 FOR NO PARAMETER LIST                                        
*                                                                               
FVAL     NTR1  BASE=BASE1                                                       
         L     RA,BASE2                                                         
         L     R7,BASE3                                                         
         XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FLEN,0              TEST IF RE-EDITING                           
         BE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         CLI   FMAX,0              TEST FOR USER SCAN LIMIT                     
         BE    *+8                 NO                                           
         IC    R3,FMAX             YES-USE IT IN PLACE OF DERIVED LIMIT         
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         BNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R0,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
         LR    R3,R0               SET REAL DATA LENGTH                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
FVAL12   CLI   0(RE),C'A'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'I'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'J'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'R'                                                       
         BNH   FVAL14              VALID ALPHA                                  
         CLI   0(RE),C'S'                                                       
         BL    FVAL13              NOT ALPHA                                    
         CLI   0(RE),C'Z'                                                       
         BNH   FVAL14              VALID ALPHA                                  
*                                                                               
FVAL13   NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
*                                                                               
FVAL14   CLI   0(RE),C'0'          TEST IF NUMERIC                              
         BL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL12                                                        
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         CLI   FLDH+5,15                                                        
         BNH   *+12                                                             
         NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     FVALX                                                            
         PACK  DUB,FLD(0)          EXECUTED                                     
*                                                                               
FVALX    MVI   FMAX,0              ALWAYS CLEARED BY FVAL                       
         B     EXXMOD                                                           
         EJECT                                                                  
* I/O CONTROLLER                                                                
*     ON ENTRY   KEY      = CONTAINS KEY FOR READ                               
*                NDXDA    = CONTAINS DISK ADDRESS FOR D/A FILE READ             
*                                                                               
*     PARAMETERS                                                                
*                P1       = I/O MASK - COMMAND/FILE/DIRECTORY OR                
*                           FILE/CONTROL SETTINGS                               
*                P2       = BYTES 1-3 - A(I/O AREA) FOR FILE READ               
*                                                                               
*     AFTER I/O                                                                 
*                IOFLAG   = 0 OR ERROR SETTING                                  
*                KEYSAVE  = KEY PASSED TO ROUTINE                               
*                KEY      = CONTAINS KEY RETURNED BY DATAMGR                    
*                AIOAREA  = A(I/O AREA) FOR RECORD                              
*                                                                               
IO       NTR1  BASE=BASE1,WORK=(RC,IOWORKX-IOWORKD)                             
         L     RA,BASE2            SECOND BASE REGISTER                         
         L     R7,BASE3            THIRD BASE REGISTER                          
         USING IOWORKD,RC          LOCAL STORAGE                                
         LM    R2,R3,0(R1)         I/O MASK AND I/O AREA                        
         STC   R2,IOWORK1          LOW ORDER BYTE OF I/O MASK                   
         MVC   IOWORK2,IOWORK1     SAVE LOW ORDER BYTE                          
         STCM  R2,2,IOWORK3        SAVE THIRD BYTE                              
         NI    IOWORK2,X'0F'       ISOLATE COMMAND NUMBER                       
         BAS   RE,SETCTL           SET CONTROL BIT                              
         ZIC   R1,IOWORK2                                                       
         SLL   R1,3                MULTIPLY BY 8 TO DEVELOP INDEX               
         LA    RE,CMNDTAB-L'CMNDTAB(R1) INDEX INTO COMMAND TABLE                
         MVC   IOCMND,0(RE)        EXTRACT COMMAND NAME                         
         SPACE 1                                                                
IO2      MVC   IOWORK2,IOWORK1     REFRESH FLAG VALUES                          
         NI    IOWORK2,X'70'       ISOLATE FILE NUMBER                          
         ZIC   R1,IOWORK2          FILE NUMBER                                  
         SRL   R1,4                DIVIDE BY 8 TO DEVELOP INDEX                 
         LA    R0,L'FILTAB                                                      
         MR    R0,R0                                                            
         LA    RE,FILTAB-L'FILTAB(R1)                                           
         MVC   IODIR,0(RE)         EXTRACT DIRECTORY NAME                       
         MVC   IOFILE,7(RE)        FILE NAME                                    
         MVC   IOVALS,14(RE)                                                    
         BAS   RE,SETSTAT          SET LAST STATUS LOG                          
         ZIC   R1,IOWORK1          TEST IOFLAG VS. EXCEPTION VALS               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    IOEXCPT,0                                                        
         BZ    *+6                                                              
         DC    H'0'                YES-BLOW UP - CHECK IOFLAG                   
         TM    IOWORK3,X'01'       TEST FOR PASS DELETES                        
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'08'                                                   
         TM    IOWORK3,X'02'       TEST FOR READ FOR UPDATE                     
         BZ    *+8                 NO                                           
         OI    DMINBITS,X'80'                                                   
         TM    IOWORK1,DIR         TEST FOR DIRECTORY READ                      
         BZ    IO6                                                              
         SPACE 1                                                                
IO4      MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IODIR,KEY,KEY                  
         LA    RE,KEY                                                           
         ZIC   R0,IODADSP                                                       
         AR    RE,R0               POINT TO DISK ADDRESS                        
         MVC   NDXDA,0(RE)                                                      
         MVC   IOFLAG,8(R1)                                                     
         B     IOX                                                              
         SPACE 1                                                                
IO6      ST    R3,AIOAREA                                                       
         LA    R2,NDXDA            POINT AT DISK ADDRESS                        
         TM    IOEXCPT,DIR         TEST FOR FILE W DIRECTORY                    
         BZ    IO7                                                              
         LA    R2,KEY              FOR IS FILE, POINT TO KEY                    
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
IO7      GOTO1 VDATAMGR,IOPARM,(DMINBITS,IOCMND),IOFILE,(R2),AIOAREA,  X        
               DMWORK                                                           
         MVC   IOFLAG,8(R1)                                                     
         TM    IOEXCPT,DIR         TEST FOR IS FILE                             
         BZ    IOX                 NO                                           
         L     RF,AIOAREA                                                       
         ZIC   R1,IOKEYL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     IOX                                                              
         MVC   KEY(0),0(RF)        EXTRACT KEY FROM RECORD                      
         SPACE 1                                                                
IOX      MVI   DMINBITS,0                                                       
         TM    IOFLAG,X'FD'        TEST FOR DATAMGR ERROR                       
         BZ    IOXX                NONE                                         
         MVI   FERN,0                                                           
         TM    IOFLAG,X'20'        TEST FOR DUPLICATE KEY ON ADD                
         BZ    ERROR               NO-LET GETMSG SORT IT OUT                    
         DC    H'0'                YES-DUMP TO INSURE PROPER RECOVERY           
*                                                                               
IOXX     MVI   IOFLAG,0                                                         
         B     EXXMOD                                                           
         SPACE 4                                                                
*--SET POSTING TYPE IN THE STATUS FIELD                                         
SETCTL   NTR1                                                                   
*--TEST FOR UPDATE COMMAND                                                      
         L     R3,AIOAREA          SET TO FILE AREA                             
         LA    R3,2(R3)       ADJUST SO KEY AND FILE AT THE SAME PLACE          
         CLI   IOWORK2,PUT                                                      
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADDREC                                                   
         BE    SETCTL20                                                         
         LA    R3,KEY                                                           
         CLI   IOWORK2,WRITE                                                    
         BE    SETCTL20                                                         
         CLI   IOWORK2,ADD                                                      
         BNE   SETCTLEX                                                         
SETCTL20 CLI   NBPOSTYP,C'N'       NETWORK                                      
         BNE   *+12                                                             
         NI    20(R3),X'FC'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'C'       CABLE                                        
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'01'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+16                                                             
         NI    20(R3),X'FC'                                                     
         OI    20(R3),X'02'                                                     
         B     SETCTLEX                                                         
         CLI   NBPOSTYP,C'O'       OTHER                                        
         BNE   *+12                                                             
         NI    20(R3),X'FC'        OTHER                                        
         OI    20(R3),X'03'                                                     
SETCTLEX B     EXXMOD                                                           
         SPACE 4                                                                
*--SET LAST ACTIVITY BIT SETTINGS IN NEW LOG ELEMENT                            
*--R3 POINTS TO RECORD                                                          
SETSTAT  NTR1                                                                   
         USING NURECD,R3                                                        
*--CHECK IF WE ARE UPDATING A UNIT RECORD                                       
         CLC   IOFILE(3),=CL3'UNT'                                              
         BNE   SETSTEX                                                          
         CLC   IOCMND(6),=CL6'ADDREC'                                           
         BE    SETST040                                                         
         CLC   IOCMND(6),=CL6'PUTREC'                                           
         BNE   SETSTEX                                                          
SETST040 CLI   0(R3),X'04'                                                      
         BNE   SETSTEX                                                          
*                                                                               
SETST050 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
*--CHECK IF ANY CHANGES WERE MADE                                               
         CLI   NUACTWHY,0                                                       
         BNE   SETST060                                                         
         CLI   NUACT2WY,0                                                       
         BE    SETSTEX                                                          
*                                                                               
SETST060 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'68',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    SETST070                                                         
         CLI   12(R1),X'06'        ELEMENT NOT FOUND                            
         BE    SETST200                                                         
         DC    H'0'                                                             
*                                                                               
SETST070 L     R2,12(R1)                                                        
         USING NUATV,R2                                                         
         XC    WORK(62),WORK                                                    
         MVC   WORK(2),0(R2)                                                    
         MVC   WORK+8(54),NUATVINF                                              
         MVC   0(62,R2),WORK                                                    
         MVC   NUATVINF(2),TODAYC                                               
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         B     SETSTEX                                                          
         DROP  R2                                                               
*                                                                               
SETST200 XC    WORK(62),WORK                                                    
         LA    R2,WORK                                                          
         USING NUATV,R2                                                         
         MVC   NUATVEL(2),=XL2'683E'                                            
         MVC   NUATVINF(2),TODAYC                                               
         MVC   NUATVINF+2(1),NUACTWHY                                           
         MVC   NUATVINF+3(1),NUACT2WY                                           
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'68',(R3)),(R2),0                   
         B     SETSTEX                                                          
*                                                                               
*--FIRST TWO POSITIONS OF SPACES MUST BE RESET,BECUASE OF                       
*--OVERFLOW IN THE FIELD "WORK".                                                
SETSTEX  MVC   SPACES(2),=XL2'4040'                                             
         B     EXXMOD                                                           
         DROP  R2,R3,R4,RC                                                      
         EJECT                                                                  
* RETURN FROM MODULE AND ROUTINES                                               
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL16'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
         SPACE 2                                                                
* TABLE OF CORE-RESIDENT MODULE ADDRESSES                                       
*                                                                               
CORETAB  DS    0X                                                               
         DC    X'030E0F111415E0272817323312'                                    
CORES    EQU   (*-CORETAB)                                                      
         SPACE 2                                                                
* TABLE OF BASE PROVIDED MODULES AND FACILITIES                                 
*                                                                               
BASETAB  DS    0F                                                               
         DC    A(0)                                                             
         DC    A(IO)                                                            
         DC    A(FVAL)                                                          
         DC    5A(0)                                                            
BASETABC EQU   (*-BASETAB)/L'BASETAB                                            
         SPACE 2                                                                
* TABLE OF I/O COMMANDS                                                         
*                                                                               
CMNDTAB  DS    0CL8                                                             
         DC    CL8'DMREAD'                                                      
         DC    CL8'DMRSEQ'                                                      
         DC    CL8'DMRDHI'                                                      
         DC    CL8'DMWRT'                                                       
         DC    CL8'DMADD'                                                       
         DC    CL8'GETREC'                                                      
         DC    CL8'PUTREC'                                                      
         DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
* TABLE OF FILES/DIRECTORIES AND THEIR ATTRIBUTES                               
*                                                                               
FILTAB   DS    0XL17                                                            
         DC    CL7'SPTDIR',CL7'SPTFILE',AL1(0,13,14)                            
         DC    CL7'UNTDIR',CL7'UNTFIL',AL1(0,20,21)                             
         DC    CL7'STATION',CL7'STATION',AL1(DIR,17,0)                          
         DC    CL7'TRFDIR',CL7'TRFFILE',AL1(0,13,14)                            
         EJECT                                                                  
* TABLE OF ACTIONS AND CONTROL VALUES                                           
*                                                                               
ACTTAB   DS    0H                                                               
         DC    CL6'BP'             BUY PACKAGE                                  
         DC    X'02',AL1(BP),X'10',X'FD'                                        
         DC    AL1(FSTCURS)        INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(PASSEQ+LOKCHEK)                                              
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'BUY (ADD) A PACKAGE'                                        
*                                                                               
         DC    CL6'DP'             DISPLAY PACKAGE                              
         DC    X'02',AL1(DP),X'10',X'FD'                                        
         DC    AL1(READONLY)      INDICATORS                                    
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DISPLAY PACKAGE'                                            
*                                                                               
         DC    CL6'CP'             CHANGE PACKAGE                               
         DC    X'02',AL1(CP),X'10',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'CHANGE PACKAGE'                                             
*                                                                               
         DC    CL6'DELP'           DELETE PACKAGE                               
         DC    X'04',AL1(DELP),X'10',X'FD'                                      
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DELETE PACKAGE'                                             
*                                                                               
         DC    CL6'COPYP'          COPY PACKAGE                                 
         DC    X'05',AL1(COPYP),X'10',X'FD'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'COPY A PACKAGE'                                             
*                                                                               
         DC    CL6'B'              BUY UNIT                                     
         DC    X'01',AL1(B),X'12',X'FC'                                         
         DC    AL1(FSTCURS+SETPROGR)   INDICATORS                               
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' ' PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'BUY A UNIT (MAKE-GOOD)'                                     
*                                                                               
         DC    CL6'BU'             BUY UNIT (UPLOAD)                            
         DC    X'01',AL1(B),X'12',X'FC'                                         
         DC    AL1(FSTCURS+SETPROGR)   INDICATORS                               
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' ' PROGRAM                                      
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'BUY A UNIT (MAKE-GOOD)'                                     
*                                                                               
         DC    CL6'D'              DISPLAY UNIT                                 
         DC    X'01',AL1(D),X'12',X'FC'                                         
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY SINGLE UNIT'                                        
*                                                                               
         DC    CL6'C'              CHANGE UNIT                                  
         DC    X'01',AL1(C),X'12',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
         DC    CL6'CU'             CHANGE UNIT (UPLOAD)                         
         DC    X'01',AL1(C),X'12',X'FC'                                         
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SINGLE UNIT'                                         
*                                                                               
*        DC    CL6'CA'             CHANGE UNIT                                  
*        DC    X'02',AL1(C),X'12',X'FC'                                         
*        DC    AL1(SETPROGR)       INDICATORS                                   
*        DC    AL1(ISINGLE),XL2'00' CLIENT                                      
*        DC    AL1(ISINGLE),XL2'00'                                             
*        DC    AL1(ISINGLE),XL4'00'                                             
*        DC    AL1(ISINGLE),X'00'                                               
*        DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
*        DC    AL1(LOKCHEK)                                                     
*        DC    XL2'00'                                                          
*        DC    CL8'DATE'                                                        
*        DC    CL22'CHANGE UNIT ACCOUNTING'                                     
*                                                                               
         DC    CL6'DEL'            DELETE UNIT                                  
         DC    X'03',AL1(DEL),X'12',X'FC'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DELETE A UNIT'                                              
*                                                                               
         DC    CL6'DD'             DEMO DISPLAY (VPH BASED)                     
         DC    X'02',AL1(DD),X'13',X'FB'                                        
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE)         VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY UNIT DEMOS'                                         
*                                                                               
         DC    CL6'DD'             DEMO DISPLAY (IMPRESSION BASED)              
         DC    X'02',AL1(DD),X'28',X'F1'                                        
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY UNIT DEMOS'                                         
*                                                                               
         DC    CL6'CD'             DEMO CHANGE (VPH BASED)                      
         DC    X'02',AL1(CD),X'13',X'FB'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE+LOKCHEK) VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE UNIT DEMOS'                                          
*                                                                               
         DC    CL6'CD'             DEMO CHANGE (IMPRESSION BASED)               
         DC    X'02',AL1(CD),X'28',X'F1'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE UNIT DEMOS'                                          
*                                                                               
         DC    CL6'CAL'            CALENDARIZE                                  
         DC    X'03',AL1(CAL),X'14',X'FA'                                       
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'CALENDARIZATION'                                            
*                                                                               
         DC    CL6'BM'             MULTIPLE BUY                                 
         DC    X'02',AL1(BM),X'14',X'FA'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATES'                                                       
         DC    CL22'BUY MULTIPLE UNITS'                                         
*                                                                               
         DC    CL6'DM'             DISPLAY MULTIPLE                             
         DC    X'02',AL1(DM),X'15',X'00'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE)         VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'DISPLAY MULTIPLE UNITS'                                     
*                                                                               
         DC    CL6'DM'             DISPLAY MULTIPLE                             
         DC    X'02',AL1(DM),X'29',X'00'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'DISPLAY MULTIPLE UNITS'                                     
*                                                                               
         DC    CL6'CM'             CHANGE MULTIPLE                              
         DC    X'02',AL1(CM),X'15',X'00'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(VPHBASE+LOKCHEK) VPH BASED PROGRAM                           
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'CHANGE MULTIPLE UNITS'                                      
*                                                                               
         DC    CL6'CM'             CHANGE MULTIPLE                              
         DC    X'02',AL1(CM),X'29',X'00'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'KEYWORDS'                                                    
         DC    CL22'CHANGE MULTIPLE UNITS'                                      
*                                                                               
         DC    CL6'COPYU'          COPY UNITS                                   
         DC    X'05',AL1(COPYU),X'16',X'F9'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'COPY UNITS'                                                 
*                                                                               
         DC    CL6'PACKU'          PACKAGE UPDATE                               
         DC    X'05',AL1(PACKU),X'17',X'F6'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PACKAGE UPDATE'                                             
*                                                                               
         DC    CL6'PD'             PUP TRANSFER DISPLAY                         
         DC    X'02',AL1(PD),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(PASSEQ+PUPTRNS+LOKCHEK)                                      
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER DISPLAY'                                       
*                                                                               
         DC    CL6'PA'             PACKAGE ADD                                  
         DC    X'02',AL1(PA),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(PASSEQ+PUPTRNS+LOKCHEK)                                      
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER ADD'                                           
*                                                                               
         DC    CL6'PC'             PACKAGE CHANGE                               
         DC    X'02',AL1(PC),X'18',X'FD'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER CHANGE'                                        
*                                                                               
         DC    CL6'PT'             PUP TRANSFER                                 
         DC    X'02',AL1(PT),X'18',X'F3'                                        
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PUP TRANSFER'                                               
*                                                                               
         DC    CL6'IP'             INFO PACKAGE                                 
         DC    X'02',AL1(IP),X'20',X'F8'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE+IALL+IRANGE+DEFAULT+OPTIONAL),XL2'01FF'              
         DC    AL1(ISINGLE+IALL+DEFAULT+OPTIONAL),XL4'00'                       
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '                                               
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'PACKAGE INFO'                                               
*                                                                               
         DC    CL6'IU'             INFO UNITS                                   
         DC    X'02',AL1(IU),X'21',X'F7'                                        
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE+IALL+IRANGE+DEFAULT+OPTIONAL),XL2'01FF'              
         DC    AL1(ISINGLE+IALL+DEFAULT+OPTIONAL),XL4'00'                       
         DC    AL1(ISINGLE+IALL),X'00'                                          
         DC    AL1(ISINGLE+IALL+IFILTER),CL6' '                                 
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'UNIT/PROGRAM INFO'                                          
*                                                                               
         DC    CL6'AUDIT'          AUDIT                                        
         DC    X'01',AL1(AUDIT),X'24',X'F4'                                     
         DC    AL1(READONLY)        INDICATORS                                  
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'AUDIT TRAIL FOR UNIT'                                       
*                                                                               
         DC    CL6'CSR'            CHANGE SPECIAL RATES                         
         DC    X'02',AL1(CSR),X'26',X'F5'                                       
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00' ESTIMATE                                    
         DC    AL1(ISINGLE),XL4'00' NETWORK                                     
         DC    AL1(ISINGLE),X'00'   PACKAGE                                     
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'CHANGE SPECIAL RATE'                                        
*                                                                               
         DC    CL6'DSR'            DISPLAY SPECIAL RATES                        
         DC    X'02',AL1(DSR),X'26',X'F5'                                       
         DC    AL1(READONLY)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00' ESTIMATE                                    
         DC    AL1(ISINGLE),XL4'00' NETWORK                                     
         DC    AL1(ISINGLE),X'00'   PACKAGE                                     
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8'DATE'                                                        
         DC    CL22'DISPLAY SPECIAL RATE'                                       
*                                                                               
         DC    CL6'TRANU'          TRANSFER UNITS                               
         DC    X'05',AL1(TRANU),X'27',X'F2'                                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'TRANSFER UNITS'                                             
*                                                                               
         DC    CL6'UPLOAD'         UPLOAD UNITS                                 
         DC    X'06',AL1(UPLOAD),X'40',X'00'                                    
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(NOEDIT),XL2'00' CLIENT                                       
         DC    AL1(NOEDIT),XL2'00'                                              
         DC    AL1(NOEDIT),XL4'00'                                              
         DC    AL1(NOEDIT),X'00'                                                
         DC    AL1(NOEDIT),CL6' '   PROGRAM                                     
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'UPLOAD UNITS'                                               
*                                                                               
         DC    CL6'DPR'            ALLOCATE UNITS                               
         DC    X'03',AL1(DPR),X'42',X'E1'                                       
         DC    AL1(SETPROGR+READONLY)       INDICATORS                          
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'ALLOCATE UNITS'                                             
*                                                                               
         DC    CL6'CPR'            ALLOCATE UNITS                               
         DC    X'03',AL1(CPR),X'42',X'E1'                                       
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'ALLOCATE UNITS'                                             
*                                                                               
         DC    CL6'DH'             DISPLAY HISTORY                              
         DC    X'03',AL1(DH),X'43',X'E2'                                        
         DC    AL1(SETPROGR)       INDICATORS                                   
         DC    AL1(ISINGLE),XL2'00' CLIENT                                      
         DC    AL1(ISINGLE),XL2'00'                                             
         DC    AL1(ISINGLE),XL4'00'                                             
         DC    AL1(ISINGLE),X'00'                                               
         DC    AL1(ISINGLE),CL6' '  PROGRAM                                     
         DC    AL1(LOKCHEK)                                                     
         DC    XL2'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'DISPLAY HISTORY'                                            
*                                                                               
HELPENT  DC    CL6'HELP'           HELP                                         
         DC    X'02',AL1(HELP),X'00',X'FE'                                      
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0),XL2'00'      CLIENT                                       
         DC    AL1(0),XL2'00'                                                   
         DC    AL1(0),XL4'00'                                                   
         DC    AL1(0),X'00'                                                     
         DC    AL1(NOEDIT),CL6' '  PROGRAM                                      
         DC    XL3'00'                                                          
         DC    CL8' '                                                           
         DC    CL22'EXPLAINS ACTIONS'                                           
*                                                                               
ACTTABX  DC    X'FF'               END-OF TABLE                                 
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* HELP SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFED                                                       
         ORG   TWAD+BASETWA                                                     
SVAREA   DS    0H                                                               
SVPROF1  DS    CL16                BUY PROFILE 1 (BUYPROF)                      
SVPROF2  DS    CL16                BUY PROFILE 2 (BUYPROF2)                     
SVPROF3  DS    CL16                BUY PROFILE 3 (NETPROF)                      
*                                                                               
LSTAGY   DS    CL2                 LAST AGENCY                                  
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTOFF   DS    CL1                 LAST OFFICE                                  
         SPACE 2                                                                
LINELEN  EQU   HELLIN2H-HELLIN1H                                                
LINES    EQU   (HELLAST-HELLIN1H)/LINELEN                                       
         EJECT                                                                  
* DSECT TO COVER I/O ROUTINE LOCAL STORAGE                                      
*                                                                               
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC, PUTREC, AND ADDREC               
IOPARM   DS    6F                  PARAMETER LIST                               
IOWORK1  DS    X                                                                
IOWORK2  DS    X                                                                
IOWORK3  DS    X                                                                
IOCMND   DS    CL7                 DATA MANAGER COMMAND                         
IODIR    DS    CL7                 DIRECTORY NAME                               
IOFILE   DS    CL7                 FILE NAME                                    
IOVALS   DS    0XL3                                                             
IOEXCPT  DS    X                   EXCEPTION IOFLAG VALUES FOR FILE             
IOKEYL   DS    X                   KEY LENGTH                                   
IODADSP  DS    X                   DISPLACEMENT TO DISK ADDRESS                 
IOWORKX  EQU   *                                                                
         EJECT                                                                  
* COMFACSD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 2                                                                
* SPGENAGY (AGYHDRD)                                                            
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         SPACE 2                                                                
* SPGENCLT (CLTHDRD)                                                            
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         SPACE 2                                                                
NADHDRD  DSECT                                                                  
       ++INCLUDE SPGENNAD                                                       
         SPACE 2                                                                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         SPACE 2                                                                
* DDCOREQUS                                                                     
       ++INCLUDE DDCOREQUS                                                      
         SPACE 2                                                                
* DDOFFICED                                                                     
       ++INCLUDE DDOFFICED                                                      
         SPACE 2                                                                
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         SPACE 2                                                                
* FALOCKETD                                                                     
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'216NEBUY00S  05/01/02'                                      
         END                                                                    
