*          DATA SET SPSFM22    AT LEVEL 013 AS OF 06/14/06                      
*PHASE T21722A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21722  -- STATION LOCKIN LIST                       *         
*                                                                     *         
*  COMMENTS:     LIST STATION LOCKIN RECORDS                          *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMF2                                      *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21722 - STATION LOCKIN LIST'                                   
T21722   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1722**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
*                   SET FILE                                      *             
*******************************************************************             
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         BAS   RE,CLRNAME          CLEAR NAME FIELDS                            
*                                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         MVI   LKEYFLAG,0          LIST KEY FLAG                                
         XC    LKEYAGMD,LKEYAGMD   PART OF LIST KEY: AGENCY-MEDIA               
         XC    LKEYBCLT,LKEYBCLT   PART OF LIST KEY: CLIENT                     
         XC    LKEYMKT,LKEYMKT     PART OF LIST KEY: MARKET                     
         XC    LKEYSTA,LKEYSTA     PART OF LIST KEY: STATION                    
         XC    LKEYPRD1,LKEYPRD1   PART OF LIST KEY: PRODUCT1                   
         XC    LKEYPRD2,LKEYPRD2   PART OF LIST KEY: PRODUCT2                   
         XC    LKEYEST,LKEYEST     PART OF LIST KEY: ESTIMATE                   
*                                                                               
         LA    R2,LSTMEDH                                                       
         CLI   LSTMEDH+5,0                                                      
         BE    ERRMIS              REQUIRED                                     
         GOTO1 VALIMED                                                          
         MVC   LKEYAGMD,BAGYMD     BINARY AGENCY-MEDIA                          
         MVC   LSTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    LSTMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,LSTCLTH                                                       
         CLI   LSTCLTH+5,0                                                      
         BE    ERRMIS              REQUIRED                                     
         GOTO1 VALICLT                                                          
         MVC   LSTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    LSTCLNH+6,X'80'                                                  
         MVC   LKEYBCLT,BCLT       BINARY CLIENT                                
*                                                                               
         LA    R2,LSTMKTH                                                       
         CLI   LSTMKTH+5,0                                                      
         BE    VK20                NOT REQUIRED, CK FOR STATION                 
         GOTO1 VALIMKT                                                          
         MVC   LSTMKN,MKTNM        DISPLAY MARKET NAME                          
         OI    LSTMKNH+6,X'80'                                                  
         MVC   LKEYMKT,BMKT                                                     
         OI    LKEYFLAG,X'80'      MKT IS PART OF KEY                           
*                                                                               
VK20     LA    R2,LSTSTAH                                                       
*                                                                               
         CLI   LSTSTAH+5,0                                                      
         BE    VK30                NOT REQUIRED, CK FOR PRODUCT                 
*                                                                               
         XC    LSTMKT,LSTMKT                                                    
         OI    LSTMKTH+6,X'80'                                                  
         XC    LSTMKN,LSTMKN                                                    
         OI    LSTMKNH+6,X'80'                                                  
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   LKEYSTA,BSTA                                                     
         OI    LKEYFLAG,X'40'      STA IS PART OF KEY                           
*                                                                               
         MVC   LKEYMKT,BMKT                                                     
*                                                                               
         MVC   LSTMKT,QMKT                                                      
         OI    LSTMKTH+6,X'80'                                                  
         MVC   LSTMKN,MKTNM                                                     
         OI    LSTMKNH+6,X'80'                                                  
*                                                                               
VK30     XC    TEMPFLD,TEMPFLD     FAKE FLD FOR PRODUCT VALIDATION              
         MVI   TEMPFLD,X'0B'       MAX DATA + EXTENSION = 11                    
         LA    R2,LSTPRDH                                                       
         CLI   LSTPRDH+5,0                                                      
         BE    VK40                NOT REQUIRED, CK FOR ESTIMATE                
*                                                                               
         XC    PRD1FLD,PRD1FLD     CLEAR TO RECIEVE DATA FROM SCANNER           
         XC    PRD2FLD,PRD2FLD                                                  
         XC    BLOCK(150),BLOCK                                                 
         GOTO1 SCANNER,DMCB,LSTPRDH,(1,BLOCK),C',=,-'                           
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         USING SCANBLKD,R5                                                      
         LA    R5,BLOCK                                                         
         MVC   PRD1LEN,SC1STLEN                                                 
         MVC   PRD2LEN,SC2NDLEN                                                 
         CLI   PRD1LEN,0                                                        
         BE    ERRINV              IMPOSSIBLE                                   
         ZIC   R4,SC1STLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PRD1FLD(0),SC1STFLD                                              
VK33     CLI   PRD2LEN,0           THERE MIGHT NOT BE A PRD2 INPUT              
         BE    VK35                                                             
         ZIC   R4,SC2NDLEN                                                      
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PRD2FLD(0),SC2NDFLD                                              
         DROP  R5                                                               
*                                                                               
VK35     CLI   PRD1LEN,3           PRODUCT1 MNEMONIC IS CL3                     
         BH    ERRINV                                                           
         MVC   TEMPFLD+5(L'PRD1LEN),PRD1LEN                                     
         ZIC   R4,PRD1LEN                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   TEMPFLD+8(0),PRD1FLD                                             
         LA    R2,LSTPRDH                                                       
         MVC   SAVEACUR,ACURFORC                                                
         ST    R2,ACURFORC         FORCE CURSOR AT PRD FIELD SCR                
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   LSTPDN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    LSTPDNH+6,X'80'                                                  
         MVC   LKEYPRD1,BPRD                                                    
         OI    LKEYFLAG,X'20'      PRD1 IS PART OF KEY                          
         MVC   ACURFORC,SAVEACUR                                                
*                                                                               
         CLI   PRD2LEN,0           PRD2 IS NOT REQUIRED                         
         BE    VK40                CK FOR ESTIMATE                              
         CLI   PRD2LEN,3           PRODUCT2 MNEMONIC IS CL3                     
         BH    ERRINV                                                           
         XC    TEMPFLD,TEMPFLD     FAKE FLD FOR PRODUCT VALIDATION              
         MVI   TEMPFLD,X'0B'       MAX DATA + EXTENSION = 11                    
         MVC   TEMPFLD+5(L'PRD2LEN),PRD2LEN                                     
         ZIC   R4,PRD2LEN                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   TEMPFLD+8(0),PRD2FLD                                             
         LA    R2,LSTPRDH                                                       
         MVC   SAVEACUR,ACURFORC                                                
         ST    R2,ACURFORC         FORCE CURSOR AT PRD FIELD SCR                
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   BLOCK(100),SPACES   FOR SQUASHER                                 
         MVC   BLOCK(7),LSTPDN                                                  
         MVI   BLOCK+7+5,C'/'                                                   
         MVC   BLOCK+7+7(L'PRDNM),PRDNM                                         
         GOTO1 SQUASHER,DMCB,BLOCK,40                                           
         MVC   LSTPDN,BLOCK                                                     
         OI    LSTPDNH+6,X'80'                                                  
         MVC   LKEYPRD2,BPRD                                                    
         OI    LKEYFLAG,X'08'      PRD2 IS PART OF KEY                          
         MVC   ACURFORC,SAVEACUR                                                
*                                                                               
VK40     TM    LKEYFLAG,X'08'      IS PRD2 PART OF KEY?                         
         BNO   VK45                NO, PRD2 IS NOT PART OF KEY                  
         MVC   TEMPFLD+5(L'PRD1LEN),PRD1LEN                                     
         ZIC   R4,PRD1LEN                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   TEMPFLD+8(0),PRD1FLD                                             
         LA    R2,LSTPRDH                                                       
         MVC   SAVEACUR,ACURFORC                                                
         ST    R2,ACURFORC         FORCE CURSOR AT PRD FIELD SCR                
         LA    R2,TEMPFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD             RESTORE AIO FOR VALIPRD OF PRD1              
         MVC   ACURFORC,SAVEACUR                                                
*                                                                               
VK45     LA    R2,LSTESTH                                                       
         CLI   LSTESTH+5,0                                                      
         BE    BUILDKEY            NOT REQUIRED, TIME TO BUILD KEY              
         CLI   LSTPRDH+5,0         CASE OF NO PRODUCT?                          
         BE    VK50                                                             
         MVI   USEIONUM,1          AIO2 HAS RESULTS FROM VALIPRD                
         GOTO1 VALIEST             ESTIMATE WITH PRODUCT                        
         L     R6,AIO              DISPLAY ESTIMATE START & END DATE            
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,LSTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,LSTESD+11)                              
         MVI   LSTESD+9,C'-'       START & END DATE SEPARATOR                   
         DROP  R6                                                               
         OI    LSTESDH+6,X'80'                                                  
         MVC   LKEYEST,BEST                                                     
         OI    LKEYFLAG,X'10'      EST IS PART OF KEY                           
         B     BUILDKEY                                                         
*                                                                               
VK50     TM    LSTESTH+4,X'08'                                                  
         BZ    ERRINV                                                           
         CLI   LSTESTH+5,3                                                      
         BH    ERRINV                                                           
         ZIC   R5,LSTESTH+5                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LSTEST(0)                                                    
         CVB   R5,DUB                                                           
         CH    R5,=H'1'                                                         
         BL    ERRINV                                                           
         CH    R5,=H'255'                                                       
         BH    ERRINV                                                           
         STC   R5,LKEYEST                                                       
         OI    LKEYFLAG,X'10'      EST IS PART OF KEY                           
*                                                                               
BUILDKEY BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         LA    R4,KEY                                                           
         USING SLKRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   SLKKTYP,SLKKTYPQ    RECORD TYPE                                  
         MVI   SLKKSUB,SLKKSUBQ    RECORD SUB-TYPE                              
         MVC   SLKKAGMD,LKEYAGMD   BINARY AGENCY-MEDIA                          
         MVC   SLKKCLT,LKEYBCLT    BINARY CLIENT                                
         MVC   SLKKMKT,LKEYMKT     BINARY MARKET                                
         MVC   SLKKSTA,LKEYSTA     BINARY STATION                               
         MVC   SLKKPRD,LKEYPRD1    PRODUCT                                      
         MVC   SLKKPRD2,LKEYPRD2   PARTNER PRODUCT                              
         MVC   SLKKEST,LKEYEST     ESTIMATE                                     
         DROP  R4                                                               
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ONLINE RECORD LISTING                         *         
***********************************************************************         
LR       DS    0X                                                               
         MVI   NLISTS,14                                                        
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR10                                                             
         MVC   KEY,SAVEKEY                                                      
*                                                                               
LR10     LA    R4,KEY                                                           
         USING SLKRECD,R4                                                       
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     DS    0H                                                               
         CLI   SLKKTYP,SLKKTYPQ    SAME RECORD TYPE?                            
         BNE   LRX                                                              
         CLI   SLKKSUB,SLKKSUBQ    SAME RECORD SUB-TYPE?                        
         BNE   LRX                                                              
         CLC   SLKKAGMD,LKEYAGMD   SAME AGENCY-MEDIA?                           
         BNE   LRX                                                              
         CLC   SLKKCLT,LKEYBCLT    SAME CLIENT?                                 
         BNE   LRX                                                              
*                                                                               
         TM    LKEYFLAG,X'80'                                                   
         BZ    *+14                                                             
         CLC   SLKKMKT,LKEYMKT     MKT IS STARTING POINT                        
         BL    LR20                                                             
         TM    LKEYFLAG,X'40'                                                   
         BZ    *+14                                                             
         CLC   SLKKSTA,LKEYSTA     STA IS STARTING POINT                        
         BL    LR20                                                             
*                                                                               
         TM    LKEYFLAG,X'20'      PRD1 IS PART OF KEY?                         
         BZ    *+14                                                             
         CLC   SLKKPRD,LKEYPRD1    PRD1 IS FILTER                               
         BNE   LR20                                                             
         TM    LKEYFLAG,X'10'      EST IS PART OF KEY?                          
         BZ    *+14                                                             
         CLC   SLKKEST,LKEYEST     EST IS FILTER                                
         BNE   LR20                                                             
         TM    LKEYFLAG,X'08'      PRD2 IS PART OF KEY?                         
         BZ    *+14                                                             
         CLC   SLKKPRD2,LKEYPRD2   PRD2 IS FILTER                               
         BNE   LR20                                                             
*                                                                               
         MVC   BINARYMS(L'SLKKMKT),SLKKMKT                                      
         MVC   BINARYMS+L'SLKKMKT(L'SLKKSTA),SLKKSTA                            
         GOTO1 MSUNPK,DMCB,BINARYMS,LMKT,LSTA                                   
*                                                                               
         CLI   SLKKPRD,0           NO PRODUCT NUMBER?                           
         BE    LR57                                                             
         MVC   PRDNUMB,SLKKPRD                                                  
         BAS   RE,PDMNNIC          USE PRD NUMB TO GET PRO MNEMONIC             
         MVC   LPRD1,PRDMNNIC      PUT PRD MNEMONIC ON LIST                     
*                                                                               
LR57     CLI   SLKKPRD2,0                                                       
         BE    LR58                                                             
         MVC   PRDNUMB,SLKKPRD2                                                 
         BAS   RE,PDMNNIC          USE PRD NUMB TO GET PRD MNEMONIC             
         MVC   LPRD2,PRDMNNIC      PUT PRD MNEMONIC ON LIST                     
*                                                                               
LR58     EDIT  SLKKEST,(L'LEST,LEST),ALIGN=LEFT                                 
         MVC   LDPT,SLKKDPT                                                     
         EDIT  SLKKLEN,(L'LLN1,LLN1),ALIGN=LEFT                                 
         EDIT  SLKKLEN2,(L'LLN2,LLN2),ALIGN=LEFT                                
******** GOTO1 HEXOUT,DMCB,KEY+14,LDSKADD,4       **** OPTIONAL ****            
*                                                                               
         GOTO1 GETREC                                                           
         GOTO1 LISTMON                                                          
         B     LR20                                                             
         DROP  R4                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRREC   MVC   ERRNUM,=AL2(BADREC)                                              
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF THE ERROR MSGS                 
BADREC   EQU   465                 POSSIBLE RECORD ERROR(S)                     
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION FROM THE LIST                    
         OI    GENSTAT2,DISTHSPG   DISPLAY THIS PAGE                            
         MVI   NLISTS,14           ONLY 14 LINES PER LIST SCREEN                
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CLEARING NAME FIELDS ON LIST SCREEN                          *         
***********************************************************************         
CLRNAME  NTR1                                                                   
         CLI   LSTMEDH+5,0                                                      
         BNE   *+14                                                             
         XC    LSTMDN,LSTMDN       MEDIA NAME                                   
         OI    LSTMDNH+6,X'80'                                                  
         CLI   LSTCLTH+5,0                                                      
         BNE   *+14                                                             
         XC    LSTCLN,LSTCLN       CLIENT NAME                                  
         OI    LSTCLNH+6,X'80'                                                  
         CLI   LSTMKTH+5,0                                                      
         BNE   *+14                                                             
         XC    LSTMKN,LSTMKN       MARKET NAME                                  
         OI    LSTMKNH+6,X'80'                                                  
         CLI   LSTPRDH+5,0         CAN'T HAVE EST DATE WITHOUT PRDNM            
         BNE   CLR20                                                            
         XC    LSTPDN,LSTPDN       PRODUCT NAME                                 
         OI    LSTPDNH+6,X'80'                                                  
         XC    LSTESD,LSTESD       ESTIMATE DATES (START & END)                 
         OI    LSTESDH+6,X'80'                                                  
         B     CLRNAMEX                                                         
CLR20    CLI   LSTESTH+5,0         NO ESTIMATE INPUT                            
         BNE   CLRNAMEX                                                         
         XC    LSTESD,LSTESD       ESTIMATE DATE                                
         OI    LSTESDH+6,X'80'                                                  
CLRNAMEX B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        GETTING PRODUCT MNEMONIC BY PRODUCT NUMBER                   *         
***********************************************************************         
PDMNNIC  NTR1                      NOTE: IT IS IMPOSSIBLE THAT THE              
         LA    R2,LSTCLTH          PRODUCT NUMBER DOESN'T HAVE THE              
         MVI   USEIONUM,2          CORRESPONDING PRODUCT MNEMONIC.              
         MVC   AIO,AIO2            THUS, THE SEARCH CAN'T FAIL!                 
*                                                                               
         MVC   SAVEKEY,KEY         SAVE THE SLK KEY                             
         BAS   RE,RSV              RESET SYSTEM VALUES                          
         GOTO1 VALICLT                                                          
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         LA    R5,CLIST                                                         
LRLOOP   CLC   PRDNUMB,3(R5)       COMPARE PRODUCT NUMBER                       
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     LRLOOP                                                           
         MVC   PRDMNNIC,0(R5)      GET PRODUCT MNEMONIC                         
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
*                                                                               
PDMNNICX BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         MVC   KEY,SAVEKEY         RESTORE THE SLK KEY                          
         B     XIT                                                              
         SPACE 2                                                                
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
***********************************************************************         
*        LITERALS                                                     *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF2D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXLK          STLOCKIN RECORD                              
         EJECT                                                                  
       ++INCLUDE SPGENCLT          FOR PRODUCT MEMONIC                          
         EJECT                                                                  
       ++INCLUDE SPGENEST          FOR ESTIMATE START AND END DATE              
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
STATUS   DS    XL1                 STATUS BYTE FLAG                             
KEYCHG   EQU   X'80'               KEY HAS CHANGED                              
SAVEKEY  DS    CL48                                                             
SVSLHKEY DS    CL48                SAVE STLOCKIN HEADER REC KEY                 
SVSLKKEY DS    CL48                SAVE STLOCKIN REC KEY                        
TEMPLKEY DS    CL48                TEMPORARY LIST KEY FOR WORK                  
LKEYFLAG DS    XL1                 LIST KEY FLAG                                
PRDNUMB  DS    XL1                 PRODUCT CODE                                 
PRDMNNIC DS    CL3                 PRODUCT MNEMONIC                             
PRD1LEN  DS    XL1                 SAVE VALUES FROM SCANNER                     
PRD2LEN  DS    XL1                 "                                            
PRD1FLD  DS    CL3                 "                                            
PRD2FLD  DS    CL3                 "                                            
TEMPFLD  DS    XL11                TMEPORARY SCREEN FIELD                       
SAVEACUR DS    XL4                 SAVE ADDRESS OF FORCED CURSOR                
*                                                                               
LKEYAGMD DS    XL1                                                              
LKEYBCLT DS    XL2                                                              
LKEYMKT  DS    XL2                                                              
LKEYSTA  DS    XL3                                                              
LKEYPRD1 DS    XL1                                                              
LKEYPRD2 DS    XL1                                                              
LKEYEST  DS    XL1                                                              
BINARYMS DS    XL5                                                              
TEMPLMKT DS    XL4                                                              
TEMPLSTA DS    XL4                                                              
TEMPSEQ  DS    XL3                                                              
TEMPAGMD DS    XL1                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LIST ACTION                       
         DS    CL1                                                              
LMKT     DS    CL4                                                              
         DS    CL2                                                              
LSTA     DS    CL4                                                              
         DS    CL2                                                              
LPRD1    DS    CL4                                                              
         DS    CL3                                                              
LPRD2    DS    CL4                                                              
         DS    CL3                                                              
LEST     DS    CL4                                                              
         DS    CL2                                                              
LDPT     DS    CL1                                                              
         DS    CL5                                                              
LLN1     DS    CL4                                                              
         DS    CL2                                                              
LLN2     DS    CL4                                                              
         DS    CL2                                                              
LDSKADD  DS    CL8                 OPTIONAL, DISPLAYS REC DISK ADDRESS          
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPSFM22   06/14/06'                                      
         END                                                                    
