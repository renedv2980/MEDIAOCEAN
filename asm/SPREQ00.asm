*          DATA SET SPREQ00    AT LEVEL 214 AS OF 02/26/20                      
*PHASE T20800A                                                                  
         TITLE 'SPREQ00 - REQUEST - ROOT CONTROLLER'                            
***                                                                             
***                                                                             
***                                                                             
***  NOTE 'RW' IS RESERVED BY STEREO                                            
***  DO NOT USE FOR REQUEST NAME FOR SPOT OR NET                                
***                                                                             
***  ENTRY (RT30) FOR CLIENT PURGE (sw) NOT ACTIVE YET                          
***        (**30)                                                               
***                                                                             
T20800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 REQTEMPX-REQTEMP,T20800,CLEAR=YES,R7                             
         LR    R9,RC                                                            
         USING REQTEMP,R9                    R9=A(W/S)                          
         ST    R9,ATEMP                                                         
         LM    R2,R4,0(R1)                                                      
         USING TWAD,R3                    R3=A(TWA)                             
         ST    R1,APARM                                                         
         ST    R3,ASAVE                                                         
         ST    RB,ABASE                                                         
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         EJECT                                                                  
*                                                                               
         LR    R6,R3                         STORE A(FIRST INPUT FLD)           
         AH    R6,0(R2)                                                         
         ST    R6,AFIRSTF                                                       
         LR    R6,R3                         STORE A(LAST INPUT FLD)            
         AH    R6,2(R2)                                                         
         ST    R6,ALASTF                                                        
         MVC   COUNTF,4(R2)                  STORE INPUT FLD COUNT              
*                                                                               
         MVI   DDS,0                                                            
         CLI   01(R3),C'*'                                                      
         BNE   *+8                                                              
         MVI   DDS,1                         SET DDS TERMINAL                   
         MVC   AGY,14(R3)                    STORE AGY ALPHA                    
         MVC   AGYB,0(R1)                    STORE AGY BINARY                   
         MVC   USRID,10(R3)        STORE USER ID NUMBER                         
         MVI   USRIDF,0                                                         
         L     R4,16(R1)                                                        
         MVC   FACLIST,0(R4)       STORE COMMON FACILITY LIST                   
         ST    R4,ACOMFACS         STORE ACOMFACS                               
         USING COMFACSD,R4                                                      
         GOTO1 GETFACT,PLIST,0                                                  
         MVC   SECRET,CSECRET                                                   
         L     R1,0(R1)                                                         
         MVC   TODAY(13),4(R1)                                                  
         DROP  R4                                                               
*                                                                               
         L     RF,CALLOV                                                        
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A14'    CLPACK                                 
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   CLPACK,DMCB                                                      
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A15'    CLUNPK                                 
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   CLUNPK,DMCB                                                      
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A7A'    STAPACK                                
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   STAPACK,DMCB                                                     
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AFA'    GETIDS                                 
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   GETIDS,DMCB                                                      
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A38'    OFFICER                                
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   OFFICER,DMCB                                                     
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A00'    BOOKVAL                                
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   BOOKVAL,DMCB                                                     
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A1D'    GETBROAD                               
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   GETBROAD,DMCB                                                    
                                                                                
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A57'    SPSLENTAB                              
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   SLNTAB,DMCB                                                      
*                                                                               
         GOTO1 DATVAL,PLIST,TODAY,TEMP                                          
         MVC   TODAY(6),TEMP       CONVERT TO YYMMDD                            
*                                            RELOCATE LOCAL ROUTINES            
         LA    R6,REQTBL                                                        
         ST    R6,AREQTBL                                                       
         LA    R6,OLAY                                                          
         ST    R6,AOLAY                                                         
         LA    R6,RSPTFILE                                                      
         ST    R6,ARSPT                                                         
         LA    R6,RSTAFILE                                                      
         ST    R6,ARSTA                                                         
         LA    R6,RDEMFILE                                                      
         ST    R6,ARDEM                                                         
         LA    R6,INITV                                                         
         ST    R6,AINITV                                                        
         LA    R6,RJN                                                           
         ST    R6,ARJN                                                          
         LA    R6,RFPINIT                                                       
         ST    R6,ARFP                                                          
         LA    RF,IORFP            SET A(RFP) & A(MINIO) BUFFERS                
         ST    RF,AIORFP                                                        
         AH    RF,=Y(L'IORFP)                                                   
         ST    RF,AIOMINIO                                                      
         AH    RF,=Y(L'IOMINIO)                                                 
         ST    RF,ARFPTAB                                                       
         AH    RF,=Y(L'RFPTAB)                                                  
         ST    RF,ASECBLK                                                       
         XC    STATSV(200),STATSV                                               
         XC    REQRECSV,REQRECSV                                                
         XC    MLTREQSW(7),MLTREQSW                                             
         LA    R6,ACCESSTB                                                      
         ST    R6,AACCESST                                                      
*                                                                               
         ST    RB,LOLAYNUM                   INIT LAST OLAY NUM & ADR           
         LA    R6,SPTWORK                                                       
         ST    R6,DMCB+16                                                       
         MVC   DMCB+16(1),0(R3)              SET TERMINAL NUM IN DMCB           
*                                                                               
**       LR    R4,R2                 ADDR OF FATIOB                             
         USING TIOBD,R2                                                         
         OI    TIOBINDS,TIOBSYS      GET SYSTEM NUMBER FOR STEREO               
*                                                                               
         LR    R2,R3                 R2=A(TWA SCREEN DATA)                      
         USING T208FFD,R2                                                       
*                                                                               
* - ARE WE IN STEREO AND NETWORK                                                
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
                                                                                
* CHECK FOR STEREO BEFORE THERE WAS MORE THAN ONE                               
*        GOTO1 CGETFACT,DMCB,(2,0)                                              
*        L     R1,0(R1)                                                         
*        USING FACTSD,R1                                                        
*        TM    FATSTAT6,X'80'      ,,IF WE ARE IN STEREO                        
*        BNO   STR15                                                            
*STR10    CLI   FAOVSYS,3           ,,AND IF IT IS NET                          
*         BNE   REQ10                                                           
                                                                                
* CHECK FOR STEREO NOW THAT THERE ARE MORE THAN ONE                             
         GOTO1 CGETFACT,DMCB,(X'80',0),F#UTLD                                   
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         TM    F@TSTAT6,TST6STRO+TST6STFU  ,, IF STEREO                         
         BNO   STR15                                                            
STR10    CLI   F@TOVSYS,3                 ,,AND IF IT IS NET                    
         BNE   REQ10                                                            
         CLI   BVRSRV+1,C'3'              ,,AND NOT SET TO NETWORK              
         BE    *+12                                                             
         MVI   BVRSRV+1,C'3'             ,,SET SRVC TO NETWORK                  
         OI    BVRSRVH+6,X'80'                                                  
         CLI   BVRNAME,X'40'       ,,AND FIRST TIME IN                          
         BH    REQ10                                                            
XIT      XIT1                      ,,EXIT                                       
                                                                                
*                                                                               
* - IF NETWORK AND NOT STEREO I NEED EXIT FIRST TIME IN                         
**STR15    CLI   FAOVSYS,3           IF NETWORK *ONLY ONE STEREO                
**         BNE   REQ10                                                          
STR15    CLI   F@TOVSYS,3           IF NETWORK                                  
         BNE   REQ10                                                            
         CLI   BVRNAME,X'40'       AND FIRST TIME IN                            
         BH    REQ10                                                            
         CLC   REQNUM,=X'0000'                                                  
         BNE   STR17                                                            
         MVI   REQNUM+1,X'FF'        BORROW FOR FLAG                            
         B     XIT                   AND EXIT                                   
* - CLEAR FLAG ON SUBSEQUENT PASSES                                             
STR17    CLC   REQNUM(2),=X'00FF'  IS IT MY FLAG                                
         BNE   REQ10               NO                                           
         XC    REQNUM(2),REQNUM    YES/CLEAR BORROWED FLG                       
         B     REQ10                   CONTINUE PROCESSING                      
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
REQ10    DS    0H                  TEST AGENCY ON NEW SECURITY                  
         USING ACCESSD,R4                                                       
         L     R4,AACCESST         PROGRAM/ACTION TABLE                         
         LA    R0,ACCESSEQ                                                      
REQ15    CLC   ACCID,BVRNUM        FIND PRG ID IN TABLE                         
         BE    REQ20                                                            
         CLC   ACCCDE,BVRNUM       FIND PRG CODE IN TABLE                       
         BE    REQ20                                                            
         LA    R4,ACCESSLN(R4)                                                  
         BCT   R0,REQ15                                                         
         B     REQ30               NOT IN TABLE-NO SECURITY                     
*                                                                               
*****    OC    4(4,R3),4(R3)                                                    
*****    BZ    REQ30                                                            
REQ20    XC    DMCB(24),DMCB       INIT SECRET BLOCK                            
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         LA    R5,ACCANUM          R5=A(ACTION)                                 
         ICM   R5,8,ACCRNUM        PASS RECORD NUMBER IN HIGH BIT               
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R5)                            
         CLI   DMCB,SECPYES                                                     
         BNE   REQ25                                                            
*                                                                               
         TM    ACCRSTAT,ACCSECL    SHOULD WE LOOK INTO FIELD SEC                
         BNO   REQ30                                                            
*                                                                               
REQ25    LA    RF,BVRNAMEH                                                      
         ST    RF,FADR                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OERRMSG                                                          
         DROP  R4                                                               
*                                                                               
***********************************************************************         
*              INTERFACE TO $RFP                                      *         
***********************************************************************         
*                                                                               
REQ30    DS    0H                                                               
*                                                                               
         MVI   RFPSTAT,0           DESTINATION MUST BE FILE                     
         ZIC   R1,BVRDESTH+5                                                    
         SH    R1,=H'1'                                                         
         BM    RFPX                                                             
         EXCLC R1,BVRDEST,=CL15'FILE'                                           
         BNE   RFPX                                                             
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPINIT   INITIALIZE $RFP INTERFACE                    
         GOTO1 ARFP,DMCB                                                        
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         ZIC   R1,BVROUTH+5                                                     
         CHI   R1,1                                                             
         BNH   RFPX                                                             
*        SH    R1,=H'1'                                                         
*        BM    RFPX                                                             
*        EX    R1,*+4              FOR IDF'S SAKE GET RID OF IT                 
*        MVC   QRFPWORK(0),BVROUT  VALIDATE GROUP NAME                          
         EXMVC R1,QRFPWORK,BVROUT                                               
         OC    QRFPWORK,SPACES                                                  
         MVI   QRFPMODE,QRFPGVAL                                                
         MVI   RFPSTAT,RFPINUSE                                                 
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    RFPX                                                             
         MVC   FERN,=AL2(IGROUPNM)                                              
         LA    R1,BVROUTH                                                       
         ST    R1,FADR                                                          
         B     OERRMSG                                                          
*                                                                               
RFPX     DS    0H                                                               
         EJECT                                                                  
CONTROL  CLI   STATUS,0                                                         
         BE    STATUS0                                                          
         CLI   STATUS,1                                                         
         BE    STATUS1                                                          
         CLI   STATUS,2                                                         
         BE    STATUS2                                                          
         CLI   STATUS,3                                                         
         BE    STATUS3                                                          
         CLI   STATUS,4                                                         
         BE    STATUS4                                                          
         DC    H'0'                                                             
         SPACE 2                                                                
STATUS0  MVI   OLAYNUM,01                    INPUT IS REQ DEFN                  
         GOTO1 AOLAY                         VALIDATE & BUILD SCREEN            
         CLC   FERN,=AL2(FF)                                                    
         BL    OERRMSG                                                          
         B     CONTROL                                                          
         SPACE 2                                                                
STATUS1  CLI   REQACTN,C'D'                                                     
         BE    STATUS1A                                                         
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(23),=C'ENTER REQUEST AMENDMENT'                           
         B     STATUS1A                                                         
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(18),=C'ENTER REQUEST DATA'                                
         B     STATUS1A                                                         
STATUS1A MVI   STATUS,2                                                         
         LA    R5,BVRFRSTH                   POSN CURSOR                        
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         ST    R5,FADR                                                          
         B     OHDR                                                             
         SPACE 2                                                                
STATUS2  MVC   REQNUM(18),LREQNUM            INPUT IS REQUEST DATA              
         MVC   KEY,LKEY                                                         
         MVC   REQREC,LREQREC                                                   
         LA    R6,BVRFRSTH                                                      
         C     R6,AFIRSTF                    WAS A REQ DEFN FLD INPUT           
         BL    *+12                          NO                                 
         MVI   STATUS,0                      YES BACK TO STATUS 0               
         B     CONTROL                                                          
         CLI   REQACTN,C'D'                                                     
         BE    UPDATE                                                           
         B     VALIDATE                                                         
         SPACE 2                                                                
STATUS3  B     UPDATE                        DISPLAY/UPDATE DATA                
         SPACE 2                                                                
STATUS4  XC    BVRHDR,BVRHDR                 INPUT WAS FOR MENU DISPLAY         
         MVC   BVRHDR(22),=C'REQUEST MENU DISPLAYED'                            
         B     UPDATE1                                                          
         EJECT                                                                  
VALIDATE L     R4,AREQTBL                                                       
         AH    R4,REQNDX1                    R4=A(REQ TBL FLD ENTRY)            
         BCTR  R4,0                                                             
         LA    RA,LREQMAP-3                  RA=A(REQ MAP TBL ENTRY)            
         B     VAL5                                                             
         SPACE 2                                                                
VAL1     CLI   0(R4),0                       END OF REQ TBL                     
         BE    VALX                          YES                                
                                                                                
*******************************************************                         
         CLC   RNUM,=C'MY'                                                      
         BNE   VL12                                                             
VL10     CLI   0(R4),X'DA'         NET ONLY FIELD                               
         BNE   VL20                                                             
         BAS   RE,SORN               IS IT NET                                  
         BE    VL20                                                             
         LA    R4,3(R4)            NO/SKIP FIELD                                
         B     VAL1                                                             
                                                                                
******************************************************                          
VL12     CLC   RNUM,=C'KB'         IF KB                                        
         BNE   VL20                                                             
         CLI   0(R4),X'F5'         IF NEW PROD FIELD                            
         BNE   VL20                                                             
         CLC   RAGY,=C'DF'         AND IF DF AGENCY                             
         BE    VL20                OK                                           
         CLC   RAGY,=C'SJ'             OR SJ AGENCY                             
         BE    VL20                OK                                           
         LA    R4,3(R4)            ELSE SKIP THE FIELD                          
         B     VAL1                                                             
**********************************************************                      
                                                                                
VL20     CLI   0(R4),X'01'         COMMENT                                      
         BNE   VAL1B                                                            
*                                  SKIP                                         
         LA    R4,2(R4)                                                         
         B     VAL1                                                             
VAL1B    CLI   0(R4),127                                                        
         BNE   *+12                                                             
         LA    R4,1(R4)                      BUMP PAST COMMENT ENTRIES          
         B     VAL1                                                             
***************************************************************                 
         TM    2(R4),X'80'         DDS ONLY FIELD                               
         BZ    VAL1D               NO                                           
*****************************************************************               
* THESE FIELDS/REPORTS PUT DATA IN REQ CARD2 BEYOND 128 (X'80')                 
* SO IT APPEARS AS IF THEY ARE DDS ONLY FIELDS                                  
         CLI   0(R4),X'DF'         K1 REPORT FIELD                              
         BE    VAL1D                                                            
         CLC   RNUM,=C'KB'         KB REPORT                                    
         BE    VAL1D                                                            
***************************************************************                 
         TM    REQFMT,X'04'        YES - WAS 1=Y INPUT (DDS REQ)                
         BNZ   VAL1D                                                            
         CLC   RNUM,=C'M2'                                                      
         BE    VAL1D                                                            
         CLC   RNUM,=C'MC'                                                      
         BE    VAL1D                                                            
         CLC   RNUM,=C'MD'                                                      
         BE    VAL1D                                                            
         CLC   RAGY,=C'MC'         SPECIAL MC FIELD ON M4                       
         BE    VAL1C                                                            
         CLC   RAGY,=C'CC'         AND COKE                                     
         BNE   VAL7                NO SKIP IT                                   
VAL1C    CLC   RNUM,=C'M2'                                                      
         BE    VAL1D                                                            
         CLC   RNUM,=C'M4'                                                      
         BNE   VAL7                                                             
VAL1D    CLC   0(1,RA),0(R4)                                                    
         BE    *+6                                                              
         DC    H'0'                          DUMP IF TBLS OUT OF STEP           
         SPACE 2                                                                
         LA    R6,FLDNUMT                    FIND REQ FLD NUM IN TBL            
VAL2     CLI   0(R6),0                       R6=A(FLD NUM TBL ENTRY)            
         BNE   *+6                                                              
         DC    H'0'                          DUMP IF REQ NUM NOT IN TBL         
         CLC   0(1,R6),0(R4)                                                    
         BE    VAL3                                                             
         LA    R6,4(R6)                                                         
         B     VAL2                                                             
         SPACE 2                                                                
VAL3     MVC   OLAYNUM,1(R6)                 SET OVERLAY REQD                   
         MVC   ROUTNUM,2(R6)                 SET VALIDATION ROUT REQD           
         MVC   ROUTSUB,3(R6)                 SET SUB FLD NUM                    
         SR    R1,R1                                                            
         IC    R1,2(R4)                      SET COL NUM - 1                    
*                                                                               
         CLI   2(R4),X'8C'         FUDGE FOR K1 FOR NOW                         
         BE    *+14                                                             
         CLC   RNUM,=C'KB'         FUDGE FOR KB FOR NOW                         
         BNE   VAL3B                                                            
*                                                                               
         BCTR  R1,0                                                             
         STH   R1,COLNUM                                                        
         B     VAL3C                                                            
VAL3B    SLL   R1,25                                                            
         SRL   R1,25                                                            
         BCTR  R1,0                                                             
         STH   R1,COLNUM                                                        
VAL3C    ST    R5,FLDHADR                    SET INPUT FLD HDR ADR              
         GOTO1 AOLAY                         PASS CONTROL TO OVERLAY            
         TM    FIND,X'01'                                                       
         BZ    VAL3D                                                            
         CLC   FERN,=AL2(FF)                                                    
         BNE   OERRMSG                       FLD INPUT INVALID                  
         B     VAL4                          FLD INPUT VALID                    
VAL3D    TM    1(R4),X'01'                   FLD NOT INPUT                      
         BZ    OERRMSG                       AND IS NOT OPTIONAL                
         B     VAL5                                                             
VAL4     NI    FIND,X'FE'                    FIND=B'XXXXXXX0'                   
         MVC   TEMP(1),1(R4)                 TEMP=B'XXXXXXXX'                   
         NC    TEMP(1),FIND                  IS FLD FORMAT OK FOR REQ           
         BNZ   VAL5                          YES                                
         MVC   FERN,=AL2(FMTNAV)        ERROR FORMAT NOT AVAILABLE              
         B     OERRMSG                                                          
         SPACE 2                                                                
VAL5     LA    RA,3(RA)                      FIND NEXT REQ MAP ENTRY            
         MVC   HALF,1(RA)                                                       
         LH    R5,HALF                                                          
         AR    R5,R3                         R5=A(NEXT UNPROT FLD HDR)          
         CLI   0(RA),126                     CARD REQUEST FORMAT                
         BNE   VAL7                                                             
         MVC   RAGY(78),8(R5)                YES SCREEN TO REQ REC              
         B     UPDATE                                                           
         SPACE 2                                                                
VAL7     LA    R4,3(R4)                      FIND NEXT REQ TBL FLD              
         CLI   0(R4),0         END OF TABLE                                     
         BE    VALX                                                             
         B     VAL1                                                             
         SPACE 2                                                                
VALX     CLI   0(RA),127                     END OF REQ MAP TBL                 
         BE    UPDATE                                                           
         DC    H'0'                          DUMP IF TBLS OUT OF STEP           
         SPACE 2                                                                
UPDATE   CLC   =C'SOON',BVROUT               CHECK SOON RESTRICTIONS            
         BNE   UPDATE5                                                          
         MVI   OLAYNUM,05                                                       
         GOTO1 AOLAY                                                            
         CLC   FERN,=AL2(FF)                                                    
         BL    OERRMSG                                                          
                                                                                
UPDATE5  MVI   OLAYNUM,02                    PASS CONTROL TO OVERLAY            
         GOTO1 AOLAY                                                            
         CLC   FERN,=AL2(FE)       CHK IF PASSING OWN MESSAGE                   
         BE    OHDR                                                             
         CLC   FERN,=AL2(FF)                 UPDATED/DISPLAYED OK               
         BL    OERRMSG                       NO ERROR                           
         CLI   REQACTN,C'D'                                                     
         BNE   *+16                                                             
         CLI   STATUS,1                                                         
         BE    CONTROL                       ASK FOR CANCELLATIONS              
         B     UPDATE1                                                          
         XC    BVRHDR,BVRHDR                                                    
         CLI   REQACTN,C'A'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(15),=C'REQUEST AMENDED'                                   
         B     UPDATE1                                                          
         CLI   REQACTN,C'N'                                                     
         BNE   *+14                                                             
         MVC   BVRHDR(13),=C'REQUEST ADDED'                                     
         B     UPDATE1                                                          
UPDATE1  LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         MVI   STATUS,0                                                         
         MVC   PREQNUM(1),LREQNUM            SAVE LAST REQ DEFN                 
         MVC   PREQACTN(1),LREQACTN                                             
         MVC   PREQNDX1(2),LREQNDX1                                             
         B     OHDR                                                             
***                                                                             
SORN     NTR1                                                                   
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   *+6                                                              
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*        SET UP SCREEN HEADER MESSAGE TO CONTAIN THE ERROR                      
*                                                                               
OERRMSG  XC    BVRHDR,BVRHDR                                                    
         CLC   TEMP(3),=C'***'     PASSING OWN ERROR MESSAGE                    
         BNE   OER5                                                             
         MVC   BVRHDR,TEMP         YES                                          
         XC    TEMP(3),TEMP                                                     
         B     OHDR                                                             
OER5     DS    0H                                                               
         SR    R5,R5                                                            
***      IC    R5,FERN                                                          
         ICM   R5,3,FERN                                                        
         L     R6,DATAMGR                                                       
***      GOTO1 GETMSG,PLIST,((R5),BVRHDR),(X'FF',DMCB),(02,(R6))                
         PRINT GEN                                                              
         LA    R1,PLIST                                                         
         USING GETTXTD,R1                                                       
         MVI   GT2INDS,0          DEFAULT                                       
         MVI   GTMSYS,2           USE SPOT SYSTEM FOR MESSAGES                  
         MVI   GTMLANG,0          DEFAULT TO CONNECTED LANGUAGE                 
         DROP  R1                                                               
         GOTO1 GETTXT,PLIST,(R5),0,(C'E',DMCB),0,0                              
         PRINT NOGEN                                                            
         SPACE 2                                                                
*        OUTPUT SCREEN HEADER MESSAGE , POSITION THE CURSOR , AND               
*        RETURN TO TERMINAL                                                     
*                                                                               
OHDR     OI    BVRHDRH+6,OI1T                                                   
         MVI   BVRHDRH+7,60                                                     
         L     R6,FADR                                                          
         OI    6(R6),OI1C                                                       
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        THIS ROUTINE PASSES CONTROL TO THE OVERLAY NUMBER SPECIFIED BY         
*        OLAYNUM.IF THE OVERLAY IS NOT IN CORE IT IS READ FROM DISK AND         
*        ITS NUM AND ADDRESS ARE SAVED.                                         
*                                                                               
OLAY     NTR1  BASE=ABASE                                                       
         CLC   OLAYNUM,LOLAYNUM              IS OVERLAY IN CORE                 
         BE    OLAY1                         YES                                
         XC    CALLOVPL(8),CALLOVPL          LOAD FROM DISK OLAYNUM             
         MVC   CALLOVPL(1),OLAYNUM                                              
         ST    R3,CALLOVPL+4                                                    
         GOTO1 CALLOV,CALLOVPL                                                  
         CLI   CALLOVPL+4,X'FF'                                                 
         BNE   *+6                                                              
         DC    H'0'                          DUMP IF CANT LOAD                  
         MVC   LOLAYNUM(4),CALLOVPL          SAVE OVERLAY NUM & ADDR            
OLAY1    GOTO1 LOLAYNUM,PHASEPL,(R9)         PASS CONTROL TO OVERLAY            
OLAYX    B     XIT                                                              
*                                                                               
*        $RFP INTERFACE                                                         
RFPINIT  NTR1  BASE=ABASE                                                       
         MVI   OLAYNUM,9                                                        
         GOTO1 AOLAY                                                            
         B     EXIT                                                             
         EJECT                                                                  
*        THIS ROUTINE READS THE SPTFILE/STATION RECORD SPECIFIED BY             
*        KEY/KEYS. THE FIELD FERN IS SET TO FF IF OK, FE IF NOT FOUND,          
*        OR TO 00 ON DISK ERROR.                                                
*                                                                               
RSPTFILE NTR1  BASE=ABASE                    SPTFILE ENTRY POINT                
         MVC   TEMP(13),KEY                                                     
         LA    R4,DMREAD                                                        
         LA    R5,SPTFILE                                                       
         B     RSPT0                                                            
RSTAFILE NTR1  BASE=ABASE                    STATION ENTRY POINT                
         MVC   TEMP(17),KEYS                                                    
         LA    R4,DMREAD                                                        
         LA    R5,STATION                                                       
         B     RSPT0                                                            
RDEMFILE NTR1  BASE=ABASE                    DEMFILE ENTRY POINT                
         MVC   TEMP(13),KEYD                                                    
         LA    R4,DMRDHI                                                        
         LA    R5,DEMFILE                                                       
RSPT0    GOTO1 DATAMGR,DMCB,(R4),(R5),TEMP,SPTREC,SPTWORK                       
         CLI   DMCB+8,0                                                         
         BE    RSPT2                         FOUND OK                           
         TM    DMCB+8,X'10'                                                     
         BO    RSPT1                         NOT FOUND                          
         MVC   FERN,=AL2(0)                                                     
         B     RSPTX                         DISK ERROR                         
RSPT1    MVC   FERN,=AL2(FE)                                                    
         B     RSPTX                                                            
RSPT2    MVC   FERN,=AL2(FF)                                                    
RSPTX    CLC   FERN,=AL2(FE)       SET CONDITION CODE                           
         B     XIT                                                              
         SPACE 2                                                                
DMREAD   DC    C'DMREAD'                                                        
DMRDHI   DC    C'DMRDHI'                                                        
SPTFILE  DC    C'SPTFILE '                                                      
STATION  DC    C'STATION '                                                      
DEMFILE  DC    C'DEMDIR  '                                                      
         DS    0H                                                               
         EJECT                                                                  
*        INITIALISE FIND,FERN,FADR FOR FLD NUM N AT TWA HDR AT R1.              
*        MOVE FLD TO IFLD AND SET R4=A(HDR) & R5=L'DATA.                        
*        R6 IS SET TO THE ADDRESS OF THE NEXT INPUT FIELD                       
*        NOTE *** - USES  HALF2 FOR ERROR RETURN                                
*                                                                               
INITV    NTR1  BASE=ABASE                                                       
         XC    HALF,HALF                     SAVE FLD NUM                       
         MVC   HALF+1(1),ROUTSUB                                                
         XC    IFLDH,IFLDH                                                      
         MVC   HALF2,=AL2(FF)                                                   
         MVC   IFLD,=CL32' '                                                    
         XC    NAME,NAME                                                        
         L     R4,FLDHADR                    R4=A(FLD HDR)                      
         SR    R5,R5                                                            
         IC    R5,5(R4)                                                         
         LTR   R5,R5                         NULLS OR SPACES NOT INPUT          
         BZ    INITV1                                                           
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),=CL32' '                                                 
         BNE   INITV2                                                           
*                                                                               
INITV1   SR    R5,R5                         MISSING FLD                        
         MVC   HALF2,=AL2(FLDMIS)                                               
         B     INITVX                                                           
*                                                                               
INITV2   LA    R5,1(R5)                      R5=TOTAL INPUT COUNT               
         SR    R1,R1                         R1=DELIMITER COUNT                 
         LA    R2,8(R4)                      R2=A(FLD START)                    
         LR    R3,R2                         R3=A(FLD NEXT CHR)                 
         CLI   ROUTSUB,0                     ONLY ONE FIELD                     
         BE    INITV7                        YES INHIBIT SCAN                   
*                                                                               
INITV3   CLI   0(R3),C','                    SCAN FOR DELIMITER                 
         BE    INITV5                                                           
         CLI   0(R3),C'-'                                                       
         BE    INITV5                                                           
INITV4   LA    R3,1(R3)                                                         
         BCT   R5,INITV3                                                        
*                                                                               
INITV5   LA    R1,1(R1)                      BUMP DELIMITER COUNT               
         CH    R1,HALF                                                          
         BE    INITV6                        FLD#N R2=START R3=END+1            
         LTR   R5,R5                                                            
         BZ    INITV1                                                           
         LA    R2,1(R3)                                                         
         B     INITV4                                                           
*                                                                               
INITV6   LR    R5,R3                                                            
         SR    R5,R2                         R5=FLD#N LENGTH                    
         BZ    INITV1                                                           
INITV7   STC   R5,IFLDH+5                                                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),0(R2)                                                    
         LA    R5,1(R5)                                                         
         OI    IFLDH,X'01'                   SET FLD INPUT                      
         CLI   IFLDH+5,3                                                        
         BNE   INITVX                                                           
         CLC   IFLD(3),=C'ALL'                                                  
         BNE   INITVX                                                           
         OI    IFLDH,X'02'                   SET FLD INPUT=C'ALL'               
*                                                                               
INITVX   LA    R6,TEMP                       SET R6=A(NEXT TWA FLD HDR)         
         TM    REQFMT,X'04'                  CHECK DDS OPTION                   
         NOP   *+12                                                             
*        IF THE ABOVE INSTRUCTION IS CHANGED TO BO THE ADDRESS OF THE           
*        NEXT INPUT FIELD IS SET TO A DUMMY LOCATION WHICH WILL NEGATE          
*        ALL FOUTS IN VALIDATION ROUTINES.                                      
         SR    R6,R6                                                            
         IC    R6,0(R4)                                                         
         AR    R6,R4                                                            
         CLI   ROUTSUB,1                     1ST OR ONLY FIELD                  
         BH    *+20                          NO                                 
         MVC   FIND,IFLDH                    YES SET FIND/FADR                  
         MVC   FERN,HALF2                                                       
         ST    R4,FADR                                                          
         CLI   FIND,X'01'          LOW=MISSING,EQL=INPUT,HIGH=C'ALL'            
         XIT1  REGS=(R4,R6)                                                     
         EJECT                                                                  
*        ROUTINE TO RIGHT JUSTIFY A FIELD AT IFLD. RETURN IN TEMP               
*        CL1   BINARY VALUE                                                     
*        CL3   RJ JUSTIFIED VALUE                                               
*                                                                               
RJN      NTR1  BASE=ABASE                                                       
         MVC   TEMP(4),=C'0000'                                                 
         MVC   FERN,=AL2(FF)                                                    
         LA    R4,IFLDH                                                         
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         LR    R8,R5                                                            
         BCTR  R8,R0                                                            
         LA    R6,3                                                             
         SR    R6,R5                                                            
         LA    R6,TEMP+1(R6)                                                    
         EX    R8,*+8                        RJ JUSTIFY AT TEMP+1(3)            
         B     *+10                                                             
         MVC   0(0,R6),8(R4)                                                    
         MVC   TEMP+10(3),=C'0000'           CHECK FOR NUMERIC                  
         MVZ   TEMP+10(3),TEMP+1                                                
         CLC   TEMP+10(3),=C'0000'                                              
         BE    *+14                                                             
         MVC   FERN,=AL2(FLDNUM)                                                
         B     RJNX                                                             
         PACK  DUB,TEMP+1(3)                 CHECK GT 0 AND LE 255              
         CVB   R6,DUB                                                           
         CHI   R6,0                                                             
         BE    RJN1                                                             
         CHI   R6,255                                                           
         BH    RJN1                                                             
         STC   R6,TEMP                       RETURN BINARY VALUE                
         B     RJNX                                                             
RJN1     MVC   FERN,=AL2(FLDINV)                   SET ERROR CODE               
RJNX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
FLDNUM   EQU   03                            NON NUMERIC INPUT FLD              
FMTNAV   EQU   05                            FORMAT NOT AVAIL FOR REQ           
SECLOCK  EQU   55                            SECURITY LOCKOUT                   
         EJECT                                                                  
*        THIS TABLE CONTAINS AN ENTRY FOR EACH REQUEST FIELD DEFINED            
*        EACH ENTRY HAS THE FORMAT :-                                           
*        CL1   FLD NUM (=0 END OF TBL)                                          
*        CL1   OVERLAY NUM OF PHASE CONTAINING VALIDATION CODE                  
*        CL1   ROUTINE NUM WITHIN OVERLAY PHASE                                 
*        CL1   SUB FLD NUM - 1=FIRST - DENOTES POSITION IN INPUT FLD            
*                                                                               
FLDNUMT  DS    0CL4                                                             
*                                                                               
*                                                                               
*        DON'T USE 1                                                            
*                                                                               
         DC    AL1(02,3,02,0)                CLIENT                             
         DC    AL1(03,3,03,0)                PRODUCT                            
         DC    AL1(04,3,04,0)                PRODUCT,MODE                       
         DC    AL1(05,3,05,0)                ESTIMATE                           
         DC    AL1(06,3,06,0)                MARKET                             
         DC    AL1(07,3,07,0)                STATION                            
         DC    AL1(08,4,01,0)                REP NUM                            
         DC    AL1(09,3,08,0)                START,END DATES                    
         DC    AL1(10,4,02,0)                MONTH OF SERVICE                   
         DC    AL1(11,4,02,0)                INVOICE DATE                       
         DC    AL1(12,4,02,0)                PAY PERIOD START                   
         DC    AL1(13,4,02,0)                PAY PERIOD END                     
         DC    AL1(14,4,03,0)                BOOK-HUT                           
         DC    AL1(15,4,07,0)                REVISIONS ONLY?                    
         DC    AL1(16,4,07,0)          ALLOCATION BASIS                         
         DC    AL1(17,4,05,0)                FLAG PERCENTAGE                    
         DC    AL1(18,4,07,0)                INCLUDE ALL MEDIA ?                
*******  DC    AL1(19)  ******* AVAILABLE  *******                              
*******  DC    AL1(20)  ******* AVAILABLE  *******                              
         DC    AL1(21,4,07,0)                PRIOR MONTHS?                      
         DC    AL1(22,4,07,0)                LATER MONTHS?                      
         DC    AL1(23,4,07,0)                MARK FILES?                        
         DC    AL1(24,4,07,0)                ORDERED OR PAID                    
         DC    AL1(25,4,07,0)                DATA COMPARE                       
         DC    AL1(26,4,07,0)                TYPE                               
         DC    AL1(27,4,07,0)          ALLOCATE EXCESS                          
         DC    AL1(28,4,07,0)                EXCEPTIONS ONLY?                   
         DC    AL1(29,4,07,0)      STAGE                                        
         DC    AL1(30,4,07,0)                DAYPART DETAIL                     
*******  DC    AL1(31)  ******* AVAILABLE  *******                              
         DC    AL1(32,4,07,0)      ID SEQUENCE        D RECAP                   
         DC    AL1(33,4,07,0)                INCLUDE PAID DATA                  
         DC    AL1(34,4,07,0)                EXCLUDE AFFIDAVID                  
         DC    AL1(35,4,07,0)                AMOUNT TYPE                        
         DC    AL1(36,4,07,0)                BILLING OPTION                     
         DC    AL1(37,4,07,0)                ANALYSIS TYPE                      
         DC    AL1(38,4,07,0)                PAID TOTALS?                       
         DC    AL1(39,4,07,0)                AUTHORISD TOTALS?                  
         DC    AL1(40,4,16,0)      DEMOGRAPHIC                                  
         DC    AL1(41,4,02,0)                BROADCAST MONTH                    
         DC    AL1(42,4,02,0)                CURRENT MONTH                      
         DC    AL1(43,4,07,0)                BUY DETAILS?                       
         DC    AL1(44,4,07,0)                XCLUD CLI DETAIL?                  
         DC    AL1(45,4,07,0)                XCLUD NET AMOUNT?                  
         DC    AL1(46,4,07,0)                CLIENT TOTALS?                     
         DC    AL1(47,4,07,0)                UNCOMFIRMED ONLY?                  
         DC    AL1(48,4,13,0)                RATING SERVICE                     
         DC    AL1(49,4,07,0)                AUDIENCE TYPE                      
         DC    AL1(50,4,17,0)      DATA TYPES                                   
         DC    AL1(51,4,07,0)                TAPE EXTRACT?                      
         DC    AL1(52,4,07,0)                PACKAGE TAPE BUY?                  
         DC    AL1(53,4,07,0)                GOALS TAPE?                        
         DC    AL1(54,4,07,0)                BUYS TAPE?                         
         DC    AL1(55,4,07,0)          ANALYZE BY OFFICE                        
         DC    AL1(56,4,07,0)                RERUN?                             
*******  DC    AL1(58) ******* AVAILABLE  *********                             
         DC    AL1(57,4,06,0)                REVISION NUMBER                    
         DC    AL1(59,4,02,0)                CUT-OFF DATE                       
         DC    AL1(60,4,07,0)                ES-LIN ORDER?                      
         DC    AL1(61,4,07,0)                SPACING                            
         DC    AL1(62,4,07,0)                SPECIAL REP?                       
         DC    AL1(63,4,07,0)                XCLUD COSTS?                       
         DC    AL1(64,4,07,0)                XCLUD DEMOS?                       
         DC    AL1(65,4,12,0)          DAYPART FILTER                           
         DC    AL1(66,4,10,0)          RPT DEMOS                                
         DC    AL1(67,4,11,0)                MARKET SEQUENCE                    
         DC    AL1(68,4,07,0)                FEE BASIS                          
         DC    AL1(69,4,07,0)                PRINT OPTION                       
*******  DC    AL1(70) *******  AVAILABLE  ********                             
         DC    AL1(71,4,02,0)          DATE ON CHECKS                           
         DC    AL1(72,4,12,0)          AFFILIATE                                
         DC    AL1(73,4,07,0)           SPECIAL REP BREAKOUT                    
         DC    AL1(74,4,02,0)          CORPORATRE BOOK                          
         DC    AL1(75,4,07,0)          DAY PART CODE                            
         DC    AL1(76,4,10,0)          DEMOS                                    
         DC    AL1(77,3,09,0)           TARGET AUDIENCE                         
         DC    AL1(78,4,07,0)           EX CLT GROUP                            
         DC    AL1(79,4,07,0)                BASED ON AFFID                     
         DC    AL1(80,4,07,0)                REALLOCATE                         
         DC    AL1(81,4,07,0)                MKT ANAL                           
         DC    AL1(82,4,01,0)      PROPERTY                                     
         DC    AL1(83,4,07,0)      CLT OWNER FILTER                             
         DC    AL1(84,4,07,0)      DETAIL OPTION                                
         DC    AL1(85,4,07,0)      RERATE OPTION                                
         DC    AL1(86,4,12,0)      PROGRAM TYPE                                 
         DC    AL1(87,4,07,0)           TOTAL OPTION                            
         DC    AL1(88,4,07,0)           TOTALS BY DAYPART                       
         DC    AL1(89,3,19,0)           DAYPART OVERRIDE                        
         DC    AL1(90,4,07,0)           WEEKLY ANALYSIS                         
         DC    AL1(91,4,07,0)           MONTHLY ANALYSIS                        
         DC    AL1(92,4,07,0)           BRAND ANALYSIS                          
         DC    AL1(93,3,10,0)           PRD OR PRD GROUP                        
         DC    AL1(94,3,11,0)           MKT OR MKT GROUP                        
         DC    AL1(95,4,07,0)           DATE OPTION                             
         DC    AL1(96,4,07,0)           RECAP OPTION                            
         DC    AL1(97,4,07,0)           DATA OPTION                             
         DC    AL1(98,4,07,0)           REPLACE DEMOS                           
         DC    AL1(99,4,07,0)           REPLACE OVERRIDES                       
         DC    AL1(100,4,07,0)          EXTRACT GOALS                           
         DC    AL1(101,4,07,0)          EXTRACT BUYS                            
         DC    AL1(102,4,07,0)          EXTRACT DETAILS                         
         DC    AL1(103,4,07,0)     XCLUD HIATUS                                 
         DC    AL1(104,4,07,0)     EXTRACT AFFIDS                               
         DC    AL1(105,03,12,0)    DUE DAYS                                     
         DC    AL1(106,4,07,0)     PGRPS                                        
         DC    AL1(107,4,07,0)     MGRPS                                        
         DC    AL1(108,4,14,0)     MGRP SCHEME                                  
         DC    AL1(109,4,07,0)     LEVEL CONTROL                                
         DC    AL1(110,4,07,0)     SUMMARIES ONLY                               
         DC    AL1(111,4,07,0)         STATIONS PER MKT                         
         DC    AL1(112,4,07,0)         GOAL/AUTH TOTALS                         
         DC    AL1(113,4,15,0)         NETWORK                                  
         DC    AL1(114,3,13,0)     AMOUNT - NEW BILLING                         
         DC    AL1(115,4,07,0)     BILLING TYPE                                 
         DC    AL1(116,4,07,0)         TEST RUN                                 
         DC    AL1(117,4,07,0)         LIST UNPOSTED                            
         DC    AL1(118,4,07,0)         LIST PREVIOUSLY POSTED                   
         DC    AL1(119,4,07,0)     ESTIMATE OPTION                              
         DC    AL1(120,4,07,0)     PRODUCTS ALL TOGETHER?                       
         DC    AL1(121,4,18,0)     REPORT                                       
         DC    AL1(122,4,07,0)     SUPPRESS SPOT LENGHT                         
         DC    AL1(123,4,07,0)     SCHEME CHECK                                 
         DC    AL1(124,4,07,0)     MULTIPLE TARGET MODE                         
         DC    AL1(125,4,07,0)     RPT BY MTHS?                                 
*              DON'T USE 126 - CARD REQ                                         
*                                                                               
*              DON'T USE 127 - TAB LINE                                         
*                                                                               
         DC    AL1(128,4,07,0)     RPT BY FLTS                                  
         DC    AL1(129,4,07,0)     RPT BY QUARTERS?                             
         DC    AL1(130,4,07,0)     RPT BY YEARS?                                
         DC    AL1(131,4,07,0)     RECORD TYPE                                  
         DC    AL1(132,4,07,0)     PRINT ALL BRANDS                             
         DC    AL1(133,4,19,0)     PARTIAL MATCH OPTS                           
         DC    AL1(134,4,07,0)     POST AFFIDS                                  
         DC    AL1(135,4,07,0)     LIST COMMENTS                                
         DC    AL1(136,4,07,0)     PRINT NETWORK AFFILIATION                    
         DC    AL1(137,4,02,0)     PURCHASE END DATE                            
         DC    AL1(138,4,20,0)     RE-AL PRDS                                   
         DC    AL1(139,4,21,0)     DEMO MENU                                    
         DC    AL1(140,4,07,0)     ADD BUYS?                                    
         DC    AL1(141,4,22,0)     REP NUM  - SNNN,PNNN,SALL,PALL               
         DC    AL1(142,4,23,0)     DEMO MENU FILTER                             
         DC    AL1(143,4,12,0)     FILM TYPE FILTER-M2-M4 (ANY ALPHA)           
         DC    AL1(144,3,14,0)     SHOW - CANADA GOES TO STATION                
         DC    AL1(145,4,07,0)     CHANGE INVOICES                              
         DC    AL1(146,4,07,0)     SPILL OPTION                                 
         DC    AL1(147,3,15,0)     WEIGHT OVERRIDES                             
         DC    AL1(148,3,16,0)     SPOT LENGHT                                  
         DC    AL1(149,3,17,0)     SORT FIELD                                   
         DC    AL1(150,4,07,0)     BUYING PERIOD                                
         DC    AL1(151,4,07,0)     CUTINS                                       
         DC    AL1(152,4,07,0)     COST OVERRIDES                               
         DC    AL1(153,4,07,0)     LOCAL DEMOS                                  
         DC    AL1(154,4,12,0)     REGION                                       
         DC    AL1(155,4,24,0)     COMMERCIAL ID                                
         DC    AL1(156,4,07,0)     PRODUCE TAPE                                 
         DC    AL1(157,4,17,0)     SPOT LENGTH FILTER                           
         DC    AL1(158,4,07,0)     GROSS/NET/BOTH                               
         DC    AL1(159,4,07,0)     PRINT REPORT/LETTERS?                        
         DC    AL1(160,3,11,0)     MKTGRP                                       
         DC    AL1(161,3,08,0)     END                                          
         DC    AL1(162,4,07,0)     SUPPRESS MARKET DETAIL                       
         DC    AL1(163,4,07,0)     SUPPRESS ALL MATCHED                         
         DC    AL1(164,4,07,0)     DETAIL FORMAT                                
         DC    AL1(165,4,07,0)     GROSS/NET DOLLARS                            
         DC    AL1(166,4,07,0)     INCLUDE PERIOD COLUMN                        
         DC    AL1(167,3,24,0)     TO-PRODUCT                                   
         DC    AL1(168,4,07,0)     DOLLAR FORMAT                                
         DC    AL1(169,4,06,0)     DOLLAR ADJUSTMENT                            
         DC    AL1(170,4,02,0)     BILL/PAY  START                              
         DC    AL1(171,4,02,0)     BILL/PAY END                                 
         DC    AL1(172,4,12,0)     OPTIONS?                                     
         DC    AL1(173,4,02,0)     LETTER DATE                                  
         DC    AL1(174,4,07,0)     XTRA GRID LINES                              
         DC    AL1(175,4,07,0)     CLASS/CML DETAIL                             
         DC    AL1(176,4,07,0)     SUMMARY OPTION                               
         DC    AL1(177,4,07,0)     UNKNOWN CML DETAIL                           
         DC    AL1(178,4,07,0)     ACTUAL BOOK?                                 
         DC    AL1(179,4,25,0)     INCLUDE REPORTS (A2,AB)                      
         DC    AL1(180,4,07,0)     BILLED TODAY ONLY                            
         DC    AL1(181,4,07,0)     BY SPOT LENGTH                               
         DC    AL1(183,4,07,0)     FILM REPORT OPTION                           
         DC    AL1(184,4,07,0)     QUARTER                                      
         DC    AL1(185,4,07,0)     DOWNLOAD                                     
         DC    AL1(186,4,07,0)     30'S ONLY                                    
         DC    AL1(187,4,07,0)     STATION SIZE ANALYSIS                        
         DC    AL1(188,4,07,0)     PRINT CHANNEL                                
         DC    AL1(189,4,07,0)     XCLUDE SPCL REP $                            
         DC    AL1(190,4,07,0)     DATA TYPE                                    
         DC    AL1(191,4,07,0)     INCLUDE $0 LETTER                            
         DC    AL1(192,4,07,0)     PRINT REPORT                                 
         DC    AL1(193,4,07,0)     POST                                         
         DC    AL1(194,4,07,0)     SUPPRESS COSTS                               
         DC    AL1(195,3,07,0)     (STATION)                                    
         DC    AL1(196,4,07,0)     BILL OPTION                                  
         DC    AL1(197,4,07,0)     TRACE DDS ONLY                               
         DC    AL1(198,4,07,0)     LIST PREV CONVRESIONS                        
         DC    AL1(199,4,07,0)     REPLACE EXISTING INVOICES                    
         DC    AL1(200,4,07,0)     CREATE Y=I2  P=PRINT                         
         DC    AL1(201,4,07,0)     (HIDDEN FIELD)                               
******   DC    AL1(202)  ****** AVAILABLE ********                              
         DC    AL1(203,4,04,0)     OPTIONS FIELD FOR REC2                       
         DC    AL1(204,4,07,0)     COST TYPE=T,I,U,B,S,O,1,2,3                  
         DC    AL1(205,3,14,0)     BILL TYPE                                    
         DC    AL1(206,4,02,0)     INTERFACE DATE                               
         DC    AL1(207,3,18,0)     ANYVAL-VARIOUS AND SUNDRY                    
         DC    AL1(208,4,07,0)     CLIENT EXCLUSION                             
         DC    AL1(209,4,07,0)     POL BREAKOUT                                 
         DC    AL1(210,4,07,0)     ACTIVITY REPORT                              
         DC    AL1(211,4,07,0)     STATION BREAKS                               
         DC    AL1(212,4,07,0)     RERATE                                       
         DC    AL1(213,4,07,0)     STATION BY AFFIL                             
         DC    AL1(214,4,02,0)     DATE                                         
         DC    AL1(215,4,07,0)     DATE TYPE                                    
         DC    AL1(216,4,26,0)     BILL NUMBER                                  
         DC    AL1(217,3,18,0)     ANYVAL-(AFFILIATE FILTER)                    
         DC    AL1(218,4,07,0)     STATION TYPE                                 
         DC    AL1(219,3,20,0)     PRD/LEN VAL                                  
         DC    AL1(220,4,27,0)     REALLOCATE PRD1                              
         DC    AL1(221,4,27,0)     REALLOCATE PRD2                              
         DC    AL1(222,4,27,0)     REALLOCATE PRD3                              
         DC    AL1(223,4,27,0)     REALLOCATE PRD4                              
         DC    AL1(224,4,07,0)     INCLUDE PB'S                                 
*******  DC    AL1(225)  *******  AVAILABLE  ******                             
         DC    AL1(226,4,12,0)     CML CLASS FILTER (=FILM TYPE FILTER)         
*                                  USED FOR MC AND MD REQUESTS                  
         DC    AL1(227,4,07,0)     SUPPRESS -S WKY                              
         DC    AL1(228,3,10,0)     PROD GRPS                                    
         DC    AL1(229,3,18,0)     LINE NUMBER                                  
         DC    AL1(230,4,07,0)     LINE NUMBER                                  
         DC    AL1(230,4,07,0)     LINE NUMBER                                  
         DC    AL1(231,4,07,0)     VERSION                                      
         DC    AL1(232,4,07,0)     NCT OPTION                                   
         DC    AL1(233,4,07,0)     DEACTIVATED ONLY                             
         DC    AL1(234,4,07,0)     CABLE BY SYSTEM                              
         DC    AL1(235,4,28,0)     PROGRAM NAME                                 
         DC    AL1(236,4,07,0)     EASI INVOICES                                
         DC    AL1(237,4,07,0)     INCLUDE AFFIDAVIT                            
         DC    AL1(238,3,18,0)     TO-CLIENT                                    
         DC    AL1(239,3,5,0)      TO-EST                                       
         DC    AL1(240,3,21,0)     PROD=WEIGHT AAA=123                          
         DC    AL1(241,4,07,0)     INCLUDE NO CHARGE SPOTS                      
         DC    AL1(242,4,07,0)     INCLUDE PREEMPTED SPOTS                      
         DC    AL1(243,3,22,0)     NEW EST (KB REPORT)                          
         DC    AL1(244,3,23,0)     NEW PERIOD START DATE  (KB REPORT)           
         DC    AL1(245,3,24,0)     NEW PROD (KB REPORT)                         
         DC    AL1(246,4,07,0)     SUPPRESS COSTS                               
         DC    AL1(247,4,07,0)     SUPPRESS DEMOS                               
         DC    AL1(248,3,18,0)     START INVOICE #                              
         DC    AL1(249,3,18,0)     ENDING INVOICE #                             
         DC    AL1(250,4,12,0)     SOURCE                                       
         DC    AL1(251,4,07,0)     SPILL MKT SORT                               
         DC    AL1(252,4,07,0)     INCLUDE SPILL MARKET                         
         DC    AL1(253,4,07,0)     PRD SUMMARY FORMAT                           
FLDNUMTX DC    XL1'00'                                                          
         EJECT                                                                  
*  ACCESS AWARE SECURITY TABLE IS COVERED BY ACCESSD DSECT                      
*                                                                               
*        NOTE THAT ACTIONS XL1'6F' AND XL1'70' ARE FREE                         
*        AND COULD BE USED IN THE FUTURE                                        
*        HIGHEST USED (AS OF 02/28/12) IS XL1'77'                               
*                                                                               
ACCESSTB DS    0C                                    DDICT#   MSG#              
*   MEDIA REPORTS                                    SP#G01     36              
         DC    C'D2',C'BRS',XL1'01',XL1'01',XL1'00'  SP#D2      35              
         DC    C'D3',C'BTS',XL1'02',XL1'01',XL1'00'  SP#D3      37              
         DC    C'D4',C'SAL',XL1'03',XL1'01',XL1'00'  SP#D4      38              
         DC    C'D5',C'PBS',XL1'04',XL1'01',XL1'00'  SP#D5      39              
         DC    C'D6',C'BDT',XL1'05',XL1'01',XL1'00'  SP#D6      40              
         DC    C'D7',C'COP',XL1'06',XL1'01',XL1'00'  SP#D7      41              
         DC    C'D8',C'SWE',XL1'07',XL1'01',XL1'00'  SP#D8      42              
         DC    C'DC',C'MCL',XL1'08',XL1'01',XL1'00'  SP#DC      43              
         DC    C'DD',C'DD-',XL1'09',XL1'01',XL1'00'  SP#DD      44              
         DC    C'DL',C'DL-',XL1'0A',XL1'01',XL1'00'  SP#DL      45              
         DC    C'DX',C'COP',XL1'0B',XL1'01',XL1'00'  SP#DX      46              
         DC    C'M2',C'BRP',XL1'0D',XL1'01',XL1'00'  SP#M2      48              
         DC    C'M3',C'BWS',XL1'0E',XL1'01',XL1'00'  SP#M3      49              
         DC    C'M4',C'MAR',XL1'0F',XL1'01',XL1'00'  SP#M4      50              
         DC    C'M6',C'PBP',XL1'10',XL1'01',XL1'00'  SP#M6      51              
         DC    C'M7',C'PMP',XL1'11',XL1'01',XL1'00'  SP#M7      52              
         DC    C'MG',C'MG-',XL1'12',XL1'01',XL1'00'  SP#SMG     53              
         DC    C'RN',C'NRS',XL1'13',XL1'01',XL1'00'  SP#RN      54              
         DC    C'RS',C'SRS',XL1'14',XL1'01',XL1'00'  SP#RS      55              
         DC    C'RX',C'NRX',XL1'15',XL1'01',XL1'00'  SP#RX      56              
         DC    C'C2',C'NET',XL1'58',XL1'01',XL1'00'  SP#SC2    198              
         DC    C'ML',C'LIN',XL1'4A',XL1'01',XL1'00'  SP#ML     192              
         DC    C'RY',C'SRY',XL1'1A',XL1'01',XL1'00'  SP#RY     199              
         DC    C'DN',C'NET',XL1'2A',XL1'01',XL1'00'  SP#DN      80              
         DC    C'SS',C'SS-',XL1'64',XL1'01',XL1'00'  SP#SS     276              
         DC    C'N5',C'NBS',XL1'67',XL1'01',XL1'00'  SP#N5     279              
         DC    C'VK',C'VK-',XL1'7D',XL1'01',XL1'00'  SP#VK     418              
*   PLANNING REPORTS                                 SP#G02     57              
         DC    C'M8',C'MMP',XL1'16',XL1'02',XL1'00'  SP#M8      58              
         DC    C'M9',C'BMD',XL1'17',XL1'02',XL1'00'  SP#M9      59              
         DC    C'K3',C'GPR',XL1'4D',XL1'02',XL1'00'  SP#K3     195              
         DC    C'KA',C'KA-',XL1'4E',XL1'02',XL1'00'  SP#KA     196              
         DC    C'KB',C'KB-',XL1'4F',XL1'02',XL1'00'  SP#KB     197              
*   FILE LIST REPORTS                                SP#G03     60              
         DC    C'L7',C'DOR',XL1'0C',XL1'03',XL1'00'  SP#L7      47              
         DC    C'40',C'AML',XL1'6B',XL1'03',XL1'00'  SP#40     221              
         DC    C'41',C'CPL',XL1'18',XL1'03',XL1'00'  SP#41      61              
         DC    C'47',C'MPL',XL1'19',XL1'03',XL1'00'  SP#47      62              
         DC    C'48',C'STL',XL1'1B',XL1'03',XL1'00'  SP#48      63              
         DC    C'74',C'RPL',XL1'1C',XL1'03',XL1'00'  SP#74      64              
         DC    C'C1',C'C1-',XL1'1D',XL1'03',XL1'00'  SP#C1      65              
         DC    C'L6',C'CML',XL1'1E',XL1'03',XL1'00'  SP#L6      66              
         DC    C'44',C'NSL',XL1'57',XL1'03',XL1'00'  SP#44     204              
         DC    C'LD',C'DML',XL1'5A',XL1'03',XL1'00'  SP#LD     205              
*   INVOICE MATCHING REPORTS                         SP#G04     67              
         DC    C'I2',C'IMR',XL1'1F',XL1'04',XL1'00'  SP#I2      68              
         DC    C'I3',C'INV',XL1'20',XL1'04',XL1'00'  SP#I3      69              
         DC    C'FF',C'FF-',XL1'21',XL1'04',XL1'00'  SP#FF     355              
         DC    C'I5',C'INL',XL1'22',XL1'04',XL1'00'  SP#I5      71              
         DC    C'I6',C'IN-',XL1'23',XL1'04',XL1'00'  SP#I6      72              
         DC    C'Z5',C'Z5-',XL1'24',XL1'04',XL1'00'  SP#Z5      73              
         DC    C'Z7',C'Z7-',XL1'69',XL1'04',XL1'00'  NE#Z7       2              
         DC    C'IM',C'IMM',XL1'68',XL1'04',XL1'00'  SP#IM     223              
*   FINANCIAL REPORTS                                SP#G05     74              
         DC    C'49',C'AGE',XL1'25',XL1'05',XL1'00'  SP#49      75              
         DC    C'A3',C'A3-',XL1'26',XL1'05',XL1'00'  SP#A3      76              
         DC    C'A8',C'BCR',XL1'28',XL1'05',XL1'00'  SP#A8      78              
         DC    C'AB',C'CFA',XL1'29',XL1'05',XL1'00'  SP#AB      79              
         DC    C'L2',C'EHR',XL1'2B',XL1'05',XL1'00'  SP#L2      81              
         DC    C'A2',C'PRO',XL1'5C',XL1'05',XL1'00'  SP#A2     207              
*   BILLING REPORTS                                  SP#G06     83              
         DC    C'B1',C'BIL',XL1'2D',XL1'06',XL1'00'  SP#B1      84              
         DC    C'B9',C'RET',XL1'2E',XL1'06',XL1'00'  SP#B9      85              
         DC    C'BD',C'RDS',XL1'2F',XL1'06',XL1'00'  SP#BD      86              
         DC    C'BT',C'BTR',XL1'30',XL1'06',XL1'00'  SP#BT      87              
         DC    C'D1',C'BIL',XL1'31',XL1'06',XL1'00'  SP#D1     208              
         DC    C'LB',C'BIL',XL1'36',XL1'06',XL1'00'  SP#LB     233              
         DC    C'BU',C'NTB',XL1'65',XL1'06',XL1'00'  NE#BU                      
         DC    C'DU',C'NTB',XL1'66',XL1'06',XL1'00'  NE#DU                      
         DC    C'VL',C'VL-',XL1'7C',XL1'06',XL1'00'  SP#SVL    416              
*   ALLOCATION REPORTS                               SP#G07     89              
         DC    C'K1',C'K1-',XL1'32',XL1'07',XL1'00'  SP#K1      90              
         DC    C'K2',C'RAL',XL1'33',XL1'07',XL1'00'  SP#K2      91              
         DC    C'K4',C'NAL',XL1'34',XL1'07',XL1'00'  SP#K4      92              
******   DC    C'K5',C'NAL',XL1'35',XL1'07',XL1'00'  SP#K5                      
         DC    C'KL',C'KL-',XL1'37',XL1'07',XL1'00'  SP#KL      95              
*   SPECIAL REPORTS                                  SP#G08     96              
         DC    C'A7',C'MAA',XL1'38',XL1'08',XL1'00'  SP#A7      97              
         DC    C'CD',C'CD-',XL1'39',XL1'08',XL1'00'  SP#CD      98              
         DC    C'CI',C'CI-',XL1'6E',XL1'08',XL1'00'  SP#CI     296              
         DC    C'CR',C'CR-',XL1'3A',XL1'08',XL1'00'  SP#CR      99              
         DC    C'DR',C'DRR',XL1'3B',XL1'08',XL1'00'  SP#DR     111              
         DC    C'EB',C'EB-',XL1'3C',XL1'08',XL1'00'  SP#EB     136              
         DC    C'J1',C'J1-',XL1'3D',XL1'08',XL1'00'  SP#J1     137              
         DC    C'LT',C'LT-',XL1'6D',XL1'08',XL1'00'  SP#LT     297              
         DC    C'm4',C'MAR',XL1'3E',XL1'08',XL1'00'  SP#M4L    138              
         DC    C'MJ',C'MJ-',XL1'3F',XL1'08',XL1'00'  SP#MJ     139              
         DC    C'CE',C'CE-',XL1'40',XL1'08',XL1'00'  SP#CE     226              
         DC    C'PF',C'PF-',XL1'41',XL1'08',XL1'00'  SP#PF     149              
         DC    C'PM',C'PM-',XL1'42',XL1'08',XL1'00'  SP#PM     176              
         DC    C'PR',C'PR-',XL1'43',XL1'08',XL1'00'  SP#PR     177              
         DC    C'W2',C'BRP',XL1'44',XL1'08',XL1'00'  SP#W2     178              
         DC    C'W4',C'SAL',XL1'45',XL1'08',XL1'00'  SP#W4     179              
         DC    C'W9',C'BMD',XL1'46',XL1'08',XL1'00'  SP#W9     185              
         DC    C'X3',C'CSP',XL1'47',XL1'08',XL1'00'  SP#X3     186              
         DC    C'J6',C'J6-',XL1'48',XL1'08',XL1'00'  SP#J6     227              
         DC    C'M5',C'IFT',XL1'49',XL1'08',XL1'00'  SP#M5     228              
         DC    C'SE',C'SE-',XL1'6C',XL1'08',XL1'00'  SP#SE     295              
         DC    C'SY',C'SPL',XL1'60',XL1'08',XL1'00'  SP#SY     229              
         DC    C'X1',C'PBR',XL1'61',XL1'08',XL1'00'  SP#X1     231              
         DC    C'XD',C'PAR',XL1'62',XL1'08',XL1'00'  SP#XD     232              
         DC    C'WC',C'PAR',XL1'63',XL1'08',XL1'00'  SP#WC     230              
         DC    C'N2',C'IMR',XL1'5B',XL1'08',XL1'00'  SP#N2     206              
         DC    C'SX',C'MFX',XL1'56',XL1'08',XL1'00'  SP#SX     235              
         DC    C'GT',C'GT-',XL1'55',XL1'08',XL1'00'  SP#GT     240              
         DC    C'CM',C'CM-',XL1'52',XL1'08',XL1'00'  SP#CM     280              
         DC    C'M1',C'CPX',XL1'53',XL1'08',XL1'00'  SP#M1     285              
         DC    C'SC',C'SFX',XL1'6A',XL1'08',XL1'00'  SP#SC     290              
         DC    C'PH',C'PH-',XL1'71',XL1'08',XL1'00'  SP#PH     299              
         DC    C'GM',C'GM-',XL1'72',XL1'08',XL1'00'  SP#GM     310              
         DC    C'EX',C'EX-',XL1'73',XL1'08',XL1'00'  SP#EX     319              
         DC    C'LO',C'LOR',XL1'74',XL1'08',XL1'00'  SP#OLR    ???              
         DC    C'TD',C'TD-',XL1'75',XL1'08',XL1'00'  SP#TD     995              
         DC    C'JW',C'JW-',XL1'76',XL1'08',XL1'00'  SP#JW     995              
         DC    C'WB',C'WB-',XL1'77',XL1'08',XL1'00'  SP#WB    1001              
         DC    C'IN',C'IN-',XL1'78',XL1'08',XL1'00'  SP#INF                     
         DC    C'CH',C'CH-',XL1'79',XL1'08',XL1'00'  SP#CHI                     
         DC    C'PZ',C'PZ-',XL1'7A',XL1'08',XL1'00'  SP#SPZ                     
         DC    C'AI',C'AI-',XL1'7B',XL1'08',XL1'00'  SP#SAI                     
*                                                                               
*              WHEN A WB IS REQUESTED AN ZB REQUEST IS CREATED                  
*              THIS WAS DONE BECAUSE A JCL BOOK FOR A WB ALREADY                
*              EXISTED - USED BY A NET WRITER PHASE                             
*                                                                               
*   PAYING REPORTS                                   SP#G09    209              
         DC    C'A5',C'STS',XL1'27',XL1'09',XL1'00'  SP#A5      77              
         DC    C'NV',C'NV-',XL1'2C',XL1'09',XL1'00'  SP#NV      82              
*   TRAFFIC REPORTS                                  SP#G0A    218              
         DC    C'T9',C'FPL',XL1'4C',XL1'0A',XL1'00'  SP#T9     194              
         DC    C'MC',C'MC-',XL1'5E',XL1'0A',XL1'00'  SP#MC     219              
         DC    C'MD',C'MD-',XL1'5F',XL1'0A',XL1'00'  SP#MD     220              
ACCESSEQ EQU   (*-ACCESSTB)/ACCESSLN                                            
*        DC    C'I4',C'INL',XL1'21',XL1'04',XL1'00'                             
*        DC    C'IL',C'IL-',XL1'67',XL1'04',XL1'00'                             
*        DC    C'K6',C'BAL',XL1'36',XL1'07',XL1'00'                             
*        DC    C'SP',C'SPG',XL1'4B',XL1'03',XL1'00'                             
*        DC    C'F2',C'COM',XL1'69',XL1'0A',XL1'00'                             
*        DC    C'F4',C'MKT',XL1'6A',XL1'0A',XL1'00'                             
*   DDS ONLY REPORTS                                                            
*        DC    C'07',C'07-',XL1'50',XL1'0X',XL1'00'                             
*        DC    C'85',C'HUT',XL1'51',XL1'0X',XL1'00'                             
*        DC    C'MY',C'MY-',XL1'54',XL1'0X',XL1'00'                             
*        EJECT                                                                  
*        THIS TABLE CONTAINS A VARIABLE LENGTH ENTRY FOR EACH REQUEST           
*        CL1   ENTRY LENGTH - ZERO=END-OF-TABLE                                 
*        CL2   REQUEST NUM / REQUEST SUB NUM                                    
*        CL1   FORMAT BITS   X'80'=TV                                           
*                            X'40'=RADIO                                        
*                            X'20'=ALL                                          
*                            X'10'=SOON REQUEST                                 
*                            X'08'=CARD REQUIRED                                
*                            X'04'=DDS REQUIRED                                 
*                            X'02'=MEDIA REQUIRED                               
*                            X'01'=REQUESTOR REQUIRED                           
*        CL22  REQUEST ID CL3 & REQUEST NAME                                    
*        CL2   N/D                                                              
*        0CLN  ENTRY FOR EACH SCREEN FOR REQUEST                                
*        CL1   ENTRY LENGTH                                                     
*        CL1   MEDIA BITS - B'TR000000'                                         
*              3RD BIT WILL BE ON FOR MEDIA A OR *                              
*        CL3   FIELD NUM / FIELD FORMAT / REQUEST CARD COLUMN                   
*              X'80' BIT OF CARD COLUMN = DDS ONLY                              
*              FIELD NUM EQ 000 (CL1) END-OF-FIELD LIST                         
*              X'01' AFTER CARD COLUMN MEANS COMMENT NUMBER FOLLOWS             
*              ODD=SAME LINE,EVEN=NEXT LINE                                     
*        XL1   ZERO FOR END OF ENTRY                                            
*       CL2    ALPHA REQUEST ID                                                 
         SPACE 2                                                                
REQTBL   DS    0CL1                                                             
         SPACE 2                                                                
RT00     DC    AL1(RT00X-*+3,00,0) 00 - UNKNOWN REQUEST                         
         DC    B'11000100',CL22'???-UNKNOWN'                                    
         DC    X'0000'                                                          
         DC    AL1(RT00X-*+1),B'11000000'                                       
         DC    X'0207060181'       O CLI=ALL/XXX                                
         DC    X'030F0C0181'       O PRO=ALL/XXX/POL                            
         DC    X'051F180181'       O EST=NNN/NNN-NNN/NO                         
         DC    X'060F0F0181'       O MARK=ALL/ALLN/NNNN                         
         DC    X'430F0A'           O MAR SEQ=DST/REG/REG-N                      
         DC    X'091D26'           O STR,END=YYMMDD/YYMM/ES                     
         DC    X'CB0551'                   O OPTIONS                            
RT00X    DC    X'00',C'00'                                                      
         SPACE 2                                                                
RT01     DC    AL1(RT01X-*+3,01,0)                                              
         DC    B'11000011',CL22'CD-COKE LOCK-IN COMP'                           
         DC    X'0000'                                                          
         DC    AL1(RT01X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'04040C0181'                 PRO=XXX  *TESTING*                 
         DC    X'0504180181'                 EST=NNN                            
         DC    X'06080F0181'                 MKT                                
         DC    X'070413'                     STA=XXXX                           
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'CB0551'                   O OPTIONS                            
RT01X    DC    X'00',C'CD'                                                      
         SPACE 2                                                                
RT02     DC    AL1(RT02X-*+3,02,0)                                              
         DC    B'11000011',CL22'EB-EDI BILLING TRANS'                           
         DC    X'0000'                                                          
         DC    AL1(RT02X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'F80532'                     STRT INVOICE #                     
         DC    X'F90536'                     END INVOICE #                      
         DC    X'74043F'                     TEST RUN=Y/N                       
         DC    X'3805400105'                 RERUN=R                            
         DC    X'CB0551'                   O OPTIONS                            
RT02X    DC    X'00',C'EB'                                                      
         SPACE 2                                                                
RT03     DC    AL1(RT03X-*+3,03,0)                                              
         DC    B'11010011',CL22'PH-PHILIP MORRIS INTFC'                         
         DC    X'0000'                                                          
         DC    AL1(RT03X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'740443'                     TEST RUN=Y/N                       
         DC    X'CB0551'                   O OPTIONS                            
RT03X    DC    X'00',C'PH'                                                      
         SPACE 2                                                                
RT04     DC    AL1(RT04X-*+3,04,0)                                              
         DC    B'11010111',CL22'SJV-DEMO TRACE REPORT'                          
         DC    X'0000'                                                          
         DC    AL1(RT04X-*+1),B'11000000'                                       
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRD                                          
         DC    X'0504180181'       EST=NNN                                      
         DC    X'070413'           STA=XXXX                                     
         DC    X'E504690123'       LINE NUMBER                                  
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550438'           RERARE TYPE                                  
         DC    X'CB0551'           OPTIONS                                      
RT04X    DC    X'00',C'JV'                                                      
         SPACE 2                                                                
RT05     DC    AL1(RT05X-*+3,05,0)                                              
         DC    B'11000011',CL22'CW-COST CONVERSION  '                           
         DC    X'0000'                                                          
         DC    AL1(RT05X-*+1),B'11000000'                                       
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRD                                          
         DC    X'0504180181'       EST=NNN                                      
         DC    X'D60926'           START DATE                                   
         DC    X'CB0551'           O OPTIONS                                    
RT05X    DC    X'00',C'CW'                                                      
*                                                                               
         SPACE 2                                                                
RT06     DC    AL1(RT06X-*+3,06,0)                                              
         DC    B'11010011',CL22'PR-PARAMOUNT TV SUMM'                           
         DC    X'0000'                                                          
         DC    AL1(RT06X-*+1),B'11000000'                                       
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRD=XXX                                      
         DC    X'0504180181'       EST=NNN                                      
         DC    X'5EAB0F0181'       MKT=ALL/NNNN,MGR=XNNN/XALL                   
         DC    X'070713'           STA=ALL/XXXX                                 
         DC    X'091826'           STR,END=YYMMDD                               
         DC    X'0E3D32'           O BOOK-HUT (FROM D8)                         
         DC    X'550538'           RERATE TYPE (FROM D8)                        
         DC    X'39053F'           O REVISION NUMBER                            
         DC    X'CB0551'           O OPTIONS                                    
RT06X    DC    X'00',C'PR'                                                      
         SPACE 2                                                                
RT07     DC    AL1(RT07X-*+3,07,0)                                              
         DC    B'11010011',CL22'PM-PARAMOUNT MED SCHED'                         
         DC    X'0000'                                                          
         DC    AL1(RT06X-*+1),B'11000000'                                       
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRD=XXX                                      
         DC    X'0504180181'       EST=NNN                                      
         DC    X'5EAB0F0181'       MKT=ALL/NNNN,MGR=XNNN/XALL                   
         DC    X'070713'           STA=ALL/XXXX                                 
         DC    X'091826'           STR,END=YYMMDD                               
         DC    X'0E3D32'           O BOOK-HUT (FROM D8)                         
         DC    X'550538'           RERATE TYPE (FROM D8)                        
         DC    X'8B0522'           O DEMO MENU (FROM D8)                        
         DC    X'87053E'         O PRINT COMMENTS=Y,N                           
         DC    X'79053F01F3'     O REPORT (TYPE)                                
         DC    X'CB0551'           O OPTIONS                                    
RT07X    DC    X'00',C'PM'                                                      
*                                                                               
         SPACE 2                                                                
RT08     DC    AL1(RT08X-*+3,08,0)                                              
         DC    B'11010011',CL22'PF-PARAMOUNT MED SCHED'                         
         DC    X'0000'                                                          
         DC    AL1(RT08X-*+1),B'11000000'                                       
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRD=XXX                                      
         DC    X'0504180181'       EST=NNN                                      
         DC    X'060B0F0181'       MKT=ALL/NNNN                                 
         DC    X'070713'           STA=ALL/XXXX                                 
         DC    X'091826'           STR,END=YYMMDD                               
         DC    X'0E3D32'           O BOOK-HUT (FROM D8)                         
         DC    X'550538'           RERATE TYPE (FROM D8)                        
         DC    X'CB0551'           O OPTIONS                                    
RT08X    DC    X'00',C'PF'                                                      
*                                                                               
         SPACE 2                                                                
RT09     DC    AL1(RT09X-*+3,09,0)                                              
         DC    B'11010011',CL22'CE-EXCLUSION REPORT'                            
         DC    X'0000'                                                          
         DC    AL1(RT09X-*+1),B'11000000'                                       
         DC    X'0202060181'       CLI=ALL                                      
         DC    X'091D26'           O STR,END=YYMMDD/YYMM/ES                     
         DC    X'CB0551'           O OPTIONS                                    
RT09X    DC    X'00',C'CE'                                                      
*                                                                               
         SPACE 2                                                                
RT10     DC    AL1(RT10X-*+3,10,0)                                              
         DC    B'11010011',CL22'CR-COKE CATEGORY REPORT'                        
         DC    X'0000'                                                          
         DC    AL1(RT10X-*+1),B'11000000'                                       
         DC    X'0908260181'       START-END                                    
         DC    X'95043E0181'       SORT FIELD                                   
         DC    X'CB0551'           OPTIONS                                      
RT10X    DC    X'00',C'CR'                                                      
*                                                                               
         SPACE 2                                                                
RT11     DC    AL1(RT11X-*+3,11,0)                                              
         DC    B'11000011',CL22'CM-CANADIAN MKT FIX'                            
         DC    X'0000'                                                          
         DC    AL1(RT11X-*+1),B'11000000'                                       
         DC    X'0206060181'       CLT=ALL,XXX                                  
         DC    X'071013'           STA=XXXX  IF CLT NOT ALL - MUST              
*                                  FIND CLT EXCEPTION                           
         DC    X'0608320181'       MKT=NNNN                                     
         DC    X'CB0551'                   O OPTIONS                            
RT11X    DC    X'00',C'CM'                                                      
*                                                                               
RT12     DC    AL1(RT12X-*+3,12,0)                                              
         DC    B'11010011',CL22'07-Spot Unbilling'                              
         DC    X'0000'                                                          
         DC    AL1(RT12X-*+1),B'11000000'                                       
         DC    X'0206060181'       CLI=XXX,ALL                                  
         DC    X'03060C0181'       PRO=ALL/XXX                                  
         DC    X'0526180181'       EST=NNN,ALL                                  
         DC    X'D60826010B'       DATE (INVOICE RUN DATE)                      
         DC    X'D80532010D'       BILL NUMBER                                  
         DC    X'0A0536'           MONTH OF SERVICE                             
         DC    X'720465010F'       AMOUNT (GROSS, HEADERS)                      
         DC    X'CB0551'           OPTIONS                                      
RT12X    DC    X'00',C'07'                                                      
*                                                                               
RT13     DC    AL1(RT13X-*+3,13,0)                                              
         DC    B'11010011',CL22'7U - Net Unbilling'                             
         DC    X'0000'                                                          
         DC    AL1(RT13X-*+1),B'11000000'                                       
         DC    X'0206060181'       CLI=XXX,ALL                                  
         DC    X'03060C0181'       PRO=ALL/XXX                                  
         DC    X'0526180181'       EST=NNN,ALL                                  
         DC    X'D60826010B'       DATE (INVOICE RUN DATE)                      
         DC    X'D80532010D'       BILL NUMBER                                  
         DC    X'720465010F'       AMOUNT (GROSS, HEADERS)                      
         DC    X'CB0551'           OPTIONS                                      
RT13X    DC    X'00',C'7U'                                                      
*                                                                               
RT14     DC    AL1(RT14X-*+3,14,0)                                              
         DC    B'11110011',CL22'GT- GT EXTRACT'                                 
         DC    X'0000'                                                          
         DC    AL1(RT14X-*+1),B'11100000'                                       
         DC    X'0224060181'       CLI=XXX,OFFICE                               
         DC    X'03060C0181'       PRO=ALL/XXX                                  
         DC    X'05EE180181'       EST=NNN,ALL                                  
         DC    X'0908260181'       START-END                                    
         DC    X'9C053E'           PRODUCE TAPE                                 
         DC    X'CB0551'           OPTIONS                                      
RT14X    DC    X'00',C'GT'                                                      
*                                                                               
RT15     DC    AL1(RT15X-*+3,15,0)                                              
         DC    B'11010011',CL22'SS- SUPERDESK STATUS'                           
         DC    X'0000'                                                          
         DC    AL1(RT15X-*+1),B'11000000'                                       
         DC    X'0267060181'       CLI=XXX/ALL/*NN,CGR=                         
         DC    X'030F0C0181'       PRO=ALL/XXX/POL                              
         DC    X'05B7180181'       EST=NNN,ALL,NO,XXX                           
         DC    X'060B0F0181'       MKT=ALL/NNNN                                 
         DC    X'0908260181'       START-END                                    
         DC    X'D7043E011B'       DATE TYPE                                    
         DC    X'57053F011D'       TOTAL OPTION                                 
         DC    X'CB0551'           OPTIONS                                      
RT15X    DC    X'00',C'SS'                                                      
*                                                                               
RT16     DC    AL1(RT16X-*+3,16,0)                                              
         DC    B'11000011',CL22'STATION LISTING (DOWN)'                         
         DC    X'0000'                                                          
         DC    AL1(RT16X-*+1),B'11000000'                                       
         DC    X'83043E0121'       REC TYPE=S                                   
         DC    X'12053F'           INCLUDE ALL MEDIA ?                          
         DC    X'CB0551'           OPTIONS                                      
RT16X    DC    X'00',C'4D'                                                      
*                                                                               
RT17     DC    AL1(RT17X-*+3,17,0)                                              
         DC    B'11000011',CL22'DEMO LOOKUP ELEM FIX'                           
         DC    X'0000'                                                          
         DC    AL1(RT17X-*+1),B'11000000'                                       
         DC    X'0206060181'       CLI=XXX,ALL                                  
         DC    X'06080F0181'       MKT=NNNN                                     
         DC    X'D60826'           START DATE (NO END)                          
         DC    X'CB0551'           OPTIONS                                      
RT17X    DC    X'00',C'DZ'                                                      
*                                                                               
RT18     DC    AL1(RT18X-*+3,18,0)                                              
         DC    B'11000011',CL22'IMM-MSSD/MKGD INV CHK'                          
         DC    X'0000'                                                          
         DC    AL1(RT18X-*+1),B'11000000'                                       
         DC    X'0226060181'                 CLI=XXX,ALL,*NN                    
         DC    X'043E0C0181'     PRO,MODE=ALL,XXX,POL,XXX-YYY,XXX-ALL           
         DC    X'05FE180181'                 EST=NO/NNN/NNN-NNN                 
         DC    X'5EAF0F0181'                 MKT OR MKTGRP                      
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'20050B'                   O ID SEQ=N,Y                         
         DC    X'CB0551'                   O OPTIONS                            
RT18X    DC    X'00',C'IM'                                                      
         SPACE 2                                                                
RT19     DC    AL1(RT19X-*+3,19,0)                                              
         DC    B'11110011',CL22'LT -LABATT INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT19X-*+1),B'11100000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'9C0441'                     PRODUCE TAPE?                      
         DC    X'CB0551'                   O OPTIONS                            
RT19X    DC    X'00',C'LT'                                                      
         SPACE 2                                                                
RT20     DC    AL1(RT20X-*+3,20,0)                                              
         DC    B'11010011',CL22'SE -SPRINT INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT20X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'7404430127'                 TEST RUN?                          
         DC    X'CB0551'                   O OPTIONS                            
RT20X    DC    X'00',C'SE'                                                      
         SPACE 2                                                                
RT21     DC    AL1(RT21X-*+3,21,0)                                              
         DC    B'11010011',CL22'INV-INVOICE CHECK LIST'                         
         DC    X'0000'                                                          
         DC    AL1(RT21X-*+1),B'11000000'                                       
         DC    X'0226060181'                 CLI=XXX,ALL,*NN                    
         DC    X'043E0C0181'     PRO,MODE=ALL,XXX,POL,XXX-YYY,XXX-ALL           
         DC    X'05FE180181'                 EST=NO/NNN/NNN-NNN                 
         DC    X'5EAF0F0181'                 MKT OR MKTGRP                      
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'21053E'                   O INCLUDE PAID DATA?                 
         DC    X'22053F'                   O EXCLUDE AFFIDAVID?                 
         DC    X'080D1E0181'               O REP NUM=NNN                        
         DC    X'CB0551'                   O OPTIONS                            
RT21X    DC    X'00',C'I3'                                                      
         SPACE 2                                                                
RT22     DC    AL1(RT22X-*+3,22,0)                                              
         DC    B'11000011',CL22'INL-INVOICE LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT22X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'030F0C0181'               O PRO=ALL/XXX/POL                    
         DC    X'05FF180181'               O EST=NO/NNN/NNN-NNN                 
         DC    X'070413'                     STA=XXXX                           
         DC    X'094426'                     MONTH YYMM AND NO END              
         DC    X'8C053E01C5'                 ADD BUYS=Y,N,T                     
         DC    X'CB0551'                   O OPTIONS                            
RT22X    DC    X'00',C'I4'                                                      
         SPACE 2                                                                
RT23     DC    AL1(RT23X-*+3,23,0)                                              
         DC    B'11010011',CL22'IMR-INVOICE MATCHING'                           
         DC    X'0000'                                                          
         DC    AL1(RT23X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
***->    DC    X'047E0C0181'     PRO,MODE=ALL,XXX,POL,XXX-YYY,XXX-ALL           
         DC    X'04FE0C0181'     PLUS PRODGROUP                                 
         DC    X'0536180181'                 EST=NO/ALL/NNN                     
         DC    X'5EAF0F0181'                 MKT OR MKTGRP                      
         DC    X'070613'                     STA=ALL/XXXX                       
         DC    X'09C826'           STRD,ENDD=YYMM OR YYMMDD AND NO END          
*                                  OR YYMMDD IN BOTH                            
         DC    X'3C053F'                   O ES-LIN ORDER?                      
         DC    X'3D0540'                   O SPACING                            
         DC    X'450541'                   O PRINT OPTION=D/S/BLK/0-9           
         DC    X'0E3D32'                   O BOOK - HUT                         
         DC    X'20050B'                     ID SEQ=N,Y                         
         DC    X'85053A01BF'               O PARTIAL MATCH=*,TLCPDWA            
         DC    X'86054201C1'               O POST AFFIDS=Y,N,P                  
**       DC    X'080D1E0181'               O REP=NNN                            
         DC    X'080D680181'               O REP=NNN                            
         DC    X'C20544'                   O SUPPRESS COSTS                     
         DC    X'B7053E'                   O FILM REP OPT (Y,N,F,E,X)           
         DC    X'CB0551'                   O OPTIONS                            
RT23X    DC    X'00',C'I2'                                                      
         SPACE 2                                                                
RT24     DC    AL1(RT24X-*+3,24,0)                                              
         DC    B'11010011',CL22'CI -CONTINENTAL'                                
         DC    X'0000'                                                          
         DC    AL1(RT24X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
*******  DC    X'7404430127'                 TEST RUN? NO-OPED                  
         DC    X'CB0551'                   O OPTIONS                            
RT24X    DC    X'00',C'CI'                                                      
*                                                                               
RT25     DC    AL1(RT25X-*+3,02,0)                                              
         DC    B'11000011',CL22'EX-XML BILLING TRANS'                           
         DC    X'0000'                                                          
         DC    AL1(RT02X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'F80532'                     STRT INVOICE #                     
         DC    X'F90536'                     END INVOICE #                      
         DC    X'74043F'                     TEST RUN=Y/N                       
         DC    X'3805400105'                 RERUN=R                            
         DC    X'CB0551'                   O OPTIONS                            
RT25X    DC    X'00',C'EX'                                                      
         SPACE 2                                                                
RT26     DC    AL1(RT26X-*+3,26,0)                                              
         DC    B'11000011',CL22'FF-FIXFILMS REPORT'                             
         DC    X'0000'                                                          
         DC    AL1(RT26X-*+1),B'11000000'                                       
         DC    X'0207060181'       CLI=XXX,ALL                                  
         DC    X'0A051A'           MONTH OF SERVICE                             
         DC    X'74043F'           TEST RUN=Y/N                                 
         DC    X'CB0551'           OPTIONS                                      
RT26X    DC    X'00',C'FF'                                                      
*                                                                               
RT27     DC    AL1(RT27X-*+3,27,0)                                              
         DC    B'11110011',CL22'LOR-L''OREAL INTERFACE'                         
         DC    X'0000'                                                          
         DC    AL1(RT27X-*+1),B'11100000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'030F0C0181'               O PRD=ALL,XXX,POL                    
         DC    X'056F180181'               O EST=NNN/NNN-NNN/ALL                
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'F80532'                     STRT INVOICE #                     
         DC    X'F90536'                     END INVOICE #                      
         DC    X'9C0441'                     PRODUCE TAPE?                      
         DC    X'CB0551'                   O OPTIONS                            
RT27X    DC    X'00',C'LO'                                                      
*                                                                               
         SPACE 2                                                                
RT28     DC    AL1(RT28X-*+3,28,0)                                              
         DC    B'11010011',CL22'TD -TAB DELIMITED'                              
         DC    X'0000'                                                          
         DC    AL1(RT28X-*+1),B'11000000'                                       
         DC    X'0234060181'                 CLI=XXX,*AA,$N                     
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'7404430127'                 TEST RUN?                          
         DC    X'CB0551'                   O OPTIONS                            
RT28X    DC    X'00',C'TD'                                                      
*                                                                               
RT29     DC    AL1(RT29X-*+3,29,0)                                              
         DC    B'11110011',CL22'JW -JWT INTERFACE'                              
         DC    X'0000'                                                          
         DC    AL1(RT29X-*+1),B'11100000'                                       
         DC    X'0266060181'                 CLI=XXX/ALL/*NN,CGR=               
         DC    X'030F0C0181'               O PRD=ALL,XXX,POL                    
         DC    X'056F180181'               O EST=NNN/NNN-NNN/ALL                
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'F80565'           O STRT INVOICE #   COL 21 CARD 2             
         DC    X'F90569'           O END INVOICE #    COL 25 CARD 2             
         DC    X'9C043E'           PRODUCE TAPE=Y/N                             
         DC    X'CB0551'                   O OPTIONS                            
RT29X    DC    X'00',C'JW'                                                      
*                                                                               
*                                                                               
*        NOTE - COPIED FROM STAN'S LIBRARY 7/27/2012                            
*              ACTIVATE WHEN RELEASED                                           
*                                                                               
**30     DC    AL1(RT30X-*+3,30,0)                                              
**       DC    B'11000011',CL22'SW -CLIENT PURGE'                               
**       DC    X'0000'                                                          
**       DC    AL1(RT30X-*+1),B'11000000'                                       
**       DC    X'0204060181'                 CLI=XXX                            
**       DC    X'74053E'                   O TEST RUN                           
**30X    DC    X'00',C'SW'                                                      
         SPACE 2                                                                
RT40     DC    AL1(RT40X-*+3,40,0)                                              
         DC    B'11000011',CL22'AML-ACTIVE MARKET LIST'                         
         DC    X'0000'                                                          
         DC    AL1(RT40X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'CB0551'                   O OPTIONS                            
RT40X    DC    X'00',C'40'                                                      
         SPACE 2                                                                
RT41     DC    AL1(RT41X-*+3,41,0)                                              
         DC    B'11010011',CL22'CPL-CLT/PRD LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT41X-*+1),B'11000000'                                       
         DC    X'0236060181'                 CLI=XXX/ALL/ALL-X                  
         DC    X'030F0C0181'               O PRD=ALL,XXX,POL                    
         DC    X'61043E019B'                 DATA OPT=C/P                       
         DC    X'CB0551'                   O OPTIONS                            
RT41X    DC    X'00',C'41'                                                      
         SPACE 2                                                                
RT42     DC    AL1(RT42X-*+3,42,0)                                              
         DC    B'11010011',CL22'CH -CHOICE HOTELS'                              
         DC    X'0000'                                                          
         DC    AL1(RT42X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'7404430127'                 TEST RUN?                          
         DC    X'CB0551'                   O OPTIONS                            
RT42X    DC    X'00',C'CH'                                                      
*                                                                               
         SPACE 2                                                                
RT43     DC    AL1(RT43X-*+3,43,0)                                              
         DC    B'11010011',CL22'PZ -PFIZER INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT43X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'03070C0181'               O PRD=ALL,XXX                        
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'7404430127'                 TEST RUN?                          
         DC    X'CB0551'                   O OPTIONS                            
RT43X    DC    X'00',C'PZ'                                                      
         SPACE 2                                                                
RT44     DC    AL1(RT44X-*+3,44,0)                                              
         DC    B'11000011',CL22'NSL-NETWK-SHOW-SPILL'                           
         DC    X'0000'                                                          
         DC    AL1(RT44X-*+1),B'11000000'                                       
         DC    X'61043E018B'                 DATA OPTION=N/S/L                  
         DC    X'0207060181'               O CLT=ALL/XXX                        
         DC    X'0527180181'               O EST=XXX,ALL                        
         DC    X'71060F018D'               O NETWORK                            
         DC    X'070713018F'                 STATION/SHOW=ALL/XXXX              
         DC    X'FB053F0109010A'           O SPILL MKT SORT=H,L,A               
         DC    X'CB0551'                   O OPTIONS                            
RT44X    DC    X'00',C'44'                                                      
         SPACE 2                                                                
RT45     DC    AL1(RT45X-*+3,45,0)                                              
         DC    B'11001011',CL22'SAC-STATION ACTIVITY'                           
         DC    X'0000'                                                          
         DC    AL1(RT45X-*+1),B'11000000'                                       
*        DC    X'CB0551'                   O OPTIONS                            
RT45X    DC    X'00',C'45'                                                      
         SPACE 2                                                                
RT47     DC    AL1(RT47X-*+3,47,0)                                              
         DC    B'11000011',CL22'MPL-MKT/PROGRP LISTING'                         
         DC    X'0000'                                                          
         DC    AL1(RT47X-*+1),B'11000000'                                       
         DC    X'0236060181'       CLI=XXX/ALL/ALL-X                            
         DC    X'6A043E'           PGRPS                                        
         DC    X'6B043F'           MGRPS                                        
         DC    X'6C050A'           MGRP SCHEME                                  
         DC    X'7B0540'          O SCHEME CHK                                  
         DC    X'CB0551'                   O OPTIONS                            
RT47X    DC    X'00',C'47'                                                      
         SPACE 2                                                                
RT48     DC    AL1(RT48X-*+3,48,0)                                              
         DC    B'11000011',CL22'STL-STATION LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT48X-*+1),B'11000000'                                       
         DC    X'83043E01BD'       REC TYPE=A,R,M,S                             
         DC    X'D90551'           O AFFILIATE FILTER(ANYVL)                    
         DC    X'BB053F'           O STATION SIZE ANLAYSIS                      
         DC    X'D50540'           O STATION BY AFFIL                           
         DC    X'E90541'           O DEACTIVATED ONLY                           
         DC    X'CB0551'                   O OPTIONS                            
RT48X    DC    X'00',C'48'                                                      
         SPACE 2                                                                
RT49     DC    AL1(RT49X-*+3,49,0)                                              
         DC    B'11000011',CL22'AGE-AGENCY SUMMARY'                             
         DC    X'0000'                                                          
         DC    AL1(RT49X-*+1),B'11000000'                                       
         DC    X'0222060181'                 CLI=ALL,*NN                        
         DC    X'09042601AD'                 STR,END=YYMM                       
         DC    X'15053E'                   O PRIOR MONTHS?                      
         DC    X'16053F'                   O LATER MONTHS?                      
         DC    X'2405C0'                DDSO BILLING OPTION                     
         DC    X'2A05B2'                DDSO CURRENT MONTH=YYMM                 
         DC    X'CB0551'                   O OPTIONS                            
RT49X    DC    X'00',C'49'                                                      
         SPACE 2                                                                
RT50     DC    AL1(RT50X-*+3,50,0)                                              
         DC    B'11010011',CL22'AI -AT&&T INTERFACE'                            
         DC    X'0000'                                                          
         DC    AL1(RT50X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'03060C0181'                 PRD=ALL,XXX                        
         DC    X'056F180181'               O EST=NNN/NNN-NNN/ALL                
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'7404430127'                 TEST RUN?                          
         DC    X'CB0551'                   O OPTIONS                            
RT50X    DC    X'00',C'AI'                                                      
         SPACE 2                                                                
RT51     DC    AL1(RT51X-*+3,51,0)                                              
         DC    B'11000011',CL22'VL-VENDOR LEVEL REPORT'                         
         DC    X'0000'                                                          
         DC    AL1(RT51X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'03060C0181'                 PRD=ALL,XXX                        
         DC    X'056F180181'               O EST=NNN/NNN-NNN/ALL                
         DC    X'09082601D5'                 STR,END=YYMMDD (BILL RUN)          
         DC    X'9C043E'                     PRODUCE TAPE=Y/N                   
         DC    X'CB0551'                   O OPTIONS                            
RT51X    DC    X'00',C'VL'                                                      
         SPACE 2                                                                
RT52     DC    AL1(RT52X-*+3,52,0)                                              
         DC    B'11000011',CL22'VK-VENDOR LOCK REPORT'                          
         DC    X'0000'                                                          
         DC    AL1(RT52X-*+1),B'11000000'                                       
         DC    X'D60826'                     DATE                               
         DC    X'74043E'                     TEST RUN=Y/N                       
         DC    X'CB0551'                   O OPTIONS                            
RT52X    DC    X'00',C'VK'                                                      
         SPACE 2                                                                
RT57     DC    AL1(RT57X-*+3,57,0)                                              
         DC    B'11000011',CL22'BBR-BASE BRAND REPORT'                          
         DC    X'0000'                                                          
         DC    AL1(RT57X-*+1),B'11000000'                                       
         DC    X'030E0C0181'           PRO=ALL/XXX/POL                          
         DC    X'057C180181'           EST=NNN/NNN-NNN/NO                       
         DC    X'091826'               STR,END=YYMMDD/ES                        
         DC    X'4A0432'               C BOOK=YYMM                              
         DC    X'4B04440183'           DPT CD=D/N/S                             
         DC    X'8B0422'               DEMO MENU                                
         DC    X'CB0551'                   O OPTIONS                            
RT57X    DC    X'00',C'57'                                                      
         SPACE 2                                                                
RT74      DC    AL1(RT74X-*+3,74,0)                                             
          DC    B'00110011',CL22'RPL-RPT PROFILE LIST'                          
          DC    X'0000'                                                         
          DC    AL1(RT74X-*+1),B'00100000'                                      
          DC    X'79063E'                     RPT=ALL,XX                        
          DC    X'CB0551'                   O OPTIONS                           
RT74X     DC    X'00',C'74'                                                     
         SPACE 2                                                                
RT85     DC    AL1(RT85X-*+3,85,0)                                              
         DC    B'10000001',CL22'HUT-HUT PRINT'                                  
         DC    X'0000'                                                          
         DC    AL1(RT85X-*+1),B'10000000'                                       
         DC    X'0206060181'                 CLI=ALL/XXX                        
         DC    X'060E0F0181'                 MARK=ALL/ALLN/NNNN                 
         DC    X'430D0A'                   O MAR SEQ=REG/REG-N                  
         DC    X'30053E'                   O RATING SERVICE                     
         DC    X'0E0E32'                     BOOK-HUT=YYMM/YYMM-XX              
         DC    X'310540'                   O AUDIENCE TYPE                      
         DC    X'CB0551'                   O OPTIONS                            
RT85X    DC    X'00',C'85'                                                      
         SPACE 2                                                                
RT91     DC    AL1(RT91X-*+3,91,0)                                              
         DC    B'11000011',CL22'BCR-BILLING/CLRNCE RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT91X-*+1),B'11000000'                                       
         DC    X'0224060181'       CLI=XXX                                      
*        DC    X'030E0C0181'       PRO=ALL,XXX,POL                              
         DC    X'5D2E0C0181'       PRD OR PGRP                                  
         DC    X'2A2432'           CUR MTH=YYMM OR NO                           
         DC    X'3B092C'         O CUT-OFF DATE=YYMMDD                          
         DC    X'77053E01A5'     O EST OPTN=1,2,3                               
         DC    X'78053F'         O PRD TOGETHER=Y,N                             
         DC    X'61054101A7'     O DATA OPTN=M,S                                
         DC    X'45054201B5'     O PRINT OPT=N,Y                                
         DC    X'79054301D9'     O REPORT=N, NETPAK UNITS ONLY                  
         DC    X'FD05400181'     O PRD SUMMARY FORMAT?                          
         DC    X'CB0551'                   O OPTIONS                            
RT91X    DC    X'00',C'A8'                                                      
         SPACE 2                                                                
*RT92     DC    AL1(RT92X-*+3,92,0)                                             
*         DC    B'11000011',CL22'CLO-CLOSE OUT ESTIMATE'                        
*         DC    X'0000'                                                         
*         DC    AL1(RT92X-*+1),B'11000000'                                      
*         DC    X'0204060181'                 CLI=XXX                           
*         DC    X'030E0C0181'                 PRO=ALL/XXX/POL                   
*         DC    X'051E180181'                 EST=NNN/NNN-NNN/NO (ALL)          
*         DC    X'3B0826'                     CUT-OFF DATE=YYMMDD               
*         DC    X'3905A4'                DDSO BILL OVERIDE?                     
*         DC    X'3A05A5'                DDSO BNP/PNB OVERIDE?                  
*         DC    X'CB0551'                   O OPTIONS                           
*RT92X    DC    X'00',C'92'                                                     
         SPACE 2                                                                
*T96     DC    AL1(RT96X-*+3,96,0)                                              
*        DC    B'11000011',CL22'BRL-BUYROLL'                                    
*        DC    X'0000'                                                          
*        DC    AL1(RT96X-*+1),B'11000000'                                       
*        DC    X'0204060181'                 CLI=XXX                            
*        DC    X'030C0C0181'                 PRO=XXX/POL                        
*        DC    X'05081801810182'             EST=NNN-NNN  COMMENT 130           
*        DC    X'460541'                   O -E BUYS ONLY                       
*        DC    X'CB0551'                   O OPTIONS                            
*T96X    DC    X'00',C'96'                                                      
         SPACE 2                                                                
RT98     DC    AL1(RT98X-*+3,98,0)                                              
         DC    B'10000011',CL22'EXR-EXCEPTION REPORT'                           
         DC    X'0000'                                                          
         DC    AL1(RT98X-*+1),B'10000000'                                       
         DC    X'030E0C0181'            PRO=ALL/XXX/POL                         
         DC    X'057C180181'            EST=NNN/NNN-NNN/NO                      
         DC    X'060A0F0181'            MKT=ALL/NNNN                            
         DC    X'43090A'              O MKTSEQ=REG-N                            
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'4A0438'                C BOOK=YYMM                             
         DC    X'4B04400183'            DPT CD=D/N/S                            
         DC    X'8B0522'              O DEMO MENU FOR TAGET AUD                 
         DC    X'CB0551'                   O OPTIONS                            
RT98X    DC    X'00',C'98'                                                      
         SPACE 2                                                                
RT101    DC    AL1(RT101X-*+3,101,0)                                            
         DC    B'11000011',CL22'PCR-PROPERTY CLEARANCE'                         
         DC    X'0000'                                                          
         DC    AL1(RT101X-*+1),B'11000000'                                      
         DC    X'520A0C0181'       PROP=ALL,NNN                                 
         DC    X'090826'           STR,END=YYMMDD                               
         DC    X'0207060181'     O CLT=ALL/XXX                                  
         DC    X'CB0551'                   O OPTIONS                            
RT101X   DC    X'00',C'S1'                                                      
         SPACE 2                                                                
RT102    DC    AL1(RT102X-*+3,102,0)                                            
         DC    B'11000011',CL22'MFX-MARKET FIX'                                 
         DC    X'0000'                                                          
         DC    AL1(RT102X-*+1),B'11000000'                                      
         DC    X'0206060181'       CLT=ALL,XXX                                  
         DC    X'06090F0117'       MKT=NNNN OLD MKT                             
         DC    X'071013'           STA=XXXX  IF CLT NOT ALL - MUST              
*                                  FIND CLT EXCEPTION                           
         DC    X'0608320119'       MKT=NNNN NEW MKT                             
         DC    X'CB0551'                   O OPTIONS                            
RT102X   DC    X'00',C'SX'                                                      
         SPACE 2                                                                
RT103    DC    AL1(RT103X-*+3,103,0)                                            
         DC    B'11000011',CL22'SFX-STATION FIX'                                
         DC    X'0000'                                                          
         DC    AL1(RT103X-*+1),B'11000000'                                      
         DC    X'0206060181'       CLT=ALL,XXX                                  
         DC    X'071013'           STA=XXXX  IF CLT NOT ALL - MUST              
*                                  FIND CLT EXCEPTION                           
         DC    X'07203201C9'       STA=XXXX  NEW CALL LETTERS                   
         DC    X'06090F0125'       MKT=NNNN NEW MKT                             
         DC    X'91053E'           CHG INVS=N/Y                                 
         DC    X'CB0551'                   O OPTIONS                            
RT103X   DC    X'00',C'SC'                                                      
         SPACE 2                                                                
RT104    DC    AL1(RT104X-*+3,104,0)                                            
         DC    B'11000011',CL22'PDR-PROPERTY DELIVERY'                          
         DC    X'0000'                                                          
         DC    AL1(RT104X-*+1),B'11000000'                                      
         DC    X'520A0C0181'       PROP=ALL,NNN                                 
         DC    X'5EAA0F0181'       MKT/MGR=ALL,NNNN,XALL,XNNNN                  
         DC    X'090826'           STR,END=YYMMDD                               
         DC    X'0207060181'     O CLT=ALL/XXX                                  
         DC    X'53053F'         O OWNER=Y,N                                    
         DC    X'54053E01850186' O DETAIL OPT=N,C,G                             
         DC    X'CB0551'                   O OPTIONS                            
RT104X   DC    X'00',C'S4'                                                      
         SPACE 2                                                                
RT105    DC    AL1(RT105X-*+3,105,0)                                            
         DC    B'11000011',CL22'STR-SYN TRACKING RPT'                           
         DC    X'0000'                                                          
         DC    AL1(RT105X-*+1),B'11000000'                                      
         DC    X'5EAE0F0181'                                                    
         DC    X'09042601AD'       STR,END=YYMM                                 
         DC    X'08081E0181'       REP=NNN - SPEC REP                           
         DC    X'4E053E'           EXCLUD CODE=X                                
         DC    X'CB0551'                   O OPTIONS                            
RT105X   DC    X'00',C'S6'                                                      
         SPACE 2                                                                
RT106    DC    AL1(RT106X-*+3,106,0)                                            
         DC    B'11000111',CL22'SPG-STAFILE PURGE'                              
         DC    X'0000'                                                          
         DC    AL1(RT106X-*+1),B'11000000'                                      
         DC    X'0206060181'       CLT=ALL,XXX                                  
         DC    X'17043E'           MARK FILES=Y,N                               
         DC    X'CB0551'                   O OPTIONS                            
RT106X   DC    X'00',C'SP'                                                      
         SPACE 2                                                                
RT107    DC    AL1(RT107X-*+3,107,0)                                            
         DC    B'11010011',CL22'SRS-STAT ROTATION SCHD'                         
         DC    X'0000'                                                          
         DC    AL1(RT107X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRO=XXX,PGRP,XXX-YYY,POL,ALL            
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'              O STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'20050B'                ID SEQ=N,Y                              
         DC    X'CB0551'                   O OPTIONS                            
RT107X   DC    X'00',C'RS'                                                      
         SPACE 2                                                                
RT109    DC    AL1(RT109X-*+3,109,0)                                            
         DC    B'11010011',CL22'FPL-TRAFFIC PLAN'                               
         DC    X'0000'                                                          
         DC    AL1(RT109X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'       PRD OR PRDGRP=PRD,POL,PGP=                   
         DC    X'5E8E0F0181'       MKT,MKTGRP                                   
         DC    X'05FC180181'                 EST=NNN/NNN-NNN/NO                 
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'48053A'                     AFFILIATE                          
         DC    X'61053E'                   O DATA OPT=B/G                       
         DC    X'6F053F'                   O STATS PER MKT                      
         DC    X'CB0551'                   O OPTIONS                            
RT109X   DC    X'00',C'T9'                                                      
         SPACE 2                                                                
RT111    DC    AL1(RT111X-*+3,111,0)                                            
         DC    B'11000111',CL22'CPX-CPP EXTRACT'                                
         DC    X'0000'                                                          
         DC    AL1(RT111X-*+1),B'11000000'                                      
         DC    X'0207860180'       DDS CLI=OPT/ALL                              
         DC    X'090826'           STR,END=YYMMDD                               
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX                        
         DC    X'550538'         O RERATE TYPE                                  
         DC    X'CB0551'                   O OPTIONS                            
RT111X   DC    X'00',C'M1'                                                      
         SPACE 2                                                                
RT112    DC    AL1(RT112X-*+3,112,0)                                            
         DC    B'11010011',CL22'BRP-BRAND PERFORMANCE'                          
         DC    X'0000'                                                          
         DC    AL1(RT112X-*+1),B'11000000'                                      
         DC    X'0244060181'            CLI=XXX,CGR=                            
         DC    X'5DFE0C0181'    PRD OR PRD GRP=ALL,POL,PRD,PGP=                 
         DC    X'5EAE0F0181'            MKT OR MGRP=ALL,NNNN,MGP=               
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'              O BOOK-HUT=YYMM/YYMM-XX                   
         DC    X'190439'                DATA COMPARE                            
         DC    X'48053A'              O AFFILATE                                
         DC    X'56053B'              O PROGRAM TYPE                            
         DC    X'1E053C'              O DAYPART1 DETAIL                         
         DC    X'590D3D'              O DAY PART OVERRIDE                       
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'6D053E'              O LEVEL CONTROL=D,S                       
         DC    X'920542'              O SPILL OPTION=0,1,2,3                    
         DC    X'E205C4'              O CML CLASS FILTER                        
         DC    X'CB0551'                   O OPTIONS                            
RT112X   DC    X'00',C'M2'                                                      
         SPACE 2                                                                
RT113    DC    AL1(RT113X-*+3,113,0)                                            
         DC    B'11010011',CL22'BWS-BRAND WEEKLY SUM'                           
         DC    X'0000'                                                          
        DC    AL1(RT113X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3D32'                   O BOOK-HUT=YYMM/YYMM-XX              
         DC    X'190439'                     DATA COMPARE                       
         DC    X'48053A'                   O AFFILATE                           
         DC    X'560D3B'                   O PROGRAM TYPE                       
         DC    X'1E053C'                   O DAYPART1 DETAIL                    
         DC    X'590D3D'                   O DAY PART OVERRIDE                  
         DC    X'8B0522'                   O DEMO MENU                          
         DC    X'5A053E'                   O WEEKLY ANAL                        
         DC    X'6D053F'                   O LEVEL CONTROL=D,S                  
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT113X   DC    X'00',C'M3'                                                      
         SPACE 2                                                                
RT114    DC    AL1(RT114X-*+3,114,0)                                            
         DC    B'11010011',CL22'MAR-MARKET PERFORMANCE'                         
         DC    X'0000'                                                          
         DC    AL1(RT114X-*+1),B'11000000'                                      
         DC    X'0208060181'                 CLI=XXX(POL)                       
         DC    X'5EAE0F0181'                 MKT OR MKTGRP                      
         DC    X'5DFC0C0181'                 PRD OR PGRP                        
         DC    X'05BE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3D32'                   O BOOK-HUT=YYMM/YYMM-XX              
         DC    X'190439'                     DATA COMPARE                       
         DC    X'48053A'                   O AFFILATE                           
         DC    X'560D3B'                   O PROGRAM TYPE                       
         DC    X'1E053C'                   O DAYPART1 DETAIL                    
         DC    X'590D3D'                   O DAYPART OVERRIDE                   
         DC    X'8B0522'                   O DEMO MENU                          
         DC    X'5B053E01CB'               O MONTHLY ANAL +Q=QUARTERLY          
         DC    X'5C053F'                   O BRAND ANAL                         
         DC    X'6D0540'                   O LEVEL CONTROL=D,S                  
*****    DC    X'8F05C4'          MC ONLY  O FILM TYPE FILTER                   
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT114X   DC    X'00',C'M4'                                                      
         SPACE 2                                                                
RT115    DC    AL1(RT115X-*+3,115,0)                                            
         DC    B'11000011',CL22'IFT-INTERFACE TAPE'                             
         DC    X'0000'                                                          
         DC    AL1(RT115X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'030E0C0181'            PRO=ALL/XXX/POL                         
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO                      
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'5EAF0F0181'          O MKT OR MKTGRP                           
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550538'              O RERATE                                  
         DC    X'59053D'              O PAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'64053E'              O EXT GOALS                               
         DC    X'65053F'              O EXT BUYS                                
         DC    X'660540'              O EXT DETAILS                             
         DC    X'680541'              O EXT AFFIDS                              
         DC    X'CB0551'                   O OPTIONS                            
RT115X   DC    X'00',C'M5'                                                      
         SPACE 2                                                                
RT118    DC    AL1(RT118X-*+3,118,0)                                            
         DC    B'11010011',CL22'MMP-MARKET MEDIA PLAN'                          
         DC    X'0000'                                                          
        DC    AL1(RT118X-*+1),B'11000000'                                       
         DC    X'0208060181'            CLI=XXX(POL)                            
         DC    X'5EAE0F0181'                                                    
         DC    X'059E180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'48053A'              O AFFILATE                                
         DC    X'1E053C'                DAYPART DETAIL                          
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'5F053E'                DATE OPTION                             
         DC    X'60053F'                RECAP OPTION                            
         DC    X'610540'                DATA OPTION                             
         DC    X'5705420187'          O TOTAL OPTION                            
         DC    X'92053201DF'          O SPILL OPTION=Y                          
         DC    X'CB0551'                   O OPTIONS                            
RT118X   DC    X'00',C'M8'                                                      
         SPACE 2                                                                
RT119    DC    AL1(RT119X-*+3,119,0)                                            
         DC    B'11010011',CL22'BMD-BRAND MEDIA PLAN'                           
         DC    X'0000'                                                          
         DC    AL1(RT119X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'       PRD OR PRDGRP=PRD,ALL,POL,PGP=               
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'48053A'                   O AFFILATE                           
         DC    X'1E053C'                     DAYPART DETAIL                     
         DC    X'5F053E'                     DATE                               
         DC    X'60053F'                     RECAP                              
         DC    X'610540'                     DATA                               
         DC    X'92054101DF'               O SPILL OPTION=Y                     
         DC    X'CB0551'                   O OPTIONS                            
RT119X   DC    X'00',C'M9'                                                      
         SPACE 2                                                                
RT121    DC    AL1(RT121X-*+3,121,0)                                            
         DC    B'11000011',CL22'BSR-BUDGET STATUS RPT'                          
         DC    X'0000'                                                          
         DC    AL1(RT121X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'       PRD OR PRDGRP=PRD,ALL,POL,PGP=               
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'890932'                   O PUR END DATE=YYMMDD                
         DC    X'CB0551'                   O OPTIONS                            
RT121X   DC    X'00',C'MB'                                                      
         SPACE 2                                                                
RT125    DC    AL1(RT125X-*+3,125,0)                                            
         DC    B'11000011',CL22'CMU-COMMERCIAL USAGE'                           
         DC    X'0000'                                                          
         DC    AL1(RT125X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'060A0F0181'       MAR=ALL/NNNN                                 
         DC    X'090826'           STR,END=YYMMDD                               
         DC    X'CB0551'                   O OPTIONS                            
RT125X   DC    X'00',C'MF'                                                      
         SPACE 2                                                                
RT127    DC    AL1(RT127X-*+3,127,0)                                            
         DC    B'11000011',CL22'KPS-KRAFT PRODUCT SCHD'                         
         DC    X'0000'                                                          
         DC    AL1(RT127X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'       PRD OR PRDGRP=PRD,ALL,POL,PGP=               
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'61053E01A3'                 DATA OPT=1/2/3                     
         DC    X'CB0551'                   O OPTIONS                            
RT127X   DC    X'00',C'MK'                                                      
         SPACE 2                                                                
RT128    DC    AL1(RT128X-*+3,128,0)                                            
         DC    B'11000011',CL22'LIN-LOCKIN'                                     
         DC    X'0000'                                                          
         DC    AL1(RT128X-*+1),B'11000000'                                      
         DC    X'0244060181'       CLI=XXX                                      
         DC    X'5DF60C0181'       PRD OR PGRP=PRD/ALL/PGP=                     
         DC    X'5EAE0F0181'       MKT OR MGRP                                  
         DC    X'0524180181'       EST=NNN                                      
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'0E3D32'        O BOOK-HUT=YYMM/YYMM-NN/ACT/ACT-NN              
         DC    X'550538'         O RERATE TYPE                                  
         DC    X'CB0551'                   O OPTIONS                            
RT128X   DC    X'00',C'ML'                                                      
         SPACE 2                                                                
RT131    DC    AL1(RT131X-*+3,131,0)                                            
         DC    B'11000011',CL22'DML-DEMO MENU LISTING'                          
         DC    X'0000'                                                          
         DC    AL1(RT131X-*+1),B'11000000'                                      
         DC    X'8E0622'           DEMO MENU FILTER=N,*,-N (X4)                 
*                                  OR ALL                                       
         DC    X'CB0551'                   O OPTIONS                            
RT131X   DC    X'00',C'LD'                                                      
         SPACE 2                                                                
RT132    DC    AL1(RT132X-*+3,132,0)                                            
         DC    B'11000011',CL22'EHR-ESTIMATE HDR RPT'                           
         DC    X'0000'                                                          
         DC    AL1(RT132X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLI=ALL,XXX,*NN                              
         DC    X'5DFE0C0181'       PRD OR PGR=ALL,XXX,PGR=XALL,XNNN             
         DC    X'05A6180181'       EST=ALL,NNN,NO+F                             
         DC    X'091D26'           O STR,END=YYMMDD/YYMM/ES                     
         DC    X'78053E'         O PRD TOGETHER=Y,N                             
         DC    X'77053F01AF'       EST OPT=A,I,B                                
         DC    X'54054001B1'       DETAIL OPT=$,D                               
         DC    X'5F054101B9'       O DATE OPT=Y                                 
         DC    X'CB0551'                   O OPTIONS                            
RT132X   DC    X'00',C'L2'                                                      
         SPACE 2                                                                
RT134    DC    AL1(RT134X-*+3,134,0)                                            
         DC    B'10000011',CL22'DOR-DEMO OVERRIDE RPT'                          
         DC    X'0000'                                                          
         DC    AL1(RT134X-*+1),B'11000000'                                      
         DC    X'71060F'                     NETWORK=ALL,XXXX                   
         DC    X'900613'                     SHOW=ALL/XXXX                      
         DC    X'0227060181'       CLI=ALL,XXX,*NN                              
         DC    X'CB0551'                   O OPTIONS                            
RT134X   DC    X'00',C'L7'                                                      
         SPACE 2                                                                
RT136    DC    AL1(RT136X-*+3,136,0)                                            
         DC    B'11000011',CL22'CML-COMMENT LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT136X-*+1),B'11000000'                                      
         DC    X'0206060181'       CLI=ALL,XXX                                  
         DC    X'030F0C0181'       PRD=OPT/ALL,XXX,POL                          
         DC    X'0507180181'       EST=OPT/ALL,NNN                              
         DC    X'83053E01C301C4'   REC TYPE=A                                   
         DC    X'CB0551'                   O OPTIONS                            
RT136X   DC    X'00',C'L6'                                                      
         SPACE 2                                                                
RT137    DC    AL1(RT137X-*+3,137,0)                                            
         DC    B'11000011',CL22'RAL-BRAND REALLOCATION'                         
         DC    X'0000'                                                          
         DC    AL1(RT137X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03040C0181'       PRO=XXX                                      
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'070713'           O STA=ALL/XXXX                               
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'8A0C32'           RE-AL PRDS=UNA,1-4 PRD=N'S                   
         DC    X'D20542'           O PRD CHANGE RPT                             
***      DC    X'D30543'           O STATION BREAKS                             
         DC    X'CB0551'                   O OPTIONS                            
RT137X   DC    X'00',C'K2'                                                      
         SPACE 2                                                                
RT138    DC    AL1(RT138X-*+3,138,0)                                            
         DC    B'11010011',CL22'NAL-NEW BRAND ALLOCTN'                          
         DC    X'0000'                                                          
         DC    AL1(RT138X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'E4210C0181'       PGR=VNNN                                     
****     DC    X'059E180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX                
****     CHANGED TO ALLOW FOR PROD GROUP                                        
         DC    X'05BE180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX                
         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'070713'           O STA=ALL/XXXX                               
         DC    X'091826'           STR,END=YYMMDD                               
         DC    X'50053E01CF'       REALLOCATE=Y/N,OR 0-9,A-E                    
         DC    X'10053F01D1'       ALLOC BASIS=$/D                              
         DC    X'1B05400199'       ALLOC EXECSS=Y/N                             
         DC    X'93053201D3'       WEIGHT OVERRIDES 0-9,A=10 7 CHARS            
         DC    X'410D3B'           O DAYPART FILTER                             
         DC    X'9D053C'           O SPOT LENGTH FILTER                         
         DC    X'740641'           TEST RUN=Y/N                                 
         DC    X'CB0551'                   O OPTIONS                            
RT138X   DC    X'00',C'K4'                                                      
         SPACE 2                                                                
RT139    DC    AL1(RT139X-*+3,139,0)                                            
         DC    B'11000011',CL22'GPR-GOAL PURGE'                                 
         DC    X'0000'                                                          
         DC    AL1(RT139X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'5DF60C0181'       PRD OR PGRP/NO POL                           
         DC    X'056C180181'       EST=NNN,NNN-NNN,(+NON-SPEC PRD)              
         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'41053E'         O DAYPT FILTER=ALPHA                           
         DC    X'94053F'         O SPOT LENGTH                                  
         DC    X'CB0551'                   O OPTIONS                            
RT139X   DC    X'00',C'K3'                                                      
         SPACE 2                                                                
RT140    DC    AL1(RT140X-*+3,140,0)                                            
         DC    B'11000011',CL22'BAL-BRAND ALLOCATION'                           
         DC    X'0000'                                                          
         DC    AL1(RT140X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'059E180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX                
         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'070713'           O STA=ALL/XXXX                               
         DC    X'090826'           STR,END=YYMMDD                               
         DC    X'50053E'           REALLOCATE=Y/N                               
         DC    X'10053F0197'       ALLOC BASIS=D,S,$                            
         DC    X'1B05400199'       ALLOC EXECSS=Y/N                             
         DC    X'740541'           TEST RUN=Y/N                                 
         DC    X'7C053201B7'      O M TARGET MODE=1,2                           
         DC    X'CB0551'                   O OPTIONS                            
RT140X   DC    X'00',C'K6'                                                      
         SPACE 2                                                                
*RT142    DC    AL1(RT142X-*+3,142,0)                                           
*         DC    B'11000011',CL22'FAL-FILM ALLOCATION'                           
*         DC    X'0000'                                                         
*         DC    AL1(RT142X-*+1),B'11000000'                                     
*         DC    X'0204060181'       CLT=XXX                                     
*         DC    X'030E0C0181'       PRD=XXX/ALL/POL                             
*         DC    X'059E180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX               
*         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN         
*         DC    X'070713'           O STA=ALL/XXXX                              
*         DC    X'090826'           STR,END=YYMMDD                              
*         DC    X'50053E'           REALLOCATE=Y/N                              
*         DC    X'10053F0197'       ALLOC BASIS=D,S,$                           
*         DC    X'740541'           TEST RUN=Y/N                                
*         DC    X'CB0551'                   O OPTIONS                           
*RT142X   DC    X'00',C'K9'                                                     
         SPACE 2                                                                
RT151    DC    AL1(RT151X-*+3,151,0)                                            
         DC    B'11000011',CL22'PAR-PROGRAM AFFLTN RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT151X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'030E0C0181'       PRD=ALL,XXX,POL                              
         DC    X'05FC180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'CB0551'                   O OPTIONS                            
RT151X   DC    X'00',C'XD'                                                      
         SPACE 2                                                                
RT152    DC    AL1(RT152X-*+3,152,0)                                            
         DC    B'11000011',CL22'PRP-PEPSI ANALYSIS RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT152X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'5DF60C0181'       PRD OR PGRP=PRD/ALL/PGP=                     
         DC    X'5EAE0F0181'       MKT OR MGRP                                  
         DC    X'05FC180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'8B0522'         O DEMO MENU                                    
         DC    X'320441'           DATA TYPES=1-9 (4 CHARS OPTS 4-7)            
         DC    X'25053F019F'     O ANALYSIS TYPE                                
         DC    X'590D3D'         O DAYPART OVERRIDE                             
         DC    X'41053B'         O DAYPT FILTER=ALPHA                           
         DC    X'7A053E'           SUPPRESS SPTLEN=Y,N                          
         DC    X'A2053A'           SUPPRESS MARKET DETAIL                       
         DC    X'CB0551'                   O OPTIONS                            
RT152X   DC    X'00',C'XA'                                                      
         SPACE 2                                                                
RT153    DC    AL1(RT153X-*+3,153,0)                                            
         DC    B'11000011',CL22'PUS-PEPSI U.S. TOTAL'                           
         DC    X'0000'                                                          
         DC    AL1(RT153X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'5DF60C0181'       PRD OR PGRP=PRD/ALL/PGP=                     
         DC    X'5EAE0F0181'       MKT OR MGRP                                  
         DC    X'05FC180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'8B0422'           DEMO MENU                                    
         DC    X'1D053F01A1'     O STAGE=1-7,L                                  
         DC    X'590D3D'         O DAYPART OVERRIDE                             
         DC    X'41053B'         O DAYPT FILTER=ALPHA                           
         DC    X'510540'         O MKT ANAL=Y,N                                 
         DC    X'7A053E'           SUPPRESS SPTLEN=Y,N                          
         DC    X'A2053A'           SUPPRESS MARKET DETAIL                       
         DC    X'CB0551'                   O OPTIONS                            
RT153X   DC    X'00',C'XB'                                                      
         SPACE 2                                                                
RT154    DC    AL1(RT154X-*+3,154,0)                                            
         DC    B'10000011',CL22'PGT-PG INTERFACE TAPE'                          
         DC    X'0000'                                                          
         DC    AL1(RT154X-*+1),B'10000000'                                      
         DC    X'0204060181'                 CLI=PG                             
         DC    X'03080C0181'                 PRO=POL                            
         DC    X'057C180181'                 EST=NNN/NNN-NNN/NO                 
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'9C043E'                     PRODUCE TAPPE                      
         DC    X'CB0551'                   O OPTIONS                            
RT154X   DC    X'00',C'XG'                                                      
         SPACE 2                                                                
RT155    DC    AL1(RT155X-*+3,155,0)         ONLY FOR YNR COLGATE               
         DC    B'11000011',CL22'C0 -YNR COLGATE EXTRAC'                         
         DC    X'0000'                                                          
         DC    AL1(RT155X-*+1),B'11000000'                                      
         DC    X'0206060181'                 CLI=ALL,XXX                        
         DC    X'030E0C0181'                 PRO=POL,ALL,XXX                    
         DC    X'0526180181'                 EST=ALL,XXX (not required)         
         DC    X'0A85320113'                 MONTH OF SERVICE RANGE             
         DC    X'9C043E'                     PRODUCE TAPE                       
         DC    X'0908260115'                 INVOICE START/END                  
         DC    X'CB0551'                   O OPTIONS                            
RT155X   DC    X'00',C'C0'                                                      
         SPACE 2                                                                
RT156    DC    AL1(RT156X-*+3,156,0)                                            
         DC    B'11000011',CL22'BJT-BJ INTERFACE TAPE'                          
         DC    X'0000'                                                          
         DC    AL1(RT156X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLI=ALL,XXX,*NN                              
         DC    X'5DFE0C0181'       PRD OR PGR=ALL,XXX,PGR=XALL,XNNN             
         DC    X'05A6180181'       EST=ALL,NNN,NO+F                             
         DC    X'09142601AD'       STR,END=YYMM/ES                              
         DC    X'CB0551'                   O OPTIONS                            
RT156X   DC    X'00',C'XJ'                                                      
         SPACE 2                                                                
RT158    DC    AL1(RT158X-*+3,158,0)                                            
         DC    B'11010011',CL22'MCL-MEDIA CALENDAR'                             
         DC    X'0000'                                                          
         DC    AL1(RT158X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAF0F0181'       MARKET OR MKT GROUP                          
         DC    X'070713'           O STA=XXX,ALL                                
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'87053F'         O PRINT COMMENTS=Y,N                           
         DC    X'880540'         O PRINT NET AFF=Y,N                            
         DC    X'BC0542'         O PRINT CHANNEL=Y,N                            
         DC    X'CB0551'                   O OPTIONS                            
RT158X   DC    X'00',C'DC'                                                      
         SPACE 2                                                                
*T159    DC    AL1(RT159X-*+3,159,0)                                            
*        DC    B'11000011',CL22'SXT-STANDARD EXTRACT'                           
*        DC    X'0000'                                                          
*        DC    AL1(RT159X-*+1),B'11000000'                                      
*        DC    X'0204060181'       CLI=XXX                                      
*        DC    X'030E0C0181'       PRO=XXX/ALL/POL                              
*        DC    X'057E180181'       EST=NNN/NNN-NNN/NO                           
*        DC    X'091826'           STR,END=YYMMDD,ES                            
*        DC    X'5F053E01BB'     O DATE OPT=B                                   
*        DC    X'540541'         O (SPOT)DETAIL OPTION                          
*        DC    X'CB0551'                   O OPTIONS                            
*T159X   DC    X'00',C'XT'                                                      
         SPACE 2                                                                
RT160    DC    AL1(RT160X-*+3,160,0)                                            
         DC    B'11010011',CL22'SAL-SALESMAN SHEET'                             
         DC    X'0000'                                                          
         DC    AL1(RT160X-*+1),B'11000000'                                      
         DC    X'0204060181'      CLI=XXX                                       
         DC    X'5DFE0C0181'      PRD OR PGRP                                   
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'          STA=ALL/XXXX                                  
         DC    X'05FE180181'      EST=NNN/NNN-NNN/NO (ALL)                      
         DC    X'091826'          STR,END=YYMMDD/ES                             
         DC    X'0E3D32'        O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX              
         DC    X'550538'        O RERARE TYPE                                   
         DC    X'48053A'        O AFFILATE                                      
         DC    X'560D3B'        O PROGRAM TYPE                                  
         DC    X'8B0522'        O DEMO MENU                                     
         DC    X'54053E'        O DETAIL OPTION                                 
         DC    X'57053F'        O TOTAL OPTION                                  
         DC    X'2F0540'        O UNCOMF ONLY                                   
         DC    X'20050B'          ID SEQ=N,Y                                    
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT160X   DC    X'00',C'D4'                                                      
         SPACE 2                                                                
RT161    DC    AL1(RT161X-*+3,161,0)                                            
         DC    B'11010011',CL22'BRS-BRAND SCHEDULE'                             
         DC    X'0000'                                                          
         DC    AL1(RT161X-*+1),B'11000000'                                      
         DC    X'0244060181'                 CLI=XXX,CGR=                       
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550538'                    O RERARE TYPE                       
         DC    X'48053A'                    O AFFILATE                          
         DC    X'560D3B'                    O PROGRAM TYPE                      
         DC    X'8B0522'                    O DEMO MENU                         
         DC    X'54053E'                    O DETAIL OPTION                     
         DC    X'57053F'                    O TOTAL OPTION                      
         DC    X'2F0540'                    O UNCOMF ONLY                       
         DC    X'20050B'                      ID SEQ=N,Y                        
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT161X   DC    X'00',C'D2'                                                      
         SPACE 2                                                                
RT162    DC    AL1(RT162X-*+3,162,0)                                            
         DC    B'11010011',CL22'BTS-BRAND TIME SHEET'                           
         DC    X'0000'                                                          
        DC    AL1(RT162X-*+1),B'11000000'                                       
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550538'              O RERARE TYPE                             
         DC    X'48053A'              O AFFILATE                                
         DC    X'560D3B'              O PROGRAM TYPE                            
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'54053E'              O DETAIL OPTION                           
         DC    X'57053F'              O TOTAL OPTION                            
         DC    X'2F0540'              O UNCOMF ONLY                             
         DC    X'20050B'                ID SEQ=N,Y                              
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT162X   DC    X'00',C'D3'                                                      
         SPACE 2                                                                
RT163    DC    AL1(RT163X-*+3,163,0)                                            
         DC    B'11010011',CL22'PBS-POL BUY SHEET'                              
         DC    X'0000'                                                          
         DC    AL1(RT163X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'              O STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'20050B'                ID SEQ=A,Y                              
         DC    X'920543'                O SPILL OPTION=0,1,2,3 (OPT6)           
         DC    X'540541'                O DETAIL OPTION                         
         DC    X'AE053E'                O XTRA GRID LINES                       
         DC    X'CB0551'                   O OPTIONS                            
RT163X   DC    X'00',C'D5'                                                      
         SPACE 2                                                                
RT164    DC    AL1(RT164X-*+3,164,0)                                            
         DC    B'10010011',CL22'NBS-NET BUY SHEET'                              
         DC    X'0000'                                                          
         DC    AL1(RT164X-*+1),B'10000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'710613'                NETWORK=XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'20050B'                ID SEQ=A,Y                              
         DC    X'97053E'              O CUTINS= N,Y                             
         DC    X'98053F'              O COST OVERRIDES=N,Y                      
         DC    X'990540'              O LOCAL DEMOS=N,Y                         
         DC    X'9A0D41'              O REGION=X                                
         DC    X'540542'              O DETAIL OPTION                           
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'CB0551'                   O OPTIONS                            
RT164X   DC    X'00',C'N5'                                                      
         SPACE 2                                                                
RT166    DC    AL1(RT166X-*+3,166,0)                                            
         DC    B'11000011',CL22'PBP-POST BRAND EVAL'                            
         DC    X'0000'                                                          
        DC    AL1(RT166X-*+1),B'11000000'                                       
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'190439'                     DATA COMPARE                       
         DC    X'1E053C'                   O DAYPART1 DETAIL                    
         DC    X'590D3D'                   O DAYPART OVERRIDE                   
         DC    X'8B0522'                   O DEMO MENU                          
         DC    X'6D053E'                   O LEVEL CONTROL=D,S                  
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT166X   DC    X'00',C'M6'                                                      
         SPACE 2                                                                
RT167    DC    AL1(RT167X-*+3,167,0)                                            
         DC    B'11000011',CL22'PMP-POST MARKET EVAL'                           
         DC    X'0000'                                                          
        DC    AL1(RT167X-*+1),B'11000000'                                       
         DC    X'0208060181'            CLI=XXX(POL)                            
         DC    X'5EAE0F0181'                                                    
         DC    X'059E180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'190439'                DATA COMPARE                            
         DC    X'48053A'              O AFFILATE                                
         DC    X'560D3B'              O PROGRAM TYPE                            
         DC    X'1E053C'              O DAYPART1 DETAIL                         
         DC    X'590D3D'              O DAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'5C053E'              O BRAND ANAL                              
         DC    X'6D0540'              O LEVEL CONTROL=D,S                       
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT167X   DC    X'00',C'M7'                                                      
         SPACE 2                                                                
RT168    DC    AL1(RT168X-*+3,168,0)                                            
         DC    B'11010011',CL22'BDT-BRAND DAILY T/S'                            
         DC    X'0000'                                                          
         DC    AL1(RT168X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLT=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550538'              O RERARE TYPE                             
         DC    X'48053A'              O AFFILATE                                
         DC    X'560D3B'              O PROGRAM TYPE                            
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'54053E'              O DETAIL OPTION                           
         DC    X'57053F'              O TOTAL OPTION                            
         DC    X'20050B'                ID SEQ=N,Y                              
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT168X   DC    X'00',C'D6'                                                      
         SPACE 2                                                                
RT169    DC    AL1(RT169X-*+3,169,0)                                            
         DC    B'11010011',CL22'SWE-SWEEP REPORT'                               
         DC    X'0000'                                                          
         DC    AL1(RT169X-*+1),B'11000000'                                      
         DC    X'0244060181'            CLI=XXX                                 
         DC    X'5DEE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'              O STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550438'                RERATE TYPE                             
         DC    X'48053A'              O AFFILATE                                
*        DC    X'560D3B'              O PROGRAM TYPE                            
         DC    X'590D3D'              O PAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'11054301CD'          O FLAG PERCENT=NN,ENN,E                   
         DC    X'58053E'              O TOTALS BY DAYPART                       
         DC    X'62053F'              O REPLACE DEMOS                           
         DC    X'630540'              O REPLACE OVERRIDES                       
         DC    X'920542'              O SPILL OPTION=0,1,2,3                    
         DC    X'CB0551'                   O OPTIONS                            
RT169X   DC    X'00',C'D8'                                                      
         SPACE 2                                                                
RT170    DC    AL1(RT170X-*+3,170,0)                                            
         DC    B'11010011',CL22'J6-BRAND DAILY TIMESEET'                        
         DC    X'0000'                                                          
         DC    AL1(RT170X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLT=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'550538'              O RERARE TYPE                             
         DC    X'CB0551'                   O OPTIONS                            
RT170X   DC    X'00',C'J6'                                                      
         SPACE 2                                                                
RT171    DC    AL1(RT171X-*+3,171,0)                                            
         DC    B'10010011',CL22'NET-CANADIAN NETWK RPT'                         
         DC    X'0000'                                                          
         DC    AL1(RT171X-*+1),B'10000000'                                      
         DC    X'0208060181'       CLI=XXX(POL)                                 
         DC    X'050E180181'       EST=NNN/NNN-NNN                              
         DC    X'E50562'           O LINE NUMBER                                
         DC    X'710613'           NETWK=XXXX                                   
         DC    X'095826'           STR,END=YYMMDD,ES                            
         DC    X'6105410191'       O OPT4=U                                     
         DC    X'8B0522'           O DEMO MENU                                  
         DC    X'FC0543'           O INCLUDE SPILL MARKET?                      
         DC    X'CB0551'                   O OPTIONS                            
RT171X   DC    X'00',C'DN'                                                      
         SPACE 2                                                                
RT172    DC    AL1(RT172X-*+3,172,0)                                            
         DC    B'10010011',CL22'NRS-NETWK ROTATN SCHD'                          
         DC    X'0000'                                                          
         DC    AL1(RT172X-*+1),B'10000000'                                      
         DC    X'0208060181'       CLI=XXX(POL)                                 
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO ETC                       
         DC    X'710613'           NETWK=XXXX                                   
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'20050B'           ID SEQ=N,Y                                   
         DC    X'45053E01B3'     O PRINT OPT=Y                                  
         DC    X'CB0551'                   O OPTIONS                            
RT172X   DC    X'00',C'RN'                                                      
         SPACE 2                                                                
RT173    DC    AL1(RT173X-*+3,173,0)                                            
         DC    B'11010011',CL22'PNG CAMPAIGN SCHEDULE'                          
         DC    X'0000'                                                          
         DC    AL1(RT173X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLT=XXX                                 
         DC    X'03080C0181'            PRO=POL                                 
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO ETC                  
         DC    X'060F0F0181'            MKT=ALL/XXXX                            
         DC    X'070713'                STA=ALL/XXXX                            
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'CB0551'                   O OPTIONS                            
RT173X   DC    X'00',C'J7'                                                      
         SPACE 2                                                                
RT174    DC    AL1(RT174X-*+3,174,0)                                            
         DC    B'11010011',CL22'DRR-DIRECT RESPONSE'                            
         DC    X'0000'                                                          
         DC    AL1(RT174X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DEE0C0181'                 PRD OR PGRP                        
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                   O STA=ALL/XXXX                       
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'CB0551'                   O OPTIONS                            
RT174X   DC    X'00',C'DR'                                                      
         SPACE 2                                                                
RT178    DC    AL1(RT178X-*+3,178,0)                                            
         DC    B'11000011',CL22'PBR-PLANNING BGT RPT'                           
         DC    X'0000'                                                          
         DC    AL1(RT178X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'030E0C0181'       PRO=XXX/POL/ALL                              
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO/NO,XXX/ALL                
         DC    X'5EAE0F0181'       MKT/MKTGRP=NNNN,ALL/XALL,XNNNN               
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'6E053E'           SUMMARIES ONLY=OPT/Y                         
         DC    X'61054001C7'       DATA OPTION=P PURCHASED ONLY                 
*                                             =S +STATION DETAIL                
         DC    X'CB0551'                   O OPTIONS                            
RT178X   DC    X'00',C'X1'                                                      
         SPACE 2                                                                
RT180    DC    AL1(RT180X-*+3,180,0)                                            
         DC    B'11000011',CL22'CSP-CHILD SPOT PERFMCE'                         
         DC    X'0000'                                                          
         DC    AL1(RT180X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAE0F0181'                                                    
         DC    X'05FC180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'0E3D32'         O BOOK-HUT=YYMM/YYMM-NN                        
         DC    X'550538'         O RERATE TYPE=P                                
         DC    X'48053A'         O AFFILIATE                                    
         DC    X'560D3B'         O PROGRAM TYPE                                 
         DC    X'8B0522'         O DEMO MENU                                    
         DC    X'590D3D'         O DAYPART OVERRIDE                             
         DC    X'7D053E'         O RPT BY MTHS=Y,N                              
         DC    X'80053F'         O RPT BY FLTS"Y,N                              
         DC    X'810540'         O RPT BY QTRS=Y,N                              
         DC    X'820541'         O RPT BY YRS=Y,N                               
         DC    X'840542'         O PRINT ALL BRANDS=Y,N                         
         DC    X'B00543'         O SUMMARY OPTION                               
*        DC    X'CB0551'                   O OPTIONS                            
RT180X   DC    X'00',C'X3'                                                      
         SPACE 2                                                                
RT181    DC    AL1(RT181X-*+3,181,0)                                            
         DC    B'11000011',CL22'BTP-BILLING TAPE'                               
         DC    X'0000'                                                          
         DC    AL1(RT181X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'030E0C0181'       PRD=ALL,XXX,POL                              
         DC    X'05FC180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'09082601D5'       STR,END=YYMMDD                               
         DC    X'CB0551'                   O OPTIONS                            
RT181X   DC    X'00',C'X4'                                                      
         SPACE 2                                                                
RT192    DC    AL1(RT192X-*+3,192,0)                                            
         DC    B'11010011',CL22'PRO-PRODUCT SUMMARY'                            
         DC    X'0000'                                                          
         DC    AL1(RT192X-*+1),B'11000000'                                      
         DC    X'0266060181'                 CLI=XXX/ALL/*NN                    
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091C26'                     STR,END=YYMMDD/ES                  
         DC    X'25053E'                   O ANALYSIS TYPE                      
         DC    X'260540'                   O PAID TOTALS?                       
         DC    X'0C0932'                   O PAY PERIOD STR=YYMMDD              
         DC    X'0D0938'                   O PAY PERIOD END=YYMMDD              
         DC    X'70053F0189'               O GOAL/AUTH TOTS=G/A/N               
         DC    X'6E0541'                   O SUMMARIES ONLY?                    
         DC    X'490542'                   O SPECIAL REP BREAKOUT               
         DC    X'20050B'                     ID SEQ=N,Y                         
         DC    X'870543'                   O PRINT COMMENTS=Y,N                 
         DC    X'B40544'                   O BILLED TODAY ONLY (B1)             
         DC    X'CB0551'                   O OPTIONS                            
RT192X   DC    X'00',C'A2'                                                      
         SPACE 2                                                                
RT193    DC    AL1(RT193X-*+3,193,0)                                            
         DC    B'11010011',CL22'CFA-CLT ACCNT SUMMARY'                          
         DC    X'0000'                                                          
         DC    AL1(RT193X-*+1),B'11000000'                                      
         DC    X'0226060181'                 CLI=XXX/ALL/*NN                    
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'25053E'                   O ANALYSIS TYPE                      
         DC    X'260540'                   O PAID TOTALS?                       
         DC    X'0C0932'                   O PAY PERIOD STR=YYMMDD              
         DC    X'0D0938'                   O PAY PERIOD END=YYMMDD              
         DC    X'70053F0189'               O GOAL/AUTH TOTS=G/A/N               
         DC    X'6E0541'                   O SUMMARIES ONLY?                    
         DC    X'490542'                   O SPECIAL REP BREAKOUT               
         DC    X'20050B'                   O ID SEQ=N,Y                         
         DC    X'870543'                   O PRINT COMMENTS=Y,N                 
         DC    X'B40544'                   O BILLED TODAY ONLY                  
         DC    X'CB0551'                   O OPTIONS                            
RT193X   DC    X'00',C'AB'                                                      
         SPACE 2                                                                
RT194    DC    AL1(RT194X-*+3,194,0)                                            
         DC    B'11000111',CL22'ACT-ACTIVITY UPDATE'                            
         DC    X'0000'                                                          
         DC    AL1(RT194X-*+1),B'11000000'                                      
         DC    X'0206060181'                 CLI=ALL/XXX                        
         DC    X'0560180181'                 EST=XXX,001-255                    
         DC    X'CB0551'                   O OPTIONS                            
RT194X   DC    X'00',C'AC'                                                      
         SPACE 2                                                                
RT195    DC    AL1(RT195X-*+3,195,0)                                            
         DC    B'11000011',CL22'MAA-MKT ACTIVITY ANAL'                          
         DC    X'0000'                                                          
         DC    AL1(RT195X-*+1),B'11000000'                                      
         DC    X'0226060181'         CLI=XXX,ALL,$N                             
         DC    X'5EA80F0181'         MKT=NNNN,OR MGR=XALL,XNNNN                 
         DC    X'2A042C'             CUR MTH=YYMM IN RENDD                      
         DC    X'6D043E019D'         LEVEL CONTROL=1-7                          
         DC    X'CB0551'                   O OPTIONS                            
RT195X   DC    X'00',C'A7'                                                      
         SPACE 2                                                                
*                                                                               
*  There's now a new request LB for soon B1 billing.                            
*  This request is not present in REQTAB.                                       
*  I fudge it in SPREQ01 and behind the sceens it's treated like B1             
*  eventhough requested like LB                                                 
*                                                                               
RT196    DC    AL1(RT196X-*+3,196,0)                                            
         DC    B'11010011',CL22'BIL-BILLING'                                    
         DC    X'0000'                                                          
         DC    AL1(RT196X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX/ALL/*NN,CGR=                         
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAB0F0181'       MKT OR MGRP                                  
         DC    X'070713'         O STATION=XXX(INVISIBLE),ALL                   
         DC    X'05EE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'73043F0193'       BILLING TYPE=4,5,6,7                         
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
***      DC    X'0B091E'         O INVOICE DATE=YYMMDD                          
         DC    X'0B0965'         O INVOICE DATE=YYMMDD R2USER                   
         DC    X'690524'         O DUE DAYS                                     
         DC    X'727D32'         O AMOUNT                                       
         DC    X'B30543'         O INCLUDE REPORT                               
         DC    X'1705C2'      DDSO MARK FILES                                   
         DC    X'CB0551'                   O OPTIONS                            
RT196X   DC    X'00',C'B1'                                                      
         SPACE 2                                                                
RT197    DC    AL1(RT197X-*+3,197,0)                                            
         DC    B'11000011',CL22'STS-STATION SUMMARY'                            
         DC    X'0000'                                                          
         DC    AL1(RT197X-*+1),B'11000000'                                      
         DC    X'0266060181'         CLI=XXX,ALL,*NN                            
         DC    X'03030C0181'       O PRD                                        
****     DC    X'05EF180181'       O EST=NNN/NNN-NNN//(ALL)                     
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'070713'           O STA=XXX,ALL                                
         DC    X'09042601AD'         STR,END=YYMM                               
         DC    X'5F053E01A9'       O DATE OPTN=1,2,3                            
         DC    X'8D5F1E0181'       O REP NUM=SNNN,PNNN,SALL,TNN,TALL            
         DC    X'370540'           O ANAL BY OFF=Y,N                            
         DC    X'2D0541'           O XCLUDE NET=Y,N                             
         DC    X'2C0542'           O XCLUDE CLT DETAILS=Y,N                     
         DC    X'4E0544'             EXCLUD CODE=X                              
         DC    X'48053A'           O AFFILIATE                                  
         DC    X'57054301AB'       O TOTAL OPTN=1,2,3                           
         DC    X'6E053D'             SUMMARIES ONLY=Y,N                         
         DC    X'CB0551'                   O OPTIONS                            
RT197X   DC    X'00',C'A5'                                                      
         SPACE 2                                                                
RT198    DC    AL1(RT198X-*+3,198,0)                                            
         DC    B'11000011',CL22'RET-RETAIL REPORT'                              
         DC    X'0000'                                                          
         DC    AL1(RT198X-*+1),B'11000000'                                      
         DC    X'0204060181'         CLI=XXX                                    
         DC    X'030E0C0181'         PRD=ALL,XXX,POL                            
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'05EE180181'         EST=ALL,NNN,NNN-NNN,NO-FFF                 
         DC    X'091826'             STR,END=YYMMDD,ES                          
         DC    X'CB0551'                   O OPTIONS                            
RT198X   DC    X'00',C'B9'                                                      
         SPACE 2                                                                
**RT199    DC    AL1(RT199X-*+3,199,0)                                          
*         DC    B'11010011',CL22'BTF-BILLING TRANSFER'                          
*         DC    X'0000'                                                         
*         DC    AL1(RT199X-*+1),B'11000000'                                     
*         DC    X'0226060181'       CLI=XXX/ALL/*NN                             
*         DC    X'03060C0181'       PRO=ALL,XXX                                 
*         DC    X'090826'           STR,END=YYMMDD                              
*         DC    X'74053E'           TEST RUN                                    
*         DC    X'75053F'           LIST UNPOSTED                               
*         DC    X'760540'           LIST PREV POSTED                            
*        DC    X'CB0551'                   O OPTIONS                            
*RT199X   DC    X'00',C'BA'                                                     
         SPACE 2                                                                
RT202    DC    AL1(RT202X-*+3,202,0)                                            
         DC    B'11000011',CL22'RDS-RETAIL DSTRBTN SUM'                         
         DC    X'0000'                                                          
         DC    AL1(RT202X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLI=XXX/ALL/*NN                              
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAB0F0181'       MKT OR MGRP                                  
         DC    X'05EE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
         DC    X'CB0551'                   O OPTIONS                            
RT202X   DC    X'00',C'BD'                                                      
         SPACE 2                                                                
RT203    DC    AL1(RT203X-*+3,203,0)                                            
         DC    B'11000011',CL22'GM- GM TAPE REQUEST'                            
         DC    X'0000'                                                          
         DC    AL1(RT203X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX/ALL/*NN,CGR=                         
******** DC    X'0236060181'       CLI=XXX/ALL/*NN                              
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'05FE180181'       EST=NNN,NNN-NNN,NO                           
         DC    X'090826'           STR,END=MMDD/YY                              
         DC    X'9C053E'           PRODUCE TAPE=Y/N                             
         DC    X'CB0551'                   O OPTIONS                            
RT203X   DC    X'00',C'GM'                                                      
         SPACE 2                                                                
*                                                                               
*        THE ZB IS REQUESTED BY ENTERING WB                                     
*                                                                               
RT204    DC    AL1(RT204X-*+3,204,0)                                            
         DC    B'11010011',CL22'WB- WB INTERFACE'                               
         DC    X'0000'                                                          
         DC    AL1(RT14X-*+1),B'11000000'                                       
         DC    X'0224060181'       CLI=XXX,OFFICE                               
         DC    X'03060C0181'       PRO=ALL/XXX                                  
         DC    X'05EE180181'       EST=NNN,ALL                                  
         DC    X'0908260181'       START-END                                    
         DC    X'9C053E'           PRODUCE TAPE                                 
         DC    X'CB0551'           OPTIONS                                      
RT204X   DC    X'00',C'ZB'                                                      
         SPACE 2                                                                
RT205    DC    AL1(RT205X-*+3,205,0)                                            
         DC    B'11010011',CL22'IN -NISSAN INTERFACE'                           
         DC    X'0000'                                                          
         DC    AL1(RT205X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'03060C0181'       PRD=NNN,ALL                                  
         DC    X'05FE180181'       EST=ALL,NNN,NNN-NNN,ALL+FILTERS              
         DC    X'09082601D5'       STR,END=YYMMDD (BILL RUN)                    
         DC    X'9C043E'           PRODUCE FILE=Y/N  OPT1                       
         DC    X'CB0551'                   O OPTIONS                            
RT205X   DC    X'00',C'IN'                                                      
*                                                                               
RT206    DC    AL1(RT206X-*+3,206,0)                                            
         DC    B'11010011',CL22'Z7-EASI CONVERSION RP'                          
         DC    X'0000'                                                          
         DC    AL1(RT206X-*+1),B'11000000'                                      
         DC    X'710713'          O NETWORK                                     
         DC    X'091826'          STR,END=YYMMDD,ES                             
         DC    X'74053E'          O TEST RUN                                    
         DC    X'C60540'          O LIST PREV CONVERSIONS                       
         DC    X'C70541'          O REPLACE EXISTING INVOICES                   
         DC    X'C8054201E9'      O Y=CREATE I2 REQ FILE                        
*                                   P= CREATE AND PRINT(DDS)                    
         DC    X'0E3D32'                   O BOOK - HUT                         
         DC    X'860543'                   O POST AFFIDS=Y,N                    
         DC    X'FA0520'                   O SOURCE                             
         DC    X'0205060181'      CLI=XXX                                       
         DC    X'CB0551'                   O OPTIONS                            
RT206X   DC    X'00',C'Z7'                                                      
         SPACE 2                                                                
*                                                                               
*  There's now a new request LB for soon BU billing.                            
*  This is cloned from spot LB                                                  
*  This request is not present in REQTAB.                                       
*  I fudge it in SPREQ01 and behind the sceens it's treated like B1             
*  eventhough requested like LB                                                 
*                                                                               
RT207    DC    AL1(RT207X-*+3,207,0)                                            
         DC    B'11010011',CL22'NTB-NETWORK BILLING'                            
         DC    X'0000'                                                          
         DC    AL1(RT207X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX/ALL/*NN                              
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'05EE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'73043F0193'       BILLING TYPE=4,5,6,7                         
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
***      DC    X'0B091E'         O INVOICE DATE=YYMMDD                          
         DC    X'0B0965'         O INVOICE DATE=YYMMDD  R2USER                  
         DC    X'690524'         O DUE DAYS                                     
         DC    X'727D32'         O AMOUNT                                       
         DC    X'1705C2'      DDSO MARK FILES                                   
         DC    X'710513'         O NETWORK                                      
         DC    X'410541'         O DAYPART FILTERS                              
         DC    X'CC053E01EB01EC' O COST TYPE=TIUBSO123AEX                       
         DC    X'CB0551'                   O OPTIONS                            
RT207X   DC    X'00',C'BU'                                                      
         SPACE 2                                                                
RT208    DC    AL1(RT208X-*+3,208,0)                                            
         DC    B'11000011',CL22'MKT/COM PERFORMANCE'                            
         DC    X'0000'                                                          
         DC    AL1(RT208X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLIENT CODE=NNN                              
         DC    X'03060C0181'       PRD=NNN,ALL                                  
         DC    X'057E180181'       EST=NNN,NNN-NNN,NO                           
         DC    X'5EAE0F0181'       MKT=NNN,ALL,MKTGRP                           
         DC    X'091826'           STR,END=MMDD/YY,ES                           
         DC    X'0E3D32'           O BOOK-HUT=YYMM,YYMM-XX,ACT                  
         DC    X'190439'             DATA COMPARE=A,B,C,D                       
         DC    X'1E0542'           O DAYPART DET=A,B,C                          
         DC    X'CB0551'                   O OPTIONS                            
RT208X   DC    X'00',C'F4'                                                      
         SPACE 2                                                                
RT209    DC    AL1(RT209X-*+3,209,0)                                            
         DC    B'11000011',CL22'COMMERCIAL PERFORMANCE'                         
         DC    X'0000'                                                          
         DC    AL1(RT209X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLIENT CODE=NNN                              
         DC    X'03060C0181'       PRD=NNN,ALL                                  
         DC    X'057E180181'       EST=NNN,NNN-NNN,NO                           
         DC    X'5E8A0F0181'       MKT=NNN,ALL,MKTGRP                           
         DC    X'9B043A'           COMM ID(NOT EDITED BUT REQUIRED)             
         DC    X'091826'           STR,END=MMDD/YY,ES                           
         DC    X'0E1D32'           O BOOK-HUT=YYMM,YYMM-XX,ACT                  
         DC    X'190539'           O DATA COMPARE=A,B,C,D                       
         DC    X'1E0542'           O DAYPART DET=A,B,C                          
         DC    X'CB0551'                   O OPTIONS                            
RT209X   DC    X'00',C'F2'                                                      
         SPACE 2                                                                
RT210    DC    AL1(RT210X-*+3,210,0)                                            
         DC    B'11000011',CL22'BTR- BT REPORT'                                 
         DC    X'0000'                                                          
         DC    AL1(RT210X-*+1),B'11000000'                                      
         DC    X'0236060181'       CLI=XXX/ALL/*NN                              
         DC    X'03060C0181'       PRD=NNN,ALL                                  
         DC    X'05FE180181'       EST=NNN,NNN-NNN,NO                           
         DC    X'099826'           STR,END=MMDD/YY,ES                           
         DC    X'F80565'           O STRT INVOICE #   COL 21 CARD 2             
         DC    X'F90569'           O END INVOICE #    COL 25 CARD 2             
         DC    X'9C043E'           PRODUCE TAPE=Y/N                             
         DC    X'C4054201E701E8'   O AOR=Y/X,C=COMMISS ONLY,N=NO COMMIS         
         DC    X'CB0551'                   O OPTIONS                            
RT210X   DC    X'00',C'BT'                                                      
         SPACE 2                                                                
***RT211    DC    AL1(RT211X-*+3,211,0)                                         
***         DC    B'11010011',CL22'NAL-NEW BRND ALLOC-K5'                       
***         DC    X'0000'                                                       
***         DC    AL1(RT211X-*+1),B'11000000'                                   
***         DC    X'0204060181'       CLT=XXX                                   
***      CHANGE TO ALLOW FOR PROD GROUP                                         
***         DC    X'05BE180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX             
***         DC    X'070713'           O STA=ALL/XXXX                            
***         DC    X'091826'           STR,END=YYMMDD                            
***         DC    X'50053E01CF'       REALLOCATE=Y/N,OR 0-9,A-E                 
***         DC    X'10053F01D1'       ALLOC BASIS=$/D                           
***         DC    X'1B05400199'       ALLOC EXECSS=Y/N                          
***         DC    X'93053201D3'       WEIGHT OVERRIDES 0-9,A=10 7 CHARS         
***         DC    X'410D7F'           O DAYPART FILTER                          
**NOTE!** POSITION FOR DPT IS NOT 7F, IT'LL GET OVERRIDEN IN REQ04              
**NOTE!** I HAVE TO KEEP IT UNDER X'80' -- SPECIAL FOR DDS                      
***         DC    X'9D053C'           O SPOT LENGTH FILTER                      
***         DC    X'740641'           TEST RUN=Y/N                              
***         DC    X'CB0551'                   O OPTIONS                         
***RT211X   DC    X'00',C'K5'                                                   
         SPACE 2                                                                
RT212    DC    AL1(RT212X-*+3,212,0)                                            
         DC    B'11000011',CL22'NV-INVOICE REPORT'                              
         DC    X'0000'                                                          
         DC    AL1(RT212X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLT=NNN/ALL/*NN/*ALL                         
         DC    X'5DFF0C0181'     O PROD OR PROD GRP                             
         DC    X'5EAF0F0181'     O MKT OR MKT GRP                               
         DC    X'070F130181'     O STATION                                      
         DC    X'05FF180181'     O ESTIMATE                                     
         DC    X'094C26'           START,END=YYMM/START OPTIONAL                
         DC    X'9E043E'           GROSS/NET/BOTH                               
         DC    X'9F053F01030102' O PRINT REPORT/LETTERS? (DEFAULT=NO)           
*        DC    X'9F053F'         O PRINT REPORT/LETTERS? (DEFAULT=NO)           
         DC    X'A3054001DD'     O SUPPRESS ALL MATCHED? (DEFFAULT=NO)          
         DC    X'AD093201E3'     O LETTER DATE (DEFAULT=TODAY)                  
         DC    X'BD054101DD'     O XCLUDE SPCL REP $(DEFAULT=NO)                
         DC    X'BF054301DD'     O INCLUDE $0 LETTER(DEFAULT=NO)                
         DC    X'D00544'         O CLIENT EXCLUSION                             
         DC    X'D1051E'         O POL BREAKOUT                                 
         DC    X'EA053D'         O CABLE BY SYSTEM                              
         DC    X'CB0551'                   O OPTIONS                            
RT212X   DC    X'00',C'NV'                                                      
         SPACE                                                                  
RT213    DC    AL1(RT213X-*+3,213,0)                                            
         DC    B'11010011',CL22'DD-DEMOGRAPHIC REPORT'                          
         DC    X'0000'                                                          
         DC    AL1(RT213X-*+1),B'11000000'                                      
         DC    X'0226060181'      CLI=XXX,ALL,*NN                               
         DC    X'5DFE0C0181'      PRD OR PGRP                                   
         DC    X'5EAF0F0181'      MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN           
         DC    X'070713'          STA=ALL/XXXX                                  
         DC    X'059E180181'      EST=ALL,NO,NNN,NNN-NNN,N/,XXX                 
         DC    X'091826'          STR,END=YYMMDD,ES                             
***      DC    X'B2053E'          O ACTUAL BOOK (DEFAULT=NO)                    
         DC    X'0E3D32'          O BOOK-HUT(LIKE IS D2)                        
         DC    X'D4053F'          O RERATE                                      
         DC    X'E30540'          O SUPPRESS -S WKY                             
         DC    X'CB0551'                   O OPTIONS                            
RT213X   DC    X'00',C'DD'                                                      
         SPACE                                                                  
RT214    DC    AL1(RT214X-*+3,214,0)                                            
         DC    B'11000011',CL22'CB-COMP BUY REP'                                
         DC    X'0000'                                                          
         DC    AL1(RT214X-*+1),B'11000000'                                      
         DC    X'0208060181'      CLI=XXX (ONLY ACCEPTS POL CLTS)               
         DC    X'030C0C0181'       PRD=NNN,ALL                                  
         DC    X'5E8A0F0181'      MKT=ALL,NNNNN,MKTGRP                          
         DC    X'090826'          STR=MON END=SUN = 12 MONTHS                   
         DC    X'8B0522'        O DEMO MENU                                     
         DC    X'B5053E'        O BY SPOT LENGTH                                
         DC    X'CB0551'                   O OPTIONS                            
RT214X   DC    X'00',C'CB'                                                      
         SPACE                                                                  
RT215    DC    AL1(RT215X-*+3,215,0)                                            
         DC    B'11000011',CL22'A3-CLIENT SUMMARY'                              
         DC    X'0000'                                                          
         DC    AL1(RT215X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'A4053F'                   O DETAIL FORMAT (1-8)                
         DC    X'A80542'                   O DOLLAR FORMAT (1-4)                
         DC    X'A5053E'                   O GROSS/NET                          
         DC    X'A60540'                   O PERIOD FORMAT                      
         DC    X'250541'                   O ANALYSIS TYPE                      
         DC    X'A90533'                   O DOLLAR ADJUSTMENT(NNN.NN)          
         DC    X'AA0932'                   O PAY/BILL PERIOD STR=YYMMDD         
         DC    X'AB0938'                   O PAY/BILL PERIOD END=YYMMDD         
         DC    X'1A051E01E1'               O P=PAY PERIOD,B=BILL PERIOD         
         DC    X'AC051F'                   O D=DOWNLOAD (RREPNO+1=D)            
*                                            B=BROADCAST MONTH COL67=Y          
*                                            A=BILL ADJUSTMENT COL68=Y          
         DC    X'CB0551'                   O OPTIONS                            
RT215X   DC    X'00',C'A3'                                                      
         SPACE                                                                  
RT216    DC    AL1(RT216X-*+3,216,0)                                            
         DC    B'11000011',CL22'XH-PRD PRORATION'                               
         DC    X'0000'                                                          
         DC    AL1(RT216X-*+1),B'11000000'                                      
         DC    X'0224060181'                 CLI=XXX/*NN                        
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'A0A00F0181'                 MGR=XALL,MGR=XNNNN                 
         DC    X'05FC180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'CB0551'                   O OPTIONS                            
RT216X   DC    X'00',C'XH'                                                      
         SPACE                                                                  
RT217    DC    AL1(RT217X-*+3,217,0)                                            
         DC    B'11000011',CL22'MC-COML/MKT PERFORM'                            
         DC    X'0000'                                                          
         DC    AL1(RT217X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DEE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'          O MKT GROUP/MKT                           
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'            O  BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX         
         DC    X'590D3D'              O DAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'E205C4'              O CML CLASS FILTER                        
         DC    X'AF053F'              O CLASS/CML DETAIL                        
         DC    X'B00540'              O SUMMARY OPTION                          
         DC    X'B10541'              O UNKNOWN CML DETAILS                     
         DC    X'CF0564'              O FILM CODE                               
         DC    X'B90542'              O DOWNLOAD                                
         DC    X'CB0551'                   O OPTIONS                            
RT217X   DC    X'00',C'MC'                                                      
         SPACE                                                                  
RT218    DC    AL1(RT218X-*+3,218,0)                                            
         DC    B'11000011',CL22'MD-MKT/COML PERFORM'                            
         DC    X'0000'                                                          
         DC    AL1(RT217X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DEE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'          O MKT GROUP/MKT                           
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'            O  BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX         
         DC    X'590D3D'              O DAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'E205C4'              O CML CLASS FILTER                        
         DC    X'AF053F'              O CLASS/CML DETAIL                        
         DC    X'B10541'              O UNKNOWN CML DETAILS                     
         DC    X'CF0564'              O FILM CODE                               
         DC    X'B00540'              O SUMMARY OPTION                          
         DC    X'B90542'              O DOWNLOAD                                
         DC    X'CB0551'                   O OPTIONS                            
RT218X   DC    X'00',C'MD'                                                      
         SPACE                                                                  
RT219    DC    AL1(RT219X-*+3,219,0)                                            
         DC    B'11010011',CL22'COP-CONFIRM PURCHASE'                           
         DC    X'0000'                                                          
         DC    AL1(RT219X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'CB0551'                   O OPTIONS                            
RT219X   DC    X'00',C'D7'                                                      
*                                                                               
RT220    DC    AL1(RT220X-*+3,220,0)                                            
         DC    B'11000011',CL22'XL-BUYER WRKLD ANALS'                           
         DC    X'0000'                                                          
         DC    AL1(RT220X-*+1),B'11000000'                                      
         DC    X'0226060181'                 CLI=XXX,ALL,*NN                    
         DC    X'A0A00F0181'                 MGR=XALL,MGR=XNNNN                 
         DC    X'090826'                     STR,END=YYMMDD                     
         DC    X'CB0551'                   O OPTIONS                            
RT220X   DC    X'00',C'XL'                                                      
         SPACE 2                                                                
RT221    DC    AL1(RT221X-*+3,221,0)                                            
         DC    B'11000011',CL22'MJ-JWT QUARTERLY TAPE'                          
         DC    X'0000'                                                          
         DC    AL1(RT221X-*+1),B'11000000'                                      
         DC    X'020A060181'            CLI=XXX(POL)                            
         DC    X'5EAE0F0181'                                                    
         DC    X'059E180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3C32'           BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX             
         DC    X'190439'                DATA COMPARE                            
         DC    X'48053A'              O AFFILATE                                
         DC    X'560D3B'              O PROGRAM TYPE                            
         DC    X'1E053C'              O DAYPART1 DETAIL                         
         DC    X'590D3D'              O DAYPART OVERRIDE                        
         DC    X'8B0522'              O DEMO MENU                               
         DC    X'5C053E'              O BRAND ANAL                              
         DC    X'6D0540'              O LEVEL CONTROL=D,S                       
         DC    X'920542'                   O SPILL OPTION=0,1,2,3               
         DC    X'CB0551'                   O OPTIONS                            
RT221X   DC    X'00',C'MJ'                                                      
*                                                                               
RT222    DC    AL1(RT222X-*+3,222,0)                                            
         DC    B'11010011',CL22'BIL-SPOT DRAFT BILLING'                         
         DC    X'0000'                                                          
         DC    AL1(RT222X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX/ALL/*NN,CGR=                         
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAB0F0181'       MKT OR MGRP                                  
         DC    X'070713'         O STATION                                      
         DC    X'05EE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'73043F0193'       BILLING TYPE=4,5,6,7                         
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
***      DC    X'0B091E'         O INVOICE DATE=YYMMDD                          
         DC    X'0B0965'         O INVOICE DATE=YYMMDD  R2USER                  
         DC    X'690524'         O DUE DAYS                                     
         DC    X'727D32'         O AMOUNT                                       
         DC    X'1705C2'      DDSO MARK FILES                                   
         DC    X'CB0551'                   O OPTIONS                            
****     DC    X'B30543'         O INCLUDE REPORT                               
RT222X   DC    X'00',C'D1'                                                      
*                                                                               
RT223    DC    AL1(RT223X-*+3,223,0)                                            
         DC    B'11010011',CL22'NTB-DRAFT BILLING'                              
         DC    X'0000'                                                          
         DC    AL1(RT223X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX/ALL/*NN/CGR=NNNN                     
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'05EE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'73043F0193'       BILLING TYPE=4,5,6,7                         
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
***      DC    X'0B091E'         O INVOICE DATE=YYMMDD                          
         DC    X'0B0965'         O INVOICE DATE=YYMMDD R2USER                   
         DC    X'690524'         O DUE DAYS                                     
         DC    X'727D32'         O AMOUNT                                       
         DC    X'1705C2'      DDSO MARK FILES                                   
         DC    X'710513'         O NETWORK                                      
         DC    X'410541'         O DAYPART FILTERS                              
         DC    X'CC053E01EB01EC' O COST TYPE=TIUBSO123AEX                       
         DC    X'CB0551'                   O OPTIONS                            
RT223X   DC    X'00',C'DU'                                                      
*                                                                               
RT224    DC    AL1(RT224X-*+3,224,0)                                            
         DC    B'11010011',CL22'MPX-MEDIA PLAN EXTRACT'                         
         DC    X'0000'                                                          
         DC    AL1(RT224X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLI=XXX/ALL/*NN                              
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'05FE180181'       EST=NNN/NNN-NNN//(ALL)                       
         DC    X'096826'           STR,END=YYMMDD-YYMMDD,YYMM,13/YY             
         DC    X'B80441'           QUARTER                                      
         DC    X'8B0422'             DEMO MENU                                  
         DC    X'BA0540'           O 30'S ONLY                                  
         DC    X'B9053F'           O DOWNLOAD                                   
         DC    X'0E3D32'           O BOOK - HUT                                 
         DC    X'CB0551'           O OPTIONS                                    
RT224X   DC    X'00',C'PX'                                                      
*                                                                               
RT225    DC    AL1(RT225X-*+3,225,0)                                            
         DC    B'10000011',CL22'PGI-PG INFORMATION TAPE'                        
         DC    X'0000'                                                          
         DC    AL1(RT225X-*+1),B'10000000'                                      
         DC    X'0204060181'                 CLI=PG                             
         DC    X'030E0C0181'                 PRO=POL                            
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO                 
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'9C043E'                     PRODUCE TAPE                       
         DC    X'C00440'                     PRINT REPORT                       
         DC    X'BE043F01E501E6'             DATA TYPE                          
         DC    X'E80541'                     NCT OPTION (COL 65)                
         DC    X'CB0551'                   O OPTIONS                            
RT225X   DC    X'00',C'X5'                                                      
         SPACE 2                                                                
RT226    DC    AL1(RT226X-*+3,226,0)                                            
         DC    B'11010011',CL22'AX-PRODUCT SUMMARY'                             
         DC    X'0000'                                                          
         DC    AL1(RT226X-*+1),B'11000000'                                      
         DC    X'0226060181'                 CLI=XXX/ALL/*NN                    
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAB0F0181'       O MKT OR MGR-ALL,MGR=XALL,MGR=XNNNN          
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'25053E'                   O ANALYSIS TYPE                      
         DC    X'260540'                   O PAID TOTALS?                       
         DC    X'0C0932'                   O PAY PERIOD STR=YYMMDD              
         DC    X'0D0938'                   O PAY PERIOD END=YYMMDD              
         DC    X'70053F0189'               O GOAL/AUTH TOTS=G/A/N               
         DC    X'6E0541'                   O SUMMARIES ONLY?                    
         DC    X'490542'                   O SPECIAL REP BREAKOUT               
         DC    X'20050B'                     ID SEQ=N,Y                         
         DC    X'870543'                   O PRINT COMMENTS=Y,N                 
         DC    X'B40544'                   O BILLED TODAY ONLY (B1)             
         DC    X'CB0551'                   O OPTIONS                            
RT226X   DC    X'00',C'AX'                                                      
*                                                                               
RT227    DC    AL1(RT227X-*+3,227,0)                                            
         DC    B'11010011',CL22'DL-SPOT DEMO UPDATE'                            
         DC    X'0000'                                                          
         DC    AL1(RT227X-*+1),B'11000000'                                      
         DC    X'0224060181'                 CLI=XXX/*NN                        
         DC    X'710513'                     O NET=ALL/XXXX                     
         DC    X'05FE180181'                   EST=NNN/NNN-NNN/NO (ALL)         
         DC    X'091826'                       STR,END=YYMMDD/ES                
         DC    X'900532'                     O SHOW=NNNN                        
         DC    X'C1043E'                       POST=P,N                         
         DC    X'CB0551'                   O OPTIONS                            
RT227X   DC    X'00',C'DL'                                                      
*                                                                               
RT228    DC    AL1(RT228X-*+3,228,0)                                            
         DC    B'11010011',CL22'Z5-EASI CONVERSION RP'                          
         DC    X'0000'                                                          
         DC    AL1(RT228X-*+1),B'11000000'                                      
         DC    X'070713'          STA=ALL/XXXX                                  
         DC    X'06090F0181'       MKT=NNNN                                     
         DC    X'091826'          STR,END=YYMMDD,ES                             
         DC    X'74053E'          O TEST RUN                                    
         DC    X'C60540'          O LIST PREV CONVERSIONS                       
         DC    X'C70541'          O REPLACE EXISTING INVOICES                   
         DC    X'C8054201E9'      O Y=CREATE I2 REQ FILE                        
*                                   P= CREATE AND PRINT(DDS)                    
         DC    X'0E3D32'                   O BOOK - HUT                         
         DC    X'860543'                   O POST AFFIDS=Y,N                    
         DC    X'FA0520'                   O SOURCE                             
         DC    X'0205060181'      CLI=XXX                                       
         DC    X'CB0551'                   O OPTIONS                            
RT228X   DC    X'00',C'Z5'                                                      
*                                                                               
         SPACE 2                                                                
RT229    DC    AL1(RT229X-*+3,229,0)                                            
         DC    B'11010011',CL22'RZ-SORTED TIME SHEETS'                          
         DC    X'0000'                                                          
         DC    AL1(RT229X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRD OR PGRP                             
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'              O STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'20050B'                ID SEQ=A,Y                              
         DC    X'920543'                O SPILL OPTION=0,1,2,3 (OPT6)           
         DC    X'540541'                O DETAIL OPTION                         
         DC    X'AE053E'                O XTRA GRID LINES                       
         DC    X'CB0251'                  OPTIONS REP=D5                        
RT229X   DC    X'00',C'RZ'                                                      
*                                                                               
         SPACE 2                                                                
RT230    DC    AL1(RT230X-*+3,230,0)                                            
         DC    B'11010011',CL22'IMR-INVOICE MATCHING'                           
         DC    X'0000'                                                          
         DC    AL1(RT230X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'047E0C0181'     PRO,MODE=ALL,XXX,POL,XXX-YYY,XXX-ALL           
         DC    X'05FE180181'                 EST=NO/NNN/NNN-NNN                 
         DC    X'710413'           NETWORK                                      
         DC    X'09C826'           STRD,ENDD=YYMM OR YYMMDD AND NO END          
*                                  OR YYMMDD IN BOTH                            
         DC    X'3D0540'                   O SPACING                            
         DC    X'450541'                   O PRINT OPTION=D/S/BLK/0-9           
         DC    X'85053A01BF'               O PARTIAL MATCH=*,TLCPDWA            
         DC    X'86054201C1'               O POST AFFIDS=Y,N,P                  
         DC    X'080D680181'               O REP=NNN                            
         DC    X'C20544'                   O SUPPRESS COSTS                     
         DC    X'B7053E'                   O FILM REP OPT (Y,N,F,E,X)           
         DC    X'CB0551'                   O OPTIONS                            
***      THE ABOVE OPTION IS SET TO GO, RELINK AND TEST                         
RT230X   DC    X'00',C'N2'                                                      
         SPACE 2                                                                
RT231    DC    AL1(RT231X-*+3,231,0)                                            
         DC    B'11010011',CL22'MY-BILL INT REVERSAL'                           
         DC    X'0000'                                                          
         DC    AL1(RT231X-*+1),B'11000000'                                      
         DC    X'0226060181'       CLI=XXX/ALL/*NN                              
         DC    X'03060C0181'       PRO=ALL,XXX                                  
         DC    X'CD0620'           BILL TYPE=AOR,RET,REG,ETC.                   
         DC    X'D60838'           DATE                                         
         DC    X'D70440'           DATE TYPE                                    
         DC    X'DA043F'           STATION TYPE                                 
         DC    X'CE0932'         O INTERFACE DATE                               
         DC    X'D80564'         O BILL NUMBERS (RCARD2)                        
         DC    X'CB0551'         O OPTIONS                                      
RT231X   DC    X'00',C'MY'                                                      
         SPACE                                                                  
RT232    DC    AL1(RT232X-*+3,232,0)                                            
         DC    B'11010011',CL22'BUY MOVE REPORT'                                
         DC    X'0000'                                                          
         DC    AL1(RT232X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=NNN                                 
         DC    X'03040C0181'            PRO=NNN                                 
         DC    X'0504180181'            EST=XXX                                 
         DC    X'06090F0181'            MKT=XXX                                 
         DC    X'070713'                STA=NNNN,ALL                            
         DC    X'EE04650181'         TO-CLI=NNN                                 
         DC    X'A702680181'         TO-PRD=NNN                                 
         DC    X'EF046B0181'         TO-EST=XXX                                 
         DC    X'CB0551'                   O OPTIONS                            
RT232X   DC    X'00',C'MV'                                                      
         SPACE 2                                                                
RT233    DC    AL1(RT233X-*+3,233,0)                                            
         DC    B'11010011',CL22'J1-BUYER INCENTIVE REP'                         
         DC    X'0000'                                                          
         DC    AL1(RT233X-*+1),B'11000000'                                      
         DC    X'0261060181'       CLI=$N                                       
         DC    X'090826'           STRD,ENDD=YYMM OR YYMMDD AND NO END          
         DC    X'0E1432'                   O BOOK - HUT                         
         DC    X'CB0551'         O OPTIONS                                      
RT233X   DC    X'00',C'J1'                                                      
         SPACE                                                                  
RT234    DC    AL1(RT234X-*+3,234,0)                                            
         DC    B'11010011',CL22'K1-BRAND REALLOCATION'                          
         DC    X'0000'                                                          
         DC    AL1(RT234X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'DB0C0C'           PRD CODE (/LEN) AAA,AAA/30,AAA-BBB           
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'5EAF0F0181'     O MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'070713'         O   STA=ALL/XXXX                               
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'DC0465'           REALLOCATE PROD 1                            
         DC    X'DD0572'           REALLOCATE PROD 2                            
         DC    X'DE057F'           REALLOCATE PROD 3                            
         DC    X'DF058C'           REALLOCATE PROD 4                            
         DC    X'D20542'           O PRD CHANGE RPT (Y/N)                       
         DC    X'E00541'           O INCLUDE PB'S   (Y/N)                       
         DC    X'97053F'           O CUTINS= (O,P)                              
         DC    X'CB0551'                   O OPTIONS                            
RT234X   DC    X'00',C'K1'                                                      
*                                                                               
RT235    DC    AL1(RT235X-*+3,235,0)                                            
         DC    B'11010011',CL22'COP-CONFIRM PURCHASE'                           
         DC    X'0000'                                                          
         DC    AL1(RT235X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'CB0551'                   O OPTIONS                            
RT235X   DC    X'00',C'DX'                                                      
*                                                                               
*                                                                               
RT236    DC    AL1(RT236X-*+3,236,0)                                            
         DC    B'11010011',CL22'C1-CABLE LISTING'                               
         DC    X'0000'                                                          
         DC    AL1(RT236X-*+1),B'11000000'                                      
         DC    X'E7043E01ED'                 VERSION =1,2,3                     
         DC    X'0207060181'               O CLI=XXX,ALL                        
         DC    X'5E8F0F0181'     O MKT OR MKTG=ALL,NNNNN,MGR=XNNN               
         DC    X'E9053F'                   O DEACTIVATED ONLY                   
         DC    X'710532'                   O NETWORK                            
         DC    X'CB0551'                   O OPTIONS                            
RT236X   DC    X'00',C'C1'                                                      
*                                                                               
         SPACE 2                                                                
RT237    DC    AL1(RT237X-*+3,237,0)                                            
         DC    B'11010011',CL22'MG-MAKEGOOD ANALYSIS'                           
         DC    X'0000'                                                          
         DC    AL1(RT237X-*+1),B'11000000'                                      
         DC    X'0264060181'                 CLI=XXX/*NN,CGR=                   
         DC    X'5DFE0C0181'                 PRD OR PGRP                        
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'F1053E'                     INCL NO CHRG SPOTS=Y/N             
         DC    X'F2053F0107'                 INCL PRE-EMPTED SPOTS=Y/N          
         DC    X'F60541'                     SUPPRESS COSTS ?                   
         DC    X'F70542'                     SUPPRESS DEMOS ?                   
         DC    X'CB0551'                   O OPTIONS                            
RT237X   DC    X'00',C'MG'                                                      
*                                                                               
         SPACE 2                                                                
RT238    DC    AL1(RT238X-*+3,238,0)                                            
         DC    B'11010011',CL22'INL-INVOICE LISTING'                            
         DC    X'0000'                                                          
         DC    AL1(RT238X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'030F0C0181'               O PRO=ALL/XXX/POL                    
         DC    X'05FF180181'               O EST=NO/NNN/NNN-NNN                 
         DC    X'070413'                     STA=XXXX                           
         DC    X'094426'                     MONTH YYMM AND NO END              
         DC    X'8C053E01C5'                 ADD BUYS=Y,N,T                     
         DC    X'CB0551'                   O OPTIONS                            
RT238X   DC    X'00',C'I5'                                                      
         SPACE 2                                                                
RT239    DC    AL1(RT239X-*+3,239,0)                                            
         DC    B'11010011',CL22'SRY-STAT ROTATION SCHD'                         
         DC    X'0000'                                                          
         DC    AL1(RT239X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'            PRO=XXX,PGRP,XXX-YYY,ALL,POL            
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'              O STA=ALL/XXXX                            
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'20050B'                ID SEQ=N,Y                              
         DC    X'CB0551'                   O OPTIONS                            
RT239X   DC    X'00',C'RY'                                                      
         SPACE                                                                  
RT240    DC    AL1(RT240X-*+3,240,0)                                            
         DC    B'10010011',CL22'NRX-NETWK ROTATN SCHD'                          
         DC    X'0000'                                                          
         DC    AL1(RT240X-*+1),B'10000000'                                      
         DC    X'0208060181'       CLI=XXX(POL)                                 
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO ETC                       
         DC    X'710613'           NETWK=XXXX,ALL                               
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'20050B'           ID SEQ=N,Y                                   
         DC    X'45053E01B3'     O PRINT OPT=Y                                  
         DC    X'CB0551'                   O OPTIONS                            
RT240X   DC    X'00',C'RX'                                                      
         SPACE                                                                  
RT241    DC    AL1(RT241X-*+3,241,0)                                            
         DC    B'11010011',CL22'SPL-SYND PROGRAM LIST'                          
         DC    X'0000'                                                          
         DC    AL1(RT241X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX(POL)                                 
         DC    X'0524180181'       EST=NNN                                      
         DC    X'EB04150181'       PROGRAM NAME (IN REC2 QUSER2)                
         DC    X'CB0551'                   O OPTIONS                            
RT241X   DC    X'00',C'SY'                                                      
         SPACE                                                                  
         SPACE                                                                  
RT242    DC    AL1(RT242X-*+3,242,0)                                            
         DC    B'11000011',CL22'IN-SPOT INVOICE LIST'                           
         DC    X'0000'                                                          
         DC    AL1(RT242X-*+1),B'11000000'                                      
         DC    X'0266060181'       CLI=XXX,ALL                                  
         DC    X'090C26'           O STR,END=YYMMDD,YYMM                        
         DC    X'EC053E01EF'     O EASI OPTION                                  
         DC    X'5F053F01F1'     O DATE OPTION                                  
         DC    X'CB0551'                   O OPTIONS                            
RT242X   DC    X'00',C'I6'                                                      
         SPACE                                                                  
*&&DO                                                                           
RT243    DC    AL1(RT243X-*+3,243,0)                                            
         DC    B'11010011',CL22'IL-INVOICE LETTER'                              
         DC    X'0000'                                                          
         DC    AL1(RT243X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'047E0C0181'     PRO,MODE=ALL,XXX,POL,XXX-YYY,XXX-ALL           
         DC    X'05FE180181'                 EST=NO/NNN/NNN-NNN                 
         DC    X'5EAF0F0181'                 MKT OR MKTGRP                      
         DC    X'070713'                     STA=ALL/XXXX                       
         DC    X'09C826'           STRD,ENDD=YYMM OR YYMMDD AND NO END          
*                                  OR YYMMDD IN BOTH                            
         DC    X'3C053F'                   O ES-LIN ORDER?                      
         DC    X'3D0540'                   O SPACING                            
         DC    X'450541'                   O PRINT OPTION=D/S/BLK/0-9           
         DC    X'0E3D32'                   O BOOK - HUT                         
         DC    X'20050B'                     ID SEQ=N,Y                         
         DC    X'85053A01BF'               O PARTIAL MATCH=*,TLCPDWA            
         DC    X'86054201C1'               O POST AFFIDS=Y,N,P                  
         DC    X'080D1E0181'               O REP=NNN                            
         DC    X'C20544'                   O SUPPRESS COSTS                     
         DC    X'B7053E'                   O FILM REP OPT (Y,N,F,E,X)           
         DC    X'CB0551'                   O OPTIONS                            
RT243X   DC    X'00',C'IL'                                                      
*&&                                                                             
         SPACE                                                                  
RT244    DC    AL1(RT244X-*+3,244,0)                                            
         DC    B'11010011',CL22'MN-              '                              
         DC    X'0000'                                                          
         DC    AL1(RT244X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'04020C0181'                 PROD=ALL                           
         DC    X'05FE180181'                 EST=ALL,XXX                        
         DC    X'06060F0181'                 MKT                                
         DC    X'070613'                     STA=ALL/XXXX                       
         DC    X'099826'                     START+NO END/ES/START+END          
         DC    X'CB0551'                   O OPTIONS                            
RT244X   DC    X'00',C'MN'                                                      
         SPACE                                                                  
RT245    DC    AL1(RT245X-*+3,245,0)                                            
         DC    B'11010011',CL22' BUY/INVOICE TAPE '                             
         DC    X'0000'                                                          
         DC    AL1(RT245X-*+1),B'11000000'                                      
         DC    X'0266060181'                 CLI=XXX                            
         DC    X'04FE0C0181'                 PROD=PRD PR PRDGRP                 
         DC    X'05FE180181'                 EST=ALL,XXX                        
         DC    X'091C26'                     (SAME AS A2)                       
         DC    X'ED053E'                   O INCLUDE AFFADAVIT DATA?            
         DC    X'CB0551'                   O OPTIONS                            
RT245X   DC    X'00',C'YT'                                                      
                                                                                
RT246    DC    AL1(RT246X-*+3,246,0)                                            
         DC    B'11010011',CL22'NETWORK BUY TRANSFER  '                         
         DC    X'0000'                                                          
         DC    AL1(RT246X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'0520180181'            EST=NNN                                 
         DC    X'070413'                STA=XXXX                                
         DC    X'EE04060181'         TO-CLI=XXX                                 
         DC    X'EF20680181'         TO-EST=NNN COL24 OF 2ND REQ CARD           
         DC    X'CB0551'                   O OPTIONS                            
RT246X   DC    X'00',C'C2'                                                      
         SPACE 2                                                                
* RT247 IS WESTERN FUDGED VERSION OF D4                                         
RT247    DC    AL1(RT247X-*+3,247,0)                                            
         DC    B'11010011',CL22'SAL-SALESMAN SHEET'                             
         DC    X'0000'                                                          
         DC    AL1(RT160X-*+1),B'11000000'                                      
         DC    X'0204060181'      CLI=XXX                                       
         DC    X'5DFE0C0181'      PRD OR PGRP                                   
         DC    X'5EAF0F0181'                                                    
         DC    X'070713'          STA=ALL/XXXX                                  
         DC    X'05FE180181'      EST=NNN/NNN-NNN/NO (ALL)                      
         DC    X'091826'          STR,END=YYMMDD/ES                             
         DC    X'0E3D32'        O BOOK-HUT=YYMM/YYMM-XX/ACT/ACT-XX              
         DC    X'550538'        O RERARE TYPE                                   
RT247X   DC    X'00',C'W4'                                                      
         SPACE 2                                                                
RT248    DC    AL1(RT248X-*+3,248,0)                                            
         DC    B'11010011',CL22'MCL-MEDIA CALENDAR'                             
         DC    X'0000'                                                          
         DC    AL1(RT248X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLI=XXX                                      
         DC    X'5DFE0C0181'       PRD OR PGRP                                  
         DC    X'5EAF0F0181'       MARKET OR MKT GROUP                          
         DC    X'070713'           O STA=XXX,ALL                                
         DC    X'05FE180181'       EST=NNN/NNN-NNN/NO                           
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'880540'         O PRINT NET AFF=Y,N                            
         DC    X'BC0542'         O PRINT CHANNEL=Y,N                            
RT248X   DC    X'00',C'WC'                                                      
         SPACE 2                                                                
RT249    DC    AL1(RT249X-*+3,249,0)                                            
         DC    B'11010011',CL22'BRP-BRAND PERFORMANCE'                          
         DC    X'0000'                                                          
         DC    AL1(RT249X-*+1),B'11000000'                                      
         DC    X'0204060181'            CLI=XXX                                 
         DC    X'5DFE0C0181'    PRD OR PRD GRP=ALL,POL,PRD,PGP=                 
         DC    X'5EAE0F0181'            MKT OR MGRP=ALL,NNNN,MGP=               
         DC    X'05FE180181'            EST=NNN/NNN-NNN/NO (ALL)                
         DC    X'091826'                STR,END=YYMMDD/ES                       
         DC    X'0E3D32'              O BOOK-HUT=YYMM/YYMM-XX                   
         DC    X'190439'                DATA COMPARE                            
RT249X   DC    X'00',C'W2'                                                      
         SPACE 2                                                                
RT250    DC    AL1(RT250X-*+3,119,0)                                            
         DC    B'11010011',CL22'BMD-BRAND MEDIA PLAN'                           
         DC    X'0000'                                                          
         DC    AL1(RT250X-*+1),B'11000000'                                      
         DC    X'0204060181'                 CLI=XXX                            
         DC    X'5DFE0C0181'       PRD OR PRDGRP=PRD,ALL,POL,PGP=               
         DC    X'5EAE0F0181'                                                    
         DC    X'05FE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
RT250X   DC    X'00',C'W9'                                                      
*                                                                               
* KL = K4 AND K5 REPORTS                                                        
RT251    DC    AL1(RT251X-*+3,211,0)                                            
         DC    B'11010011',CL22'KL-NEW BRND ALLOC'                              
         DC    X'0000'                                                          
         DC    AL1(RT251X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'E4210C0181'       PGR=VNNN    for testing                      
******   DC    X'03080C0181'       POL                                          
***      FOR POL ONLY PER MEL                                                   
         DC    X'05BE180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX                
*******  DC    X'059E180181'       EST=ALL,NO,NNN,NNN-NNN,N/,XXX                
         DC    X'5EAD0F0181'       MKT OR MKTG=NNNNN,MGR=XALL,XNNN              
         DC    X'070713'           O STA=ALL/XXXX                               
         DC    X'091826'           STR,END=YYMMDD                               
         DC    X'50053E01CF'       REALLOCATE=Y/N,OR 0-9,A-E                    
         DC    X'10053F01D1'       ALLOC BASIS=$/D                              
         DC    X'1B05400199'       ALLOC EXECSS=Y/N                             
         DC    X'93053201D3'       WEIGHT OVERRIDES 0-9,A=10 7 CHARS            
         DC    X'410D3B'           O DAYPART FILTER                             
         DC    X'9D053C'           O SPOT LENGTH FILTER                         
         DC    X'740641'           TEST RUN=Y/N                                 
         DC    X'CB0551'                   O OPTIONS                            
RT251X   DC    X'00',C'KL'                                                      
         SPACE 2                                                                
RT252    DC    AL1(RT252X-*+3,252,0)                                            
         DC    B'11010011',CL22'KA-GOAL REALLOCATION'                           
         DC    X'0000'                                                          
         DC    AL1(RT252X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'04040C0181'       PROD=XXX                                     
***->    DC    X'5DF60C0181'       PRD OR PGRP/NO POL                           
         DC    X'F005650181'       PRD/WEIGHT                                   
         DC    X'F0056B0181'       PRD/WEIGHT                                   
         DC    X'F005710181'       PRD/WEIGHT                                   
         DC    X'F005770181'       PRD/WEIGHT                                   
         DC    X'0504180181'       EST=NNN                                      
         DC    X'5EAF0F0181'       MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'41057D'         O DAYPT FILTER=ALPHA                           
         DC    X'94057E'         O SPOT LENGTH                                  
         DC    X'74063E'           TEST RUN=Y/N                                 
         DC    X'CB0551'                   O OPTIONS                            
RT252X   DC    X'00',C'KA'                                                      
         SPACE 2                                                                
RT253    DC    AL1(RT253X-*+3,253,0)                                            
         DC    B'11010011',CL22'MAR-MARKET PERFORMANCE'                         
         DC    X'0000'                                                          
         DC    AL1(RT253X-*+1),B'11000000'                                      
         DC    X'0208060181'                 CLI=XXX(POL)                       
         DC    X'5EAE0F0181'                 MKT OR MKTGRP                      
         DC    X'5DFC0C0181'                 PRD OR PGRP                        
         DC    X'05BE180181'                 EST=NNN/NNN-NNN/NO (ALL)           
         DC    X'091826'                     STR,END=YYMMDD/ES                  
         DC    X'0E3D32'                   O BOOK-HUT=YYMM/YYMM-XX              
         DC    X'190439'                     DATA COMPARE                       
RT253X   DC    X'00',C'm4'                                                      
         SPACE 2                                                                
*                                                                               
RT254    DC    AL1(RT254X-*+3,254,0)                                            
         DC    B'11010011',CL22'KB-GOAL PERIOD XFER'                            
         DC    X'0000'                                                          
         DC    AL1(RT254X-*+1),B'11000000'                                      
         DC    X'0204060181'       CLT=XXX                                      
         DC    X'04040C0181'       PROD=XXX                                     
         DC    X'0504180181'       EST=NNN                                      
         DC    X'091826'           STR,END=YYMMDD,ES                            
         DC    X'F50387'         O NEW PROD                                     
         DC    X'F30365'         O NEW EST                                      
         DC    X'F4037D'         O NEW PERIOD START DATE                        
         DC    X'5EAF0F0181'     O MKT OR MKTG=ALL,NNNNN,MGR=XALL,XNNN          
         DC    X'410583'         O DAYPT FILTER=ALPHA                           
         DC    X'940584'         O SPOT LENGTH                                  
         DC    X'74063E'           TEST RUN=Y/N                                 
         DC    X'CB0551'                   O OPTIONS                            
RT254X   DC    X'00',C'KB'                                                      
*                                                                               
**RT255    DC    AL1(RT255X-*+3,255,0)                                          
**         DC    B'11010011',CL22'WL-TEST PERIOD XFER'                          
**         DC    X'0000'                                                        
**         DC    AL1(RT255X-*+1),B'11000000'                                    
**         DC    X'0204060181'       CLT=XXX                                    
**         DC    X'04040C0181'       PROD=XXX                                   
**RT255X   DC    X'00',C'WL'                                                    
                                                                                
REQTBLX  DC    X'00'                                                            
         EJECT                                                                  
ACCESSD  DSECT                                                                  
ACCID    DS    CL2                  PROGRAM ID                                  
ACCCDE   DS    CL3                  PROGRAM REQUEST CODE                        
ACCANUM  DS    XL1                  PROGRAM ACTION NUMBER                       
ACCRNUM  DS    XL1                  PROGRAM RECORD NUMBER                       
ACCRSTAT DS    XL1                  PROGRAM RECORD STATUS BYTE                  
ACCSECL  EQU   X'80'                READ FOR SECURITY                           
ACCESSLN EQU   *-ACCID                                                          
         EJECT                                                                  
       ++INCLUDE SPREQSAVE                                                      
       ++INCLUDE SPREQTEMP                                                      
       ++INCLUDE SPREQFFBD                                                      
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'214SPREQ00   02/26/20'                                      
         END                                                                    
