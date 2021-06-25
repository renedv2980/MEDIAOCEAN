*          DATA SET SPSFM5E    AT LEVEL 032 AS OF 01/29/03                      
*PHASE T2175EA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T2175E  -- PGEST MAINTENANCE                        *          
*                                                                     *         
*  INPUTS:       SCREEN SCSFM50 (MAINT) & SCSFM51 (LIST)              *         
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
         TITLE 'T2175E - PGEST MAINTENANCE'                                     
T2175E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**175E**,R7,RR=R3                                              
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
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DELR                                                             
         CLI   MODE,LISTRECS       DISPLAY RECORD                               
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT BASED ON LIST                   
         BNE   XIT                                                              
         LA    R5,HDHOOK                                                        
         ST    R5,HEADHOOK                                                      
         LA    R5,HEDSPECS                                                      
         ST    R5,SPECS                                                         
         B     LR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
         MVI   FILTFLAG,X'00'                                                   
*                                                                               
         LA    R3,KEY1                                                          
         USING PGESTD,R3                                                        
*                                                                               
         XC    PGKEY,PGKEY                                                      
         MVI   PGKRID,PGKNDIRQ       RECORD TYPE                                
         MVI   PGKSID,PGKNDISQ              SUBTYPE                             
*                                                                               
         MVC   PGEMEDE,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    PGEMEDEH+6,X'80'      AND PRODUCT NAME                           
         MVC   PGECLIE,SPACES                                                   
         OI    PGECLIEH+6,X'80'                                                 
         MVC   PGEPRDE,SPACES                                                   
         OI    PGEPRDEH+6,X'80'                                                 
*                                                                               
* MEDIA                                                                         
         LA    R2,PGEMEDSH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   PGEMEDE,MEDNM         MEDIA NAME                                 
         OI    PGEMEDEH+6,X'80'                                                 
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK05                                                             
         CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VK05                                                             
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
*                                                                               
VK05     DS    0H                                                               
         MVC   PGKAM,BAGYMD          COPY MEDIA INTO KEY                        
*                                                                               
* CLIENT                                                                        
         LA    R2,PGECLISH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   PGECLIE,CLTNM         CLIENT NAME                                
         OI    PGECLIEH+6,X'80'                                                 
         MVC   PGKCLT,BCLT          COPY CLIENT INTO KEY                        
*                                                                               
* PRODUCT                                                                       
         LA    R2,PGEPRDSH           PRODUCT                                    
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK08                  IF NOT LIST, PROCEED AS USUAL              
*                                                                               
         CLI   PGEPRDSH+5,0          INPUT IN PRODUCT?                          
         BE    VK12                  NO, SKIP TO ESTIMATE                       
*                                                                               
VK08     DS    0H                                                               
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   PGEPRDE,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    PGEPRDEH+6,X'80'      PRODUCT NAME                               
*                                                                               
VK10     MVC   PGKPRD,QPRD          COPY PRODUCT INTO KEY                       
         OI    FILTFLAG,FPRODQ       IF NO PRODUCT ENTERED                      
         OC    PGKPRD,SPACES                                                    
*                                                                               
* ESTIMATE                                                                      
VK12     DS    0H                                                               
         LA    R2,PGEESTSH           ESTIMATE                                   
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK13                                                             
         CLI   PGEESTSH+5,0          ANY INPUT FOR ESTIMATE?                    
         BE    VKX                                                              
*                                                                               
VK13     DS    0H                                                               
         CLI   PGEESTSH+5,0                                                     
         BE    ERRMIS                                                           
*                                                                               
VK20     DS    0H                                                               
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR1)  ESTIMATE CODE MUST BE NUMERIC              
         TM    PGEESTSH+4,X'08'      AND HAVE A LENGTH <=3                      
         BZ    SPERREX                                                          
*                                                                               
VK30     DS    0H                                                               
         ZIC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         MVC   ERRNUM,=AL2(ESTERR2)  EST MUST BE BETWEEN 0 - 255                
         CHI   RE,0                                                             
         BL    SPERREX                                                          
         CHI   RE,255                                                           
         BH    SPERREX                                                          
         STC   RE,BEST               STORE BINARY ESTIMATE                      
*                                                                               
VK35     MVC   PGKEST,BEST          SAVE ESTIMATE CODE INTO KEY                 
         OI    FILTFLAG,FESTQ                                                   
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PGKEY),PGKEY                                               
         MVC   SVPGEKEY,KEY                                                     
*                                                                               
         CLI   PGKEST,0                                                         
         BNE   XIT                                                              
         OI    PGEBRNH+6,X'40'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTADD        ACTION ADD                                  
         BE    VR10                                                             
*                                                                               
* DELETE ALL X'10' ELEMENTS                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFIL'),('PGSTEIDQ',AIO),0                  
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR10     DS    0H                  FIRST ELEMENT - CHANGE PERIOD                
         LA    R2,PGECHRH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VR20                                                             
*                                                                               
         USING PGSTELMD,R6                                                      
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'01'      INITIALIZE SEQUENCE NUMBER                   
         LA    R2,PGECHRH          CHARGE PERIOD HEADER                         
*                                                                               
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    ERRINV              NO                                           
         CLI   5(R2),L'PGECHR      EXACT LENGTH?                                
         BNE   ERRINV              NO                                           
         MVC   PGSTNAME,=CL8'CHRGPER'                                           
         MVC   PGSTDATA(3),PGECHR                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR20     DS    0H                   ACCOUNT                                     
         LA    R2,PGEACCH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VR30                                                             
*                                                                               
         CLI   5(R2),L'PGEACC                                                   
         BNE   ERRINV                                                           
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'02'      SECOND ELEMENT                               
         MVC   PGSTNAME,=C'ACCOUNT '                                            
         MVC   PGSTDATA(6),PGEACC                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = *                       
* BRAND REQUIRED REGARDLESS OF ESTIMATE                                         
* = = = = = = = = = = = = = = = = = = = = = = = = = = = *                       
VR30     LA    R2,PGEBRNH          BRAND                                        
         GOTO1 ANY                                                              
*                                                                               
         CLI   5(R2),L'PGEBRN                                                   
         BNE   ERRINV                                                           
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'03'      THIRD ELEMENT                                
         MVC   PGSTNAME,=CL8'BRAND'                                             
         MVC   PGSTDATA(4),PGEBRN                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
* ESTIMATE NUMBER                                                               
VR40     DS    0H                                                               
         LA    R2,PGEESTH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VR50                                                             
*                                                                               
         CLI   5(R2),L'PGEEST                                                   
         BNE   ERRINV                                                           
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'04'      FOURTH ELEMENT                               
         MVC   PGSTNAME,=CL8'ESTIMATE'                                          
         MVC   PGSTDATA(4),PGEEST                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR50     DS    0H                  EVENT CODE                                   
         LA    R2,PGEEVTH          EVENT CODE                                   
         BAS   RE,TESTPG0                                                       
         BE    VR60                                                             
*                                                                               
         CLI   5(R2),L'PGEEVT                                                   
         BNE   ERRINV                                                           
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'05'      FIFTH ELEMENT                                
         MVC   PGSTNAME,=CL8'EVENTCD'                                           
         MVC   PGSTDATA(6),PGEEVT                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR60     LA    R2,PGEMBFH          MULTI-BRAND FLAG (Y/N)                       
         BAS   RE,TESTPG0                                                       
         BE    VR70                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'06'      SIXTH ELEMENT                                
         MVC   PGSTNAME,=CL8'MLTBRND'                                           
         MVI   PGSTDATA,C'N'       DEFAULT=N                                    
         CLI   PGEMBFH+5,0                                                      
         BE    VR65                                                             
         CLI   PGEMBF,C'Y'                                                      
         BE    *+12                                                             
         CLI   PGEMBF,C'N'                                                      
         BNE   ERRINV                                                           
         MVC   PGSTDATA(1),PGEMBF                                               
*                                                                               
VR65     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                  NO BRAND FLAG (Y/N)                          
VR70     LA    R2,PGENOBH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VR80                                                             
*                                  NO BRAND FLAG (Y/N)                          
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'07'      7TH ELEMENT                                  
         MVC   PGSTNAME,=CL8'NOBRAND'                                           
         MVI   PGSTDATA,C'Y'       DEFAULT=Y                                    
         CLI   PGENOBH+5,0                                                      
         BE    VR75                                                             
         CLI   PGENOB,C'Y'                                                      
         BE    VR75                                                             
         MVI   PGSTDATA,C'N'                                                    
         CLI   PGENOB,C'N'                                                      
         BE    VR75                                                             
         B     ERRINV                                                           
*                                                                               
VR75     DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                  FISCAL YEAR END DATE                         
VR80     LA    R2,PGEENDH                                                       
         CLI   5(R2),0                                                          
         BE    VR90                FIELD NOT REQUIRED                           
         BAS   RE,TESTPG0                                                       
         BE    VR90                                                             
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'08'      8TH ELEMENT                                  
         MVC   PGSTNAME,=C'FISYREND'                                            
         GOTO1 DATVAL,DMCB,(0,PGEEND),PGSTDATA                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRINV                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VR90     DS    0H                                                               
         LA    R2,PGEXFRH          TRANSFER ESTIMATE NUMBER                     
         CLI   5(R2),0                                                          
         BE    VR100               FIELD NOT REQUIRED                           
         BAS   RE,TESTPG0                                                       
         BE    VR100                                                            
         CLI   PGEXFRH+6,X'80'                                                  
         BE    VR100                                                            
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'09'      9TH ELEMENT                                  
         MVC   PGSTNAME,=C'ACCEST  '                                            
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    ERRINV                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,PGSTDATA         SAVE ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    ERRINV                                                           
         CH    R0,=H'255'                                                       
         BH    ERRINV                                                           
         CLC   PGSTDATA,SVKEY+8    CANNOT REF ITSELF                            
         BE    ERRINV                                                           
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
* TEST ACC PG EST RECORD EXISTS                                                 
*                                                                               
         L     R2,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY,0(R2)                                                        
         MVC   KEY+8(1),PGSTDATA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRNOFND                                                         
*                                                                               
         MVC   KEY,0(R2)                                                        
         GOTO1 HIGH                                                             
*                                                                               
VR100    DS    0H                                                               
         LA    R2,PGESUFH          BRAND SUFFIX                                 
*        BAS   RE,TESTPG0                                                       
*        BE    VRX                                                              
*        CLI   5(R2),0                                                          
*        BE    ERRMIS              FIELD REQUIRED                               
         CLI   5(R2),0                                                          
         BE    VRX                 FIELD REQUIRED                               
*                                                                               
         CLI   5(R2),L'PGESUF      TEST INPUT                                   
         BL    ERRINV              NO, EXIT                                     
*                                                                               
         TM    4(R2),X'04'         ALPHABETIC?                                  
         BNO   ERRINV                                                           
*                                                                               
         BAS   RE,SETUPEL                                                       
         MVI   PGSTESEQ,X'0A'      10TH ELEMENT                                 
         MVC   PGSTNAME,=CL8'BRDSUFF'                                           
         MVC   PGSTDATA(2),PGESUF                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'SPTFIL'),(0,AIO),ELEM                        
         CLI   DMCB+12,0                                                        
         BNZ   ERRINV                                                           
*                                                                               
VRX      DS    0H                                                               
         MVI   ACTELOPT,C'N'                                                    
         B     EQXIT               PASSIVE POINTED HAS TO BE RESET              
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
SETUPEL  DS    0H                                                               
E        USING PGSTELMD,ELEM                                                    
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   E.PGSTEID,PGSTEIDQ                                               
         MVI   E.PGSTELN,PGSTELNQ                                               
         BR    RE                                                               
         DROP  E                                                                
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
* R2 IS EXPECTED TO ADDRESS FIELD HEADER                                        
* IF ESTIMATE = 0 AND FIELD HAS NO INPUT - EXIT WITH EQUAL CONDITION            
* IF ESTIMATE <> 0 AND FIELD HAS INPUT - EXIT WITH UNEQUAL CONDITION            
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *         
TESTPG0  NTR1                                                                   
         L     R3,AIO                                                           
         USING PGESTD,R3                                                        
*                                                                               
         CLI   PGKEST,0                                                         
         BNE   TESTPG02                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
         B     EQXIT                                                            
*                                                                               
TESTPG02 DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         B     NEQXIT                                                           
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                          PUTREC                                     *         
***********************************************************************         
PTREC    NTR1                                                                   
         MVI   USEIO,C'Y'          DON'T LET GENCON DO PUTREC                   
         MVC   KEY,SVPGEKEY        NOW WE HAVE THE CORRECT DISK ADDRESS         
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         B     XIT                                                              
***********************************************************************         
*                          ADD A RECORD                               *         
***********************************************************************         
ADREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       DELETE RECORD                                 *         
***********************************************************************         
DELR     NTR1                                                                   
         LA    R2,PGEESTEH                                                      
         SR    R0,R0                                                            
DELR2    ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROT                                    
         BO    DELR2                                                            
         CLC   8(3,R2),=C'DEL'                                                  
         BE    EQXIT                                                            
*                                                                               
DELR10   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=CL29'* ERROR * INVALID DELETE CODE'                 
         B     MSGERR                                                           
*                                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
         L     R3,AIO                                                           
         USING PGKEY,R3                                                         
*                                                                               
         GOTO1 CLUNPK,DMCB,PGKCLT,PGECLIS                                       
         OI    PGECLISH+6,X'80'                                                 
         MVI   PGECLISH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
         MVC   PGEPRDS,PGKPRD                                                   
         MVI   PGEPRDSH+5,3                                                     
         OI    PGEPRDSH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
* GET FULL PRODUCT NAME                                                         
         LA    R0,PGEPRDE                                                       
         ST    R0,FULL                                                          
         LHI   R0,L'PGEPRDE                                                     
         STC   R0,FULL                                                          
         BAS   RE,GETPNAM                                                       
         OI    PGEPRDEH+6,X'80'      TRANSMIT PRODUCT NAME TO SCREEN            
*                                                                               
         EDIT  PGKEST,PGEESTS,ALIGN=LEFT,ZERO=NOBLANK                           
         OI    PGEESTSH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN           
         OI    PGEESTSH+4,X'08'      NUMERIC CODE                               
         MVI   PGEESTSH+5,3                                                     
*                                                                               
         CLI   THISLSEL,C'C'         SELECT FOR CHANGE                          
         BNE   DKX                                                              
*                                                                               
         CLI   T217FFD+1,C'*'        TEST DDS TERM                              
         BE    DKX                                                              
         TM    T217FFD+12,X'10'                                                 
         BO    ERRSEC2               ON = NO CHANGE                             
*                                                                               
*                                                                               
DKX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0H                                                               
*                                                                               
         L     R6,AIO              A(REC)                                       
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
*                                                                               
         TWAXC PGECHRH                                                          
*                                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         CLI   8(R6),0             CHECK IF ESTIMATE NUMBER = 0                 
         BE    DR120                                                            
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         B     DR06                                                             
*                                                                               
DR05     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
DR06     DS    0H                                                               
         CLI   PGSTESEQ,X'01'                                                   
         BNE   DR20                                                             
         FOUT  PGECHRH,PGSTDATA,3  CHARGE PERIOD                                
         B     DR05                                                             
*                                                                               
DR20     DS    0H                                                               
         CLI   PGSTESEQ,X'02'                                                   
         BNE   DR30                                                             
         FOUT  PGEACCH,PGSTDATA,6  ACCOUNT                                      
         B     DR05                                                             
*                                                                               
DR30     DS    0H                                                               
         CLI   PGSTESEQ,X'03'                                                   
         BNE   DR40                                                             
         FOUT  PGEBRNH,PGSTDATA,4  BRAND                                        
         B     DR05                                                             
*                                                                               
DR40     DS    0H                                                               
         CLI   PGSTESEQ,X'04'                                                   
         BNE   DR50                                                             
         FOUT  PGEESTH,PGSTDATA,4  ESTIMATE #                                   
         B     DR05                                                             
*                                                                               
DR50     DS    0H                                                               
         CLI   PGSTESEQ,X'05'                                                   
         BNE   DR60                                                             
         FOUT  PGEEVTH,PGSTDATA,6  EVENT CODE                                   
         B     DR05                                                             
*                                                                               
DR60     DS    0H                                                               
         CLI   PGSTESEQ,X'06'                                                   
         BNE   DR70                                                             
         FOUT  PGEMBFH,PGSTDATA,1  MULTI-BRAND FLAG                             
         B     DR05                                                             
*                                                                               
DR70     DS    0H                                                               
         CLI   PGSTESEQ,X'07'                                                   
         BNE   DR80                                                             
         FOUT  PGENOBH,PGSTDATA,1  NO BRAND                                     
         B     DR05                                                             
*                                                                               
DR80     DS    0H                                                               
         CLI   PGSTESEQ,X'08'                                                   
         BNE   DR90                                                             
         GOTO1 DATCON,DMCB,PGSTDATA,(8,DUB)                                     
         FOUT  PGEENDH,DUB,8       FISCAL YEAR END                              
         B     DR05                                                             
*                                                                               
DR90     DS    0H                                                               
         CLI   PGSTESEQ,X'09'                                                   
         BNE   DR100                                                            
         ZIC   R0,PGSTDATA                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         FOUT  PGEXFRH,WORK,3      ACC XFR ESTIMATE                             
         B     DR05                                                             
*                                                                               
DR100    DS    0H                                                               
         CLI   PGSTESEQ,X'0A'                                                   
         BNE   DR05                                                             
         FOUT  PGESUFH,PGSTDATA,2  BRAND SUFFIX                                 
         B     DR05                                                             
*                                                                               
*                                                                               
* DISPLAY LIMITED DATA FOR ESTIMATE 0 RECORD            *                       
*                                                                               
DR120    DS    0H                                                               
*                                                                               
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         B     DR130                                                            
*                                                                               
DR125    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
*                                                                               
DR130    DS    0H                                                               
         CLI   PGSTESEQ,X'03'                                                   
         BNE   DR135                                                            
         FOUT  PGEBRNH,PGSTDATA,4                                               
         B     DR125                                                            
*                                                                               
DR135    DS    0H                                                               
         CLI   PGSTESEQ,X'0A'                                                   
         BNE   DR125                                                            
         FOUT  PGESUFH,PGSTDATA,2  BRAND SUFFIX                                 
         B     DR125                                                            
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0H                                                               
*                                                                               
         XC    PGLTTLE,PGLTTLE                                                  
         XC    PGLUND2,PGLUND2                                                  
         LA    R1,PGLTTLE                                                       
         LA    R2,PGLUND2                                                       
*                                                                               
         TM    FILTFLAG,FPRODQ                                                  
         BO    LR02                                                             
*                                                                               
         USING PLINED,R1                                                        
         MVC   PPROD,=CL3'Prd'                                                  
         MVC   PPROD2,=CL10'Prd Desc'                                           
         DROP  R1                                                               
         USING PLINED,R2                                                        
         MVC   PPROD,=3C'-'                                                     
         MVC   PPROD2,=10C'-'                                                   
         DROP  R2                                                               
         LA    R1,PLINEDLQ(R1)                                                  
         LA    R2,PLINEDLQ(R2)                                                  
*                                                                               
LR02     DS    0H                                                               
*                                                                               
         USING LINED,R1                                                         
         MVC   LEST,=CL3'Est'                                                   
         MVC   LESTNAME,=CL12'Est Name'                                         
         MVC   LCHGPER,=CL3'Crg'                                                
         MVC   LACC,=CL6'Acct'                                                  
         MVC   LPGBRAND,=CL4'Brnd'                                              
         MVC   LPGEST,=CL6'PG Est'                                              
         MVC   LEVENTCD,=CL6'Event'                                             
         DROP  R1                                                               
*                                                                               
         USING LINED,R2                                                         
         MVC   LEST,=3C'-'                                                      
         MVC   LESTNAME,=12C'-'                                                 
         MVC   LCHGPER,=3C'-'                                                   
         MVC   LACC,=6C'-'                                                      
         MVC   LPGBRAND,=4C'-'                                                  
         MVC   LPGEST,=6C'-'                                                    
         MVC   LEVENTCD,=6C'-'                                                  
         DROP  R2                                                               
*                                                                               
         OI    PGLTTLEH+6,X'80'                                                 
         OI    PGLUND2H+6,X'80'                                                 
*                                                                               
LR08     DS    0H                                                               
*                                                                               
         MVI   NLISTS,12                                                        
         OC    KEY,KEY                                                          
         BNZ   LR09                                                             
         MVC   KEY,SVPGEKEY                                                     
*                                                                               
LR09     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
LR10     DS    0H                                                               
         GOTO1 SEQ                                                              
LR20     DS    0H                                                               
*                                                                               
* FILTERING TAKES PLACE HERE                                                    
         LA    R1,KEY                                                           
         USING PGESTD,R1                                                        
*                                                                               
         CLC   KEY(5),SVPGEKEY       SAME A/M, CLIENT?                          
         BNE   LRXIT                                                            
*                                                                               
         TM    FILTFLAG,FPRODQ       FILTERING BY PRODUCT?                      
         BNO   LR30                  IF NOT, SKIP TO ESTIMATE FILTER            
         CLC   PGKPRD,SVPGEKEY+5     SAME PRODUCT?                              
         BNE   LR10                  IF NOT, GET NEXT RECORD                    
LR30     DS    0H                                                               
         TM    FILTFLAG,FESTQ        FILTERING BY ESTIMATE?                     
         BNO   LR40                  IF NOT, DISPLAY IT                         
*                                                                               
         CLC   PGKEST,SVPGEKEY+8     SAME ESTIMATE?                             
         BNE   LR10                  IF NOT, GET NEXT RECORD                    
         DROP  R1                                                               
*                                                                               
LR40     DS    0H                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
* = = = = = = = = = *                                                           
* DATA DISPLAY PART *                                                           
* = = = = = = = = = *                                                           
         L     R6,AIO                                                           
         USING PGESTD,R6                                                        
*        LA    R3,LISTAR                                                        
*        XC    LISTAR,LISTAR                                                    
         LA    R3,P                                                             
         XC    P,P                                                              
*                                                                               
         TM    FILTFLAG,FPRODQ           PRODUCT FILTER?                        
         BO    LR50             YES, NO NEED TO DISPLAY PRODUCT                 
         USING PLINED,R3                                                        
         MVC   PPROD,PGKPRD                                                     
* GET FULL PRODUCT NAME                                                         
         LA    R0,PPROD2                                                        
         ST    R0,FULL                                                          
         LHI   R0,L'PPROD2                                                      
         STC   R0,FULL                                                          
         BAS   RE,GETPNAM                                                       
*                                                                               
         DROP  R3                                                               
         LA    R3,PLINEDLQ(R3)                                                  
*                                                                               
         USING LINED,R3                                                         
*                                                                               
LR50     DS    0H                                                               
         EDIT  PGKEST,LEST,ZERO=NOBLANK                                         
* GET FULL ESTIMATE NAME                                                        
         LA    R0,LESTNAME                                                      
         ST    R0,FULL                                                          
         LHI   R0,L'LESTNAME                                                    
         STC   R0,FULL                                                          
         BAS   RE,GETENAM                                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING PGSTELMD,R6                                                      
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
*                                                                               
         CLI   8(R6),0             CHECK IF ESTIMATE NUMBER = 0                 
         BE    LR120                                                            
*                                                                               
* ELEMENTS WITH SEQUENCE NUMBERS 1-7 ARE REQUIRED.  OTHERS ARE OPTIONAL         
*                                                                               
         BAS   RE,GETEL            ELEM SEQUENCE # = 1                          
         BNE   LR130                                                            
         MVC   LCHGPER(3),PGSTDATA    CHARGE PERIOD                             
*                                                                               
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 2                          
         BNE   LR130                                                            
         MVC   LACC(6),PGSTDATA    ACCOUNT                                      
*                                                                               
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 3                          
         BNE   LR130                                                            
         MVC   LPGBRAND(4),PGSTDATA  BRAND                                      
*                                                                               
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 4                          
         BNE   LR130                                                            
         MVC   LPGEST(4),PGSTDATA  ESTIMATE #                                   
*                                                                               
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 5                          
         BNE   LR130                                                            
         MVC   LEVENTCD(6),PGSTDATA  EVENT CODE                                 
*                                                                               
*** SEPARATION BETWEEN LIST AND REPORT HERE                                     
         CLI   MODE,PRINTREP       VALIDATE RECORD KEY                          
         BNE   LR130                                                            
***                                                                             
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 6                          
         BNE   LR130                                                            
         MVC   LMBRAND(1),PGSTDATA                                              
*                                                                               
         BAS   RE,NEXTEL           ELEM SEQUENCE # = 7                          
         BNE   LR130                                                            
         MVC   LNOBRAND(1),PGSTDATA                                             
*                                                                               
LR60     DS    0H                  OTHER ELEMENTS ARE OPTIONAL                  
         BAS   RE,NEXTEL                                                        
         BNE   LR130                                                            
*                                                                               
         CLI   PGSTESEQ,X'08'                                                   
         BNE   LR90                                                             
         GOTO1 DATCON,DMCB,PGSTDATA,(8,DUB)                                     
         MVC   LSRS(8),DUB         FISCAL YEAR END                              
         B     LR60                                                             
*                                                                               
LR90     DS    0H                                                               
         CLI   PGSTESEQ,X'09'                                                   
         BNE   LR100                                                            
         ZIC   R0,PGSTDATA                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         MVC   LORIGEST(3),WORK        ACC XFR ESTIMATE                         
         B     LR60                                                             
*                                                                               
LR100    DS    0H                                                               
         CLI   PGSTESEQ,X'0A'                                                   
         BNE   LR60                                                             
         MVC   LBRDSUF(2),PGSTDATA    BRAND SUFFIX                              
         B     LR60                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
LR120    DS    0H         DISPLAY LIMITED DATA FOR ESTIMATE 0 RECORD            
*                                                                               
         L     R6,AIO                                                           
         USING PGSTELMD,R6                                                      
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
         BAS   RE,GETEL            ONLY ELEMENT IS PGBRAND                      
         BNE   ERRMIS                                                           
         MVC   LPGBRAND(3),PGSTDATA                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
LR130    DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR132                                                            
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR10                                                             
LR132    DS    0H                                                               
         MVC   LISTAR,P                                                         
         GOTO1 LISTMON                                                          
         B     LR10                                                             
*                                                                               
LRXIT    DS    0H                                                               
         B     EQXIT                                                            
         DROP  R3                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
* FULL IS EXPECTED TO ADDRESS PRODUCT NAME OUTPUT AREA WITH                     
* FIRST BYTE CONTAINING THE LENGTH                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
GETPNAM  NTR1                                                                   
*                                                                               
         MVC   SAVEA(4),FULL                                                    
         MVC   KEY1,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),KEY1+2  COPY A/M,CLT,PRD FROM PGEST TO PROD KEY         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPN10                                                          
*                                                                               
         SR    R3,R3                                                            
         IC    R3,SAVEA                                                         
         BCTR  R3,0                                                             
         L     R2,SAVEA                                                         
         L     R5,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDR,R1                                                        
         EXMVC R3,0(R2),PNAME                                                   
         ST    R5,AIO                                                           
         DROP  R1                                                               
*                                                                               
GETPN10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     EQXIT                                                            
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
* FULL IS EXPECTED TO ADDRESS ESTIMATE NAME OUTPUT AREA WITH                    
* FIRST BYTE CONTAINING THE LENGTH                                              
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *               
GETENAM  NTR1                                                                   
*                                                                               
         MVC   SAVEA(4),FULL                                                    
         MVC   KEY1,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(7),KEY1+2  COPY A/M-EST, FROM PGEST TO EST KEY             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETEN10                                                          
*                                                                               
         SR    R3,R3                                                            
         IC    R3,SAVEA                                                         
         BCTR  R3,0                                                             
         L     R2,SAVEA                                                         
         L     R5,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING ESTHDR,R1                                                        
         EXMVC R3,0(R2),EDESC                                                   
         ST    R5,AIO                                                           
         DROP  R1                                                               
*                                                                               
GETEN10  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     EQXIT                                                            
*                                                                               
***********************************************************************         
*        HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,30,C'SPOT PG ESTIMATES'                                       
*                                                                               
         SSPEC H2,1,C'MEDIA:'                                                   
         SSPEC H2,30,C'---- -- ---------'                                       
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H3,1,C'CLIENT:'                                                  
         SSPEC H3,59,REPORT                                                     
*                                                                               
         SSPEC H4,59,RUN                                                        
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
HDHOOK   NTR1                                                                   
         L     R3,AIO                                                           
         USING PGKEY,R3                                                         
*                                                                               
         MVC   BYTE,PGKAM            ISOLATE MEDIA CODE                         
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
HD10     CLC   BYTE,1(R5)                                                       
         BE    HD20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   HD10                                                             
HD20     MVC   H2+10(1),0(R5)         COPY MEDIA CODE TO REPORT                 
*                                                                               
         GOTO1 CLUNPK,DMCB,PGKCLT,H3+10                                         
*                                                                               
         TM    FILTFLAG,FPRODQ           PRODUCT FILTER?                        
         BNO   HD25                                                             
         MVC   H4(10),=CL10'Product:'                                           
         MVC   H4+10,PGKPRD                                                     
*                                                                               
HD25     DS    0H                                                               
* FETCH AGENCY NAME AND ADDRESS FROM AGENCY RECORD                              
         MVC   KEY1,KEY                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY RECORD TYPE                           
         MVC   KEY+1(2),PGKALAG                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   HD30                                                             
*                                                                               
         L     R5,AIO                                                           
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING AGYHDR,R1                                                        
         MVC   H1+58(33),AGYNAME                                                
         MVC   H2+58(33),AGYADDR                                                
         DROP  R1                                                               
*                                                                               
HD30     DS    0H                                                               
         ST    R5,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEY1                                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
* PRINT HEADLINES HERE                                                          
*                                                                               
         LA    R1,H6                                                            
         LA    R2,H7                                                            
         LA    R3,H8                                                            
*                                                                               
         TM    FILTFLAG,FPRODQ                                                  
         BO    HD40                                                             
*                                                                               
         USING PLINED,R2                                                        
         MVC   PPROD,=CL3'Prd'                                                  
         MVC   PPROD2,=CL10'Prd Desc'                                           
         DROP  R2                                                               
         USING PLINED,R3                                                        
         MVC   PPROD,=3C'-'                                                     
         MVC   PPROD2,=10C'-'                                                   
         DROP  R3                                                               
*                                                                               
         LA    R1,PLINEDLQ(R1)                                                  
         LA    R2,PLINEDLQ(R2)                                                  
         LA    R3,PLINEDLQ(R3)                                                  
*                                                                               
HD40     DS    0H                                                               
*                                                                               
         USING LINED,R1                                                         
         MVC   LMBRAND,=CL4'Mult'                                               
         MVC   LNOBRAND,=CL4'No'                                                
         MVC   LSRS,=CL8'Sht Rate'                                              
         MVC   LORIGEST,=CL3'Org'                                               
         MVC   LBRDSUF,=CL4'Brnd'                                               
         DROP  R1                                                               
*                                                                               
         USING LINED,R2                                                         
         MVC   LEST,=CL3'Est'                                                   
         MVC   LESTNAME,=CL12'Est Name'                                         
         MVC   LCHGPER,=CL3'Crg'                                                
         MVC   LACC,=CL6'Acct'                                                  
         MVC   LPGBRAND,=CL4'Brnd'                                              
         MVC   LPGEST,=CL6'PG Est'                                              
         MVC   LEVENTCD,=CL6'Event'                                             
         MVC   LMBRAND,=CL4'Brnd'                                               
         MVC   LNOBRAND,=CL4'Brnd'                                              
         MVC   LSRS,=CL8'Start'                                                 
         MVC   LORIGEST,=CL3'Est'                                               
         MVC   LBRDSUF,=CL4'Suff'                                               
         DROP  R2                                                               
*                                                                               
         USING LINED,R3                                                         
         MVC   LEST,=3C'-'                                                      
         MVC   LESTNAME,=12C'-'                                                 
         MVC   LCHGPER,=3C'-'                                                   
         MVC   LACC,=6C'-'                                                      
         MVC   LPGBRAND,=4C'-'                                                  
         MVC   LPGEST,=6C'-'                                                    
         MVC   LEVENTCD,=6C'-'                                                  
         MVC   LMBRAND,=4C'-'                                                   
         MVC   LNOBRAND,=4C'-'                                                  
         MVC   LSRS,=8C'-'                                                      
         MVC   LORIGEST,=3C'-'                                                  
         MVC   LBRDSUF,=4C'-'                                                   
*                                                                               
         J     EQXIT                                                            
         DROP  R3                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRNOFND MVC   ERRNUM,=AL2(RECNOFND)                                            
         B     SPERREX                                                          
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
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
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
CLTINGRP EQU   387                 CLIENT STILL EXIST IN GROUP                  
PRDEXIST EQU   526                 PRODUCT STILL EXIST FOR THIS CLIENT          
NODELADD EQU   525                 CAN'T DELETE ON ADD                          
NOCOORD  EQU   1075                AGENCY DOES NOT AUTHORIZE COORD.             
NOTCLPD  EQU   517                 NO TRAFFIC CLT/SEQ & PRD ALLOWED             
INVTCLT  EQU   518                 INVALID TRAFFIC CLT CODE                     
INVTCLTQ EQU   519                 INVALID TRAFFIC CLT SEQ NUMBER               
CHATCLT  EQU   520                 CAN'T CHANGE TRAF CLT/SEQ                    
CHATPRD  EQU   543                 CAN'T CHANGE TRAF PROD CODE                  
INVOFC   EQU   544                 INVALID OFFICE CODE #                        
OFCONE   EQU   1068                ONE CHAR OFFICE CODE REQUIRED                
OFCTWO   EQU   1069                TWO CHAR OFFICE CODE REQUIRED                
INVAAGY  EQU   545                 INVALID ACCOUNT AGENCY CODE                  
SYSNOPEN EQU   1071                ACC SYSTEM NOT OPEN                          
SYSWS    EQU   1072                CAN'T SWITCH TO ACC SYSTEM                   
NOACCCMP EQU   1073                UNABLE TO READ ACC COMPANY REC               
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1 - 255                
RECNOFND EQU   53                  RECORD NOT FOUND                             
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS                                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
LINED    DSECT                                                                  
LEST     DS    CL3                                                              
         DS    CL1                                                              
LESTNAME DS    CL12                                                             
         DS    CL1                                                              
LCHGPER  DS    CL3                                                              
         DS    CL1                                                              
LACC     DS    CL6                                                              
         DS    CL1                                                              
LPGBRAND DS    CL4                                                              
         DS    CL1                                                              
LPGEST   DS    CL6                                                              
         DS    CL1                                                              
LEVENTCD DS    CL6                                                              
         DS    CL1                                                              
LMBRAND  DS    CL4                                                              
         DS    CL1                                                              
LNOBRAND DS    CL4                                                              
         DS    CL1                                                              
LSRS     DS    CL8                                                              
         DS    CL1                                                              
LORIGEST DS    CL3                                                              
         DS    CL1                                                              
LBRDSUF  DS    CL4                                                              
LINEDLQ  EQU   *-LINED                                                          
*                                                                               
PLINED   DSECT                                                                  
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PPROD2   DS    CL10                                                             
         DS    CL2                                                              
PLINEDLQ EQU   *-PLINED                                                         
*                                                                               
SCANTABD DSECT                                                                  
SCANTF1L DS    CL1                 1ST FIELD LENGTH                             
SCANTF2L DS    CL1                 2ND FIELD LENGTH                             
SCANTF1V DS    CL1                 1ST FIELD BITWISE VALIDATION INFO            
SCANTF2V DS    CL1                 2ND FIELD BITWISE VALIDATION INFO            
SCANTF1B DS    CL4                 1ST FIELD BINARY VALUE                       
SCANTF2B DS    CL4                 2ND FIELD BINARY VALUE                       
SCANTF1  DS    CL10                1ST FIELD INPUT STRING                       
SCANTF2  DS    CL10                2ND FIELD INPUT STRING                       
SCANTABQ EQU   *-SCANTABD                                                       
*                                                                               
PRDD     DSECT                                                                  
PRDPRD   DS    0CL4                                                             
PRDAC    DS    CL3                 ALPHA PRODUCT CODE                           
PRDBSN   DS    XL1                 BINARY PRODUCT SEQUENCE NUMBER               
PRDQ     EQU   *-PRDD                                                           
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
SAVEA    DS    A                   ADDRESS LOCATION                             
*                                                                               
HALF2    DS    H                                                                
QXIT     DS    X                   QUICK EXIT FLAG TO SKIP VALREC               
*                                                                               
FLAG1    DS    XL1                 AGYFLAG1                                     
ACCOFC   DS    XL1                 AGYOFC2                                      
ACCAGY   DS    XL24                AGYACCAG                                     
*                                                                               
SVCOFFC  DS    C                   BACKUP OFFICE NUMBER                         
SVPGEKEY DS    CL48                BACK UP KEY                                  
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
PSTOUT   DS    CL64                PSTVAL OUTPUT FIELD                          
APSTVAL  DS    A                   A(PSTVAL)                                    
*                                                                               
SCANTAB  DS    CL32                SCANNER INPUT TABLE                          
*                                                                               
KEY1     DS    CL48                                                             
KEY2     DS    CL48                                                             
*                                                                               
SYSSW    DS    XL1                 SE NUM FOR SWITCH                            
POWCODE  DS    CL2                 ACCOUNT AGENCY OVERRIDE                      
ACCOFF   DS    CL2                 ACCOUNT OFFICE CODE                          
OFFLEN   DS    XL1                 ACCOUNT OFFICE LENGTH                        
COMPCD   DS    XL1                 AGENCY BINARY CODE                           
GTFACTB  DS    CL88                                                             
MYACCKEY DS    CL42                ACCOUNT REC KEY                              
CTKEY    DS    CL28                                                             
SVDATE   DS    XL3                                                              
*                                                                               
FILTFLAG DS    X                                                                
FPRODQ   EQU   X'01'                                                            
FESTQ    EQU   X'02'                                                            
*                                                                               
MYCLTCOD DS    CL3                 TMP CLT CODE(2) & PROD SEQ#(1)               
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM50D          MAINTENACE SCREEN                            
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM51D          LIST SCREEN                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
       ++INCLUDE CTGENFILE         FOR CTSYSD IN VALACC                         
*PREFIX=AC$                                                                     
       ++INCLUDE ACGENFILE         FOR CPYELD & OFFRECD IN VALACC               
*PREFIX=                                                                        
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDPSTBLK          FOR PSTVAL                                   
       ++INCLUDE DDCOREQUS         FOR PSTVAL                                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENGRP          FOR DELETING CLIENTS                         
       ++INCLUDE SPGENNDEF         FOR DELETING CLIENTS                         
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPSFM5E   01/29/03'                                      
         END                                                                    
