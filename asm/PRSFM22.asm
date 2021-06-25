*          DATA SET PRSFM22    AT LEVEL 013 AS OF 11/06/18                      
*PHASE T41C22A                                                                  
*        TITLE 'T41C22  CUSTOM COLUMN RECORDS'                                  
*                                                                               
         TITLE 'T41C22  CUSTOM COLUMN RECORDS'                                  
**** CHANGE LOG                                                                 
*                                                                               
* KWAN JUL14/14 SUPPORT MEDIA B(MOBILE), V(NVIDEO), W (LVIDEO)                  
*                                                                               
* KWAN MAR06/14 SUPPORT MEDIA L (SOCIAL)                                        
*                                                                               
* BOBY   04/11  ADD TRACKING CHANGES FOR IDESK OPTION                           
*                                                                               
* BOBY   09/06  ADD STANDARD CUSTOM COLUMNS                                     
*                                                                               
* SMYE   01/04  ADD THREE FIELDS TO RECORD AND SCREEN                           
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T41C22 - CUSTOM COLUMN RECORDS MAINT/LIST/REPORT      *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT           *         
*                                                                     *         
*  INPUTS       SCREEN T41CDA (MAINTENANCE)                           *         
*               SCREEN T41CCA (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED CUSTOM COLUMN RECORDS                         *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - GLOBAL LITERAL                                        *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T41C22  CUSTOM COLUMN RECORDS'                                  
T41C22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C22                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LARL  R7,GLOBALS                                                       
         USING GLOBALS,R7          R7=A(GLOBAL LITERALS)                        
*                                                                               
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   INIT10                                                           
*                                                                               
         CLI   1(RA),C'*'          MUST BE A DDS TERMINAL                       
         BNE   INITERR                                                          
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0                                        
         CLI   DMCB+4,0                                                         
         B     *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH                       
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        UPDATE PASSIVE RECORD                        
         BE    XR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         B     TRAPERR                                                          
*                                                                               
INITERR  DS    0H                                                               
         LA    R2,CONACTH           SECURITY LOCKOUT                            
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
*                                                                               
NOCHG    EQU   79                  FIELD CANNOT BE CHANGED                      
NOTOT    EQU   84                  DATA TYPE CANNOT BE TOTALLED                 
INVLEN   EQU   121                 LENGTH INVALID FOR DATA TYPE                 
NOSHRT   EQU   122                 LENGTH CANNOT BE DECREASED                   
NODEC    EQU   168                 DATA TYPE CANNOT HAVE DECIMALS               
STDID    EQU   602                 STANDARD CODE STARTS WITH !                  
CCLID    EQU   603                 CUSTOM   CODE CAN'T START WITH !             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'T41C22  CUSTOM COLUMN RECORDS - VALKEY'                         
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
*                                                                               
*        MEDIA IS ONLY "A" FOR ALL MEDIA                                        
*                                                                               
         XC    SVCCLKEY,SVCCLKEY        CLEAR                                   
         LA    R4,SVCCLKEY                                                      
*                                                                               
         CLI   RECNUM,RSTDCOLQ     SKIP IF STANDARD COLUMNS                     
         BE    VKSTD                                                            
*                                                                               
         USING PCOLKEY,R4                                                       
*                                                                               
         MVC   PCOLKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVI   PCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   PCOLKRCD,X'61'                  ID                               
*                                                                               
         XC    SVCODE,SVCODE                                                    
         LA    R2,COLCODEH         COLUMN CODE FIELD ON SCREEN                  
*                                                                               
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK10                YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VK90                                                             
         CLI   ACTNUM,ACTREP       OK IF REPORT                                 
         BE    VK90                                                             
*                                                                               
         B     VKMISS              MISSING                                      
*                                                                               
VK10     DS    0H                                                               
*                                                                               
         MVC   PCOLKCOD,COLCODE                                                 
         OC    PCOLKCOD,SPACES                                                  
*                                                                               
         CLI   PCOLKCOD,C'!'       CAN NOT START WITH SHRIEK                    
         BE    VKCCLNV                                                          
*                                                                               
         MVC   SVCODE,PCOLKCOD                                                  
*                                                                               
         B     VK90X                                                            
*                                                                               
VK90     DS    0H                                                               
*                                                                               
*        VALIDATE FCON FILTER                                                   
*                                                                               
         MVI   FCONFLT,0           CLEAR FCON FILTER                            
         LA    R2,SELFCONH         POINT TO FCON FILTER IN HEADER               
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BE    VKFCONX             NO                                           
         TM    4(R2),X'08'         VALID NUMERIC ?                              
         BNO   VRNOTNUM            NO                                           
         CLI   5(R2),1             ONE-CHAR LENGTH ?                            
         BH    VRINV               NO                                           
*                                                                               
         CLI   8(R2),C'0'          BETWEEN 1 AND 8 ?                            
         BNH   VRINV                                                            
         CLI   8(R2),C'9'                                                       
         BNL   VRINV                                                            
*                                                                               
         ZIC   R1,5(R2)            LENGTH INPUT                                 
         BCTR  R1,0                PREP FOR EXECUTED PACK                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,FCONFLT          OUTPUT TO FCON FILTER                        
*                                                                               
VKFCONX  DS    0H                                                               
*                                                                               
*        VALIDATE TRACKING FILTER                                               
*                                                                               
VKTRK    DS    0H                                                               
         MVI   TRKFILT,0           CLEAR TRK  FILTER                            
*                                                                               
         LA    R2,SELTRKH          POINT TO TRK FILTER IN HEADER                
*                                                                               
         CLI   5(R2),1             ANYTHING INPUT ?                             
         BL    VKTRKX              NO                                           
         BH    VRINV               TOO MUCH                                     
*                                                                               
         CLI   8(R2),C'Y'          IF C'Y' OR C'N' - OKAY                       
         BE    *+8                                                              
         CLI   8(R2),C'N'          IF C'Y' OR C'N' - OKAY                       
         BNE   VRINV                                                            
*                                                                               
         MVC   TRKFILT,8(R2)          SAVE FILTER                               
*                                                                               
VKTRKX   DS    0H                                                               
*                                                                               
VK90X    DS    0H                                                               
*                                                                               
         MVC   KEY,SVCCLKEY        SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKSTD    DS    0H                  STANDARD COLUMN - STORED ON GENFILE          
*                                                                               
         USING GCOLKEY,R4                                                       
*                                                                               
         MVC   GCOLKRID,=AL3(GCOLKRIQ)                                          
         MVI   GCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   GCOLKRCD,X'61'                  ID                               
*                                                                               
         XC    SVCODE,SVCODE                                                    
         LA    R2,COLCODEH         COLUMN CODE FIELD ON SCREEN                  
*                                                                               
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VKSTD10             YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    VKSTD90                                                          
         CLI   ACTNUM,ACTREP       OK IF REPORT                                 
         BE    VKSTD90                                                          
*                                                                               
         B     VKMISS              MISSING                                      
*                                                                               
VKSTD10  DS    0H                                                               
*                                                                               
         MVC   GCOLKCOD,COLCODE                                                 
         OC    GCOLKCOD,SPACES                                                  
*                                                                               
         CLI   GCOLKCOD,C'!'       MUST START WITH SHRIEK                       
         BNE   VKSTDNV                                                          
*                                                                               
         MVC   SVCODE,GCOLKCOD                                                  
*                                                                               
         B     VKSTD90X                                                         
*                                                                               
VKSTD90  DS    0H                                                               
*                                                                               
         MVI   FCONFLT,0           CLEAR FCON FILTER                            
         LA    R2,SELFCONH         POINT TO FCON FILTER IN HEADER               
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BE    VKSTD90X            NO                                           
         TM    4(R2),X'08'         VALID NUMERIC ?                              
         BNO   VRNOTNUM            NO                                           
         CLI   5(R2),1             ONE-CHAR LENGTH ?                            
         BH    VRINV               NO                                           
*                                                                               
         CLI   8(R2),C'0'          BETWEEN 1 AND 8 ?                            
         BNH   VRINV                                                            
         CLI   8(R2),C'9'                                                       
         BNL   VRINV                                                            
*                                                                               
         ZIC   R1,5(R2)            LENGTH INPUT                                 
         BCTR  R1,0                PREP FOR EXECUTED PACK                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,FCONFLT          OUTPUT TO FCON FILTER                        
*                                                                               
VKSTD90X DS    0H                                                               
*                                                                               
         MVC   KEY,SVCCLKEY        SET KEY                                      
*                                                                               
VKSTD900 B     EXIT                                                             
*                                                                               
VKSTDNV  DS    0H               STANDARD COLUMN CODE MUST START WITH !          
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'STDMSG),STDMSG                                         
         OI    CONHEADH+6,X'80'                                                 
         GOTOR ERREX2                                                           
*                                                                               
VKCCLNV  DS    0H                  CUSTOM CODE CAN'T START WITH !               
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CCLMSG),CCLMSG                                         
         OI    CONHEADH+6,X'80'                                                 
         GOTOR ERREX2                                                           
*                                                                               
VKMISS   LA    RF,MISSING                                                       
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
*                                                                               
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
*                                                                               
         B     TRAPERR                                                          
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
*                                                                               
*        WORKS FOR STANDARD AND CUSTOM COLUMNS                                  
*                                                                               
         L     R4,AIO                                                           
*                                                                               
         LA    R3,PCOLKCOD-PCOLKEY(R4) POINT TO CCL CODE                        
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+8                                                              
         LA    R3,GCOLKCOD-GCOLKEY(R4)    POINT TO CCL CODE                     
*                                                                               
         FOUT  COLCODEH,0(R3),12                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
*                                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMS                    
*                                                                               
         MVC   SVCCLKEY,KEY        SAVE THE RECORD KEY                          
*                                                                               
         L     R4,AIO                                                           
         USING PCOLRECD,R4                                                      
*                                                                               
         CLI   ACTNUM,ACTADD      SEE IF ADDING                                 
         BNE   VR05                                                             
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   VR03                                                             
*                                                                               
         USING GCOLKEY,R4          ESTABLISH AS STANDARD COLUMN REC             
*                                                                               
         XC    GCOLRECD(100),GCOLRECD   INIT RECORD BUILD ARE                   
         MVC   GCOLKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,GCOLFRST-GCOLKEY MINIMUM LENGTH OF RECORD                     
         LA    RF,L'PCOLELEM(RF)   ADD ON CODE ELEM LENGTH                      
*                                                                               
         STCM  RF,3,GCOLFLEN       SET RECORD LENGTH                            
*                                                                               
         LA    R6,GCOLFRST         POINT TO FIRST ELEMENT                       
*                                                                               
         B     VR04                                                             
*                                                                               
VR03     DS    0H                                                               
*                                                                               
         USING PCOLKEY,R4          ESTABLISH AS CUSTOM COLUMN REC               
*                                                                               
         XC    PCOLRECD(100),PCOLRECD   INIT RECORD BUILD ARE                   
         MVC   PCOLKEY,KEY         SET KEY                                      
*                                                                               
         LA    RF,PCOLFRST-PCOLKEY MINIMUM LENGTH OF RECORD                     
         LA    RF,L'PCOLELEM(RF)   ADD ON CODE ELEM LENGTH                      
*                                                                               
         STCM  RF,3,PCOLLEN        SET RECORD LENGTH                            
*                                                                               
         LA    R6,PCOLFRST         POINT TO FIRST ELEMENT                       
*                                                                               
VR04     DS    0H                                                               
*                                                                               
         USING PCOLELEM,R6         ESTABLISH COLUMN DESCRIPTION ELM             
*                                                                               
         MVC   PCOLELEM(2),=X'6143'  SET ELM ID AND LENGTH                      
*                                                                               
VR05     DS    0H                                                               
*                                                                               
         LA    R6,PCOLFRST-PCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+8                                                              
         LA    R6,GCOLFRST-GCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         USING PCOLELEM,R6         ESTABLISH COLUMN DESCRIPTION ELM             
*                                                                               
         MVC   PCOLDESC,SPACES                                                  
         LA    R2,COLDESCH                                                      
*                                                                               
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VKMISS                                                           
*                                                                               
         MVC   PCOLDESC,COLDESC                                                 
         OC    PCOLDESC,SPACES                                                  
*                                                                               
         MVC   PCOLHDR1,SPACES     FLD=HEADER 1                                 
         LA    R2,COLHDR1H                                                      
*                                                                               
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VKMISS                                                           
*                                                                               
         MVC   PCOLHDR1,COLHDR1                                                 
         OC    PCOLHDR1,SPACES                                                  
*                                                                               
         MVC   PCOLHDR2,SPACES     FLD=HEADER 2                                 
*                                                                               
         LA    R2,COLHDR2H                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    *+10                OPTIONAL                                     
         MVC   PCOLHDR2,COLHDR2                                                 
*                                                                               
         OC    PCOLHDR2,SPACES                                                  
*                                                                               
         LA    R2,COLTYPH          FLD=DATA TYPE                                
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VKMISS                                                           
         CLI   COLTYP,C'T'                                                      
         BE    VR20                VALID "TYPE" (TEXT)                          
         CLI   COLTYP,C'N'                                                      
         BE    VR20                VALID "TYPE" (NUMERIC)                       
         CLI   COLTYP,C'$'                                                      
         BE    VR20                VALID "TYPE" (DOLLARS)                       
         CLI   COLTYP,C'D'                                                      
         BE    VR20                VALID "TYPE" (DATE)                          
         CLI   COLTYP,C'%'                                                      
         BE    VR20                VALID "TYPE" (PERCENT)                       
         CLI   COLTYP,C'P'                                                      
         BE    VR20                VALID "TYPE" (BINARY)                        
*                                                                               
         B     VRINV               VALID "TYPE" NOT FOUND                       
*                                                                               
VR20     DS    0H                                                               
         TM    PCOLSTAT,X'80'      HAS RECORD BEEN USED IN BUY ?                
         BNO   VR20M               NO                                           
         CLC   PCOLTYP,COLTYP                                                   
         BNE   VRNOCHG             DATA ID ("TYPE") CANNOT BE CHANGED           
VR20M    MVC   PCOLTYP,COLTYP                                                   
*                                                                               
         MVI   PCOLTOT,C' '                                                     
         LA    R2,COLTOTH          FLD=TOTAL TYPE                               
         CLI   5(R2),0             ANYTHING INPUT                               
         BE    VR40                NO                                           
         CLI   COLTOT,C'T'                                                      
         BE    VR30                VALID TOTAL TYPE                             
         CLI   COLTOT,C'A'                                                      
         BE    VR30                VALID TOTAL TYPE                             
         CLI   COLTOT,C' '                                                      
         BNE   VRINV               INVALID TOTAL TYPE                           
VR30     DS    0H                                                               
         CLI   COLTOT,C' '         ANY TOTAL TYPE DEFINITION ?                  
         BE    VR40                NO                                           
         CLI   PCOLTYP,C'D'        "DATE" TYPE ?                                
         BE    VRNOTOT             YES - CANNOT BE TOTALLED                     
         CLI   PCOLTYP,C'P'        "DATE RANGE" TYPE ?                          
         BE    VRNOTOT             YES - CANNOT BE TOTALLED                     
         CLI   PCOLTYP,C'T'        "TEXT" TYPE ?                                
         BE    VRNOTOT             YES - CANNOT BE TOTALLED                     
         CLI   PCOLTYP,C'B'        "BINARY" TYPE ?                              
         BE    VRNOTOT             YES - CANNOT BE TOTALLED                     
         MVC   PCOLTOT,COLTOT                                                   
*                                                                               
VR40     DS    0H                  FLD=NUMBER OF DECIMALS                       
         MVC   SCOLDECS,PCOLDECS   SAVE FOR POSSIBLE CHANGE TEST                
         MVI   PCOLDECS,0                                                       
         LA    R2,COLDECH                                                       
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BE    VR40DF              NO - TO DEFAULT TESTING                      
         TM    4(R2),X'08'         VALID NUMERIC ?                              
         BNO   VRNOTNUM            NO                                           
         CLI   8(R2),C'6'                                                       
         BNL   VRINV               INVALID - MAX ENTRY IS 5                     
         CLI   PCOLTYP,C'D'        "DATE" TYPE ?                                
         BE    VRNODEC             ERROR - CANNOT HAVE DECIMAL ENTRY            
         CLI   PCOLTYP,C'P'        "DATE RANGE" TYPE ?                          
         BE    VRNODEC             ERROR - CANNOT HAVE DECIMAL ENTRY            
         CLI   PCOLTYP,C'T'        "TEXT" TYPE ?                                
         BE    VRNODEC             ERROR - CANNOT HAVE DECIMAL ENTRY            
*                                                                               
         B     VR40CNV                                                          
*                                                                               
VR40DF   DS    0H                                                               
         CLI   PCOLTYP,C'$'        "DOLLARS" TYPE ?                             
         BE    VR40DF2                                                          
         CLI   PCOLTYP,C'%'        "PERCENT" TYPE ?                             
         BNE   VR40ENDX            LEAVE PCOLDECS AS BINARY ZERO                
VR40DF2  DS    0H                                                               
         LA    R1,2                2 DECIMAL PLACE DEFAULT                      
         B     VR40END                                                          
VR40CNV  DS    0H                                                               
         PACK  DUB,8(1,R2)                                                      
         CVB   R1,DUB                                                           
VR40END  DS    0H                                                               
         STC   R1,PCOLDECS         OUTPUT TO RECORD                             
VR40ENDX TM    PCOLSTAT,X'80'      HAS RECORD BEEN USED IN BUY ?                
         BNO   VR40X               NO - OK                                      
         CLC   PCOLDECS,SCOLDECS                                                
         BNE   VRNOCHG             NO. OF DECIMALS CANNOT BE CHANGED            
VR40X    DS    0H                                                               
*                                                                               
VR50     DS    0H                  FLD=MAXIMUM DATA LENGTH                      
         MVC   SCOLMLEN,PCOLMLEN   SAVE FOR POSSIBLE CHANGE TEST                
         MVI   PCOLMLEN,0                                                       
         LA    R2,COLMAXH                                                       
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BE    VR50DEF             NO - TO DEFAULT HANDLING                     
         TM    4(R2),X'08'         VALID NUMERIC ?                              
         BNO   VRNOTNUM            NO                                           
         ZIC   R1,5(R2)            PREP FOR EXECUTED PACK                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,PCOLMLEN         OUTPUT TO RECORD                             
         LTR   R1,R1               ZERO ENTERED ?                               
         BZ    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'%'        "PERCENT" TYPE ?                             
         BE    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'D'        "DATE" TYPE ?                                
         BE    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'$'        "DOLLARS" TYPE ?                             
         BE    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'N'        "NUMERIC" TYPE ?                             
         BE    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'P'        "DATE RANGE" TYPE ?                          
         BE    VR50DEF             YES - TO DEFAULT HANDLING                    
         CLI   PCOLTYP,C'T'        "TEXT" TYPE ?                                
         BE    VR50T                                                            
         DC    H'0'                PCOLTYP NG (SHOULD NEVER HAPPEN)             
VR50C    DS    0H                  PERCENT TYPE                                 
         CLI   PCOLMLEN,6                                                       
         BNH   VR50END             LENGTH FIELD OK                              
         B     VRNGLEN             LENGTH INVALID FOR DATA TYPE                 
VR50D    DS    0H                  DATE TYPE                                    
         CLI   PCOLMLEN,8                                                       
         BNH   VR50END             LENGTH FIELD OK                              
         B     VRNGLEN             LENGTH INVALID FOR DATA TYPE                 
VR50N    DS    0H                  DATE TYPE                                    
         CLI   PCOLMLEN,13                                                      
         BNH   VR50END             LENGTH FIELD OK                              
         B     VRNGLEN             LENGTH INVALID FOR DATA TYPE                 
VR50P    DS    0H                  DATE TYPE                                    
         CLI   PCOLMLEN,17                                                      
         BNH   VR50END             LENGTH FIELD OK                              
         B     VRNGLEN             LENGTH INVALID FOR DATA TYPE                 
VR50T    DS    0H                  DATE TYPE                                    
*NOP*    CLI   PCOLMLEN,240                                                     
         CLI   PCOLMLEN,60                                                      
         BNH   VR50END             LENGTH FIELD OK                              
         B     VRNGLEN             LENGTH INVALID FOR DATA TYPE                 
*                                                                               
VR50DEF  DS    0H                  OUTPUT DEFAULT LENGTH VALUE TO REC           
         MVI   PCOLMLEN,6                                                       
         CLI   PCOLTYP,C'%'        "PERCENT" TYPE ?                             
         BE    VR50END                                                          
         MVI   PCOLMLEN,8                                                       
         CLI   PCOLTYP,C'D'        "DATE" TYPE ?                                
         BE    VR50END                                                          
         MVI   PCOLMLEN,13                                                      
         CLI   PCOLTYP,C'$'        "DOLLARS" TYPE ?                             
         BE    VR50END                                                          
         CLI   PCOLTYP,C'N'        "NUMERIC" TYPE ?                             
         BE    VR50END                                                          
         MVI   PCOLMLEN,17                                                      
         CLI   PCOLTYP,C'P'        "DATE RANGE" TYPE ?                          
         BE    VR50END                                                          
*SMY*    MVI   PCOLMLEN,60                                                      
         MVI   PCOLMLEN,20         CAN BE UP TO 60 FOR (T)EXT                   
         CLI   PCOLTYP,C'T'        "TEXT" TYPE ?                                
         BE    VR50END                                                          
         DC    H'0'                PCOLTYP NG (SHOULD NEVER HAPPEN)             
VR50END  DS    0H                                                               
         TM    PCOLSTAT,X'80'      HAS RECORD BEEN USED IN BUY ?                
         BNO   VR50X               NO - OK                                      
         CLC   PCOLMLEN,SCOLMLEN                                                
         BL    VRNORED             LENGTH MAY NOT BE DECREASED                  
VR50X    DS    0H                                                               
*                                                                               
VR90     DS    0H                  FLD=MEDIA                                    
         MVI   PCOLMED,X'00'                                                    
         LA    R2,COLMEDH                                                       
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VKMISS                                                           
         CLI   5(R2),3             MAYBE "ALL" ?                                
         BNE   VR90C                                                            
         CLC   =C'ALL',8(R2)                                                    
         BNE   VR90C                                                            
         MVI   PCOLMED,PCOLM_AQ    ALL MEDIA CODES (INDICATOR 1)                
         OI    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2                      
         MVI   PCOLMED2,PCOLMA2Q   ALL MEDIA CODES (INDICATOR 2)                
         B     VR90X               DONE WITH MEDIA                              
VR90C    DS    0H                                                               
         ZICM  R0,5(R2)            LOOP COUNTER                                 
         LA    R1,8(R2)            POINT TO MEDIA FIELD                         
VR90F    DS    0H                  START OF LOOP                                
         CLI   0(R1),C' '                                                       
         BNH   VRINV               CANNOT BE BLANK                              
         CLI   0(R1),C','                                                       
         BE    VR90FMV             MOVE PAST COMMA                              
         CLI   0(R1),C'I'                                                       
         BNE   *+12                                                             
         OI    PCOLMED,PCOLM_IQ                                                 
         B     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'M'                                                       
         BNE   *+12                                                             
         OI    PCOLMED,PCOLM_MQ                                                 
         B     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'N'                                                       
         BNE   *+12                                                             
         OI    PCOLMED,PCOLM_NQ                                                 
         B     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'O'                                                       
         BNE   *+12                                                             
         OI    PCOLMED,PCOLM_OQ                                                 
         B     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'S'                                                       
         BNE   *+12                                                             
         OI    PCOLMED,PCOLM_SQ                                                 
         B     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'T'                                                       
         JNE   *+12                                                             
         OI    PCOLMED,PCOLM_TQ                                                 
         J     VR90FMV             MOVE PAST                                    
         CLI   0(R1),C'L'                                                       
         JNE   *+12                                                             
         OI    PCOLMED,PCOLM_LQ                                                 
         J     VR90FMV             MOVE PAST                                    
*                                                                               
         CLI   0(R1),C'B'          MOBILE?                                      
         JNE   *+16                                                             
         OI    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2                      
         OI    PCOLMED2,PCOLM_BQ                                                
         J     VR90FMV             MOVE PAST                                    
*                                                                               
         CLI   0(R1),C'V'          NVIDEO? (NATIONAL VIDEO)                     
         JNE   *+16                                                             
         OI    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2                      
         OI    PCOLMED2,PCOLM_VQ                                                
         J     VR90FMV             MOVE PAST                                    
*                                                                               
         CLI   0(R1),C'W'          LVIDEO? (LOCAL VIDEO)                        
         JNE   *+16                                                             
         OI    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2                      
         OI    PCOLMED2,PCOLM_WQ                                                
         J     VR90FMV             MOVE PAST                                    
*                                                                               
         CLI   0(R1),C'D'          DIGITAL AUDIO?                               
         JNE   *+16                                                             
         OI    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2                      
         OI    PCOLMED2,PCOLM_DQ                                                
         J     VR90FMV             MOVE PAST                                    
*                                                                               
         J     VRINV               NO VALID MEDIA CODE                          
*                                                                               
VR90FMV  DS    0H                                                               
         LA    R1,1(R1)            MOVE TO RIGHT                                
         BCT   R0,VR90F            TEST NEXT COLUMN                             
         CLI   PCOLMED,X'00'       DONE - ANYTHING FOUND ?                      
         BE    VRINV               NO - INVALID                                 
VR90X    DS    0H                                                               
*                                                                               
VR100    DS    0H                  FLD=USE ON INSERTION ORDERS                  
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STDCOL                                    
         BNE   *+12                                                             
         MVI   PCOLINS,0              DEFAULTS TO NOT ON INS ORDERS             
         B     VR100X                                                           
*                                                                               
         MVI   PCOLINS,0                                                        
         LA    R2,COLINSH                                                       
         CLI   5(R2),0                                                          
         BE    VR100X              NOTHING ENTERED                              
         CLI   COLINS,C'Y'                                                      
         BE    VR100M                                                           
         CLI   COLINS,C'N'                                                      
         BNE   VRINV               IF ENTERED, MUST BE Y OR N                   
         B     VR100X              N IS SAME AS NOTHING ENTERED                 
VR100M   DS    0H                                                               
         OI    PCOLINS,X'80'       MEANS USE IN INSERTION ORDERS                
VR100X   DS    0H                                                               
*                                                                               
VR110    DS    0H                  FLD=CUST COLUMN FCON GROUP                   
*                                                                               
         MVI   PCOLFCON,0                                                       
         LA    R2,COLFCONH                                                      
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BE    VR110X              NO                                           
         TM    4(R2),X'08'         VALID NUMERIC ?                              
         BNO   VRNOTNUM            NO                                           
*SMY*    CLI   5(R2),2             3-CHAR LENGTH ?                              
         CLI   5(R2),1             ONE-CHAR LENGTH ?                            
         BH    VRINV               NO                                           
*SMY*    CLC   8(3,R2),=C'255'                                                  
*                                                                               
         CLI   8(R2),C'0'          BETWEEN 1 AND 8 ?                            
         BNH   VRINV                                                            
         CLI   8(R2),C'9'                                                       
         BNL   VRINV                                                            
*                                                                               
         ZIC   R1,5(R2)            LENGTH INPUT                                 
         BCTR  R1,0                PREP FOR EXECUTED PACK                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,PCOLFCON         OUTPUT TO RECORD                             
*                                                                               
VR110X   DS    0H                                                               
*                                                                               
*        TRACKING CHANGES FOR IDESK - Y/N                                       
*                                                                               
VRTRK    DS    0H                                                               
*                                                                               
         MVI   PCOLTRK,0           INIT FIELD                                   
*                                                                               
         LA    R2,COLCHGSH         POINT TO TRACKING FIELD                      
*                                                                               
         CLI   5(R2),1             CHECK INPUT LENGTH                           
         BL    VRTRKX              DONE IF NO ENTRY                             
         BH    VRINV               MUST BE AT MOST 1 CHARACTER                  
*                                                                               
         CLI   RECNUM,RSTDCOLQ     MUST BE STANDARD COLUMN                      
         BNE   VRTRKER                                                          
*                                                                               
         CLI   8(R2),C'N'          DONE IF NO                                   
         BE    VRTRKX                                                           
         CLI   8(R2),C'Y'          IF YES                                       
         BNE   VRINV                                                            
*                                                                               
         MVI   PCOLTRK,C'Y'           SET 'Y' IN RECORD                         
*                                                                               
VRTRKX   DS    0H                                                               
*                                                                               
*        READ ONLY IN ADBUYER - Y/N                                             
*                                                                               
VRRDWR   DS    0H                                                               
*                                                                               
         MVI   PCOLRDWR,0          INIT FIELD                                   
*                                                                               
         LA    R2,COLRDWRH         POINT TO READ ONLY FIELD                     
*                                                                               
         CLI   5(R2),1             CHECK INPUT LENGTH                           
         BL    VRRDWRX             DONE IF NO ENTRY                             
         BH    VRINV               MUST BE AT MOST 1 CHARACTER                  
*                                                                               
         CLI   8(R2),C'N'          DONE IF NO                                   
         BE    VRRDWRX                                                          
         CLI   8(R2),C'Y'          IF YES                                       
         BNE   VRINV                                                            
*                                                                               
         MVI   PCOLRDWR,C'Y'          SET 'Y' IN RECORD                         
*                                                                               
VRRDWRX  DS    0H                                                               
*                                                                               
*        DROP DOWN MENU       - Y/N                                             
*                                                                               
VRMENU   DS    0H                                                               
*                                                                               
         MVI   PCOLMENU,0          INIT FIELD                                   
*                                                                               
         LA    R2,COLMENUH         POINT TO READ ONLY FIELD                     
*                                                                               
         CLI   5(R2),1             CHECK INPUT LENGTH                           
         BL    VRMENUX             DONE IF NO ENTRY                             
         BH    VRINV               MUST BE AT MOST 1 CHARACTER                  
*                                                                               
         CLI   8(R2),C'N'          DONE IF NO                                   
         BE    VRMENUX                                                          
         CLI   8(R2),C'Y'          IF YES                                       
         BNE   VRINV                                                            
*                                                                               
         MVI   PCOLMENU,C'Y'          SET 'Y' IN RECORD                         
*                                                                               
VRMENUX  DS    0H                                                               
*                                                                               
VR900    DS    0H                  END OF RECORD VALIDATION                     
         CLI   ACTNUM,ACTADD       ADDING A RECORD ?                            
         BNE   VRX                 NO - DISPLAY AND EXIT                        
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   VR910                                                            
*                                                                               
         OI    PCOLSTAT,X'80'         ASSUME USED IN A BUY                      
*                                     STOPS ANY CHANGES                         
*                                  ADD A PASSIVE KEY AND GET A UNIQUE           
         BRAS  RE,SNXTSQN#           SEQUENCE NUMBER FOR NEW RECORD             
*                                                                               
         B     VR920                                                            
*                                                                               
VR910    DS    0H                                                               
*                                  ADD A PASSIVE KEY AND GET A UNIQUE           
         BRAS  RE,VNXTSQN#           SEQUENCE NUMBER FOR NEW RECORD             
*                                                                               
VR920    DS    0H                                                               
*                                                                               
         MVC   PCOLSQN,QSQN#       SET SEQUENCE NUMBER INTO RECORD              
         XC    PCOLSQN,=X'FFFF'    UNCOMPLEMENT NEW SEQUENCE NUMBER             
*                                                                               
VRX      DS    0H                                                               
*                                                                               
         B     DR                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
         B     TRAPERR                                                          
VRNOTOT  MVI   ERROR,NOTOT         DATA TYPE CANNOT BE TOTALLED                 
         B     TRAPERR                                                          
VRNOTNUM MVI   ERROR,NOTNUM        NOT VALID NUMERIC                            
         B     TRAPERR                                                          
VRNOCHG  MVI   ERROR,NOCHG         FIELD CANNOT BE CHANGED                      
         B     TRAPERR                                                          
VRNODEC  MVI   ERROR,NODEC         DATA TYPE CANNOT HAVE DECIMALS               
         B     TRAPERR                                                          
VRNGLEN  MVI   ERROR,INVLEN        LENGTH INVALID FOR DATA TYPE                 
         B     TRAPERR                                                          
VRNORED  MVI   ERROR,NOSHRT        LENGTH CANNOT BE DECREASED                   
         B     TRAPERR                                                          
*                                                                               
VRTRKER  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TRKERMSG),TRKERMSG                                     
         OI    CONHEADH+6,X'80'                                                 
         GOTOR ERREX2                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
*                                                                               
         L     R4,AIO                                                           
         USING PCOLRECD,R4                                                      
*                                                                               
         LA    R6,PCOLFRST-PCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+8                                                              
         LA    R6,(GCOLFRST-GCOLKEY)(R4)   POINT TO FIRST ELM IN REC            
*                                                                               
         USING PCOLELEM,R6         ESTABLISH CODE DESCRIPTION ELEMENT           
*                                                                               
         EDIT  (B2,PCOLSQN),(5,COLSEQ),ALIGN=LEFT                               
         OI    COLSEQH+6,X'80'     XMIT                                         
*                                                                               
         FOUT  COLMSGH,SPACES,60                                                
         FOUT  COLDESCH,PCOLDESC,25                                             
         FOUT  COLHDR1H,PCOLHDR1,12                                             
         FOUT  COLHDR2H,PCOLHDR2,12                                             
         FOUT  COLTYPH,PCOLTYP,1                                                
         FOUT  COLTOTH,PCOLTOT,1                                                
         FOUT  COLINSH,SPACES,1                                                 
         FOUT  COLCHGSH,SPACES,3                                                
         FOUT  COLCHGSH,PCOLTRK,1                                               
         FOUT  COLRDWRH,SPACES,3                                                
         FOUT  COLRDWRH,PCOLRDWR,1                                              
         FOUT  COLMENUH,PCOLMENU,1                                              
*                                                                               
DR20     DS    0H                                                               
         EDIT  (B1,PCOLDECS),(1,COLDEC)          BLANK IF ZERO                  
         OI    COLDECH+6,X'80'     XMIT                                         
         CLI   PCOLTYP,C'D'        "DATE" TYPE ?                                
         BE    DR20X               CANNOT HAVE DECIMAL ENTRY                    
         CLI   PCOLTYP,C'P'        "DATE RANGE" TYPE ?                          
         BE    DR20X               CANNOT HAVE DECIMAL ENTRY                    
         CLI   PCOLTYP,C'T'        "TEXT" TYPE ?                                
         BE    DR20X               CANNOT HAVE DECIMAL ENTRY                    
         CLI   PCOLTYP,C'B'        "BINARY" TYPE ?                              
         BE    DR20X               CANNOT HAVE DECIMAL ENTRY                    
         EDIT  (B1,PCOLDECS),(1,COLDEC),ZERO=NOBLANK                            
DR20X    DS    0H                                                               
*                                                                               
DR30     DS    0H                                                               
         EDIT  (B1,PCOLMLEN),(3,COLMAX),ALIGN=LEFT                              
         OI    COLMAXH+6,X'80'     XMIT                                         
DR30X    DS    0H                                                               
*                                                                               
DR40     DS    0H                  TEST "INSTRUCTIONS"                          
         TM    PCOLINS,X'80'       USE ON INSERTION ORDERS ?                    
         BNO   DR40X               NO                                           
         MVI   COLINS,C'Y'         YES - SET "USE ON INS ORDERS" TO Y           
         OI    COLINSH+6,X'80'     XMIT                                         
DR40X    DS    0H                                                               
*                                                                               
DR50     DS    0H                                                               
         EDIT  (B1,PCOLFCON),(3,COLFCON),ALIGN=LEFT                             
         OI    COLFCONH+6,X'80'     XMIT                                        
DR50X    DS    0H                                                               
*                                                                               
DR90     DS    0H                                                               
         FOUT  COLMEDH,SPACES,9    CLEAR                                        
*                                                                               
         TM    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2?                     
         JNZ   *+16                                                             
         CLI   PCOLMED,PCOLM_AQ    ALL MEDIA W/O EXTENDED IND 2?                
         JE    DR92_X                                                           
         J     DR96                                                             
*                                                                               
         CLI   PCOLMED2,PCOLMA2Q   ALL MEDIA ON INDICATOR 2?                    
         JNE   DR96                                                             
         CLI   PCOLMED,X'FF'       ALL MEDIA ON INDICATOR 1?                    
         JNE   DR96                                                             
*                                                                               
DR92_X   FOUT  COLMEDH,=C'ALL',3                                                
         B     DR90X               DONE WITH MEDIA                              
*                                                                               
DR96     DS    0H                                                               
         LA    R1,COLMED                                                        
         BRAS  RE,DISPMEDC         DISPLAY MEDIA CODES                          
*                                                                               
DR90X    DS    0H                                                               
*                                                                               
DR900    DS    0H                                                               
         TM    PCOLSTAT,X'80'      HAS RECORD BEEN USED IN BUY ?                
         BNO   DR980               NO                                           
         XC    COLMSG,COLMSG                                                    
         MVC   COLMSG(L'USEDMSG),USEDMSG    "CHANGES LIMITED"                   
         OI    COLMSGH+6,X'80'     XMIT                                         
DR980    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*         * * *  AFTER ADDING A RECORD (XRECADD)   * * *                        
*                                                                               
*     GET THE DISK ADDRESS FOR RECORD JUST ADDED AND PLACE THIS IN              
*       THE KEY OF THE PASSIVE POINTER (UPDATE THE POINTER)                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
XR       DS    0H                                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY         GET THE PASSIVE POINTER FOR ABOVE RECORD         
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BE    XRSTD                  SKIP UPDATING PASSIVE PTR                 
*                                                                               
         USING PCOLRECD,R4                                                      
*                                                                               
*        COULD BE TRICKY HERE                                                   
*                                                                               
*                                                                               
         MVC   KEY(PCOLKCOD-PCOLKEY),SVCCLKEY S/B KEY OF LAST REC ADDED         
*                                                                               
         MVI   PCOLPRCD,PCOLPRCQ     CUSTOM COL PASS POINTER TYPE               
         MVC   PCOLPSQN,QSQN#        2'S COMPLEMENTED SEQUENCE NUMBER           
         GOTO1 HIGH                GET THE PASSIVE POINTER                      
*                                                                               
         CLC   KEY(L'PCOLKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                       MUST BE FOUND                         
*                                                                               
         MVC   KEY+27(4),DMDSKADD  PUT D/A OF RECORD INTO PASSIVE PNTR          
*                                                                               
         GOTO1 WRITE               WRITE MODIFIED POINTER                       
*                                                                               
         B     XRX                                                              
*                                                                               
*        UPDATE PASSIVE POINTER FOR STANDARD COLUMNS                            
*                                                                               
XRSTD    DS    0H                                                               
*                                                                               
         USING GCOLRECD,R4                                                      
*                                                                               
         MVC   GCOLPRID,=AL3(GCOLPRIQ) SET PASSIVE RECORD ID                    
         MVI   GCOLPMED,C'A'       ALWAYS MEDIA A                               
         MVI   GCOLPRCD,GCOLPRCQ     CUSTOM COL PASS POINTER TYPE               
         MVC   GCOLPSQN,QSQN#        2'S COMPLEMENTED SEQUENCE NUMBER           
*                                                                               
         GOTO1 HIGH                GET THE PASSIVE POINTER                      
*                                                                               
         CLC   KEY(L'GCOLKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                       MUST BE FOUND                         
*                                                                               
         MVC   GCOLDDA,DMDSKADD    PUT D/A OF RECORD INTO PASSIVE PNTR          
*                                                                               
         GOTO1 WRITE               WRITE MODIFIED POINTER                       
*                                                                               
XRX      DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         CLI   RECNUM,RSTDCOLQ     SKIP IF STANDARD COLUMNS                     
         BE    LRSTD                                                            
*                                                                               
         USING PCOLRECD,R4                                                      
*                                                                               
         MVC   PCOLKAGY,AGENCY  CREATE KEY  -- AGENCY                           
*                                                                               
         MVI   PCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   PCOLKRCD,X'61'                  REC ID                           
         MVC   PCOLKCOD,SVCODE                 CUSTOM COLUMN CODE               
*                                                                               
         B     LRSTDX                                                           
*                                                                               
LRSTD    DS    0H                                                               
*                                                                               
         USING GCOLKEY,R4          ESTABLISH GENDIR KEY                         
*                                                                               
         MVC   GCOLKRID,=AL3(GCOLKRIQ)    USE GENFILE RECORD ID                 
*                                                                               
         MVI   GCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   GCOLKRCD,X'61'                  REC ID                           
         MVC   GCOLKCOD,SVCODE                 CUSTOM COLUMN CODE               
*                                                                               
LRSTDX   DS    0H                                                               
*                                                                               
LR010    GOTO1 HIGH                                                             
*                                                                               
         B     LR030                                                            
*                                                                               
LR020    GOTO1 HIGH                RE-POINT GENFILE                             
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   LR040                                                            
*                                                                               
         CLC   KEY(5),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
LR040    DS    0H                                                               
*                                                                               
         GOTO1 GETREC              GET THE COL RECORD                           
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,PCOLFRST-PCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+8                                                              
         LA    R6,GCOLFRST-GCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         USING PCOLELEM,R6         ESTABLISH COLUMN DESC ELM                    
*                                                                               
         CLI   FCONFLT,0           FILTER ON FCON ?                             
         BE    LR050               NO                                           
         CLC   PCOLFCON,FCONFLT    FCON IN RECORD EQUAL TO FILTER ?             
         BNE   LR020               NO - SKIP                                    
*                                                                               
LR050    DS    0H                                                               
*                                                                               
*        HANDLE FILTERING ON TRACKING FOR IDESK OPTION                          
*                                                                               
LRTRK    DS    0H                                                               
*                                                                               
         CLI   TRKFILT,0           FILTER ON TRACKING?                          
         BE    LRTRKX              NO                                           
*                                                                               
         CLI   TRKFILT,C'Y'        IF TRACKING                                  
         BNE   LRTRK10                                                          
*                                                                               
         CLI   PCOLTRK,C'Y'           TRACKING OPTION MUST BE ON                
         BE    LRTRKX                                                           
*                                                                               
         B     LR020               ELSE SKIP RECORD                             
*                                                                               
LRTRK10  DS    0H                                                               
*                                                                               
         CLI   PCOLTRK,C'Y'        ELSE TRACKING OPTION CAN'T BE ON             
         BE    LR020                                                            
*                                                                               
LRTRKX   DS    0H                                                               
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
*                                                                               
         MVC   LISTCODE,PCOLKCOD-PCOLKEY(R4)                                    
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+10                                                             
         MVC   LISTCODE,GCOLKCOD-GCOLKEY(R4)                                    
*                                                                               
         MVC   LISTDESC,PCOLDESC                                                
         MVC   LISTHDR1,PCOLHDR1                                                
         MVC   LISTHDR2,PCOLHDR2                                                
         MVC   LISTTYP,PCOLTYP                                                  
         MVC   LISTTOT,PCOLTOT                                                  
         MVC   LISTTOT,PCOLTOT                                                  
         MVC   LISTMENU,PCOLMENU                                                
         EDIT  (B1,PCOLFCON),(3,LISTFCON),ALIGN=LEFT                            
         MVC   LISTTRK,PCOLTRK                                                  
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         JNE   LR070                                                            
         EDIT  (B2,PCOLSQN),(4,LISTRDWR),ALIGN=LEFT                             
*                                                                               
LR070    FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LAY   R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   RECNUM,RSTDCOLQ     SKIP IF STANDARD COLUMNS                     
         BE    PRSTD                                                            
*                                                                               
         USING PCOLRECD,R4                                                      
*                                                                               
         MVC   PCOLKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVI   PCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   PCOLKRCD,X'61'                  ID                               
*                                                                               
         B     PRSTDX                                                           
*                                                                               
PRSTD    DS    0H                                                               
*                                                                               
         USING GCOLRECD,R4                                                      
*                                                                               
         MVC   GCOLKRID,=AL3(GCOLKRIQ)    USE CTFILE RECORD ID                  
         MVI   GCOLKMED,C'A'                   ALWAYS "A"                       
         MVI   GCOLKRCD,X'61'                  ID                               
*                                                                               
PRSTDX   DS    0H                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(4),KEYSAVE      CHECK THRU RECORD CODE                       
         BNE   PR110               END OF RECORDS                               
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   PR40                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE      TEST FOR ALL DONE                            
         BNE   PR110                                                            
*                                                                               
PR40     DS    0H                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 GETREC              GET THE COL RECORD                           
*                                                                               
         L     R4,AIO              POINT TO RECORD                              
*                                                                               
         LA    R6,PCOLFRST-PCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         BNE   *+8                                                              
         LA    R6,GCOLFRST-GCOLKEY(R4) POINT TO FIRST ELEMENT                   
*                                                                               
         USING PCOLELEM,R6         ESTABLISH COLUMN DESC ELM                    
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND CUSTOM COLUMN REC                 
*                                                                               
         MVC   P1+00(12),PCOLKCOD-PCOLKEY(R4)                                   
*                                                                               
         CLI   RECNUM,RSTDCOLQ     IF STANDARD COLUMNS                          
         JNE   *+10                                                             
         MVC   P1+00(12),GCOLKCOD-GCOLKEY(R4)                                   
*                                                                               
         MVC   P1+14(24),PCOLDESC                                               
         MVC   P1+39(11),PCOLHDR1                                               
         MVC   P1+52(11),PCOLHDR2                                               
         MVC   P1+67(01),PCOLTYP                                                
         MVC   P1+72(01),PCOLTOT                                                
         MVC   P1+75(01),PCOLRDWR                                               
         MVC   P1+78(01),PCOLMENU                                               
*                                                                               
         LA    R1,P1+80                                                         
*                                                                               
         TM    PCOLMED,PCOLMXXQ    EXTENDED TO INDICATOR 2?                     
         JNZ   *+16                                                             
         CLI   PCOLMED,PCOLM_AQ    ALL MEDIA W/O EXTENDED IND 2?                
         JE    PR50                                                             
         J     PR60                                                             
*                                                                               
         CLI   PCOLMED2,PCOLMA2Q   ALL MEDIA ON INDICATOR 2?                    
         JNE   PR60                                                             
         CLI   PCOLMED,X'FF'       ALL MEDIA ON INDICATOR 1?                    
         JNE   PR60                                                             
*                                                                               
PR50     MVC   0(3,R1),=C'ALL'                                                  
         B     PR100               DONE                                         
*                                                                               
PR60     DS    0H                                                               
         BRAS  RE,DISPMEDC         DISPLAY MEDIA CODES                          
*                                                                               
PR100    DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    B     EXIT                                                             
*                                                                               
RECFOUND DC    X'0'                                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
STDMSG   DC    C'STANDARD COLUMN CODE MUST START WITH "!".'                     
CCLMSG   DC    C'CUSTOM COLUMN CODE CANNOT START WITH "!".'                     
TRKERMSG DC    C'FEATURE RESERVED FOR STANDARD COLUMNS'                         
USEDMSG  DC    C'** RECORD USED IN BUY - CHANGES LIMITED *'                     
*                                                                               
         LTORG ,                                                                
*                                                                               
         EJECT                                                                  
HEDSPECS SSPEC H1,1,C'     '                                                    
         SSPEC H1,42,C'PRINT CUSTOM COLUMN REPORT'                              
         SSPEC H2,42,C'--------------------------'                              
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,RUN                                                        
         SSPEC H4,95,REPORT                                                     
         SSPEC H5,95,PAGE                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H7,1,C'CODE          DESCRIPTION'                                
         SSPEC H8,1,C'----          -----------'                                
         SSPEC H7,41,C'HEADER 1      HEADER 2       TYP  TOT   MEDIA'           
         SSPEC H8,41,C'--------      --------       ---  ---   -----'           
         DC    X'00'                                                            
         SPACE 3                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISPMEDC NTR1  BASE=*,LABEL=*      DISPLAY MEDIA CODES                          
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING PCOLELEM,R6         ESTABLISH COLUMN DESC ELM                    
*                                                                               
         TM    PCOLMED2,PCOLM_BQ                                                
         BNO   *+14                                                             
         MVC   0(2,R1),=C'B,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_IQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'I,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_LQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'L,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_MQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'M,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_NQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'N,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_OQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'O,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_SQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'S,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED,PCOLM_TQ                                                 
         BNO   *+14                                                             
         MVC   0(2,R1),=C'T,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED2,PCOLM_VQ                                                
         BNO   *+14                                                             
         MVC   0(2,R1),=C'V,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED2,PCOLM_WQ                                                
         BNO   *+14                                                             
         MVC   0(2,R1),=C'W,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         TM    PCOLMED2,PCOLM_DQ                                                
         BNO   *+14                                                             
         MVC   0(2,R1),=C'D,'                                                   
         LA    R1,2(R1)           MOVE TO RIGHT                                 
*                                                                               
         AHI   R1,-1                                                            
         MVI   0(R1),C' '         CLEAR LAST COMMA                              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
         TITLE 'PRSFM22 - PRINT CUSTOM COLUMN MAINTENANCE - VNXTSQN#'           
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE CUSTOM COLUMN RECORD     *         
*        UNIQUE SEQUENCE NUMBER                                       *         
*                                                                     *         
*        IF THERE ARE NO CUSTOM COLUMN RECORDS ON FILE                *         
*           START NUMBERING AT 1,000                                  *         
*                                                                     *         
*        ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS              *         
*           SEQUENCE NUMBERS IN 2'S COMPLEMENT. READS LOWEST NUMBER   *         
*           (REALLY HIGHEST) FOR UPDATE AND THEN ADDS POINTER FOR     *         
*           NEXT NUMBER. THIS RESERVES NEXT NUMBER FOR THIS CALL TO   *         
*           THE SUBROUTINE.                                           *         
*           IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS         *         
*           REPEATED.                                                 *         
*                                                                     *         
*EXIT    QSQN#  =  FOUND NEW SEQUENCE NUMBER                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTSQN# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         MVC   SVCCLKEY,KEY        SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS CUSTOM COLUMN               
         USING PCOLRECD,R4           SEQUENCE# PASSIVE                          
*                                                                               
NXTSQNLP DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   PCOLPAGY,AGENCY     SET AGENCY                                   
         MVI   PCOLPMED,C'A'       SET MEDIA (ALWAYS "A")                       
         MVI   PCOLPRCD,PCOLPRCQ   SET RECORD CODE                              
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   PCOLPKEY(PCOLPSQN-PCOLPKEY),KEYSAVE   SKIP IF FOUND              
         BE    NXTSQN1                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   PCOLPSQN,=X'03E8'   START SQN# AT 1,000                          
         XC    PCOLPSQN,=X'FFFF'   2'S COMPLEMENT                               
*                                                                               
         B     NXTSQN2                                                          
*                                                                               
*        READ RECORD AND RESERVE NEXT SQN#                                      
*                                                                               
NXTSQN1  DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
*                                                                               
         GOTO1 READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'80'   RESET                                      
*                                                                               
         ZICM  RF,PCOLPSQN,2       MOVE SEQUENCE NUMBER TO WORK                 
*                                                                               
         AHI   RF,-1               DECREMENT 2'S COMPLEMENT = NXT SQN#          
*                                                                               
         STH   RF,PCOLPSQN         PUT NEW COMPLEMENT IN KEY                    
*                                                                               
NXTSQN2  DS    0H                                                               
*                                                                               
         OI    GENSTAT4,NODUPDIE   DON'T DIE ON DUPLICATE KEY ON ADD            
*                                                                               
         GOTO1 ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSQNDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSQNCN DS    0H                                                               
*                                                                               
         B     NXTSQNLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSQNDN DS    0H                                                               
*                                                                               
         MVC   QSQN#,PCOLPSQN      SAVE NEXT SERIAL NUMBER                      
*                                                                               
         NI    GENSTAT4,X'FF'-NODUPDIE   RESET                                  
*                                                                               
         MVC   KEY,SVCCLKEY        RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
VNXTSQNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRSFM22 - PRINT CUSTOM COLUMN MAINTENANCE - SNXTSQN#'           
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE STANDARD COLUMN RECORD   *         
*        UNIQUE SEQUENCE NUMBER                                       *         
*                                                                     *         
*        IF THERE ARE NO CUSTOM COLUMN RECORDS ON FILE                *         
*           START NUMBERING AT X'2000'                                *         
*                                                                     *         
*        ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS              *         
*           SEQUENCE NUMBERS IN 2'S COMPLEMENT. READS LOWEST NUMBER   *         
*           (REALLY HIGHEST) FOR UPDATE AND THEN ADDS POINTER FOR     *         
*           NEXT NUMBER. THIS RESERVES NEXT NUMBER FOR THIS CALL TO   *         
*           THE SUBROUTINE.                                           *         
*           IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS         *         
*           REPEATED.                                                 *         
*                                                                     *         
*EXIT    QSQN#  =  FOUND NEW SEQUENCE NUMBER                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SNXTSQN# NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         MVC   SVCCLKEY,KEY        SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS STANDARD COLUMN             
         USING GCOLRECD,R4           SEQUENCE# PASSIVE                          
*                                                                               
SNXSQNLP DS    0H                                                               
*                                                                               
         LA    R4,KEY              POINT TO KEY WORKAREA                        
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   GCOLPRID,=AL3(GCOLPRIQ)  RECORD CODE                             
         MVI   GCOLPMED,C'A'       SET MEDIA (ALWAYS "A")                       
         MVI   GCOLPRCD,GCOLPRCQ   SET RECORD CODE                              
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   GCOLPKEY(GCOLPSQN-GCOLPKEY),KEYSAVE SKIP IF A SQN REC            
         BE    SNXSQN1                FOUND                                     
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   GCOLPSQN,=X'2000'   START SQN# AT X'2000'                        
         XC    GCOLPSQN,=X'FFFF'   2'S COMPLEMENT                               
*                                                                               
         B     SNXSQN2                                                          
*                                                                               
*        READ RECORD AND RESERVE NEXT SQN#                                      
*                                                                               
SNXSQN1  DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
*                                                                               
         GOTO1 READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'80'   RESET                                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,GCOLPSQN       MOVE SEQUENCE NUMBER TO WORK                 
*                                                                               
         BCTR  RF,0                DECREMENT 2'S COMPLEMENT = NXT SQN#          
*                                                                               
         STCM  RF,3,GCOLPSQN       PUT NEW COMPLEMENT IN KEY                    
*                                                                               
SNXSQN2  DS    0H                                                               
*                                                                               
         OI    GENSTAT4,NODUPDIE   DON'T DIE ON DUPLICATE KEY ON ADD            
*                                                                               
         GOTO1 ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    SNXSQNDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
SNXSQNCN DS    0H                                                               
*                                                                               
         B     SNXSQNLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
SNXSQNDN DS    0H                                                               
*                                                                               
         MVC   QSQN#,GCOLPSQN      SAVE NEXT SERIAL NUMBER                      
*                                                                               
         NI    GENSTAT4,X'FF'-NODUPDIE   RESET                                  
*                                                                               
         MVC   KEY,SVCCLKEY        RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
SNXTSQNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMCAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMDAD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
FCONFLT  DS    XL1                 FCON FILTER FIELD                            
TRKFILT  DS    XL1                 TRK  FILTER FIELD                            
SCOLDECS DS    XL1                                                              
SCOLMLEN DS    XL1                                                              
SVCODE   DS    CL12                                                             
X        DS    XL100                                                            
*                                                                               
RSTDCOLQ EQU   70                  STANDARD COLUMN RECORD CODE                  
*                                                                               
SVCCLKEY DS    XL32                KEY SAVEAREA                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
LISTD    DSECT                                                                  
LISTCODE DS    CL12                                                             
         DS    CL1                                                              
LISTDESC DS    CL20                                                             
         DS    CL1                                                              
LISTHDR1 DS    CL11                                                             
         DS    CL1                                                              
LISTHDR2 DS    CL11                                                             
         DS    CL2                                                              
LISTTYP  DS    CL1                                                              
         DS    CL1                                                              
LISTTOT  DS    CL1                                                              
         DS    CL1                                                              
LISTFCON DS    CL1                                                              
         DS    CL2                                                              
LISTTRK  DS    CL1                                                              
         DS    CL2                                                              
LISTRDWR DS    CL1                                                              
         DS    CL2                                                              
LISTMENU DS    CL1                                                              
         EJECT                                                                  
* PCOLREC                                                                       
                                                                                
PCOLRECD DSECT                                                                  
PCOLKRCQ EQU   X'61'               CUSTOM COLUMN RECORD TYPE                    
PCOLELEQ EQU   X'61'               CUSTOM COLUMN ELEMENT CODE                   
PCOLPRCQ EQU   X'D1'               CUSTOM COL PASSIVE KEY RECORD TYPE           
       ++INCLUDE PCOLREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PRSFM22   11/06/18'                                      
         END                                                                    
