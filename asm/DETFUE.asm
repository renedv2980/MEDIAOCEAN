*          DATA SET DETFUE     AT LEVEL 024 AS OF 04/05/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETFUEA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINT132              <=== NEED THIS TO WRITE TO SYSPRIN2              
*INCLUDE SORTER                                                                 
***********************************************************************         
*                                                                     *         
* THIS IS A STANDALONE CONVERSION FOR THE FUSION AIUE AND CARRIAGE UE *         
* ESTIMATE FILES FROM NCC. THIS CONVERSION RUNS SUBSEQUENTLY TO AN    *         
* ICETOOL JOB STEP CONTROLLED BY PANVALET MEMBER DETFUEICE.           *         
*                                                                     *         
* INPUT DATSETS:                                                      *         
*   UEIN:   PRODUCED BY THE ICETOOL STEP. CONTAINS AIUE AND CARRIAGE  *         
*           UE DATA RECORDS.                                          *         
*   MSOIN:  PRODUCED BY THE ICETOOL STEP. CONTAINS MSO DATA.          *         
*                                                                     *         
* OUTPUT DATASETS:                                                    *         
*   UEOUT:  EXTENDED PASSIVE POINTERS CONTAINING AIUE AND CARRIAGE UE *         
*           DATA, TO BE LOADED TO DEMDIRR (FOR READ EFFICIENCY).      *         
*   MSOOUT: MSO DATA RECORDS TO BE LOADED TO DEMDIRA/DEMFILA.         *         
*                                                                     *         
* ==================== EXTREMELY IMPORTANT ========================== *         
*                                                                     *         
* THE "REPLACE=YES" CONTROL CARD IS NEVER TO BE USED UNLESS REQUESTED *         
* BY PRODUCT.                                                         *         
*                                                                     *         
* IT IS GENERALLY NOT SAFE TO DO A COMPLETE REPLACEMENT OF THE "GTF"  *         
* (AIUE) KEYS FOR A GIVEN SURVEY, BECAUSE NCC DOESN'T SEND US         *         
* HISTORICAL DATA. IF WE SUDDENLY OVERWRITE THE GTF RECORDS WITH NEW  *         
* VALUES, WE WILL CALCULATE RATINGS USING DIFFERENT INDICES. WE DON'T *         
* WANT TO DO THAT. THEREFORE, WE DO NOT UPDATE ANY "G" KEY IF IT IS   *         
* ALREADY PRESENT ON OUR FILE.                                        *         
*                                                                     *         
* SO IF NCC REISSUES AIUE DATA, OUR DEFAULT POSITION IS THAT WE WILL  *         
* ONLY *ADD* NEW AIUE RECORDS, BUT WE WILL NOT UPDATE ANYTHING THAT   *         
* WE'VE ALREADY LOADED. IF NCC SAYS THAT THEY WANT US TO OVERWRITE    *         
* PREVIOUSLY-LOADED AIUE RECORDS, THEN WE NEED TO LOOK AT WHAT'S      *         
* CHANGED. PRODUCT MUST SIGN OFF ON ANY CHANGES TO PRE-EXISTING AIUE  *         
* RECORDS.                                                            *         
*                                                                     *         
* TO HELP WITH THIS ANALYSIS, THIS PROGRAM NOW PRODUCES A PIPE-       *         
* DELIMITED FILE (INTENDED FOR IMPORT INTO EXCEL), SHOWING THE        *         
* DIFFERENCES BETWEEN ANY PRE-EXISTING GTF RECORDS AND THE NEW GTF    *         
* RECORDS.                                                            *         
*                                                                     *         
* NOTE THAT IN ADDITION TO THE *BUSINESS* QUESTION OF WHETHER WE WANT *         
* TO CHANGE AIUE DATA IN A PARTICULAR INSTANCE, WE STILL HAVE TO DEAL *         
* WITH THE HORRIBLE "PARTIAL KEY" PROBLEM. ANY CHANGES TO *AIUE*      *         
* VALUES (AS OPPOSED TO *CARRIAGE* VALUES) MEAN THAT WE NEED TO       *         
* PURGE PRE-EXISTING AIUE RECORDS (BECAUSE WE CANNOT HAVE MORE THAN   *         
* ONE ACTIVE KEY FOR A GIVEN *PARTIAL* KEY ON THE FILE).              *         
*                                                                     *         
* THEREFORE, THE "REPLACE=YES" FUNCTION NOW HANDLES (FINALLY) THE     *         
* "PARTIAL KEY" SITUATION. IF ANY AIUE VALUES CHANGE, WE WRITE OUT    *         
* THE OBSOLETE KEYS AND MARK THEM FOR DELETION.                       *         
*                                                                     *         
* THE "B" RECORDS HAVE SYSCODE IN THE MINOR KEY, SO WE ALWAYS NEED TO *         
* REPLACE THE *ENTIRE* MONTH OF "B" RECORDS.                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'FUSION AUDIENCE ESTIMATE CONVERSION'                            
DETFUE   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         PRINT NOGEN                                                            
         NBASE 0,*DETFUE*,=V(REGSAVE),R9                                        
*                                                                               
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)       SYSPRINT (TO LOG DATASET)                    
T        USING DPRINT,R8                                                        
         L     R8,=V(CPRINT2)      UETRACE (TRACE SYSPRINT DATASET)             
*                                                                               
         USING IHADCB,RF                                                        
         L     RF,=V(PRNTER2)      A(SYSPRIN2 DCB)                              
         MVC   DCBDDNAM,=C'UETRACE ' REDIRECT SYSPRIN2 TO DD UETRACE            
         DROP  RF                                                               
*                                                                               
         MVC   TITLE(35),=C'FUSION UNIVERSE ESTIMATE CONVERSION'                
         MVC   T.TITLE(35),=C'FUSION UNIVERSE ESTIMATE CONVERSION'              
*                                                                               
         MVC   P(24),=C'VALIDATING CONTROL CARDS'                               
         GOTO1 VPRINTER                                                         
         MVC   T.P(24),=C'VALIDATING CONTROL CARDS'                             
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
*                                                                               
READCARD DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD                                                   
         GOTO1 VPRINTER            PRINT THE CONTROL CARD                       
         MVC   T.P(L'CARD),CARD                                                 
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    OPENUE              EOF                                          
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    READCARD            YES: IGNORE                                  
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   *+18                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      DDSIO= OVERRIDE                              
         B     READCARD                                                         
*                                                                               
         CLC   =C'DSPACE=',CARD                                                 
         BNE   *+18                                                             
         LA    RF,SSB                                                           
         MVC   SSODSPAC-SSOOFF(,RF),CARD+7   DSPACE= OVERRIDE                   
         B     READCARD                                                         
*                                                                               
         CLC   =C'SYSCODE=',CARD   SYSCODE= CARD?                               
         BNE   RDCARD10            NO                                           
         CLC   =C'ALL ',CARD+8     SYSCODE=ALL?                                 
         BE    READCARD            YES: NO SYSCODE FILTER                       
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)  VALIDATE THE SYSCODE #             
         CLI   DMCB,0              VALID NUMERIC?                               
         BNE   INVCARD             YES                                          
         MVC   SYSCFILT,DMCB+6     SAVE THE SYSCODE FILTER                      
         B     READCARD                                                         
*                                                                               
RDCARD10 DS    0H                                                               
         CLC   =C'REPLACE=',CARD   REPLACE= CARD?                               
         BNE   INVCARD             NO: INVALID CONTROL CARD                     
         CLC   =C'NO ',CARD+8      REPLACE=NO?                                  
         BE    READCARD            THAT'S THE DEFAULT                           
         CLC   =C'YES ',CARD+8     REPLACE=YES?                                 
         BNE   INVCARD             NO: INVALID OPTION                           
         MVI   REPLACEG,C'Y'       REPLACE THE "G" DIRECTORY RECORDS            
         B     READCARD                                                         
*                                                                               
INVCARD  MVC   T.P(34),=C'*** ERROR *** INVALID CONTROL CARD'                   
         GOTO1 VPRINTER2                                                        
         B     BAD                                                              
*                                                                               
OPENUE   DS    0H                                                               
         MVC   T.P(26),=C'CARD VALIDATION SUCCESSFUL'                           
         GOTO1 VPRINTER2                                                        
         MVI   T.P,0               SKIP A LINE                                  
         GOTO1 VPRINTER2                                                        
*                                                                               
         MVC   T.P(54),=C'*********************************************+        
               *********'                                                       
         GOTO1 VPRINTER2                                                        
         MVC   T.P(54),=C'*** FIND THE WORD "ERROR" TO SEE CRITICAL ERR+        
               ORS *****'                                                       
         GOTO1 VPRINTER2                                                        
         MVC   T.P(54),=C'*** FIND THE WORD "PREVIOUS" TO SEE IMPORTANT+        
                INFO ***'                                                       
         GOTO1 VPRINTER2                                                        
         MVC   T.P(54),=C'*********************************************+        
               *********'                                                       
         GOTO1 VPRINTER2                                                        
         MVI   T.P,0               SKIP A LINE                                  
         GOTO1 VPRINTER2                                                        
*                                                                               
         OPEN  UEIN                                                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'DEMO',                   +        
               =C'NCTFILE NDEMDIRRX'                                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,(X'80',RECCARD),(X'80',0)               
*                                                                               
M        USING T.P,T.MID1                                                       
         MVC   M.P_UE_YEAR,=C'YYYY'                                             
         MVC   M.P_UE_MONTH,=C'MM'                                              
         MVC   M.P_UE_DMA#,=C'DMA'                                              
         MVC   M.P_UE_SYSCODE,=C'SYSC'                                          
         MVC   M.P_UE_SOURCE,=C'  SOURCE  '                                     
         MVC   M.P_UE_SAMPLE_TYPE,=C'SAMPLE'                                    
         MVC   M.P_UE_NETWORK_ID,=C'NTWK#'                                      
         MVC   M.P_UE_AI_UE,=C'   AIUE   '                                      
         MVC   M.P_UE_CARRIAGE_UE,=C' CARRIAGE '                                
         DROP  M                                                                
*                                                                               
M        USING T.P,T.MID2                                                       
         MVC   M.P_UE_YEAR,=C'----'                                             
         MVC   M.P_UE_MONTH,=C'--'                                              
         MVC   M.P_UE_DMA#(4),=C'+400'                                          
         MVC   M.P_UE_SYSCODE,=C'----'                                          
         MVC   M.P_UE_SOURCE,=C'----------'                                     
         MVC   M.P_UE_SAMPLE_TYPE,=C'------'                                    
         MVC   M.P_UE_NETWORK_ID,=C'-----'                                      
         MVC   M.P_UE_AI_UE,=C'----------'                                      
         MVC   M.P_UE_CARRIAGE_UE,=C'----------'                                
         DROP  M                                                                
*                                                                               
NXTUEREC DS    0H                                                               
         GET   UEIN,IREC                                                        
*                                                                               
         USING UERECD,IREC                                                      
         MVC   T.P_UE_YEAR,UE_YEAR                                              
         MVC   T.P_UE_MONTH,UE_MONTH                                            
         MVC   T.P_UE_DMA#,UE_DMA#                                              
         MVC   T.P_UE_SYSCODE,UE_SYSCODE                                        
         MVC   T.P_UE_SOURCE(L'UE_SOURCE),UE_SOURCE                             
         MVC   T.P_UE_SAMPLE_TYPE(L'UE_SAMPLE_TYPE),UE_SAMPLE_TYPE              
         MVC   T.P_UE_NETWORK_ID,UE_NETWORK_ID                                  
         CLC   T.P_UE_NETWORK_ID,=C'00000' SYSTEM-LEVEL UE?                     
         BE    *+16                       YES: PRINT AS ALL ZEROES              
         CLI   T.P_UE_NETWORK_ID,C'0' DISTRIBUTOR CODE > 9999 ?                 
         BNE   *+8                                                              
         MVI   T.P_UE_NETWORK_ID,C' ' NO: DON'T PRINT 1ST LEADING ZERO          
         MVC   T.P_UE_AI_UE,UE_AI_UE                                            
         MVC   T.P_UE_CARRIAGE_UE,UE_CARRIAGE_UE                                
         GOTO1 VPRINTER2                                                        
*                                                                               
         LA    R3,OREC                                                          
         USING DFUEKEY,R3                                                       
         XC    OREC(DFUERCLQ),OREC CLEAR POINTER BEFORE BUILDING IT             
         MVC   ORECRDW,=H'27'      23-BYTE POINTER + 4 FOR RDW LENGTH           
*                                                                               
         MVI   DFUECODE,DFUECODQ   RECORD TYPE 'G'                              
         MVI   DFUEMED,C'T'        MEDIA: TV                                    
         MVI   DFUESRC,C'F'        SOURCE: FUSION                               
         MVC   DUB(4),UE_YEAR      BOOK IN YYYYMM FORMAT                        
         MVC   DUB+4(2),UE_MONTH                                                
         MVC   DUB+6(2),=C'01'     FORCE FIRST DAY OF MONTH FOR DATCON          
         GOTO1 =V(DATCON),DMCB,(9,DUB),(3,THREE)                                
         MVC   DFUEBOOK,THREE      BINARY YEAR/MONTH                            
         XC    DFUEBOOK,=X'FFFF'                                                
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,UE_DMA#,L'UE_DMA#                                
         L     R0,DMCB+4           GIVEN DMA #                                  
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'DMA # NOT NUMERIC'                               
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         SHI   R0,400              NCC'S DMA#S ARE +400 FROM NIELSEN'S          
         CHI   R0,124              IS THIS NIELSEN'S NEW ATLANTA DMA#?          
         BNE   *+8                                                              
         LHI   R0,168              YES: USE NIELSEN'S OLD ATLANTA DMA#          
         STCM  R0,3,DFUEMKT        SAVE THE DMA#                                
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,UE_SYSCODE,L'UE_SYSCODE                          
         ICM   R0,15,DMCB+4        GIVEN SYSCODE #                              
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'SYSCODE # NOT NUMERIC'                           
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
*                                                                               
         OC    SYSCFILT,SYSCFILT   ANY SYSCODE FILTER PRESENT?                  
         BZ    *+12                NO                                           
         CH    R0,SYSCFILT         YES: HONOR IT                                
         BNE   UE900                                                            
*                                                                               
         STCM  R0,3,DFUESYSC       SYSCODE #                                    
*                                                                               
         CLC   UE_NETWORK_ID,=C'00000'  SYSTEM-LEVEL UE?                        
         BE    CHKUEVAL            YES: FIELD WILL CONTAIN NULLS                
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,UE_NETWORK_ID,L'UE_NETWORK_ID                    
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'NON-NUMERIC NETWORK CODE'                        
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
*                                                                               
         MVI   DFUESTN+4,C'T'      ALWAYS FORCE MEDIA 'T'                       
         ICM   R0,15,DMCB+4        NETWORK (DISTRIBUTOR) CODE                   
         CHI   R0,9999             CODE < 9999 ?                                
         BNH   *+12                                                             
         STCM  R0,15,DFUESTN       YES: STORE IN BINARY                         
         B     *+10                                                             
         MVC   DFUESTN(4),UE_NETWORK_ID+1  ELSE EBCDIC NETWORK ID               
*                                                                               
CHKUEVAL DS    0H                                                               
         CLC   =C'00',UE_AI_UE     1ST 2 DIGITS OF AIUE ZEROES?                 
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'AIUE TOO HIGH'                                   
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         GOTO1 =V(NUMVAL),DMCB,UE_AI_UE,L'UE_AI_UE                              
         ICM   R0,15,DMCB+4        GIVEN AIUE VALUE                             
         BNZ   *+18                CAN NEVER BE ZERO                            
         MVC   T.PERRWHY,=CL36'AIUE = ZERO'                                     
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'AIUE NOT NUMERIC'                                
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         STCM  R0,7,DFUEAIUE       SAVE AD-INSERTABLE UNIVERSE ESTIMATE         
*                                                                               
         MVI   UPSTATUS,X'40'      EXTENDED PASSIVE POINTER                     
*                                                                               
         CLI   UE_SAMPLE_TYPE,C' ' SAMPLE TYPE FIELD IS PRESENT?                
         BH    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'DMA SAMPLE TYPE FIELD MISSING'                   
         BAS   RE,BADREC                                                        
         B     UE900                                                            
*                                                                               
         CLI   UE_SAMPLE_TYPE,UE_SAMPLE_TYPE_TOTAL_DMA                          
         BNE   *+8                                                              
         OI    UPEXFLGS,DFUEFLGS_TOTAL_DMA  YES: SET TOTAL DMA FLAG             
*                                                                               
* INSERT CODE HERE TO SET THE SOURCE VALUE, IF IT'S EVER ANYTHING               
* OTHER THAN NIELSEN. SINCE NIELSEN GETS A VALUE OF ZERO, WE DON'T              
* NEED TO DO ANYTHING YET. THE X'70' BITS OF DFUEFLGS ARE RESERVED FOR          
* THIS PURPOSE, LEAVING ROOM FOR 8 POSSIBLE SOURCES.                            
*                                                                               
         CLC   =C'00',UE_CARRIAGE_UE  1ST 2 DIGITS OF CARRIAGE UE 0 ?           
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'CARRIAGE UE TOO HIGH'                            
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         GOTO1 =V(NUMVAL),DMCB,UE_CARRIAGE_UE,L'UE_CARRIAGE_UE                  
         ICM   R0,15,DMCB+4        GIVEN CARRIAGE UE VALUE                      
         BNZ   *+18                CAN NEVER BE ZERO                            
         MVC   T.PERRWHY,=CL36'CARRIAGE UE = ZERO'                              
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'CARRIAGE UE NOT NUMERIC'                         
         BAS   RE,BADREC                                                        
         B     UE900               SKIP THIS RECORD                             
         STCM  R0,7,UPEXCAUE       SAVE CARRIAGE UNIVERSE ESTIMATE              
*                                                                               
         MVI   DIFFFLAG,C'N'       ASSUME THAT PARTIAL KEY WON'T CHANGE         
*                                                                               
** CHECK FOR A MATCH ON THE PARTIAL KEY (WITHOUT THE AIUE VALUE).               
** NOTE THAT WE AREN'T READING FOR DELETES HERE, BUT THAT SHOULDN'T             
** MATTER.                                                                      
**                                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(DFUEAIUE-DFUEKEY),OREC   READHI FOR *PARTIAL* KEY            
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'DEMDIR',KEY,KEY               
         CLI   DMCB+8,0            RECORD FOUND?                                
         BE    *+6                 YES                                          
         DC    H'0'                FATAL DATAMGR ERROR                          
*                                                                               
OLD      USING DFUEKEY,KEY                                                      
         CLC   KEY(DFUEAIUE-DFUEKEY),OREC   PARTIAL KEY ALREADY THERE?          
         BE    CHKKEY              YES: CHECK IT OUT FURTHER                    
*                                                                               
         AP    UERECS#A,=P'1'      NO: INCREMENT "RECORD ADDED" COUNTER         
         MVC   T.PERRPRFX,=C'*** PREVIOUS RECORD WILL BE ADDED        '         
         GOTO1 VPRINTER2                                                        
         B     PUTSRTG             IT'S SAFE TO ADD THE NEW RECORD              
*                                                                               
CHKKEY   DS    0H                                                               
         CLC   OLD.DFUEAIUE,DFUEAIUE  COMPARE AIUE VALUES                       
         BNE   DIFF                                                             
         CLC   OLD.DFUECAUE,UPEXCAUE  COMPARE CARRIAGE UE VALUES                
         BNE   DIFF                                                             
         CLC   OLD.DFUEFLGS,UPEXFLGS  COMPARE FLAG BYTE                         
         BNE   DIFF                                                             
*                                                                               
         AP    UERECS#R,=P'1'      INCREMENT "SKIPPED" COUNTER                  
         B     UE900               RECORD IS UNCHANGED: IGNORE IT               
*                                                                               
DIFF     DS    0H                                                               
         BC    0,SKIPHEAD                                                       
         MVI   *-3,X'F0'           *** SELF-MODIFYING CODE ***                  
*                                                                               
* //DIFFS DD SYSOUT=*                                                           
         GOTO1 =V(DYNALLOC),DMCB,(X'FD',=CL8'DIFFS'),SPACES                     
*                                                                               
         OPEN  (DIFFS,(OUTPUT))                                                 
*                                                                               
         MVC   CARD(5),=C'SEP=|'                                                
         PUT   DIFFS,CARD                                                       
         MVC   CARD,=CL80'YEAR|MON|MKT|SYSC|NTWK|OLD SAMP.|NEW SAMP.|OL+        
               D AIUE|NEW AIUE|OLD CARR.|NEW CARR.'                             
         PUT   DIFFS,CARD                                                       
*                                                                               
         MVC   P(54),=C'** SEE DIFFS IN SPOOLED JOB OUTPUT FOR MORE DET+        
               AILS **'                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
SKIPHEAD DS    0H                                                               
         MVC   THREE(2),DFUEBOOK                                                
         XC    THREE(2),=X'FFFF'                                                
         MVI   THREE+2,1           DAY NUMBER FOR DATCON                        
         GOTO1 =V(DATCON),DMCB,(3,THREE),(20,DUB)                               
         MVC   D_UE_BOOK_YEAR,DUB                                               
         MVC   D_UE_BOOK_MONTH,DUB+4                                            
         EDIT  DFUEMKT,D_UE_DMA#                                                
         EDIT  DFUESYSC,D_UE_SYSCODE                                            
*                                  DECODE NETWORK # CORRECTLY                   
         IF  (CLI,DFUESTN,GE,C'0')  IF NETWORK# IS CHARACTERS...                
           EDIT  (C4,DFUESTN),D_UE_NETWORK_ID  ...THEN IT'S <= 9999             
         ELSE  ,                    O/W IT'S STORED IN BINARY (OR 0)            
           EDIT  (B4,DFUESTN),D_UE_NETWORK_ID,ZERO=NOBLANK                      
         ENDIF ,                                                                
*                                                                               
         MVI   D_OLD_SAMPLE_TYPE,UE_SAMPLE_TYPE_WIRED                           
         TM    OLD.DFUEFLGS,DFUEFLGS_TOTAL_DMA                                  
         BNO   *+8                                                              
         MVI   D_OLD_SAMPLE_TYPE,UE_SAMPLE_TYPE_TOTAL_DMA                       
         MVI   D_NEW_SAMPLE_TYPE,UE_SAMPLE_TYPE_WIRED                           
         TM    UPEXFLGS,DFUEFLGS_TOTAL_DMA                                      
         BNO   *+8                                                              
         MVI   D_NEW_SAMPLE_TYPE,UE_SAMPLE_TYPE_TOTAL_DMA                       
         EDIT  OLD.DFUEAIUE,D_OLD_UE_AI_UE                                      
         EDIT  DFUEAIUE,D_NEW_UE_AI_UE                                          
         EDIT  OLD.DFUECAUE,D_OLD_UE_CARRIAGE_UE                                
         EDIT  UPEXCAUE,D_NEW_UE_CARRIAGE_UE                                    
         PUT   DIFFS,DIFFREC                                                    
*                                                                               
         CLI   REPLACEG,C'Y'       REPLACE=YES?                                 
         BE    *+14                                                             
         AP    UERECS#R,=P'1'      NO: INCREMENT "SKIPPED" COUNTER              
         B     UE900               DON'T OVERWRITE ANYTHING                     
*                                                                               
         AP    UERECS#C,=P'1'      INCREMENT "RECORD CHANGED" COUNTER           
         MVC   T.PERRPRFX,=C'*** PREVIOUS RECORD WILL BE UPDATED      '         
         GOTO1 VPRINTER2                                                        
*                                                                               
         MVI   DIFFFLAG,C'Y'       THIS PARTIAL KEY HAS CHANGED                 
*                                                                               
PUTSRTG  DS    0H                                                               
*                                                                               
* PUT THE CONSTRUCTED AIUE RECORD TO SORTER.                                    
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',ORECRDW                                  
*                                                                               
         CLI   DIFFFLAG,C'Y'       DID AN EXISTING PARTIAL KEY CHANGE?          
         BNE   UE900               NO WORRIES: IT'S A NEW PARTIAL KEY           
*                                                                               
         CLC   OLD.DFUEAIUE,DFUEAIUE  IT CHANGED: COMPARE AIUE VALUES           
         BE    UE900               THEY MATCH: NOTHING ELSE TO DO               
*                                                                               
* THE AIUE VALUE HAS CHANGED, MEANING THAT IT ISN'T SUFFICIENT TO               
* SIMPLY WRITE THE UPDATED AIUE RECORD. WE MUST ALSO GENERATE AN AIUE           
* RECORD WITH THE *OLD* AIUE VALUE, MARK IT FOR DELETION, AND WRITE             
* THAT RECORD AS WELL.                                                          
*                                                                               
         MVC   DFUEAIUE,OLD.DFUEAIUE  GET RID OF THE OLD AIUE                   
         MVI   UPEXFLGS,0          FLAGS ARE IRRELEVANT                         
         XC    UPEXCAUE,UPEXCAUE   CARRIAGE UE IS IRRELEVANT                    
         MVC   UPRECLEN,=X'FFFF'   MARK AS PASSIVE (RECLEN = X'FFFF')           
         MVI   UPSTATUS,X'80'      MARK FOR DELETION                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',ORECRDW                                  
*                                                                               
         AP    UERECS#D,=P'1'      INCREMENT "RECORD DELETED" COUNTER           
*                                                                               
         MVC   T.PERRPRFX,=C'*** OLD PREVIOUS RECORD WILL BE DELETED  '         
         GOTO1 VPRINTER2                                                        
*                                                                               
UE900    DS    0H                                                               
         B     NXTUEREC                                                         
*                                                                               
EOFUEIN  DS    0H                                                               
         CLOSE UEIN                                                             
         CLOSE DIFFS                                                            
*                                                                               
         DROP  R3                                                               
         DROP  OLD                                                              
*                                                                               
         OPEN  (UEOUT,(OUTPUT))                                                 
*                                                                               
GETSORTR DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    ENDSORTR                                                         
*                                                                               
         PUT   UEOUT,(R2)                                                       
         B     GETSORTR                                                         
*                                                                               
ENDSORTR DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (UEOUT,)                                                         
         EJECT                                                                  
         OPEN  MSOIN                                                            
*                                                                               
         GOTO1 =V(SORTER),DMCB,SRTCARDB,(X'80',RECCARDB),(X'80',0)              
*                                                                               
         MVC   T.LINE,=PL2'72'     FORCE PAGE EJECT                             
         MVC   T.MID1,T.SPACES     BUILD NEW MIDLINE 1                          
         MVC   T.MID2,T.SPACES     BUILD NEW MIDLINE 2                          
*                                                                               
M        USING T.P,T.MID1                                                       
         MVC   M.P_MSO_YEAR,=C'YYYY'                                            
         MVC   M.P_MSO_MONTH,=C'MM'                                             
         MVC   M.P_MSO_SYSCODE,=C'SYSC'                                         
         MVC   M.P_MSO_DMA#,=C'DMA'                                             
         MVC   M.P_MSO_SOURCE,=C'  SOURCE  '                                    
         MVC   M.P_MSO_SAMPLE_TYPE,=C'SAMPLE'                                   
         MVC   M.P_MSO_DMA_NAME,=C'----- DMA NAME -----'                        
         MVC   M.P_MSO_MSO_NAME,=C'------------- MSO NAME ------------'         
         MVC   M.P_MSO_SYSCODE_NAME,=C'------------- SYSCODE NAME -----+        
               --------'                                                        
         DROP  M                                                                
*                                                                               
M        USING T.P,T.MID2                                                       
         MVC   M.P_MSO_YEAR,=C'----'                                            
         MVC   M.P_MSO_MONTH,=C'--'                                             
         MVC   M.P_MSO_SYSCODE,=C'----'                                         
         MVC   M.P_MSO_DMA#(4),=C'+400'                                         
         MVC   M.P_MSO_SOURCE,=C'  ------  '                                    
         MVC   M.P_MSO_SAMPLE_TYPE,=C'------'                                   
         DROP  M                                                                
*                                                                               
NXTMSORC DS    0H                                                               
         GET   MSOIN,IREC                                                       
         USING MSORECD,IREC                                                     
*                                                                               
         MVC   T.P_MSO_YEAR,MSO_YEAR                                            
         MVC   T.P_MSO_MONTH,MSO_MONTH                                          
         MVC   T.P_MSO_SYSCODE,MSO_SYSCODE                                      
         MVC   T.P_MSO_DMA#,MSO_DMA#                                            
         MVC   T.P_MSO_SOURCE(L'MSO_SOURCE),MSO_SOURCE                          
         MVC   T.P_MSO_SAMPLE_TYPE(L'MSO_SAMPLE_TYPE),MSO_SAMPLE_TYPE           
         MVC   T.P_MSO_DMA_NAME,MSO_DMA_NAME                                    
         MVC   T.P_MSO_MSO_NAME,MSO_MSO_NAME                                    
         MVC   T.P_MSO_SYSCODE_NAME,MSO_SYSCODE_NAME                            
         GOTO1 VPRINTER2                                                        
*                                                                               
         USING DSYKEY,R3                                                        
         LA    R3,OREC             CLEAR OUTPUT RECORD                          
         XCEFL (R3),2000                                                        
*                                                                               
         MVI   DSYCODE,DSYCDEQU    'B' RECORD TYPE                              
         MVI   DSYMEDIA,C'T'       ALWAYS 'T'                                   
         MVI   DSYSRC,C'F'         ALWAYS 'F' (FUSION)                          
         MVC   DUB(4),MSO_YEAR     BOOK IN YYYYMM FORMAT                        
         MVC   DUB+4(2),MSO_MONTH                                               
         MVC   DUB+6(2),=C'01'     FORCE FIRST DAY OF MONTH FOR DATCON          
         GOTO1 =V(DATCON),DMCB,(9,DUB),(3,THREE)                                
         MVC   DSYBKEFF,THREE      BINARY YEAR/MONTH                            
         XC    DSYBKEFF,=X'FFFF'                                                
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,MSO_SYSCODE,L'MSO_SYSCODE                        
         ICM   R0,15,DMCB+4        GIVEN SYSCODE #                              
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'SYSCODE # NOT NUMERIC'                           
         BAS   RE,BADREC                                                        
         B     MSO900              SKIP THIS RECORD                             
         STCM  R0,3,DSYSYSCD       SYSCODE #                                    
*                                                                               
         LHI   R0,DSYRECLQ         RECORD LENGTH WITH NO DATA                   
         AHI   R0,1                ADD NULL AT EOR                              
         STCM  R0,3,DSYRLEN        RECORD LENGTH WITH NO ELEMENTS               
*                                                                               
         LA    R2,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING FDTELEM,R2                                                       
         MVI   FDTCODE,FDTCODEQ    ELEMENT CODE                                 
         MVI   FDTLEN,FDTLENEQ     ELEMENT LENGTH                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,FDTLDDTE)   LOAD DATE                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'DEMFIL'),OREC,ELEMENT,0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING FAUELEM,R2                                                       
         MVI   FAUCODE,FAUCODEQ    ELEMENT CODE                                 
         MVI   FAULEN,FAULENEQ     ELEMENT LENGTH                               
         MVC   FAUMSONM,MSO_MSO_NAME        MSO NAME                            
         MVC   FAUSYSNM,MSO_SYSCODE_NAME    SYSCODE NAME                        
         MVC   FAUMKTNM,MSO_DMA_NAME        MARKET NAME                         
*                                                                               
         CLI   MSO_SAMPLE_TYPE,MSO_SAMPLE_TYPE_TOTAL_DMA                        
         BNE   *+8                                                              
         MVI   FAUFLAGS,FAUFLAGS_TOTAL_DMA  YES: SET TOTAL DMA FLAG             
*                                                                               
* INSERT CODE HERE TO SET THE SOURCE VALUE, IF IT'S EVER ANYTHING               
* OTHER THAN NIELSEN. SINCE NIELSEN GETS A VALUE OF ZERO, WE DON'T              
* NEED TO DO ANYTHING YET. FIELD FAUSRCE IS RESERVED FOR THIS.                  
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,MSO_DMA#,L'MSO_DMA#                              
         L     R0,DMCB+4           GIVEN DMA #                                  
         CLI   DMCB,0              VALID NUMERIC?                               
         BE    *+18                YES                                          
         MVC   T.PERRWHY,=CL36'DMA # NOT NUMERIC'                               
         BAS   RE,BADREC                                                        
         B     MSO900              SKIP THIS RECORD                             
         SHI   R0,400              NCC'S DMA#S ARE +400 FROM NIELSEN'S          
         CHI   R0,124              IS THIS NIELSEN'S NEW ATLANTA DMA#?          
         BNE   *+8                                                              
         LHI   R0,168              YES: USE NIELSEN'S OLD ATLANTA DMA#          
         STCM  R0,3,FAUMKT#        SAVE THE DMA#                                
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'DEMFIL'),OREC,ELEMENT,0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DSYRLEN        RECORD LENGTH                                
         AHI   R0,4                PLUS 4 FOR RDW                               
         STH   R0,ORECRDW          SAVE RDW                                     
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',ORECRDW                                  
         AP    MSORECS#,=P'1'      INCREMENT MSO RECORD COUNT                   
*                                                                               
MSO900   DS    0H                                                               
         B     NXTMSORC                                                         
*                                                                               
EOFMSOIN DS    0H                                                               
         CLOSE MSOIN                                                            
         DROP  R3                                                               
*                                                                               
         OPEN  (MSOOUT,(OUTPUT))                                                
*                                                                               
GETSORTB DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    ENDSORTB                                                         
*                                                                               
         PUT   MSOOUT,(R2)                                                      
         B     GETSORTB                                                         
*                                                                               
ENDSORTB DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (MSOOUT,)                                                        
*                                                                               
         MVC   P(59),=C'** SEE UETRACE AND/OR SHOWERRS IN SPOOL FOR MOR+        
               E DETAILS **'                                                    
         GOTO1 VPRINTER                                                         
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(39),=C'NCC FUSION UE (GTF)  RECORDS CREATED = '                
         EDIT  UERECS#A,(15,P+39),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
         MVC   T.P(39),=C'NCC FUSION UE (GTF)  RECORDS CREATED = '              
         EDIT  UERECS#A,(15,T.P+39),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         MVC   P(39),=C'NCC FUSION UE (GTF)  RECORDS DELETED = '                
         EDIT  UERECS#D,(15,P+39),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
         MVC   T.P(39),=C'NCC FUSION UE (GTF)  RECORDS DELETED = '              
         EDIT  UERECS#D,(15,T.P+39),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         MVC   P(39),=C'NCC FUSION UE (GTF)  RECORDS CHANGED = '                
         EDIT  UERECS#C,(15,P+39),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
         MVC   T.P(39),=C'NCC FUSION UE (GTF)  RECORDS CHANGED = '              
         EDIT  UERECS#C,(15,T.P+39),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         MVC   P(39),=C'NCC FUSION UE (GTF)  RECORDS SKIPPED = '                
         EDIT  UERECS#R,(15,P+39),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
         MVC   T.P(39),=C'NCC FUSION UE (GTF)  RECORDS SKIPPED = '              
         EDIT  UERECS#R,(15,T.P+39),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         MVC   P(39),=C'NCC FUSION MSO (BTF) RECORDS CREATED = '                
         EDIT  MSORECS#,(15,P+39),COMMAS=YES,ZERO=NOBLANK                       
         GOTO1 VPRINTER                                                         
         MVC   T.P(39),=C'NCC FUSION MSO (BTF) RECORDS CREATED = '              
         EDIT  MSORECS#,(15,T.P+39),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
*                                                                               
         CLI   ERRFLAG,C'Y'        ANY ERRORS FOUND?                            
         BE    BAD                 YES                                          
         MVC   P(15),=C'NO ERRORS FOUND'                                        
         GOTO1 VPRINTER                                                         
         MVC   T.P(15),=C'NO ERRORS FOUND'                                      
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         SR    R3,R3               DEFAULT RETURN CODE IS ZERO                  
         B     BYE                                                              
*                                                                               
BAD      DS    0H                                                               
         MVC   P(69),=C'*** ERRORS FOUND *** SEE SHOWERRS AND/OR UETRAC+        
               E IN SPOOL FOR DETAILS'                                          
         GOTO1 VPRINTER                                                         
         MVC   T.P(20),=C'*** ERRORS FOUND ***'                                 
         GOTO1 VPRINTER2           PRINT TO SYSPRINT LOG DATASET                
         LHI   R3,8                CONVERSION ERROR(S) OCCURRED: SET RC         
*                                                                               
BYE      DS    0H                                                               
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'     CLOSE UETRACE                       
*                                                                               
* WE DON'T WANT TO RUN THE UPDATE IF ANY ERRORS ARE FOUND !                     
*                                                                               
         XBASE RC=(R3)             RC = 0 IS OKAY, RC = 8 IS FATAL              
         EJECT                                                                  
BADREC   NTR1                                                                   
*                                                                               
         MVI   ERRFLAG,C'Y'        SO WE SEND AN E-MAIL WARNING LATER           
         MVC   T.PERRPRFX,=C'*** ERROR: PREVIOUS RECORD IS INVALID ***'         
         GOTO1 VPRINTER2                                                        
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
VPRINTER DC    V(PRINTER)                                                       
VPRINTER2 DS   0V                                                               
         DC    X'01'               FORCE SYSPRINT TO SYSPRIN2                   
         DC    VL3(PRINTER)                                                     
         SPACE 3                                                                
UEIN     DCB   DDNAME=UEIN,DSORG=PS,MACRF=GM,RECFM=FB,                 +        
               BLKSIZE=0,EODAD=EOFUEIN                                          
MSOIN    DCB   DDNAME=MSOIN,DSORG=PS,MACRF=GM,RECFM=FB,                +        
               BLKSIZE=0,EODAD=EOFMSOIN                                         
UEOUT    DCB   DDNAME=UEOUT,DSORG=PS,RECFM=VB,LRECL=2000,BLKSIZE=8200, +        
               MACRF=PM                                                         
MSOOUT   DCB   DDNAME=MSOOUT,DSORG=PS,RECFM=VB,LRECL=2000,BLKSIZE=8200,+        
               MACRF=PM                                                         
DIFFS    DCB   DDNAME=DIFFS,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM                 
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,18,BI,A)'                                    
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=27'                                    
SRTCARDB DC    CL80'SORT FIELDS=(5,20,A),FORMAT=BI'                             
RECCARDB DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
         SPACE 3                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
WORK     DS    XL17                                                             
CARD     DS    CL80                SYSIN PARAMETER CARD                         
UERECS#A DC    PL8'0'              NUMBER OF UE RECORDS GENERATED               
UERECS#R DC    PL8'0'              NUMBER OF DUPLICATE UE RECS SKIPPED          
UERECS#D DC    PL8'0'              NUMBER OF OVERRIDDEN UE RECS DELETED         
UERECS#C DC    PL8'0'              NUMBER OF OVERRIDDEN UE RECS CHANGED         
MSORECS# DC    PL8'0'              NUMBER OF MSO RECORDS GENERATED              
ERRFLAG  DC    C'N'                ASSUME NO ERRORS WILL OCCUR                  
REPLACEG DC    C'N'                ASSUME NO REPLACEMENT OF "G" KEYS            
DIFFFLAG DS    C                   'Y' = THIS PARTIAL KEY HAS CHANGED           
SYSCFILT DC    H'0'                SYSCODE FILTER (IF NON-ZERO)                 
THREE    DS    XL3                                                              
KEY      DS    XL23                DEMDIR KEY                                   
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMO SYSTEM                          
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL16'****DETFUESSB***'                                           
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         SPACE 3                                                                
DIFFREC  DC    CL80' '                                                          
         ORG   DIFFREC                                                          
D_UE_BOOK_YEAR       DS CL4        BOOK (YYYY)                                  
                     DC C'|'                                                    
D_UE_BOOK_MONTH      DS CL2        BOOK (MM)                                    
                     DC C'|'                                                    
D_UE_DMA#            DS CL3        NIELSEN DMA NUMBER (+400)                    
                     DC C'|'                                                    
D_UE_SYSCODE         DS CL4        SYSCODE                                      
                     DC C'|'                                                    
D_UE_NETWORK_ID      DS CL5        NETWORK ID                                   
                     DC C'|'                                                    
D_OLD_SAMPLE_TYPE    DS CL1        OLD (TOTAL DMA VS. WIRED)                    
                     DC C'|'                                                    
D_NEW_SAMPLE_TYPE    DS CL1        NEW (TOTAL DMA VS. WIRED)                    
                     DC C'|'                                                    
D_OLD_UE_AI_UE       DS CL10       OLD AD-INSERTABLE UNIVERSE ESTIMATE          
                     DC C'|'                                                    
D_NEW_UE_AI_UE       DS CL10       NEW AD-INSERTABLE UNIVERSE ESTIMATE          
                     DC C'|'                                                    
D_OLD_UE_CARRIAGE_UE DS CL10       OLD CARRIAGE UNIVERSE ESTIMATE               
                     DC C'|'                                                    
D_NEW_UE_CARRIAGE_UE DS CL10       NEW CARRIAGE UNIVERSE ESTIMATE               
         ORG                                                                    
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'**IREC**'                                                      
IREC     DS    CL(MSOLENQ)         MUST BE >= LARGEST LRECL                     
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'**OREC**'                                                      
ORECRDW  DS    H                                                                
         DC    H'0'                                                             
OREC     DS    2000X               DEMDIR EXTENDED PASSIVE KEY                  
         SPACE 3                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE DETFUERECD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
P_UE_YEAR          DS CL4          YEAR                                         
         DS C                                                                   
P_UE_MONTH         DS CL2          MONTH                                        
         DS C                                                                   
P_UE_DMA#          DS CL3          NIELSEN DMA NUMBER (+400)                    
         DS C                                                                   
P_UE_SYSCODE       DS CL4          SYSCODE                                      
         DS C                                                                   
P_UE_SOURCE        DS CL10         SOURCE                                       
         DS C                                                                   
P_UE_SAMPLE_TYPE   DS CL6          SAMPLE TYPE ("W" OR "D")                     
         DS C                                                                   
P_UE_NETWORK_ID    DS CL5          NETWORK ID                                   
         DS C                                                                   
P_UE_AI_UE         DS CL10         AD-INSERTABLE UNIVERSE ESTIMATE              
         DS C                                                                   
P_UE_CARRIAGE_UE   DS CL10         CARRIAGE UNIVERSE ESTIMATE                   
*                                                                               
         ORG   P                                                                
P_MSO_YEAR         DS CL4          YEAR                                         
         DS C                                                                   
P_MSO_MONTH        DS CL2          MONTH                                        
         DS C                                                                   
P_MSO_SYSCODE      DS CL4          SYSCODE                                      
         DS C                                                                   
P_MSO_DMA#         DS CL3          NIELSEN DMA NUMBER (+400)                    
         DS C                                                                   
P_MSO_SOURCE       DS CL10         SOURCE                                       
         DS C                                                                   
P_MSO_SAMPLE_TYPE  DS CL6          SAMPLE TYPE ("W" OR "D")                     
         DS C                                                                   
P_MSO_DMA_NAME     DS CL20         DMA MAME                                     
         DS C                                                                   
P_MSO_MSO_NAME     DS CL35         MSO NAME (CAN'T FIT ALL 40 BYTES)            
         DS C                                                                   
P_MSO_SYSCODE_NAME DS CL40         SYSCODE NAME                                 
*                                                                               
         ORG   P                                                                
PERRPRFX DC    C'*** ERROR: PREVIOUS RECORD IS INVALID ***'                     
         DS    C                                                                
PERRWHY  DS    CL36                ANY LONGER WOULDN'T BE VISIBLE...            
*                                  ...WITHOUT SCROLLING                         
         ORG                                                                    
         SPACE 3                                                                
* REDEFINE THE LAST 5 BYTES OF THE EXTENDED PASSIVE KEY AS REQUIRED FOR         
* INPUT TO THE DELDXMOD UPDATE PROCESS.                                         
*                                                                               
DFUEKEY  DSECT                                                                  
         ORG   DFUESTAT                                                         
UPEXFLGS DS    X                   VARIOUS (SEE DFUEFLGS IN DEDEMFILE)          
UPEXCAUE DS    0FL3                CARRIAGE UE                                  
         DS    X                                                                
UPRECLEN DS    XL2                 ONLY NEEDED FOR DELETIONS                    
UPSTATUS DS    X                   STATUS BYTE FOR UPDATE INPUT                 
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024DETFUE    04/05/19'                                      
         END                                                                    
