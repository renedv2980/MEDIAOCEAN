*          DATA SET STREPFXOU1 AT LEVEL 042 AS OF 06/04/99                      
*PHASE SPFX02C                                                                  
*INCLUDE BINSRCH2                                                               
*        TITLE 'STREPFXOU1 - RE-CREATE N TYPE PASSIVES'                         
         TITLE 'STREPFXOU1 - RE-CREATE N TYPE PASSIVES'                         
***********************************************************************         
*                                                                     *         
*        STREPFXOU1 - RE-CREATE N TYPE PASSIVE KEYS                   *         
*              READS IN ALL STATIONS FOR AGENCY                       *         
*              USES STAPACK TO PACK MARKET/STATION                    *         
*              BUILDS FILE OF NEW N TYPE PASSIVE KEYS                 *         
*                                                                     *         
*        THIS MODULE IS PART OF A SET TO COVER ALL SPOT MASTER RECORDS*         
*                                                                     *         
*        STREPFXOU1 - BUILDS LIST OF OLD AND NEW STATION NUMBERS      *         
*        STLDOU1    - STAFIL     RECORDS                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8,CLEAR=YES                                            
*                                                                               
         L     RA,0(R1)            ESTABLISH SPWORKD                            
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,RUNFRST        FIRST TIME FOR RUN                           
         BE    FXRUN1ST                                                         
*                                                                               
         CLI   MODE,REQFRST        FIRST TIME FOR REQUEST                       
         BE    FXREQ1ST                                                         
*                                                                               
         CLI   MODE,RUNLAST        LAST  TINE FOR RUN                           
         BE    FXRUNLST                                                         
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'STREPFXOU1 - RE-CREATE N TYPE PASSIVES- FXRUN1ST'               
***********************************************************************         
*                                                                     *         
*        INIT BINSRCH PARAMETERS                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FXRUN1ST DS    0H                                                               
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    RF,TABLE                                                         
         ST    RF,BSPATAB          A(TABLE)                                     
*                                                                               
         LA    RF,CDNRECLQ         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         LA    RF,CDNKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,CDNKEY-CDNREC DISPLACEMENT OF KEY                        
*                                                                               
         LHI   RF,CDNRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
FXRUN1SX DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'STREPFXOU1 - RE-CREATE N TYPE PASSIVES- FXREQ1ST'               
***********************************************************************         
*                                                                     *         
*        READ STATION FILE VIA 'S' POINTERS                           *         
*        ADD 'N' PASSIVE TO FILE VIA BINSRCH                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FXREQ1ST DS    0H                                                               
*                                                                               
*        BUILD STARTING STATION KEY                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
*                                                                               
         MVI   STAKTYPE,C'S'       WANT MASTER KEY                              
         MVC   STAKMED,QMED        SET REQUEST MEDIA                            
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
*                                                                               
         LA    R6,CDNRECC          ESTABLISH RECORD FOR CONVERSION FILE         
         USING CDNREC,R6                                                        
*                                                                               
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
*                                                                               
FXSTALP  DS    0H                                                               
*                                                                               
         L     R5,ADSTAT               ADSTAT-->RECORD FOUND.                   
*                                                                               
         CLC   STAKTYPE(STAKCALL-STAKEY),SAVEKEY DONE ON BREAK IN               
         BNE   FXSTADN             MEDIA                                        
*                                                                               
         CLC   STAKAGY,QAGY        MUST MATCH ON AGENCY                         
         BNE   FXSTACN                                                          
*                                                                               
*        BUILD CONVERSION FILE RECORD                                           
*                                                                               
         XC    CDNREC(CDNRECLQ),CDNREC INIT CONVERSION FILE RECORD              
*                                                                               
         MVI   CDNTYPE,CDNTYPEQ    SET PASSIVE KEY ID                           
         MVC   CDNMED,STAKMED      SET MEDIA                                    
*                                                                               
         MVC   STAKEYSV,STAKEY     SAVE CURRENT KEY                             
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK          ESTABLISH STAPACK PARAMETER LIST             
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'        PACK MARKET/STATION                          
         MVC   STAPAGY,QAGY        AGENCY                                       
         MVC   STAPCTRY,COUNTRY    COUNTRY                                      
         MVC   STAPMED,QMED        MEDIA                                        
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,SMKT       MARKET                                       
         MVC   STAPQSTA(8),STAKCALL STATION                                     
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
*                                                                               
         CLI   STAPERR,0           CHECK FOR ERROR                              
         BNE   FXSTAERR                                                         
*                                                                               
         MVC   CDNMKSTA,STAPMKST   RETURN RESULT                                
*                                                                               
         MVC   CDNAGY,STAKAGY                                                   
         MVC   CDNCLT,STAKCLT      SET CLIENT                                   
         MVC   CDNFILL,=16C'0'     SET FILL CHARACTER                           
         MVC   CDNRLEN,=AL2(20)                                                 
*                                                                               
         MVC   CDNSTA,STAKCALL     STATION                                      
         MVC   CDNMKT,SMKT         MARKET NUMBER                                
*                                                                               
*        ADD RECORD TO TABLE                                                    
*        PRINT RESULT                                                           
*                                                                               
         GOTO1 BINSRCH,BSPPRMS,('BSPADD',CDNREC) ADD TO TABLE                   
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FULL                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     FXSTAPRT                                                         
*                                                                               
FXSTAERR DS    0H                                                               
*                                                                               
         MVC   CDNREC(30),=CL30'MSPACK ERROR FOR STATION'                       
         MVC   CDNREC+30(32),STAWORK                                            
*                                                                               
FXSTAPRT DS    0H                                                               
*                                                                               
         MVC   P1(20),0(R5)        PRINT MASTER KEY                             
         MVC   P1+50(80),CDNREC    PRINT N PASSIVE                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,STAKEYSV        RESTORE CURRENT KEY                          
*                                                                               
         GOTO1 HIGHSTA             RESTORE FILE POINTERS                        
*                                                                               
FXSTACN DS     0H                                                               
*                                                                               
         GOTO1 SEQSTA              READ NEXT STATION ON FILE                    
*                                                                               
         B     FXSTALP                                                          
*                                                                               
FXSTADN  DS    0H                                                               
*                                                                               
         GOTO1 AENDREQ             STOP REQUEST                                 
*                                                                               
         TITLE 'STREPFXOU1 - RE-CREATE N TYPE PASSIVES- FXRUNLST'               
***********************************************************************         
*                                                                     *         
*        AT EOF WRITE TABLE OF PASSIVES TO OUTPUT                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FXRUNLST DS    0H                                                               
*                                                                               
         L     R4,BSPNOR           GET NUMBER OF RECORDS IN TABLE               
*                                                                               
         SR    R2,R2                                                            
*                                                                               
         L     R6,BSPATAB          POINT TO START OF TABLE                      
         USING CDNREC,R6           ESTABLISH TABLE ENTRY                        
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))  OPEN CONVERSION FILE                         
*                                                                               
FXTABLP  DS    0H                                                               
*                                                                               
         LHI   RF,CDNKEYLQ         GET OUTPUT RECORD LENGTH                     
         AHI   RF,4                ADD ON VARIABLE BLOCK OVERHEAD               
         STCM  RF,3,RRLEN          SET OUTPUT RECORD LENGTH                     
*                                                                               
         MVC   NEWREC(CDNKEYLQ),CDNKEY   COPY KEY PART OF TABLE ENTRY           
*                                                                               
         PUT   FILEOUT,RRLEN       ADD TO CONVERSION FILE                       
*                                                                               
FXTABCN  DS    0H                                                               
*                                                                               
         A     R6,BSPLENR          BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,FXTABLP                                                       
*                                                                               
FXTABDN  DS    0H                  END OF TABLE                                 
*                                                                               
         CLOSE FILEOUT             CLOSE CONVERSION FILE                        
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - FILEOUT'              
***********************************************************************         
*                                                                     *         
*        DCB FOR INPUT FILE CONTAINING STATIONS WITH OLD AND NEW      *         
*        NUMBERS                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF' HEX CONVERSION TABLE                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - WORKAREAS'            
***********************************************************************         
*                                                                     *         
*        WORKAREAS                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SAVEKEY  DS    XL32                                                             
STAKEYSV DS    XL32                                                             
CDNRECC  DS    XL(CDNRECLQ)        CONVERSION RECORD BUILDAREA                  
         DS    0F                                                               
FXMAX    DC    XL4'0000EFFF'       MAX SEQUENCE NUMBER                          
FXMIN    DC    XL4'00000100'       MIN SEQUENCE NUMBER                          
*                                                                               
STAWORK  DS    XL32                WORKAREA FOR STAPACK CALL                    
*                                                                               
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
         DS    0D                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                                                              
NEWREC   DS    XL256               OUTPUT RECORD BUILD AREA                     
*                                                                               
         DS    0D                                                               
TABLE    DS    5000XL(CDNRECLQ)   CONVERSION TABLE                              
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - CDNRECD'              
**********************************************************************          
*                                                                               
*        DSECT FOR NUMBER CONVERSION FILE                                       
*                                                                               
**********************************************************************          
         SPACE 2                                                                
CDNRECD  DSECT                                                                  
CDNREC   DS    0XL1                CONVERSION RECORD                            
CDNKEY   DS    0XL20               KEY FOR RECORD                               
CDNTYPE  DS    CL1                 RECORD TYPE                                  
CDNTYPEQ EQU   C'N'                N-TYPE PASSIVE                               
CDNAGY   DS    CL2                 AGENCY                                       
CDNMED   DS    CL1                 MEDIA                                        
CDNMKSTA DS    CL5                 MARKET/STATION PACKED                        
CDNCLT   DS    CL3                 CLT FOR EXCEPTIONS(DEFAULT IS '000')         
CDNFILL  DS    CL3                 C'000'                                       
CDNRLEN  DS    XL2                 RECORD LENGTH                                
         DS    XL3                 SPARE                                        
CDNKEYLQ EQU   *-CDNKEY            KEY LENGTH                                   
*                                                                               
CDNSTA   DS    CL5                 STATION CALL LETTERS                         
CDNMKT   DS    CL4                 MARKET                                       
         DS    XL(80-(*-CDNRECD))  SPARE                                        
CDNRECLQ EQU   *-CDNREC            RECORD LENGTH                                
*                                                                               
CDNRMAXQ EQU   10000               MAXIMUM NUMBER OF STATIONS                   
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - PLINED'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR PRINT LINE                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PLINED   DSECT                                                                  
PAGY     DS    XL2                 AGENCY                                       
         DS    XL2                                                              
PSTA     DS    CL5                 STATION CALL LETTERS WITH MEDIA              
         DS    XL2                                                              
POLDNUM  DS    XL4                 OLD 2 BYTE NUMBER                            
         DS    XL2                                                              
PNEWNUM  DS    XL4                 NEW 2 BYTE NUMBER                            
         DS    XL2                                                              
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - STARECD'              
**********************************************************************          
*                                                                               
*        FIXED-RECORDS DSECT                                                    
*                                                                               
**********************************************************************          
         SPACE 2                                                                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042STREPFXOU106/04/99'                                      
         END                                                                    
