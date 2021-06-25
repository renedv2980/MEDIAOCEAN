*          DATA SET STREPFXCDN AT LEVEL 023 AS OF 01/29/99                      
*PHASE SPFX02C                                                                  
*INCLUDE BINSRCH2                                                               
*        TITLE 'SPREPFXCDN - ASSIGN NEW STATION SEQUENCE NUMBERS'               
         TITLE 'SPREPFXCDN - ASSIGN NEW STATION SEQUENCE NUMBERS'               
***********************************************************************         
*                                                                     *         
*        SPREPFXCDN - ASSIGN NEW SEQUENCE NUMBERS TO CANADIAN STATIONS*         
*              READS IN ALL STATIONS FOR AGENCY                       *         
*              DIVIDES RANGE AVAILABLE BY TOTAL NUMBER OF STATIONS    *         
*              QUOTIENT DETERMINES SPACING OF NEW NUMBERS             *         
*              BUILDS FILE OF CALL LETTERS, OLD AND NEW SEQUENCE NMBRS*         
*                                                                     *         
*        THIS MODULE IS PART OF A SET TO COVER ALL SPOT RECORDS       *         
*                                                                     *         
*        STREPFXCDN - BUILDS LIST OF OLD AND NEW STATION NUMBERS      *         
*        SPLDEXTCDN - SPTDIR/FIL RECORDS                              *         
*        SXLDEXTCDN - XSPDIR/FIL RECORDS                              *         
*        STLDEXTCDN - TRFDIR/FIL RECORDS                              *         
*        STLDCDN    - STAFIL     RECORDS                              *         
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
         CLI   MODE,REQFRST        ONLY INTERESTED IN 1ST TIME CALL             
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'SPREPFXCDN - ASSIGN NEW STATION SEQUENCE NUMBERS- FX'           
***********************************************************************         
*                                                                     *         
*        INIT BINSRCH PARAMETERS                                      *         
*        READ STATION FILE VIA 'X' PASSIVE POINTERS                   *         
*        ADD STATION AND OLD SEQ NMUMBER TO FILE VIA BINSRCH          *         
*        AT EOF DETERMINE SPACING FOR STATION SEQ NUMBERS             *         
*        ADD EACH STATION WITH OLD AND NEW SEQ NMBRS TO OUTPUT FILE   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FX       DS    0H                                                               
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
*        BUILD STARTING STATION KEY                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
*                                                                               
         MVI   STXKTYPE,C'X'       WANT CANADIAN PASSIVE KEY                    
         MVC   STXKAGY,QAGY        SET AGENCY                                   
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
         CLC   STXKTYPE(STXKSTA-STAKEY),SAVEKEY DONE ON BREAK IN                
         BNE   FXSTADN             KEY TYPE OR AGENCY                           
*                                                                               
*        BUILD CONVERSION FILE RECORD                                           
*                                                                               
         MVC   CDNREC(CDNRECLQ),SPACES INIT CONVERSION FILE RECORD              
*                                                                               
         MVC   CDNAGY,STXKAGY      AGENCY                                       
         MVC   CDNSTA,STXKSTA      STATION                                      
         MVC   CDNAGYMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   CDNOLDNM,STXKNUM    OLD SEQUENCE NUMBER                          
*                                                                               
*        ADD RECORD TO TABLE                                                    
*                                                                               
         GOTO1 BINSRCH,BSPPRMS,('BSPADD',CDNREC) ADD TO TABLE                   
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FULL                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   BSPCNTL,BSPNF       OKAY IF NOT FOUND                            
         BE    FXSTACN                                                          
*                                  ELSE DUPLICATION - PRINT                     
*                                                                               
         LA    R1,P2               ESTABLISH PRINT LINE                         
         USING PLINED,R1                                                        
*                                                                               
         MVC   PSTA,CDNSTA         PRINT STATION                                
         MVC   PAGY,CDNAGY         AGENCY                                       
         UNPK  DUB(5),CDNOLDNM(3)  PRINT OLD SEQ NUMBER                         
         TR    DUB(4),HEXTAB-C'0'                                               
         MVC   POLDNUM,DUB                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
FXSTACN DS     0H                                                               
*                                                                               
         GOTO1 SEQSTA              READ NEXT STATION ON FILE                    
*                                                                               
         B     FXSTALP                                                          
*                                                                               
FXSTADN  DS    0H                                                               
*                                                                               
*        RENUMBER ALL OF THE STATIONS IN THE TABLE                              
*        ADD CONVERSION RECORD TO FILE                                          
*        PRINT RESULT                                                           
*                                                                               
         L     R4,BSPNOR           GET NUMBER OF RECORDS IN TABLE               
         AHI   R4,1                ADD ONE FOR GOOD MEASURE                     
*                                                                               
         SR    R2,R2                                                            
*                                                                               
         L     R3,FXMAX            MAXIMUM SEQ NUMBER                           
         S     R3,FXMIN            RANGE OF NUMBERS ALLOWED                     
         DR    R2,R4               R3 WILL HAVE NEW SPACING FOR NUMBERS         
*                                                                               
         BCTR  R4,0                GET BACK TO RECORD COUNT                     
*                                                                               
         L     R6,BSPATAB          POINT TO START OF TABLE                      
         USING CDNREC,R6           ESTABLISH TABLE ENTRY                        
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))  OPEN CONVERSION FILE                         
*                                                                               
         L     R2,FXMIN            INIT NEW SEQUENCE NUMBER                     
*                                                                               
FXTABLP  DS    0H                                                               
*                                                                               
         AR    R2,R3               NEW SEQUENCE NUMBER                          
         STCM  R2,3,CDNNEWNM       UPDATE TABLE ENTRY                           
*                                                                               
         PUT   FILEOUT,CDNREC      ADD TO CONVERSION FILE                       
*                                                                               
         LA    R1,P2               ESTABLISH PRINT LINE                         
         USING PLINED,R1                                                        
*                                                                               
         MVC   PSTA,CDNSTA         PRINT STATION                                
         MVC   PAGY,CDNAGY         AGENCY                                       
         UNPK  DUB(5),CDNOLDNM(3)  PRINT OLD SEQ NUMBER                         
         TR    DUB(4),HEXTAB-C'0'                                               
         MVC   POLDNUM,DUB                                                      
*                                                                               
         UNPK  DUB(5),CDNNEWNM(3)  PRINT NEW SEQ NUMBER                         
         TR    DUB(4),HEXTAB-C'0'                                               
         MVC   PNEWNUM,DUB                                                      
*                                                                               
         GOTO1 REPORT                                                           
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
FX2000   GOTO1 AENDREQ             STOP REQUEST                                 
*                                                                               
         TITLE 'STREPFXCDN - RENUMBER CANADIAN STATIONS - FILEOUT'              
***********************************************************************         
*                                                                     *         
*        DCB FOR INPUT FILE CONTAINING STATIONS WITH OLD AND NEW      *         
*        NUMBERS                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=FB,LRECL=80,MACRF=PM               
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
CDNRECC  DS    XL(CDNRECLQ)        CONVERSION RECORD BUILDAREA                  
         DS    0F                                                               
FXMAX    DC    XL4'0000EFFF'       MAX SEQUENCE NUMBER                          
FXMIN    DC    XL4'00000100'       MIN SEQUENCE NUMBER                          
*                                                                               
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
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
CDNKEY   DS    0XL1                KEY FOR RECORD                               
CDNAGY   DS    CL2                 AGENCY CODE                                  
CDNSTA   DS    CL5                 STATION CALL LETTERS WITH MEDIA              
CDNKEYLQ EQU   *-CDNKEY            KEY LENGTH                                   
*                                                                               
CDNAGYMD DS    XL1                 AGENCY/MEDIA                                 
CDNOLDNM DS    XL2                 OLD 2 BYTE NUMBER                            
CDNNEWNM DS    XL2                 NEW 2 BYTE NUMBER                            
         DS    XL(80-(*-CDNRECD))  SPARE                                        
CDNRECLQ EQU   *-CDNREC            RECORD LENGTH                                
*                                                                               
CDNRMAXQ EQU   5000                MAXIMUM NUMBER OF STATIONS                   
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
**PAN#1  DC    CL21'023STREPFXCDN01/29/99'                                      
         END                                                                    
