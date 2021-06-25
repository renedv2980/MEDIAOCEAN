*          DATA SET SPEZF40    AT LEVEL 004 AS OF 08/11/00                      
*PHASE T23040A                                                                  
*        TITLE 'T23040 - WRITE EZMMPCNV ERROR REPORT TO PRINT QUEUE'            
         TITLE 'T23040 - WRITE EZMMPCNV ERROR REPORT TO PRINT QUEUE'            
***********************************************************************         
*                                                                     *         
*  TITLE: T23040 - WRITE EZMMPCNV ERROR REPORT                        *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, PZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23040 - EZMMPCNV ERRORS - INITIALIZATION'                      
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
T23040   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3040**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
*                                                                               
         TITLE 'T23040 - EZMMPCNV ERRORS - MODE'                                
***********************************************************************         
*                                                                     *         
*        DETERMINE PROCESSING MODE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PREP                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T23040 - EZMMPCNV ERRORS - VKEY'                                
***********************************************************************         
*                                                                     *         
*        VKEY - VALIDATE KEY                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        INITIALIZE REMOTE AREA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRREMOT  EQU   *                                                                
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTDSC(L'TITLE),TITLE    SET REPORT ID                         
*                                                                               
         MVC   REMOTJID,=C'ERR'                                                 
*                                                                               
         OI    GENSTAT2,NOREQDET   DON'T PRINT REQUEST PAGE                     
*                                                                               
PRREMOTX EQU   *                                                                
*                                                                               
VKXIT    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         TITLE 'T23040 - EZMMPCNV ERRORS - PREP'                                
***********************************************************************         
*                                                                     *         
*        PRINT ERROR MESSAGES                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PREP     DS    0H                                                               
*                                                                               
*        OPEN ERROR TAPE                                                        
*                                                                               
PROPEN   DS    0H                                                               
*                                                                               
         L     R2,=A(ERRORDCB)     POINT TO TAPE DCB                            
         OPEN  ((R2),INPUT)        OPEN IT AS INPUT                             
*                                                                               
         LTR   RF,RF               CHECK FOR ERRORS                             
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROPENX  DS    0H                                                               
*                                                                               
         LM    R0,R1,=A(HEADING,HDHK)   SET PRINTING ADDRESSES                  
         A     R0,RELO                                                          
         ST    R0,SPECS                                                         
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     R3,=A(ERRORDCB)     POINT TO TAPE DCB                            
         LA    R2,ERRRECIN         POINT TO INPUT AREA                          
         XC    0(L'ERRRECIN,R2),0(R2)  CLEAR AREA                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOOP TO READ NEXT REPORT ON TAPE AND PROCESS                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRRPTLP  DS    0H                                                               
*                                                                               
         GET   (R3),(R2)           GET ERROR DESCRIPTION RECORD                 
*                                                                               
         USING ERRMSG,R2           ESTABLISH ERROR RECORD                       
*                                                                               
         CLI   LINE,45             CHECK FOR ROOM ON THE PAGE                   
         BNH   *+8                                                              
         MVI   LINE,100            FORCE NEW PAGE                               
*                                                                               
         MVC   PERRT,=C'**ERROR**'                                              
         MVC   PERRMSG,ERRMSG      PRINT ERROR MESSAGE                          
         MVC   PSQNT,=C'RECORD NO. ='                                           
         EDIT  (P8,ERRSEQ),(8,PSQN),ALIGN=LEFT  PRINT SQN                       
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         MVC   WRKFLDD,ERRFLDD     SAVE DISPLACEMENT                            
         MVC   WRKFLDL,ERRFLDL     SAVE FIELD LENGTH                            
         MVC   WRKID,ERRID         SAVE RECORD IN ERROR ID                      
*                                                                               
         GET   (R3),(R2)           GET A1 RECORD                                
*                                                                               
         MVC   PERRREC,ERRREC      PRINT FIRST HALF OF A1 RECORD                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         CLC   WRKID,=C'H1'        SKIP IF ERROR IN H1 RECORD                   
         BE    PRRPTERR                                                         
*                                                                               
         GET   (R3),(R2)           GET H1 RECORD                                
*                                                                               
         MVC   PERRREC,ERRREC      PRINT FIRST HALF OF H1 RECORD                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         MVC   PERRREC,ERRREC+128  PRINT SECOND HALF OF H1 RECORD               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         CLC   WRKID,=C'H2'        SKIP IF ERROR IN H2 RECORD                   
         BE    PRRPTERR                                                         
*                                                                               
         GET   (R3),(R2)           GET H2 RECORD                                
*                                                                               
         MVC   PERRREC,ERRREC      PRINT FIRST HALF OF H2 RECORD                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         MVC   PERRREC,ERRREC+128  PRINT SECOND HALF OF H2 RECORD               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
PRRPTERR DS    0H                                                               
*                                                                               
         GET   (R3),(R2)           GET RECORD IN ERROR                          
*                                                                               
         MVC   ERRSPACS,BLNKS      INIT PRINT AREA                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,WRKFLDD           GET FIELD DISPLACEMENT                    
         LA    RE,ERRSPACS(RE)        POINT TO SPOT IN RECORD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,WRKFLDL           GET FIELD LENGTH                          
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),=64C'X'        HIGHLIGHT ERROR                           
*                                                                               
         MVC   PERRREC,ERRREC      PRINT FIRST HALF OF RECORD                   
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         CLC   ERRSPACS(L'PERRREC),BLNKS SKIP LINE IF ALL SPACES                
         BE    PREP20                                                           
*                                                                               
         MVC   PERRREC,ERRSPACS    PRINT IDENTIFIER FOR ERROR                   
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
PREP20   DS    0H                                                               
*                                                                               
         MVC   PERRREC,ERRREC+128  PRINT SECOND HALF OF RECORD                  
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
         CLC   ERRSPACS+128(L'PERRREC),BLNKS SKIP LINE IF ALL SPACES            
         BE    PREP30                                                           
*                                                                               
         MVC   PERRREC,ERRSPACS+128    PRINT IDENTIFIER FOR ERROR               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT IT                                     
*                                                                               
PREP30   DS    0H                                                               
*                                                                               
         XC    P,P                                                              
         MVI   P,X'41'             FORCE A SEPARATION LINE                      
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT SEPARATION LINE                        
*                                                                               
PRRPTCN  DS    0H                                                               
         B     PRRPTLP                                                          
*                                                                               
PRRPTDN  DS    0H                  END OF FILE                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        CLOSE FILES                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRCLOSE  DS    0H                                                               
*                                                                               
*        ERROR TAPE                                                             
*                                                                               
         L     R2,=A(ERRORDCB)     POINT TO TAPE DCB                            
         CLOSE (R2)                CLOSE IT                                     
*                                                                               
PRCLOSEX DS    0H                                                               
*                                                                               
         B     PRREPX                                                           
*                                                                               
PRREPX   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
TITLE    DC    C'COKE ERRORS'                                                   
BLNKS    DC    CL256' '            SPACES                                       
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,54,C'CONVERSION ERRORS'                                       
         SSPEC H2,54,C'-----------------'                                       
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*   HEADHOOK ROUTINE FOR REPORT                                       *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
HDHKX    XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
*        DCB INFORMATION                                              *         
***********************************************************************         
         SPACE 1                                                                
ERRORDCB DCB   DDNAME=ERRORDCB,DSORG=PS,EODAD=PRRPTDN,MACRF=GM                  
         EJECT                                                                  
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DMREQHDR                                                                      
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ERROR MESSAGE RECORD LAYOUT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ERRRECD  DSECT                                                                  
ERRREC   DS    0CL256              ERROR RECORD                                 
ERRMSG   DS    CL50                ERROR MESSAGE                                
ERRFLDD  DS    XL2                 FIELD IN ERROR DISPLACMENT                   
ERRFLDL  DS    XL1                 FIELD IN ERROR LENGTH                        
ERRSEQ   DS    PL8                 RECORD SEQUENCE NUMBER                       
ERRID    DS    CL2                 RECORD ID                                    
         DS    XL(256-(*-ERRREC))  SPARE                                        
         TITLE 'T23040 - EZMMPCNV ERRORS - PPEZFWORKD'                          
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* PPEZFWORKD                                                                    
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
         TITLE 'T23040 - WORKAREAS'                                             
***********************************************************************         
*                                                                     *         
*        WORKAREAS                                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                     *         
*        LOCAL WORKING STORAGE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
         DS    0D                                                               
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
X        DS    XL256                                                            
*                                                                               
WRKFLDD  DS    XL2                 ERROR FIELD DISPLACEMENT                     
WRKFLDL  DS    XL1                 ERROR FIELD LENGTH                           
WRKID    DS    CL2                 ERROR RECORD ID                              
*                                                                               
MDSPOOK  DS    XL(SPOOKL)          SPOOK BUILD AREA                             
MDREQHDR DS    XL(REQEOH-REQHDRD)  REQUEST HEADER BUILD AREA                    
MDREQREC DS    XL80                80 BYTE REQUEST RECORD                       
*                                                                               
         DS    0D                                                               
ERRRECIN DS    CL256               ERROR INPUT BUFFER                           
ERRSPACS DS    CL256               ERROR WORKAREA                               
*                                                                               
         EJECT                                                                  
* SPEZFFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFC0D                                                                      
       ++INCLUDE SPEZFC0D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         TITLE 'T23040 - EZMMPCNV ERRORS - REPORT LINE'                         
***********************************************************************         
*                                                                     *         
*        LAYOUT OF REPORT LINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PERRT    DS    C'**ERROR**'                                                     
         DS    CL1                                                              
PERRMSG  DS    CL50                ERROR MESSAGE                                
         DS    CL1                                                              
PSQNT    DS    CL12                'RECORD NO, ='                               
         DS    CL1                                                              
PSQN     DS    CL8                 SEQUENCE NUMBER                              
         DS    CL(132-(P-*))       SPARE                                        
         ORG   P                                                                
PERRREC  DS    CL128               RECORD IN ERROR                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPEZF40   08/11/00'                                      
         END                                                                    
