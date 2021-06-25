*          DATA SET SPTRA0D    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T2160DA                                                                  
         TITLE 'T2160D SATELLITE RECORD DISPLAY, CHANGE, ADD, DELETE, LI        
               IST'                                                             
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 -                                                            
*             AIO3 - REC SAVED FROM AIO1 IN VR (VALREC) AND RESTORED            
* TO AIO1 AT END OF VR RTN                                                      
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - UNUSED                                                            
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
         EJECT                                                                  
T2160D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SATL**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
* VALIDATE KEY ROUTINE                                                          
         SPACE 3                                                                
VK       LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 VALIMED                                                          
         LA    R2,TRAPRNTH         PARENT                                       
         XC    PARENT,PARENT                                                    
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      IF LIST, NOT NEEDED                          
         BE    VK20                                                             
         B     MISSERR                                                          
VK10     GOTO1 VALISTA                                                          
         MVC   PARENT,QSTA                                                      
VK20     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING SATKEY,R4                                                        
         MVC   SATKID,=XL2'0A26'                                                
         MVC   SATKAM,BAGYMD                                                    
         MVC   SATKPRNT,PARENT                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 3                                                                
VR       LA    R0,8                                                             
         L     R1,AIO                                                           
         L     R2,AIO3                                                          
         LR    R4,R2                                                            
VR06     MVC   0(250,R2),0(R1)                                                  
         LA    R1,250(,R1)                                                      
         LA    R2,250(,R2)                                                      
         BCT   R0,VR06                                                          
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO3                                                         
         MVI   ELCODE,X'10'        ADDRESS PART OF ELEMENT                      
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING SATDTAEL,R6                                                      
* FOR ADD, FORMAT DUMMY ELEMENT HEADER                                          
         MVI   SATDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   SATDTALN,7          ELEMENT LENGTH                               
         SPACE                                                                  
         LA    R2,TRASAT1H         SATELLITE 1                                  
         LA    R3,4                TOTAL FLDS TO VALIDATE                       
         MVI   VALSATSW,0                                                       
         GOTO1 VSAT                                                             
         CLI   VALSATSW,0          ANY VALID SATELLITES                         
         BE    MISATER             YES                                          
         MVC   KEY(L'SVKEY),SVKEY                                               
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    VR24                YES, NO NEED FOR GETREC                      
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
VR24     LA    R0,8                                                             
         L     R1,AIO3                                                          
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
VR30     MVC   0(250,R2),0(R1)                                                  
         LA    R1,250(,R1)                                                      
         LA    R2,250(,R2)                                                      
         BCT   R0,VR30                                                          
         B     DR                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 2                                                                
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         LA    R3,4                MAX SATELLITES                               
         LA    R2,TRASAT1H                                                      
         USING SATDTAEL,R6                                                      
DR10     LA    R5,SATSAT           STATION FLD TO EDIT                          
         GOTO1 PSTA                GO EDIT STATION                              
         CLC   8(L'TRASAT1,R2),WORK SAME AS ON SCREEN                           
         BE    *+14                YES NO TRANSMIT NEEDED                       
         MVC   8(L'TRASAT1,R2),WORK MOVE IN NEW                                 
         OI    6(R2),X'80'         FORCE TRANSMIT                               
         ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD                          
         BAS   RE,NEXTEL                                                        
         BNE   DR20                NO MORE ELEMENTS                             
         BCT   R3,DR10             NO MORE THAN 4 SATELLITES                    
         BAS   RE,NEXTEL                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
DR20     BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    EXIT                                                             
         XC    WORK(L'TRASAT1),WORK                                             
DR30     CLC   8(L'TRASAT1,R2),WORK                                             
         BE    DR32                                                             
         MVC   8(L'TRASAT1,R2),WORK                                             
         OI    6(R2),X'80'                                                      
DR32     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R3,DR30                                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       L     R4,AIO                                                           
         USING SATKEY,R4                                                        
         MVC   BAGYMD,SATKAM                                                    
         MVC   QSTA,SATKPRNT                                                    
         MVC   PARENT,SATKPRNT                                                  
         LA    R5,SATKPRNT         STATION FLD TO EDIT                          
         GOTO1 PSTA                GO EDIT STATION                              
         CLC   TRAPRNT,WORK        SEE IF SAME PARENT                           
         BE    *+14                                                             
         MVC   TRAPRNT,WORK        MOVE IN PARENT                               
         OI    TRAPRNTH+6,X'80'    SET ON TRANSMIT BIT                          
         DROP  R4                                                               
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE 3                                                                
LR       LA    R4,KEY                                                           
         USING SATKEY,R4                                                        
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, CONTINUE WITH READ SEQUENTIAL            
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDGRTN           HEADING ROUTINE FOR REPORT                   
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
* BUILD KEY, AND DO READHI                                                      
         MVC   SATKID(2),=XL2'0A26'                                             
         MVC   SATKAM,BAGYMD                                                    
         MVC   SATKPRNT,PARENT                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BNE   EXIT                                                             
         B     LR30                                                             
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         MVC   SVKEY(2),=XL2'0A26'                                              
         MVC   SVKEY+2(1),BAGYMD                                                
         CLC   SVKEY(3),KEY        INSURE ONLY VALID RECS                       
         BL    EXIT                                                             
         BH    LR20                                                             
LR30     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SATDTAEL,R6                                                      
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      LA    R3,P                PRINT LINE ADDRESS                           
         MVC   P,SPACES                                                         
         USING PRTLINE,R3                                                       
         LA    R5,SATKPRNT                                                      
         GOTO1 PSTA                                                             
         MVC   PPRNT,WORK                                                       
         MVI   ELCODE,10                                                        
         LA    R2,PSAT1                                                         
         LA    R7,4                MAX OF 4                                     
LRR10    LA    R5,SATSAT                                                        
         GOTO1 PSTA                                                             
         MVC   0(7,R2),WORK                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LRR20                                                            
         LA    R2,PSAT2-PSAT1(,R2)                                              
         BCT   R7,LRR10                                                         
LRR20    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         DROP  R3                                                               
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      LA    R3,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R3                                                       
         MVC   LPRNT,SATKPRNT                                                   
         LA    R5,SATKPRNT                                                      
         GOTO1 PSTA                                                             
         MVC   LPRNT,WORK                                                       
         LA    R2,LSAT1                                                         
         LA    R7,4                MAX OF 4                                     
LRL10    LA    R5,SATSAT                                                        
         GOTO1 PSTA                                                             
         MVC   0(7,R2),WORK                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LRL20                                                            
         LA    R2,LSAT2-LSAT1(,R2)                                              
         BCT   R7,LRL10                                                         
LRL20    GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
* VALIDATE SATELLITES                                                           
         SPACE                                                                  
         USING SATDTAEL,R6                                                      
VSAT     NTR1                                                                   
VSAT10   CLI   5(R2),0             ANY ENTRY                                    
         BE    VSAT40              NO                                           
         GOTO1 VALISTA             GO VALIDATE STATION                          
         CLC   PARENT,QSTA                                                      
         BE    DUPSTAER                                                         
         MVC   SATSAT,QSTA                                                      
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   VSAT30                                                           
VSAT20   CLC   SATSAT,QSTA                                                      
         BE    DUPSTAER                                                         
         BAS   RE,NEXTEL                                                        
         BE    VSAT20                                                           
VSAT30   LA    R6,ELEM                                                          
         OI    VALSATSW,1                                                       
         MVC   AIO,AIO3                                                         
         GOTO1 ADDELEM                                                          
VSAT40   ZIC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD                          
         BCT   R3,VSAT10                                                        
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT AND PRINT STATIONS                                                     
         SPACE                                                                  
PSTA     NTR1                                                                   
         XC    WORK(10),WORK                                                    
         MVC   WORK(4),0(R5)       MOVE IN CALL LETTERS                         
         LA    R1,WORK+4           SET UP FOR 4                                 
         CLI   WORK+3,C' '         SEE IF ONLY 3                                
         BH    PSTA10              NO, 4                                        
         BCTR  R1,0                DROP BACK 1                                  
PSTA10   CLI   4(R5),C'T'          IS THIS TV                                   
         BE    EXIT                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R5)       MOVE IN MEDIA                                
         MVI   2(R1),C'M'                                                       
         B     EXIT                                                             
         SPACE                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
*          DATA SET SPTRA0A    AT LEVEL 019 AS OF 03/18/84                      
HDGRTN   NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         B     EXIT                                                             
         SPACE 3                                                                
DUPSTAER MVI   ERROR,DUPLSTA                                                    
         B     TRAPERR                                                          
MISATER  LA    R2,TRASAT1H                                                      
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*          DATA SET SPTRA0A    AT LEVEL 019 AS OF 03/18/84                      
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,40,C'SATELLITE LIST'                                          
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,40,C'--------------'                                          
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H4,73,REPORT                                                     
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'PARENT.'                                                  
         SSPEC H8,9,C'SATELLITE.1'                                              
         SSPEC H8,22,C'SATELLITE.2'                                             
         SSPEC H8,35,C'SATELLITE.3'                                             
         SSPEC H8,58,C'SATELLITE.4'                                             
         SSPEC H9,3,C'-------'                                                  
         SSPEC H9,22,C'-------'                                                 
         SSPEC H9,35,C'-------'                                                 
         SSPEC H9,58,C'-------'                                                 
         DC    X'00'               END MARKER FOR SSPEC                         
PRTLINE  DSECT                                                                  
PPRNT    DS    CL7                                                              
         DS    C                                                                
PSAT1    DS    CL7                                                              
         DS    CL6                                                              
PSAT2    DS    CL7                                                              
         DS    CL6                                                              
PSAT3    DS    CL7                                                              
         DS    CL6                                                              
PSAT4    DS    CL7                                                              
LSTLINE  DSECT                                                                  
LPRNT    DS    CL7                                                              
         DS    C                                                                
LSAT1    DS    CL7                                                              
         DS    CL6                                                              
LSAT2    DS    CL7                                                              
         DS    CL6                                                              
LSAT3    DS    CL7                                                              
         DS    CL6                                                              
LSAT4    DS    CL7                                                              
         EJECT                                                                  
       ++INCLUDE SPTRSAT                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAFDD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
PARENT   DS    CL5                                                              
VALSATSW DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPTRA0D   05/01/02'                                      
         END                                                                    
