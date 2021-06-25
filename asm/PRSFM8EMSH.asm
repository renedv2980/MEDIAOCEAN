*          DATA SET PRSFM8EMSH AT LEVEL 012 AS OF 09/25/01                      
*PHASE T41C8EA,*                                                                
         TITLE 'PRSFM8EMSH - PROGRAM TO CONVERT MP DATA FOR DOWNLOAD'           
***********************************************************************         
*                                                                     *         
*        ROUTINE READS DATASET OF COSTS PRODUCED BY THE MSHR          *         
*         MEDIA PLANNING SYSTEM AND PUTS DATA ON THE FACPAK           *         
*         PRINT QUEUE IN DOWNLOAD FORMAT SO USERS CAN DOWNLOAD        *         
*         IT INTO A LOTUS SPREADSHEET.                                *         
*                                                                     *         
*        OUTPUT OF MPBT140                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRSFM8E  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MS8E**,RR=RE                                                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         ST    RE,RELO                                                          
*                                                                               
         MVI   IOOPT,C'Y'          WE DO ALL IO                                 
*                                                                               
         CLI   MODE,VALREC         VALIDATE REQUEST                             
         BE    VALREQ                                                           
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PRTREP                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   DS    0H                                                               
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO GENERATE DOWNLOAD DATA                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRTREP   DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        OPEN COST PER POINT TAPE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROPEN   DS    0H                                                               
*                                                                               
         L     R2,=A(EXTRACT)      POINT TO TAPE DCB                            
         OPEN  ((R2),INPUT)        OPEN IT AS INPUT                             
*                                                                               
PROPENX  DS    0H                                                               
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
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XCEF  (R2),L'TPBUFFER     CLEAR AREA                                   
*                                                                               
         GET   (R1),(R2)           GET USERID RECORD                            
*                                                                               
         PACK  DUB,0(5,R2)         GET USRID FOR REPORT                         
         CVB   RF,DUB              CVB                                          
         STCM  RF,3,REPUSRID       UPDATE USERID                                
*                                                                               
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XCEF  TPBUFFER,L'TPBUFFER CLEAR AREA                                   
         GET   (R1),(R2)           GET FIRST CPP RECORD                         
*                                                                               
         CLC   =C'END',0(R2)       CHECK FOR END OF DATA                        
         BE    RPPRDONE              CLOSE REPORT                               
*                                                                               
         MVC   REPDESC(11),=CL11'REPORT 140'                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE REMOTE AREA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRREMOT  EQU   *                                                                
*                                                                               
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTKEY(L'REPDESC),REPDESC    SET REPORT ID                     
         MVC   REMOTDST,REPUSRID   SET DESTINATION                              
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         MVC   REMOTJID,=C'CPP'                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT IT                                     
*                                                                               
         MVI   P1,C'*'             DUMMY FIRST PAGE FOR REPORT                  
         MVC   P1+20(20),=CL20'CPP DOWNLOAD'                                    
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT IT                                     
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE LATER                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        FILL IN COLUMN TITLES BASED ON WHICH FIELDS IN FIRST         *         
*              RECORD HAVE DATA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         LA    R3,MP140TAB         POINT TO RECORD DESCRIPTION TABLE            
         USING MPTABD,R3           ESTABLISH ENTRY IN TABLE                     
         LA    R4,P1               POINT TO START OF PRINT LINES                
         MVC   P1,SPACES           CLEAR PRINT AREA                             
         MVC   P2,SPACES           CLEAR PRINT AREA                             
         MVC   P3,SPACES           CLEAR PRINT AREA                             
         MVC   P4,SPACES           CLEAR PRINT AREA                             
*                                                                               
         SR    RF,RF                                                            
*                                                                               
RPTTLLP  DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         DONE AT END OF TABLE                         
         BE    RPTTLDN                                                          
*                                                                               
         CLI   0(R2),DNLDEOLQ      CHECK FOR END OF LINE                        
         BE    RPTTLDN                                                          
*                                                                               
         ICM   RF,1,MPTABLEN       GET LENGTH OF ENTRY'S TITLE                  
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
*        CHECK THAT NEXT FIELD IS EMPTY                                         
*                                                                               
         LR    R1,R2               COPY START OF DATA                           
         SR    R0,R0                                                            
*                                                                               
RPSPCTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),DNLDEOFQ      CHECK FOR EOF                                
         BE    RPSPCTDN                                                         
*                                                                               
         CLI   0(R2),C' '          IF DATA IN FIELD                             
         BNH   *+8                                                              
         LHI   R0,1                   SET DATA FOUND SWITCH                     
*                                                                               
RPSPCTCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT POSITION IN FIELD               
         B     RPSPCTLP                                                         
*                                                                               
RPSPCTDN DS    0H                                                               
*                                                                               
         LTR   R0,R0               SKIP IF NO DATA IN FIELD                     
         BZ    RPTTLCN                                                          
*                                                                               
RPSPCTFD DS    0H                  DATA IN FIELD PUT OUT TITLE                  
*                                                                               
         MVI   0(R4),DBLQUOTE      FIELD SEPARATOR                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),MPTABTTL    FILL IN TITLE OF COLUMN                      
*                                                                               
         LA    R4,2(RF,R4)         POINT TO END OF FIELD                        
         MVI   0(R4),DBLQUOTE      FIELD SEPARATOR                              
         MVI   1(R4),C' '          FILLER                                       
*                                                                               
         LA    R4,2(R4)            NEXT PRINT POSITION                          
*                                                                               
RPTTLCN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            NEXT FIELD ON TAPE                           
         LA    R3,2(RF,R3)         NEXT ENTRY IN TABLE                          
*                                                                               
         B     RPTTLLP                                                          
*                                                                               
RPTTLDN  DS    0H                                                               
*                                                                               
         MVI   0(R4),DNLDEOL       INDICATE END OF LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT COLUMN TITLES                          
         MVI   LINE,2              RESET LINE NUMBER FOR DOWNLOAD               
*                                                                               
*        PRINT LINE FOR EACH RECORD IN FILE                                     
*                                                                               
RPPRLOOP DS    0H                                                               
*                                                                               
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         LA    R3,MP140TAB         POINT TO RECORD DESCRIPTION TABLE            
         USING MPTABD,R3           ESTABLISH ENTRY IN TABLE                     
         LA    R4,P1               POINT TO START OF PRINT LINES                
         MVC   P1,SPACES           CLEAR PRINT AREA                             
         MVC   P2,SPACES           CLEAR PRINT AREA                             
         MVC   P3,SPACES           CLEAR PRINT AREA                             
         MVC   P4,SPACES           CLEAR PRINT AREA                             
*                                                                               
         SR    RF,RF                                                            
*                                                                               
RPDATLP  DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         DONE AT END OF TABLE                         
         BE    RPDATDN                                                          
*                                                                               
         CLI   0(R2),DNLDEOLQ      CHECK FOR END OF LINE                        
         BE    RPDATDN                                                          
*                                                                               
*        CHECK IF NEXT FIELD IS EMPTY                                           
*                                                                               
         LR    R1,R2               COPY START OF DATA                           
         SR    R0,R0               INIT DATA FOUND SWITCH                       
*                                                                               
RPSPCDLP DS    0H                                                               
*                                                                               
         CLI   0(R2),DNLDEOFQ      CHECK FOR EOF                                
         BE    RPSPCDDN                                                         
*                                                                               
         CLI   0(R2),C' '          DATA IN FIELD                                
         BNH   *+8                                                              
         LHI   R0,1                   SET DATA IN FIELD SWITCH                  
*                                                                               
RPSPCDCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT POSITION IN FIELD               
         B     RPSPCDLP                                                         
*                                                                               
RPSPCDDN DS    0H                                                               
*                                                                               
         LTR   R0,R0               SKIP IF NO DATA IN FIELD                     
         BZ    RPDATCN                                                          
*                                                                               
RPSPCDFD DS    0H                  DATA IN FIELD PUT OUT TITLE                  
*                                                                               
         MVI   0(R4),DBLQUOTE      FIELD SEPARATOR                              
*                                                                               
         LR    RF,R2               COPY END OF DATA ADDRESS                     
         SR    RF,R1               LENGHT OF DATA                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),0(R1)       PASS DATA TO PRINT LINE                      
*                                                                               
         LA    R4,2(RF,R4)         POINT TO END OF FIELD                        
         MVI   0(R4),DBLQUOTE      FIELD SEPARATOR                              
         MVI   1(R4),C' '          FILLER                                       
*                                                                               
         LA    R4,2(R4)            NEXT PRINT POSITION                          
*                                                                               
RPDATCN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            NEXT FIELD ON TAPE                           
         ICM   RF,1,MPTABLEN       GET LENGTH OF ENTRY'S TITLE                  
         LA    R3,1(RF,R3)         NEXT ENTRY IN TABLE                          
*                                                                               
         B     RPDATLP                                                          
*                                                                               
RPDATDN  DS    0H                                                               
*                                                                               
         MVI   0(R4),DNLDEOL       INDICATE END OF LINE                         
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT DATA                                   
         MVI   LINE,2              RESET LINE NUMBER FOR DOWNLOAD               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOOP TO READ NEXT RECORD ON TAPE AND PROCESS                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RPPRCONT EQU   *                                                                
*                                                                               
         L     R1,=A(EXTRACT)      POINT TO TAPE DCB                            
         L     R2,=A(TPBUFFER)     POINT TO INPUT AREA                          
         XCEF  0(R2),L'TPBUFFER    CLEAR AREA                                   
*                                                                               
         GET   (R1),(R2)           GET NEXT RECORD                              
*                                                                               
         CLC   =C'END',0(R2)       END OF REPORT TEST                           
         BNE   RPPRLOOP                                                         
*                                                                               
RPPRDONE DS    0H                  END OF FILE                                  
*                                                                               
         MVC   P1,SPACES           CLEAR PRINT AREA                             
         MVC   P2,SPACES           CLEAR PRINT AREA                             
         MVC   P3,SPACES           CLEAR PRINT AREA                             
         MVC   P4,SPACES           CLEAR PRINT AREA                             
*                                                                               
         MVI   P1,DNLDEOF          SET END OF FILE                              
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD  PRINT DATA                                   
         MVI   LINE,2              RESET LINE NUMBER FOR DOWNLOAD               
*                                                                               
PRRPTCN  DS    0H                                                               
         B     PRRPTLP                                                          
*                                                                               
PRRPTDN  DS    0H                                                               
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
*        COST PER POINT TAPE                                                    
*                                                                               
         L     R2,=A(EXTRACT)      POINT TO TAPE DCB                            
         CLOSE (R2)                CLOSE IT                                     
*                                                                               
PRCLOSEX DS    0H                                                               
*                                                                               
PRRPTDN1 DS    0H                  UNOPENED FILE                                
*                                                                               
PRTREPX  DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
DBLQUOTE EQU   C'"'                DOUBLE QUOTE FOR LOTUS                       
DNLDEOL  EQU   X'5E'               END OF LINE = SEMI-COLON                     
DNLDEOF  EQU   C':'                END OF FILE                                  
DNLDEOFQ EQU   C'@'                END OF FIELD COMING IN                       
DNLDEOLQ EQU   C'%'                END OF LINE COMING IN                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EXIT     DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
REPDESCL DC    C'DOWNLOAD'                                                      
*                                                                               
SYNAD    DS    0H                  ERR EXIT                                     
         XIT1                                                                   
*                                                                               
MP140TAB DS    0D                  TAPE LAYOUT FOR MPBT140 EXTRACT              
         DC    AL1(1),CL1'M'       MEDIA                                        
         DC    AL1(3),CL3'CLT'     CLIENT                                       
         DC    AL1(3),CL3'PRD'     PRODUCT                                      
         DC    AL1(8),CL8'DEMO'    DEMO                                         
         DC    AL1(4),CL4'MKT'     MARKET CODE                                  
         DC    AL1(33),CL33'MARKET NAME' MARKET NAME                            
         DC    AL1(4),CL4'DMA'     DMA ID                                       
         DC    AL1(3),CL3'RNK'     MARKET RANK                                  
         DC    AL1(3),CL3'DPT'     DAYPART                                      
         DC    AL1(12),CL12'COSTS TYPE' CPP TYPE                                
         DC    AL1(10),CL10'CLIENT' CLIENT                                      
         DC    AL1(10),CL10'PRODUCT' PRODUCT                                    
         DC    AL1(8),CL8'DEMO'    DEMO                                         
         DC    AL1(10),CL10'REQUEST' REQUEST                                    
         DC    AL1(4),CL4'YEAR'    YEAR                                         
         DC    AL1(1),CL1'Q'       QUARTER                                      
         DC    AL1(3),CL3'MON'     MONTH                                        
         DC    AL1(10),CL10'COSTS' CPP                                          
         DC    AL1(3),CL3'IDX'     INDEX                                        
         DC    AL1(12),CL12'COSTS TYPE' CPP TYPE                                
         DC    AL1(10),CL10'CLIENT' CLIENT                                      
         DC    AL1(10),CL10'PRODUCT' PRODUCT                                    
         DC    AL1(8),CL8'DEMO'    DEMO                                         
         DC    AL1(10),CL10'REQUEST' REQUEST                                    
         DC    AL1(4),CL4'YEAR'    YEAR                                         
         DC    AL1(1),CL1'Q'       QUARTER                                      
         DC    AL1(3),CL3'MON'     MONTH                                        
         DC    AL1(10),CL10'COSTS' CPP                                          
         DC    AL1(3),CL3'IDX'     INDEX                                        
         DC    AL1(12),CL12'COSTS TYPE' CPP TYPE                                
         DC    AL1(10),CL10'CLIENT' CLIENT                                      
         DC    AL1(10),CL10'PRODUCT' PRODUCT                                    
         DC    AL1(8),CL8'DEMO'    DEMO                                         
         DC    AL1(10),CL10'REQUEST' REQUEST                                    
         DC    AL1(4),CL4'YEAR'    YEAR                                         
         DC    AL1(1),CL1'Q'       QUARTER                                      
         DC    AL1(3),CL3'MON'     MONTH                                        
         DC    AL1(10),CL10'COSTS' CPP                                          
         DC    AL1(3),CL3'IDX'     INDEX                                        
         DC    AL1(12),CL12'COSTS TYPE' CPP TYPE                                
         DC    AL1(10),CL10'CLIENT' CLIENT                                      
         DC    AL1(10),CL10'PRODUCT' PRODUCT                                    
         DC    AL1(8),CL8'DEMO'    DEMO                                         
         DC    AL1(10),CL10'REQUEST' REQUEST                                    
         DC    AL1(4),CL4'YEAR'    YEAR                                         
         DC    AL1(1),CL1'Q'       QUARTER                                      
         DC    AL1(3),CL3'MON'     MONTH                                        
         DC    AL1(10),CL10'COSTS' CPP                                          
         DC    AL1(3),CL3'IDX'     INDEX                                        
         DC    X'FF'               EOT                                          
         EJECT                                                                  
***********************************************************************         
*        DCB INFORMATION                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
EXTRACT  DCB   DDNAME=EXTRACT,DSORG=PS,EODAD=PRRPTDN,MACRF=GM,         X        
               RECFM=FB,LRECL=500,BLKSIZE=5000,SYNAD=SYNAD,EROPT=ACC            
         DS    0D                                                               
         DC    CL8'TPBUFFER'                                                    
TPBUFFER DS    XL1000              TAPE INPUT BUFFER                            
         EJECT                                                                  
***********************************************************************         
*        DATA DESCRIPTION TABLE DSECT                                 *         
***********************************************************************         
         SPACE 1                                                                
MPTABD   DSECT                                                                  
MPTABLEN DS    AL1                 LENGTH OF TABLE ENTRY                        
MPTABTTL DS    0C                  TITLE FOR TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
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
       ++INCLUDE PRSFMFFD                                                       
*                                                                               
       ++INCLUDE PRSFMBFD                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
*GEND                                                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        LOCAL WORKING STORAGE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                     ** DSECT TO COVER LOCAL W/S **               
RELO     DS    A                                                                
REPDESC  DS    CL11                REPORT DESCRIPTION FOR DQU                   
REPUSRID DS    XL2                 USER ID                                      
*                                                                               
MDSPOOK  DS    XL(SPOOKL)          SPOOK BUILD AREA                             
MDREQHDR DS    XL(REQEOH-REQHDRD)  REQUEST HEADER BUILD AREA                    
MDREQREC DS    XL80                80 BYTE REQUEST RECORD                       
         DS    0D                                                               
SAVFLDS  DS    0XL12               LAST USED FIELD VALUES                       
SAVDEM   DS    CL8                 LAST USED DEMO                               
SAVMKT   DS    CL4                 LAST USED MARKET CODE                        
*                                                                               
SYSDX    DS    0D                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PRSFM8EMSH09/25/01'                                      
         END                                                                    
