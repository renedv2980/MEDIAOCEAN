*          DATA SET WBSIOS     AT LEVEL 042 AS OF 05/01/02                      
*PHASE T00A4BA,*                                                                
WBSIO    TITLE 'T00A4B - WBS I/O HANDLER'                                       
WBSIO    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*WBSIO*,RR=RE,CLEAR=YES                              
         LR    R9,RC                                                            
         USING WORKD,R9            R9 = A(LOCAL WORKING STORAGE)                
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING WBSIO,RB,RA,RC                                                   
*                                                                               
         L     R7,0(R1)                                                         
         USING WBSBLKD,R7          R7 = A(WBSBLOCK)                             
         L     R8,WBCOMFAC                                                      
         USING COMFACSD,R8         R8 = A(COMFACS)                              
         STM   R7,RC,SAVER7                                                     
         ST    RE,RELO                                                          
*                                                                               
         MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
         MVI   XFF,X'FF'                                                        
         MVC   XFF+1(L'XFF-1),XFF                                               
         MVC   IOKEYEQ,=CL8'**IOKEY*'                                           
*                                                                               
         MVC   WBMODEQ,=C'***MODE='                                             
         MVC   WBQAREA,=C'REQUESTS'                                             
*                                                                               
         L     RE,=A(IOPRINT)                                                   
         A     RE,RELO                                                          
         MVC   0(4,RE),WBPRINT     SET ADDRESS OF PRINT FOR IOTRACE             
*                                                                               
         MVC   AIO1(8),WBAIO1      SET IO AREAS FROM BLOCK                      
         MVI   IONUM,2             SET NUMBER OF I/O AREAS                      
*                                  SET T00A PHASE ADDRESSES                     
         LA    R2,PHASES           R2=A(PHASE LIST)                             
         LA    R3,COREFACS         R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAXIMUM NUMBER OF PHASES                  
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,PARM                                                          
         L     RF,CCALLOV                                                       
WB10     ICM   R0,1,0(R2)                                                       
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,WB10                                                          
         EJECT                                                                  
*                                  SET WBSIO FACILITY ADDRESSES                 
         LA    R0,CONADDRN         R0=NUMBER OF CONTROLLER ADDRESSES            
         LTR   R0,R0                                                            
         BZ    WB15                                                             
         SR    RE,RE               RE=INDEX TO ADDRESS VALUE                    
         L     RF,=A(CONADDRS)                                                  
         A     RF,RELO                                                          
         L     R1,0(RF,RE)                                                      
         A     R1,RELO             RELOCATE AND STORE IN W/S                    
         ST    R1,CONFACS(RE)                                                   
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                  FIND FILE TABLE ENTRY (FOR IO)               
WB15     L     R1,AFILTAB          R1 = A(FILE NAMES TABLE)                     
         LA    R1,3(R1)                                                         
         ST    R1,AFILNTRY         SAVE A(FILE TABLE ENTRY)                     
*                                                                               
         OC    WBQTODAY,WBQTODAY                                                
         BNZ   WB16                                                             
         GOTO1 CDATCON,PARM,(5,0),(3,WBQTODAY)                                  
*                                                                               
WB16     DS    0H                                                               
         XC    WBQSTB(6),WBQSTB                                                 
         XC    WBQSTP(4),WBQSTP                                                 
         OC    WBQSTART,WBQSTART   TEST REQUEST DATES PRESENT                   
         BZ    *+8                 NO                                           
         BAS   RE,PACKDTS                                                       
*                                                                               
WB18     DS    0H                                                               
         BAS   RE,GETAGYMD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WB20     BAS   RE,GETCLT                                                        
         MVI   WBPRD,0             RESET PRODUCT IN PROCESS                     
*                                                                               
         OC    WBQCLL,WBQCLL                                                    
         BNZ   *+10                                                             
         MVC   WBQCLL(3),=C'ALL'                                                
*                                                                               
         CLC   =C'BYR',WBQCLL      TEST BUYER SEQUENCE                          
         BE    WB25                                                             
         CLC   =C'ALL',WBQCLL                                                   
         BNE   WB30                                                             
         CLI   WBQCLLTY,C'F'       TEST BUYER FILTER                            
         BE    WB25                                                             
         MVI   WBQCLLTY,0                                                       
         B     WB30                                                             
*                                                                               
WB25     BAS   RE,BLDBYR                                                        
         EJECT                                                                  
WB30     ICM   R1,15,WBAMSBF       CLEAR MKTSTA BUFFER IF PRESENT               
         BZ    WB32                                                             
         ICM   R0,15,WBLMSBF                                                    
         BZ    WB32                                                             
         BAS   RE,CLRBUFF                                                       
         XC    WBCMSBF,WBCMSBF     RESET COUNTER                                
*                                                                               
WB32     BAS   RE,GETPRD           GET (NEXT) PRODUCT                           
         BNE   XIT                                                              
*                                                                               
         MVI   WBCMP,0             RESET CAMPAIGN IN PROCESS                    
         BAS   RE,BLDCMP           BUILD CAMPAIGN TABLE                         
*                                                                               
         CLI   WBQREAD,0           TEST USER WANTS ANY OTHER RECORDS            
         BE    EXIT                NO - EXIT                                    
         SPACE 1                                                                
* PROCESS ACTIVE CAMPAIGNS *                                                    
         SPACE 1                                                                
         L     R4,WBACMPBF         POINT TO CAMPAIGN BUFFER                     
         LA    R5,255                                                           
*                                                                               
         LA    R0,CMPBFLEN         GET ENTRY LENGTH                             
         OC    WBCMPTMK,WBCMPTMK   TEST EXTENDED ENTRIES REQ'D                  
         BZ    *+8                                                              
         LA    R0,CMPBFLEN+L'CMPBFCIC                                           
         USING CMPBUFFD,R4                                                      
*                                                                               
WB34     CLI   CMPBFCMP,0          TEST ACTIVE                                  
         BNE   WB38                                                             
*                                                                               
WB36     AR    R4,R0                                                            
         BCT   R5,WB34                                                          
         B     WB30                NO MORE CAMPAIGNS                            
*                                                                               
WB38     MVC   WBCMP,CMPBFCMP      SET ACTIVE CAMPAIGN NUMBER                   
         ZIC   R6,WBCMP                                                         
         CVD   R6,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WBCMPEBC,DUB                                                     
*                                                                               
         CLC   WBQCLL(3),=C'ALL'   TEST ALL CELL REQ                            
         BE    WB39                                                             
         CLC   WBQCLL(3),=C'BYR'   TEST BUYER SEQ                               
         BE    WB39                                                             
         CLI   WBQCLLTY,C'F'       TEST BUYER FILTER                            
         BE    WB39                                                             
*                                                                               
         BAS   RE,BLDCELL          BUILD CELL LIST                              
*                                                                               
WB39     CLI   WBQCMP,0            TEST ALL CAMPAIGN REQUEST                    
         BE    *+8                                                              
         LA    R6,1                ELSE POINT TO FIRST SLOT                     
         BCTR  R6,0                                                             
         LA    R0,CMPBFLEN         GET ENTRY LENGTH                             
         OC    WBCMPTMK,WBCMPTMK   TEST EXTENDED ENTRIES REQ'D                  
         BZ    *+8                                                              
         LA    R0,CMPBFLEN+L'CMPBFCIC                                           
         STH   R0,DUB                                                           
         MH    R6,DUB                                                           
         A     R6,WBACMPBF         POINT TO SLOT IN BUFFER                      
         USING CMPBUFFD,R6                                                      
         MVC   WBCMPNAM,CMPBFDSC                                                
         MVC   WBCMPST,CMPBFST                                                  
         MVC   WBCMPEND,CMPBFEND                                                
         MVC   WBCMPCPO,CMPBFCPO                                                
         DROP  R6                                                               
*                                                                               
         MVI   WBMODE,WBCMPFST                                                  
         BAS   RE,GO                                                            
*                                                                               
         CLI   WBSKIP,C'Y'         TEST TO SKIP THIS CAMPAIGN                   
         BE    WB42                                                             
*                                                                               
         OC    WBQSTART,WBQSTART                                                
         BZ    *+8                                                              
         BAS   RE,PACKDTS          RE-PACK DATES IN CASE USER CHANGED           
*                                                                               
         BAS   RE,BLDWKS                                                        
*                                                                               
         TM    WBQREAD,WBQRDGOL    TEST TO READ GOAL RECORDS                    
         BZ    WB40                                                             
*                                                                               
         BAS   RE,GETGOAL          GET GOALS FOR THIS CAMP/PRD                  
*                                                                               
WB40     XC    WBMKTSTA,WBMKTSTA   CLEAR MKT/STA                                
*                                                                               
         BAS   RE,GETBUY           GET BUYS FOR THIS CMP/PRD                    
*                                                                               
         MVI   WBMODE,WBENDPRD                                                  
         BAS   RE,GO                                                            
*                                                                               
WB42     CLI   WBQCMP,0            TEST ALL CAMPAIGNS                           
         BE    WB36                YES - NEXT CAMPAIGN                          
         B     WB30                ELSE NEXT PRODUCT                            
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RE,RE                                                            
         B     EXIT                                                             
*                                                                               
XIT      EQU   *                                                                
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
PACKDTS  NTR1                                                                   
         GOTO1 CDATCON,PARM,WBQSTART,(3,WBQSTB)                                 
         GOTO1 (RF),(R1),,(2,WBQSTP)                                            
         GOTO1 (RF),(R1),WBQEND,(3,WBQENDB)                                     
         GOTO1 (RF),(R1),,(2,WBQENDP)                                           
         B     EXIT                                                             
         EJECT                                                                  
*======================*                                                        
* GET AGENCY AND MEDIA *                                                        
*======================*                                                        
         SPACE 1                                                                
GETAGYMD NTR1                                                                   
         GOTO1 WBMEDGET,PARM,(WBQMED,WBQAGY),CDATAMGR,WORK                      
         CLI   8(R1),FF            TEST MEDIA NOT VALID                         
         BE    NEQXIT                                                           
         MVC   WBAGYMD,WORK                                                     
         PACK  WBAGY,WBAGYMD                                                    
         NI    WBAGY,X'0F'                                                      
         MVC   WBMEDNM,WORK+1                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING AGYKEY,R2                                                        
         MVI   AGYKTYPE,X'06'      READ AGENCY HEADER                           
         MVC   AGYKAGY,WBQAGY                                                   
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(L'AGYKEY),IOKEYSAV                                         
         BNE   NEQXIT                                                           
*                                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
         L     R2,IOADDR                                                        
         MVC   WBAGYNAM,AGYNAME                                                 
         MVC   WBAGYADR,AGYADDR                                                 
         MVC   WBAPROF,AGYPROF                                                  
*                                                                               
         MVI   WBMODE,WBPROCAM                                                  
         BAS   RE,GO                                                            
         B     EQXIT                                                            
         EJECT                                                                  
*===============================*                                               
* SUBROUTINE TO GET CLIENT DATA *                                               
*===============================*                                               
         SPACE 1                                                                
GETCLT   NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CLTHDRD,R2                                                       
         MVC   CKEYAM,WBAGYMD                                                   
         MVC   CKEYCLT,WBQCLT                                                   
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(L'AGYKEY),IOKEYSAV                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
         L     R2,IOADDR                                                        
         MVC   WBCLT,CKEYCLT                                                    
         MVC   WBCNAME,CNAME                                                    
         MVC   WBCPROF,CPROF                                                    
         MVC   WBCXTRA,CEXTRA                                                   
*                                                                               
         CLI   WBQPRD,0            TEST ALL PRD REQUEST                         
         BNE   GETCLT10            NO - SAVE REQ PRODUCT ONLY                   
*                                                                               
         LA    R0,4                SAVE THE PRODUCT LIST                        
         LA    R1,CLIST                                                         
         L     RF,WBCLIST                                                       
         MVC   0(220,RF),0(R1)                                                  
         LA    R1,220(R1)                                                       
         LA    RF,220(RF)                                                       
         BCT   R0,*-14                                                          
         B     GETCLT20                                                         
*                                                                               
GETCLT10 LA    R1,CLIST                                                         
*                                                                               
GETCLT12 CLC   WBQPRD,3(R1)        MATCH PRD CODE                               
         BE    GETCLT14                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   GETCLT12                                                         
         DC    H'0'                                                             
*                                                                               
GETCLT14 L     RF,WBCLIST                                                       
         MVC   0(4,RF),0(R1)                                                    
*                                                                               
GETCLT20 MVI   WBMODE,WBPROCCL                                                  
         BAS   RE,GO                                                            
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* SUBR TO READ CELL RECORD AND SAVE STATION LIST IF NEEDED *                    
* NOTE THAT IF CELL IS NOT A STATION LIST, ROUTINE         *                    
* SIMPLY EXITS                                             *                    
*==========================================================*                    
         SPACE 1                                                                
BLDCELL  NTR1                                                                   
         L     R1,WBACLLBF                                                      
         L     R0,WBLCLLBF                                                      
         BAS   RE,CLRBUFF                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CELRECD,R2                                                       
*                                                                               
         MVI   CELKTYPE,CELKTYPQ                                                
         MVI   CELKSTYP,CELKSTPQ                                                
         MVC   CELKAM,WBAGYMD                                                   
         MVC   CELKCLT,WBCLT                                                    
         MVC   CELKPRD,WBPRD                                                    
         MVC   CELKCMP,WBCMP                                                    
         MVC   CELKCEL,WBQCLL                                                   
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
*                                                                               
         L     R2,IOADDR                                                        
         CLC   CLDFLTR,=C'INCL'                                                 
         BE    BLDCEL5                                                          
         CLC   CLDFLTR,=C'EXCL'                                                 
         BE    BLDCEL5                                                          
         B     EXIT                                                             
         EJECT                                                                  
BLDCEL5  MVI   WBQCLLTY,C'L'       SET FLAG FOR STATION LIST IN USE             
         MVC   SVCLTYPE,CLDFLTR    SAVE INCL/EXCL                               
*                                                                               
         L     R5,WBLCLLBF                                                      
         L     R4,WBACLLBF         POINT TO BUFFER                              
         USING CLLBUFFD,R4                                                      
*                                                                               
         L     R2,IOADDR                                                        
         MVI   ELCODE,X'05'        STATION LIST ELEM                            
         LA    R6,24(R2)                                                        
         USING CLSTELEM,R6                                                      
         B     BLDCEL15                                                         
*                                                                               
BLDCEL10 MVC   CLLBFMKT,CLSTMKT                                                 
         MVC   CLLBFSTA,CLSTSTA                                                 
         LA    R4,CLLBFLEN(R4)                                                  
         SH    R5,=Y(CLLBFLEN)                                                  
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCEL15 BAS   RE,NEXTEL                                                        
         BE    BLDCEL10                                                         
         B     EXIT                                                             
         EJECT                                                                  
*==================================================*                            
* BUILD LIST OF MARKETS BY BUYER FROM PASSIVE KEYS *                            
*==================================================*                            
         SPACE 1                                                                
BLDBYR   NTR1                                                                   
         L     R1,WBACLLBF                                                      
         L     R0,WBLCLLBF                                                      
         BAS   RE,CLRBUFF                                                       
*                                                                               
         CLI   WBQCLLTY,0                                                       
         BNE   *+8                                                              
         MVI   WBQCLLTY,C'B'       SET 'BUYER SEQUENCE' FLAG                    
         XC    SVBYRBFA,SVBYRBFA   CLEAR ENTRY ADDRESS                          
*                                                                               
         L     R5,WBLCLLBF         GET BUFFER LENGTH                            
         L     R4,WBACLLBF         POINT TO BUFFER START                        
         USING BYRBUFFD,R4                                                      
*                                                                               
         XC    DUB,DUB             CLEAR BUYER SAVE AREA                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING MARRECD,R2                                                       
         MVI   MAPKTYPE,MAPKTYPQ                                                
         MVI   MAPKSTYP,MAPKSTPQ                                                
         MVC   MAPKAM,WBAGYMD                                                   
         CLI   WBQCLLTY,C'F'       TEST BUYERID FILTER                          
         BNE   *+10                                                             
         MVC   MAPKBYR,WBBYRID                                                  
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
BLDBYR2  CLC   IOKEY(MAPKBYR-MAPKEY),IOKEYSAV TYP/A-M                           
         BNE   BLDBYR6                                                          
         CLI   WBQCLLTY,C'F'       TEST BUYERID FILTER                          
         BNE   *+14                                                             
         CLC   IOKEY(MAPKMKT-MAPKEY),IOKEYSAV  TYP/A-M/BUYER                    
         BNE   BLDBYR6                                                          
*                                                                               
         CLC   DUB,MAPKBYR         TEST SAME BUYER                              
         BE    BLDBYR4                                                          
         MVC   DUB,MAPKBYR         SAVE NEW BUYER                               
         LA    R0,BYRBF1LN         GET 01 ELEMENT LENGTH                        
         SR    R5,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         MVI   BYRBF1EL,X'01'                                                   
         STC   R0,BYRBF1EL+1                                                    
         MVC   BYRBF1ID,MAPKBYR                                                 
         AR    R4,R0                                                            
*                                                                               
BLDBYR4  LA    R0,BYRBF2LN         GET 02 ELEMENT LENGTH                        
         SR    R5,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         MVI   BYRBF2EL,X'02'                                                   
         STC   R0,BYRBF2EL+1                                                    
         MVC   BYRBF2MK,MAPKMKT                                                 
         AR    R4,R0                                                            
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ                                                
         B     BLDBYR2                                                          
*                                                                               
BLDBYR6  XC    0(2,R4),0(R4)       SET E-O-L FLAG                               
         B     EXIT                                                             
         EJECT                                                                  
*================================*                                              
* SUBROUTINE TO GET NEXT PRODUCT *                                              
*================================*                                              
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PRDHDRD,R2                                                       
         MVC   PKEYAM,WBAGYMD                                                   
         MVC   PKEYCLT,WBCLT                                                    
*                                                                               
         CLI   WBQPRD,0            TEST ALL PRODUCT REQUEST                     
         BE    GETPR10                                                          
         SPACE 1                                                                
* SINGLE PRODUCT REQUEST *                                                      
         SPACE 1                                                                
         CLI   WBPRD,0             TEST FIRST TIME                              
         BNZ   NEQXIT                                                           
*                                                                               
         LA    R0,220                                                           
         L     R1,WBCLIST                                                       
*                                                                               
GETPRD2  CLC   WBQPRD,3(R1)                                                     
         BE    GETPRD4                                                          
         LA    R1,4(R1)                                                         
         BCT   R0,GETPRD2                                                       
         DC    H'0'                                                             
*                                                                               
GETPRD4  MVC   WBPRDEBC,0(R1)      SET EBCDIC PRODUCT                           
         MVC   PKEYPRD,0(R1)       SET REQUESTED PRODUCT                        
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    GETPR20                                                          
         DC    H'0'                MISSING PRODUCT                              
         SPACE 1                                                                
* MULTIPLE PRODUCTS *                                                           
         SPACE 1                                                                
GETPR10  MVC   PKEYPRD,WBPRDEBC    SET LAST PRODUCT PROCESSED                   
         MVC   PKEYPRD+3(10),XFF   FORCE NEXT PRD                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(PKEYPRD-PRDHDRD),IOKEYSAV  SAME A-M/CLT                    
         BNE   NEQXIT                                                           
*                                                                               
GETPR20  MVC   WBPRDEBC,PKEYPRD    SET PRODUCT IN PROCESS                       
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
*                                                                               
         L     R2,IOADDR                                                        
         MVC   WBPRD,PCODE+1                                                    
         CLI   WBPRD,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WBPRDNAM,PNAME                                                   
*                                                                               
         CLI   WBQPGR,C' '         TEST PRDGRP REQUEST                          
         BE    GETPR40                                                          
*                                                                               
         LA    R0,3                                                             
         LA    R1,PGRP1                                                         
*                                                                               
GETPR30  CLC   WBQPGR,0(R1)                                                     
         BE    GETPR34                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,GETPR30                                                       
*                                                                               
         LA    R0,2                                                             
         LA    R1,PGRP4                                                         
*                                                                               
GETPR32  CLC   WBQPGR,0(R1)                                                     
         BE    GETPR34                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,GETPR32                                                       
*                                                                               
         MVC   WBPGR(1),WBQPGR     UNKNOWN PRDGRP                               
         MVC   WBPGR+1(2),=X'9999'                                              
         B     GETPR40                                                          
*                                                                               
GETPR34  MVC   WBPGR,0(R1)         MOVE ACTUAL PRDGRP                           
*                                                                               
GETPR40  MVI   WBMODE,WBPROCPR                                                  
         BAS   RE,GO                                                            
         B     EQXIT                                                            
         EJECT                                                                  
*===============================================*                               
* SUBROUTINE TO BUILD TABLE OF CAMPAIGN DETAILS *                               
*===============================================*                               
         SPACE 1                                                                
BLDCMP   NTR1                                                                   
         L     R1,WBACMPBF         GET ADDRESS                                  
         L     R0,WBLCMPBF          AND LENGTH OF CAMPAIGN BUFFER               
         BAS   RE,CLRBUFF                                                       
*                                                                               
         LA    R0,CMPBFLEN         GET BUFFER ENTRY LENGTH                      
         OC    WBCMPTMK,WBCMPTMK   TEST EXTENDED ENTRIES REQ'D                  
         BZ    *+8                                                              
         LA    R0,CMPBFLEN+L'CMPBFCIC                                           
         CLI   WBQCMP,0            TEST ALL CAMPAIGN REQUEST                    
         BNE   *+8                 IF NOT ONLY NEED ROOM FOR ONE CMP            
         SLL   R0,8                X 256                                        
         C     R0,WBLCMPBF         COMPARE TO LENGTH OF BUFFER                  
         BNH   *+6                                                              
         DC    H'0'                NEED TO EXPAND BUFFER                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CMPRECD,R2                                                       
         MVI   CMPKTYPE,CMPKTYPQ                                                
         MVI   CMPKSTYP,CMPKSTPQ                                                
         MVC   CMPKAM,WBAGYMD                                                   
         MVC   CMPKCLT,WBCLT                                                    
         MVC   CMPKPRD,WBPRD                                                    
*                                                                               
         CLI   WBQCMP,0            TEST ALL CAMPAIGN REQUEST                    
         BE    BLDCM10                                                          
         SPACE 1                                                                
* SINGLE CAMPAIGN REQUEST *                                                     
         SPACE 1                                                                
         MVC   CMPKCMP,WBQCMP      SET REQUESTED CAMPAIGN                       
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BE    BLDCM20                                                          
         DC    H'0'                MISSING CAMPAIGN RECORD                      
         SPACE 1                                                                
* MULTIPLE CAMPAIGNS *                                                          
         SPACE 1                                                                
BLDCM10  LA    R2,IOKEY                                                         
         MVC   CMPKCMP,WBCMP       SET LAST CAMPAIGN PROCESSED                  
         MVC   CMPKCMP+1(6),XFF    FORCE NEXT                                   
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(CMPKCMP-CMPRECD),IOKEYSAV  SAME TY/A-M/CLT/PRD             
         BNE   NEQXIT                                                           
*                                                                               
BLDCM20  MVC   WBCMP,CMPKCMP       SET CAMPAIGN IN PROCESS                      
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
         L     R2,IOADDR                                                        
*                                                                               
         CLI   WBQCMP,0            TEST ALL CAMPAIGN REQUEST                    
         BNE   BLDCM25             NO                                           
         EJECT                                                                  
* TEST CAMPAIGN IN REQUEST PERIOD *                                             
         SPACE 1                                                                
         OC    WBQSTB(6),WBQSTB   TEST NO DATES SPECIFIED                       
         BZ    BLDCM25            YES - PROCESS ALL CAMPAIGNS                   
*                                                                               
         CLC   WBQEND,CDSTART      REQ END TO CMP START                         
         BL    BLDCM10                                                          
         CLC   WBQSTART,CDEND      REQ START TO CMP END                         
         BH    BLDCM10                                                          
*                                                                               
BLDCM25  DS    0H                                                               
         ZIC   R4,CMPKCMP                                                       
         CLI   WBQCMP,0            TEST ALL CAMPAIGN REQ                        
         BE    *+8                                                              
         LA    R4,1                ELSE POINT TO FIRST SLOT                     
         BCTR  R4,0                                                             
         LA    R0,CMPBFLEN         GET ENTRY LENGTH                             
         OC    WBCMPTMK,WBCMPTMK   TEST EXTENDED ENTRIES REQ'D                  
         BZ    *+8                                                              
         LA    R0,CMPBFLEN+L'CMPBFCIC                                           
         STH   R0,DUB                                                           
         MH    R4,DUB                                                           
         A     R4,WBACMPBF         POINT TO SLOT                                
         USING CMPBUFFD,R4                                                      
         MVC   CMPBFCMP,CMPKCMP    SAVE CAMPAIGN NUMBER                         
*                                                                               
         L     R2,IOADDR                                                        
         GOTO1 CDATCON,PARM,CDSTART,(3,CMPBFST)                                 
         GOTO1 (RF),(R1),CDEND,(3,CMPBFEND)                                     
         MVC   CMPBFDSC,CDDESC                                                  
         MVC   CMPBFDEM,CDDEMOS                                                 
         MVC   CMPBFCPO,CDTCPO                                                  
*                                                                               
         OC    WBCMPTMK,WBCMPTMK   TEST TELEMKTING FILTER                       
         BZ    BLDCMP40                                                         
*                                                                               
         LA    R6,24(R2)                                                        
         MVI   ELCODE,CICODEQ                                                   
*                                                                               
         USING CIELEM,R6                                                        
BLDCMP30 BAS   RE,NEXTEL                                                        
         BNE   BLDCMP40                                                         
         CLC   WBCMPTMK,CIMKT      MATCH                                        
         BNE   BLDCMP30                                                         
         MVC   CMPBFCIC,CIMCODE    SAVE TELEMKTING CMPGN CODE                   
         DROP  R6                                                               
*                                                                               
BLDCMP40 MVI   WBMODE,WBPROCCM                                                  
         BAS   RE,GO                                                            
*                                                                               
         CLI   WBQCMP,0            TEST ALL CAMPAIGN REQUEST                    
         BE    BLDCM10                                                          
         B     EQXIT                                                            
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE READS CONTRACTS, BUYS, AND LOGS OR INVOICES *                      
* FOR ALL MARKETS AND STATIONS                           *                      
* ORDERS ARE ALSO READ IF NEEDED                         *                      
*========================================================*                      
         SPACE 1                                                                
GETBUY   NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING REGRECD,R2                                                       
*                                                                               
         MVI   REGKTYPE,REGKTYPQ                                                
         MVI   REGKSTYP,REGKSTPQ                                                
         MVC   REGKAM,WBAGYMD                                                   
         MVC   REGKCLT,WBCLT                                                    
         MVC   REGKPRD,WBPRD                                                    
         MVC   REGKCMP,WBCMP       MOVE CAMPAIGN TO KEY                         
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
         CLC   IOKEY(REGKMKT-REGKEY),IOKEYSAV TY/AM/CLT/PRD/CMP                 
         BNE   EXIT                                                             
*                                                                               
GB10     DS    0H                                                               
         OC    WBQMKT,WBQMKT       TEST ALL MARKET REQUEST                      
         BZ    GB14                                                             
         OC    WBMKT,WBMKT         TEST FIRST TIME                              
         BNZ   EXIT                NO - DONE                                    
         SPACE 1                                                                
* SINGLE MARKET REQUEST *                                                       
         SPACE 1                                                                
GB12     LA    R2,IOKEY                                                         
         XC    REGKMKT(6),REGKMKT                                               
         MVC   REGKMKT,WBQMKT      MOVE REQUESTED MARKET                        
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(REGKSTA-REGKEY),IOKEYSAV TY/AM/CLT/PRD/CMP/MKT             
         BE    GB26                                                             
         B     EXIT                NEXT CAMPAIGN                                
         SPACE 1                                                                
* ALL MARKET REQUEST *                                                          
         SPACE 1                                                                
GB14     LA    R2,IOKEY                                                         
         XC    REGKMKT(6),REGKMKT                                               
         MVC   REGKMKT,WBMKT       SET LAST MARKET PROCESSED                    
         MVC   REGKSTA,XFF                                                      
*                                                                               
         CLI   WBQCLLTY,C'F'       TEST BUYER FILTER                            
         BE    *+12                                                             
         CLI   WBQCLLTY,C'B'         OR BUYER SEQUENCE                          
         BNE   GB24                                                             
         EJECT                                                                  
* BUYER SEQUENCE - GET NEXT MARKET IN LIST *                                    
         SPACE 1                                                                
         ICM   R4,15,SVBYRBFA      GET ADDRESS OF PREVIOUS ENTRY                
         BNZ   GB16                                                             
         L     R4,WBACLLBF         OR START OF LIST                             
         CLI   0(R4),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GB20                                                             
*                                                                               
         USING BYRBUFFD,R4                                                      
GB16     DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),2             TEST NEXT MARKET FOR BUYER                   
         BE    GB22                                                             
*                                                                               
GB18     MVI   WBMODE,WBPROCYX     SET MODE = END BUYER                         
         BAS   RE,GO                                                            
         CLI   0(R4),0             TEST E-O-L                                   
         BE    EXIT                                                             
*                                                                               
GB20     MVC   WBBYRID,BYRBF1ID    SET NEW BUYER ID                             
         MVI   WBMODE,WBPROCYR     SET MODE = NEW BUYER                         
         BAS   RE,GO                                                            
*                                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),2             TEST FOR MARKET ELEMENT                      
         BNE   GB18                                                             
*                                                                               
GB22     ST    R4,SVBYRBFA         SAVE ENTRY ADDRESS                           
         XC    REGKMKT(6),REGKMKT  CLEAR OUT JUNK                               
         MVC   REGKMKT,BYRBF2MK    SET NEW MARKET NUMBER                        
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(REGKSTA-REGKEY),IOKEYSAV  TY/AM/C/P/CM/MKT                 
         BE    GB30                            GO PROCESS STATION               
         MVC   IOKEY,IOKEYSAV      RESTORE ORIGINAL KEY                         
         B     GB16                 AND TRY NEXT MARKET                         
         EJECT                                                                  
* GET NEXT MARKET (FILE SEQUENCE)                                               
         SPACE 1                                                                
GB24     DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(REGKMKT-REGKEY),IOKEYSAV TY/AM/CLT/PRD/CMP                 
         BNE   GB120                          NO                                
*                                                                               
GB26     MVC   WBMKT,REGKMKT       SET CURRENT MARKET                           
*                                                                               
         OC    WBQSTA,WBQSTA       TEST ALL STATION REQUEST                     
         BZ    GB30                                                             
         EJECT                                                                  
* SINGLE STATION REQUEST *                                                      
         SPACE 1                                                                
         OC    WBSTA,WBSTA         TEST FIRST TIME                              
         BNZ   GB110               NEXT MARKET                                  
         XC    REGKSTA(4),REGKSTA                                               
         MVC   REGKSTA,WBQSTA      MOVE REQUESTED STATION                       
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(REGKLIN-REGKEY),IOKEYSAV TY/AM/CL/PR/CMP/MKT/STA           
         BE    GB30                                                             
         B     EXIT                NEXT CAMPAIGN                                
         SPACE 1                                                                
* ALL STATION REQUEST *                                                         
         SPACE 1                                                                
GB28     LA    R2,IOKEY                                                         
         XC    REGKSTA(4),REGKSTA                                               
         ICM   RE,7,WBSTA          GET LAST STATION PROCESSED                   
         LA    RE,1(RE)            FORCE NEXT STATION                           
         STCM  RE,7,REGKSTA                                                     
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(REGKSTA-REGKEY),IOKEYSAV TY/AM/CL/PR/CMP/MKT               
         BNE   GB110               NEXT MARKET                                  
*                                                                               
GB30     DS    0H                                                               
         MVC   SVBUYKEY,REGKEY     SAVE CURRENT BUY KEY                         
         MVC   WBMKTSTA,REGKMKT    SAVE CURRENT MKT/STA                         
         CLI   REGKLIN,0           MUST BE A CONTRACT HEADER                    
         BE    *+6                                                              
         DC    H'0'                ELSE DIE                                     
         SPACE 1                                                                
*                                                                               
         CLI   WBQCLLTY,C'L'       TEST CELL IS A STATION LIST                  
         BNE   GB40                                                             
         EJECT                                                                  
* LOOK FOR MKT/STA IN CELL BUFF TO INCLUDE/EXCLUDE *                            
         SPACE 1                                                                
         L     R4,WBACLLBF                                                      
         USING CLLBUFFD,R4                                                      
*                                                                               
GB32     CLC   WBMKTSTA,CLLBFMKT   REQUESTED STA TO BUFFER                      
         BE    GB34                GOT IT                                       
         BH    GB36                STATION NOT IN LIST                          
         LA    R4,CLLBFLEN(R4)                                                  
         B     GB32                                                             
*                                                                               
GB34     CLI   SVCLTYPE,C'I'       STA FOUND - TEST 'INCLUDE' LIST              
         BE    GB40                YES - PROCESS                                
         B     GB28                NO - SKIP                                    
*                                                                               
GB36     CLI   SVCLTYPE,C'I'       STA NOT FOUND - TEST 'INCLUDE' LIST          
         BE    GB28                YES - SKIP                                   
         EJECT                                                                  
GB40     DS    0H                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
*                                                                               
         CLI   WBQCLLTY,C'L'       TEST CELL IS A STATION LIST                  
         BE    GB46                                                             
         SPACE 1                                                                
* READ CELL ASSIGNMENT ELEMENTS *                                               
         SPACE 1                                                                
         MVI   ELCODE,RACODEQ                                                   
         L     R2,IOADDR                                                        
         LA    R6,24(R2)                                                        
         USING RAELEM,R6                                                        
*                                                                               
         LA    R4,WBCELLS                                                       
         LA    R5,(WBCELLX-WBCELLS)/L'WBCELLS                                   
         XC    0(L'WBCELLS,R4),0(R4)                                            
         LA    R4,L'WBCELLS(R4)                                                 
         BCT   R5,*-10                                                          
*                                                                               
         LA    R4,WBCELLS                                                       
         LA    R5,(WBCELLX-WBCELLS)/L'WBCELLS                                   
*                                                                               
GB42     BAS   RE,NEXTEL                                                        
         BNE   GB44                                                             
*                                                                               
         MVC   0(1,R4),RASPLN      MOVE SLN                                     
         MVC   3(7,R4),RADATE      MOVE DATE AND CELL ID                        
         CLC   3(3,R4),WBQSTB      TEST DATE PRIOR TO REQ START                 
         BNL   *+10                                                             
         MVC   3(3,R4),WBQSTB      YES - USE REQ START DATE                     
*                                                                               
         BAS   RE,GBDTQH           SET DTQH                                     
*                                                                               
         LA    R4,L'WBCELLS(R4)                                                 
         BCT   R5,GB42                                                          
         DC    H'0'                TOO MANY CELL ASSIGNMENTS                    
         SPACE 1                                                                
GB44     CLC   WBQCLL(3),=C'ALL'  TEST IF STATION WAS EVER IN CELL              
         BE    GB46                                                             
         CLC   WBQCLL(3),=C'BYR'                                                
         BE    GB46                                                             
         LA    R4,WBCELLS                                                       
         LA    R5,(WBCELLX-WBCELLS)/L'WBCELLS                                   
GB45     CLC   6(4,R4),WBQCLL                                                   
         BE    GB46                                                             
         LA    R4,L'WBCELLS(R4)                                                 
         BCT   R5,GB45                                                          
         B     GB28               SKIP STATION NOT IN REQ CELL                  
         SPACE                                                                  
* SAVE MKT/STA AND GRADE IN BUFFER *                                            
         SPACE 1                                                                
         USING MSBUFFD,R4                                                       
GB46     ICM   R4,15,WBAMSBF       GET MKT/STA BUFFER ADDRESS                   
         BZ    GB48                                                             
         ICM   R5,15,WBCMSBF       GET MKT/STA BUFFER COUNT                     
         MH    R5,=Y(MSBFLEN)                                                   
         AR    R4,R5                                                            
*                                                                               
         L     R0,WBAMSBF          TEST BUFFER BIG ENOUGH                       
         A     R0,WBLMSBF                                                       
         SH    R0,=Y(MSBFLEN)                                                   
         CR    R4,R0                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   MSBFMKT,REGKMKT                                                  
         MVC   MSBFSTA,REGKSTA                                                  
         MVC   MSBFGRAD,RHGRADE                                                 
         L     RE,WBCMSBF                                                       
         LA    RE,1(RE)                                                         
         ST    RE,WBCMSBF          BUMP COUNTER                                 
         EJECT                                                                  
GB48     OC    WBMSUNPK,WBMSUNPK                                                
         BZ    GB48X                                                            
         GOTO1 WBMSUNPK,PARM,WBMKTSTA,WBMKTPRT,WBSTAPRT                         
         CLI   WBSTAPRT+4,C' '                                                  
         BNE   GB48A                                                            
         MVC   WBSTAPRT+4(3),=C'-TV'                                            
         B     GB48B                                                            
*                                                                               
GB48A    MVC   WBSTAPRT+5(1),WBSTAPRT+4                                         
         MVI   WBSTAPRT+4,C'-'                                                  
         MVI   WBSTAPRT+6,C'M'                                                  
*                                                                               
GB48B    XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
*                                                                               
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,WBQMED                                                   
         MVC   MKTKMKT,WBMKTPRT                                                 
         MVC   MKTKAGY,WBQAGY                                                   
         GOTO1 AIO,IOSTAFIL+IOHI+IO2                                            
*                                                                               
         L     R2,IOADDR                                                        
         MVC   WBMKTNAM,=CL24'** UNKNOWN **'                                    
         CLC   IOKEY(8),0(R2)                                                   
         BNE   GB48X                                                            
         MVC   WBMKTNAM,MKTNAME                                                 
         MVC   WBTIMZON,MKTZONE    SAVE TIME ZONE CODE                          
*                                                                               
GB48X    MVC   IOADDR,AIO1         RESTORE I/O AREA OF CONTRACT                 
         MVI   WBMODE,WBPROCCN     SET MODE=PROCESS CONTRACT                    
         BAS   RE,GO                                                            
*                                                                               
GB48Z    CLI   WBSKIP,C'Y'                                                      
         BE    GB105                                                            
         B     GB50                                                             
         EJECT                                                                  
*==========================================*                                    
* SUBROUTINE TO SET STARTING DTQH FOR CELL *                                    
*==========================================*                                    
         SPACE 1                                                                
GBDTQH   NTR1                                                                   
         ZIC   RE,4(R4)            GET MONTH IN RE                              
         ZIC   RF,5(R4)            AND DAY IN RF                                
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DTBFJUL)    X WIDTH OF EACH MONTH                        
         L     R1,WBADTBF                                                       
         LA    R1,(DTBFJUL-DTBUFFD)(R1)   POINT TO JULIAN DATE TABLE            
         AR    RE,R1                      POINT TO THIS MONTH                   
         BCTR  RF,0                DAY-1                                        
         AR    RF,RF               X 2                                          
         AR    RE,RF               POINT TO JULIAN DATE                         
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          GET JULIAN DATE                              
         BCTR  RF,0                                                             
         LA    RE,96               X QH PER DAY                                 
         MR    RE,RE               GIVES RESULT IN RF                           
         STCM  RF,3,1(R4)          AND PASS RESULT                              
         B     EXIT                                                             
         EJECT                                                                  
*===========================*                                                   
* READ BUY RECORDS INTO IO2 *                                                   
*===========================*                                                   
         SPACE 1                                                                
GB50     DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING REGRECD,R2                                                       
*                                                                               
         L     R1,WBABUYBF                                                      
         L     R0,WBLBUYBF                                                      
         BAS   RE,CLRBUFF                                                       
*                                                                               
         LA    R0,BUYBFLEN         GET BUFFER ENTRY LENGTH                      
         SLL   R0,8                X 256                                        
         C     R0,WBLBUYBF         COMPARE TO LENGTH OF BUFFER                  
         BNH   *+6                                                              
         DC    H'0'                BUFFER NOT LARGE ENOUGH                      
*                                                                               
         MVC   IOKEY,SVBUYKEY      RESTORE CONTRACT KEY                         
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
         L     R4,WBABUYBF                                                      
         USING BUYBUFFD,R4                                                      
*                                                                               
GB52     DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ                                                
         CLC   IOKEY(REGKLIN-REGKEY),IOKEYSAV TY/AM/CL/PR/CMP/MKT/STA           
         BNE   GB58                                                             
*                                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO2                                           
         L     R2,IOADDR                                                        
         EJECT                                                                  
* SAVE BUY DETAILS IN BUFFER *                                                  
         SPACE 1                                                                
         GOTO1 CDATCON,PARM,(3,RMSTART),(2,BUYBFST)                             
         GOTO1 (RF),(R1),(3,RMEND),(2,BUYBFEND)                                 
         MVC   BUYBFDAY,RMDAY                                                   
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,RMTIME                                                      
         CH    R0,=H'600'                                                       
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
         STCM  R0,3,BUYBFTIM                                                    
*                                                                               
         ICM   R0,3,RMTIME+2                                                    
         CH    R0,=H'600'                                                       
         BNL   *+8                                                              
         AH    R0,=H'2400'                                                      
         STCM  R0,3,BUYBFTIM+2                                                  
*                                                                               
         MVC   BUYBFSLN,RMSPLN                                                  
         MVI   BUYBFFLG,0                                                       
         SR    R0,R0                                                            
         ICM   R0,7,RMCOST                                                      
         BNZ   *+12                                                             
         OI    BUYBFFLG,X'80'      SET N/C FLAG                                 
         ICM   R0,7,RMEST                                                       
         STCM  R0,7,BUYBFCOS                                                    
         LA    R4,BUYBFLEN(R4)                                                  
*                                                                               
GB54     MVI   WBMODE,WBPROCBY                                                  
         BAS   RE,GO                                                            
         B     GB52                                                             
         SPACE 1                                                                
* NO MORE BUY RECORDS *                                                         
         SPACE 1                                                                
GB58     MVI   WBMODE,WBPROCBX                                                  
         BAS   RE,GO                                                            
         EJECT                                                                  
*====================================*                                          
* ROUTINE TO PROCESS INVOICE RECORDS *                                          
*====================================*                                          
         SPACE 1                                                                
GB60     DS    0H                                                               
         XC    SVMTCHEL,SVMTCHEL                                                
         L     R4,WBABUYBF                                                      
         TM    WBQREAD,WBQRDINV    TEST USER WANTS INVOICE RECORDS              
         BZ    GB70                NO                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING INVRECD,R2                                                       
*                                                                               
         MVI   INPKTYPE,INPKTYPQ                                                
         MVI   INPKSTYP,INPKSTPQ                                                
         MVC   INPKAM,WBAGYMD                                                   
         MVC   INPKCLT,WBCLT                                                    
         MVC   INPKPRD,WBPRD                                                    
         MVC   INPKCMP,WBCMP                                                    
         MVC   INPKSTA,WBSTA                                                    
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   GB70                                                             
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
         L     RE,IOADDR                                                        
         MVC   IOKEY(13),0(RE)     MOVE ACTIVE KEY FROM RECORD                  
         XC    INVEKEY,INVEKEY     CLEAR ELEMENT KEY                            
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI   READ FIRST INV DATA KEY                      
*                                                                               
         CLC   IOKEY(INVEKEY-INVRECD),IOKEYSAV   TY/AM/SEQ                      
         BNE   GB70                                                             
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
         EJECT                                                                  
* DIG OUT THE HEADER ELEMENTS (IN FIRST RECORD) FOR MATCH STATUS) *             
         SPACE 1                                                                
         MVI   ELCODE,IHCODEQ                                                   
         L     R6,IOADDR                                                        
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         BAS   RE,NEXTEL2                                                       
         BNE   GB62X               NO MONTHS MATCHED - SKIP INVOICES            
*                                                                               
         USING IHELEM,R6                                                        
*                                                                               
GB62     DS    0H                                                               
         ZIC   RE,IHMONTH                                                       
         LA    RE,SVMTCHEL-1(RE)                                                
         CLI   IHMATCH,C'Y'        TEST MATCHED                                 
         BNE   GB62A                                                            
         OC    IHAPPR,IHAPPR       TEST APPROVED FOR PAYMENT                    
         BZ    GB62A               NO - USE LOG DATA                            
         MVI   0(RE),C'Y'                                                       
*                                                                               
GB62A    BAS   RE,NEXTEL                                                        
         BE    GB62                                                             
*                                                                               
GB62X    TM    WBQREAD,WBQRDUNM    TEST READ UNMATCHED INV'S                    
         BO    GB68                                                             
         OC    SVMTCHEL,SVMTCHEL   TEST ANY MONTHS MATCHED                      
         BZ    GB70                NO - SKIP INVOICE DATA                       
         B     GB68                                                             
*                                                                               
GB64     DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ   READ NEXT INV RECORD                         
*                                                                               
GB66     CLC   IOKEY(INVEKEY-INVRECD),IOKEYSAV   TY/AM/SEQ                      
         BNE   GB70                                                             
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
*                                                                               
GB68     MVI   BYTE2,C'I'          INDICATE PROCESSING INVREC                   
         TM    WBQREAD,WBQRDUNM    TEST READ UNMATCHED INVOICES                 
         BO    *+8                 YES - DON'T TOUCH ELEMENTS                   
         BAS   RE,TESTELS          DELETE ELEMENTS IN UNMATCHED MONTHS          
*                                                                               
         MVI   WBMODE,WBPROCIN                                                  
         BAS   RE,GO                                                            
         CLI   WBSKIP,C'Y'         TEST SKIP REMAINING INVOICES                 
         BE    GB70                YES - DO IT                                  
         B     GB64                                                             
         EJECT                                                                  
*================================*                                              
* ROUTINE TO PROCESS LOG RECORDS *                                              
*================================*                                              
         SPACE 1                                                                
GB70     DS    0H                                                               
*                                                                               
         TM    WBQREAD,WBQRDLOG    TEST USER WANTS LOG DATA                     
         BZ    GB80                NO                                           
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING LOGRECD,R2                                                       
*                                                                               
         MVI   LOPKTYPE,LOPKTYPQ                                                
         MVI   LOPKSTYP,LOPKSTPQ                                                
         MVC   LOPKAM,WBAGYMD                                                   
         MVC   LOPKCLT,WBCLT                                                    
         MVC   LOPKPRD,WBPRD                                                    
         MVC   LOPKCMP,WBCMP                                                    
         MVC   LOPKSTA,WBSTA                                                    
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   GB80                                                             
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
         L     RE,IOADDR                                                        
         MVC   IOKEY(13),0(RE)     MOVE ACTIVE KEY FROM RECORD                  
         XC    LOGEKEY,LOGEKEY     CLEAR ELEMENT KEY                            
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI   READ FIRST LOG DATA KEY                      
         B     GB74                                                             
*                                                                               
GB72     DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ   READ NEXT LOG RECORD                         
*                                                                               
GB74     CLC   IOKEY(LOGEKEY-LOGRECD),IOKEYSAV   TY/AM/SEQ                      
         BNE   GB80                                                             
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
*                                                                               
         MVI   BYTE2,C'L'          INDICATE PROCESSING LOGREC                   
         BAS   RE,TESTELS          DELETE ELEMENTS IN MATCHED MONTHS            
*                                                                               
         MVI   WBMODE,WBPROCLG                                                  
         BAS   RE,GO                                                            
         B     GB72                                                             
         EJECT                                                                  
*===========================*                                                   
* ROUTINE TO PROCESS ORDERS *                                                   
*===========================*                                                   
         SPACE 1                                                                
GB80     TM    WBQREAD,WBQRDORD    TEST USER WANTS ORDERS                       
         BZ    GB100                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING ORDRECD,R2                                                       
*                                                                               
         MVI   ORPKTYPE,ORPKTYPQ                                                
         MVI   ORPKSTYP,ORPKSTPQ                                                
         MVC   ORPKAM,WBAGYMD                                                   
         MVC   ORPKCLT,WBCLT                                                    
         MVC   ORPKPRD,WBPRD                                                    
         MVC   ORPKCMP,WBCMP                                                    
         MVC   ORPKSTA,WBSTA                                                    
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   GB100                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
         L     RE,IOADDR                                                        
         MVC   IOKEY(13),0(RE)     MOVE ACTIVE KEY FROM RECORD                  
         XC    ORDEKEY,ORDEKEY     CLEAR ELEMENT KEY                            
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI   READ FIRST ORD DATA KEY                      
         B     GB84                                                             
*                                                                               
GB82     DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ   READ NEXT ORD RECORD                         
*                                                                               
GB84     CLC   IOKEY(ORDEKEY-ORDRECD),IOKEYSAV   TY/AM/SEQ                      
         BNE   GB100                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
*                                                                               
         MVI   WBMODE,WBPROCOR                                                  
         BAS   RE,GO                                                            
         B     GB82                                                             
         EJECT                                                                  
* END OF STATION *                                                              
         SPACE 1                                                                
GB100    DS    0H                                                               
         MVI   WBMODE,WBENDSTA     SET END OF STATION                           
         BAS   RE,GO                                                            
*                                                                               
GB105    OC    WBQSTA,WBQSTA       TEST ALL STATION REQUEST                     
         BNZ   EXIT                NO - DONE                                    
         MVC   IOKEY,SVBUYKEY      RESTORE BUY KEY                              
         B     GB28                                                             
         SPACE 1                                                                
* END OF MARKET *                                                               
         SPACE 1                                                                
GB110    OC    WBQMKT,WBQMKT       TEST ALL MARKET REQUEST                      
         BNZ   EXIT                NO - DONE                                    
         MVC   IOKEY,SVBUYKEY      RESTORE BUY KEY                              
         B     GB14                                                             
         EJECT                                                                  
*===========================================================*                   
* NO MORE STATIONS - TRY UNKNOWN (ZZ..) STATIONS            *                   
*===========================================================*                   
         SPACE 1                                                                
GB120    TM    WBQREAD,WBQRDORD    TEST USER WANTS ORDERS                       
         BZ    EXIT                                                             
*                                                                               
         MVC   WBMKT,=H'9999'      FORCE MARKET                                 
         MVC   WBSTA,STAZZAA       SET LOWEST POSSIBLE ZZ STATION               
         MVC   WBMKTNAM,=CL24'** UNKNOWN **'                                    
* CLEAR CELL TABLE SO NO RANDOM ASSIGNMENTS                                     
         LA    R4,WBCELLS                                                       
         LA    R5,(WBCELLX-WBCELLS)/L'WBCELLS                                   
         XC    0(L'WBCELLS,R4),0(R4)                                            
         LA    R4,L'WBCELLS(R4)                                                 
         BCT   R5,*-10                                                          
*                                                                               
GB122    XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING ORDRECD,R2                                                       
*                                                                               
         MVI   ORPKTYPE,ORPKTYPQ                                                
         MVI   ORPKSTYP,ORPKSTPQ                                                
         MVC   ORPKAM,WBAGYMD                                                   
         MVC   ORPKCLT,WBCLT                                                    
         MVC   ORPKPRD,WBPRD                                                    
         MVC   ORPKCMP,WBCMP                                                    
         MVC   ORPKSTA,WBSTA                                                    
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
         CLC   IOKEY(7),IOKEYSAV   SAME TYP/A-M/CLT/PRD/CAMP                    
         BNE   EXIT                                                             
         MVC   WBSTA,ORPKSTA       SAVE ACTUAL STATION                          
*                                                                               
         MVI   WBMODE,WBPROCST     TELL APPL ABOUT NEW STATION                  
         BAS   RE,GO                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
         L     RE,IOADDR                                                        
         MVC   IOKEY(13),0(RE)     MOVE ACTIVE KEY FROM RECORD                  
         XC    ORDEKEY,ORDEKEY     CLEAR ELEMENT KEY                            
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI   READ FIRST ORD DATA KEY                      
         B     GB126                                                            
*                                                                               
GB124    DS    0H                                                               
         GOTO1 AIO,IOSPTDIR+IOSQ   READ NEXT ORD RECORD                         
*                                                                               
GB126    CLC   IOKEY(ORDEKEY-ORDRECD),IOKEYSAV   TY/AM/SEQ                      
         BNE   GB128                                                            
*                                                                               
         GOTO1 AIO,IOGET+IOSPTFIL+IO2                                           
*                                                                               
         MVI   WBMODE,WBPROCOR                                                  
         BAS   RE,GO                                                            
         B     GB124                                                            
*                                                                               
GB128    DS    0H                                                               
         MVI   WBMODE,WBENDSTA                                                  
         BAS   RE,GO                                                            
*                                                                               
         ZIC   RE,WBSTA+2          GET LAST BYTE OF STATION                     
         LA    RE,1(RE)                                                         
         STC   RE,WBSTA+2          BUMP IT                                      
         B     GB122               AND TRY AGAIN                                
*                                                                               
STAZZAA  DC    X'E72000'                                                        
         EJECT                                                                  
*===========================================================*                   
* SUBROUTINE SEARCHES ELEMENTS IN LOG OR INVOICE RECORD AND *                   
* FLAGS THOSE IN MONTHS THAT HAVE MATCHED INVOICE DATA      *                   
*===========================================================*                   
         SPACE 1                                                                
TESTELS  NTR1                                                                   
         MVI   ELCODE,LECODEQ                                                   
         L     R2,IOADDR                                                        
         LA    R6,24(R2)                                                        
         USING LEELEM,R6                                                        
*                                                                               
         BAS   RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
TESTEL2  BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         L     R4,WBADTBF                                                       
         LA    R4,(DTBFMONS-DTBUFFD)(R4)                                        
*                                                                               
TESTEL4  CLC   LEDATE,0(R4)        ELEMENT TO START OF MONTH                    
         BL    TESTEL6                                                          
         CLC   LEDATE,2(R4)        ELEMENT TO END OF MONTH                      
         BNH   TESTEL10                                                         
*                                                                               
TESTEL6  LA    R4,8(R4)            NEXT MONTH                                   
         CLI   0(R4),X'FF'                                                      
         BNE   TESTEL4                                                          
         B     TESTEL2                                                          
*                                                                               
TESTEL10 SR    RE,RE                                                            
         ICM   RE,3,2(R4)          GET END DATE OF MONTH                        
         SLL   RE,23               DROP YEAR                                    
         SRL   RE,28               DROP DAY                                     
         LA    RE,SVMTCHEL-1(RE)   POINT TO MATCH FLAG                          
*                                                                               
         CLI   BYTE2,C'L'          TEST PROCESSING LOG RECORD                   
         BNE   TESTEL12                                                         
         CLI   0(RE),C'Y'          TEST MONTH MATCHED TO INVOICE                
         BNE   *+8                                                              
         OI    0(R6),X'80'         MODIFY ELEMENT CODE                          
         B     TESTEL2                                                          
*                                                                               
TESTEL12 CLI   BYTE2,C'I'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),C'Y'                                                       
         BE    *+8                                                              
         OI    0(R6),X'80'         MODIFY ELEMENT CODE                          
         B     TESTEL2                                                          
         EJECT                                                                  
*=============================================*                                 
* SUBROUTINE BUILDS LIST OF MONDAY WEEK DATES *                                 
* IN CAMPAIGN OR REQUEST PERIOD               *                                 
*=============================================*                                 
         SPACE 1                                                                
BLDWKS   NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R4,WBQSTB                                                        
         OC    0(3,R4),0(R4)       TEST REQUEST DATES PRESENT                   
         BNZ   *+8                                                              
         LA    R4,WBCMPST                                                       
         GOTO1 CDATCON,PARM,(3,0(R4)),WORK                                      
         GOTO1 (RF),(R1),(3,3(R4)),WORK+6                                       
*                                                                               
         MVC   WORK+12(4),WBGETBRD    A(GETBROAD)                               
         MVC   WORK+16(4),CADDAY                                                
         MVC   WORK+20(4),CGETDAY                                               
         MVC   WORK+24(4),CDATCON                                               
*                                                                               
         L     R1,WBADTBF                                                       
         L     R0,WBLDTBF                                                       
         CH    R0,=Y(DTBFLEN)                                                   
         BNL   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CLRBUFF                                                       
         SPACE 1                                                                
* GET BROADCAST WEEK DATES FROM START DATE *                                    
         SPACE 1                                                                
         GOTO1 WBMOBILE,PARM,(53,WORK),(4,AIO2),WORK+12,0                       
*                                                                               
         L     R1,WBADTBF                                                       
         LA    R1,(DTBFWKS-DTBUFFD)(R1)                                         
         LA    R0,53                                                            
         L     RE,AIO2                                                          
*                                                                               
BLDWKS8  MVC   0(4,R1),0(RE)       MOVE WEEKS WITH SPACE FOR QH'S               
         LA    R1,8(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    *+8                                                              
         BCT   R0,BLDWKS8                                                       
         MVI   0(R1),X'FF'                                                      
         SPACE 1                                                                
* NOW GET BROADCAST MONTH DATES *                                               
         SPACE 1                                                                
BLDWKS10 DS    0H                                                               
         GOTO1 WBMOBILE,PARM,(12,WORK),(0,AIO2)                                 
*                                                                               
         L     R1,WBADTBF                                                       
         LA    R1,(DTBFMONS-DTBUFFD)(R1)                                        
         LA    R0,13                                                            
         L     RE,AIO2                                                          
*                                                                               
BLDWKS12 MVC   0(4,R1),0(RE)       MOVE WEEKS WITH SPACE FOR QH'S               
         LA    R1,8(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    *+8                                                              
         BCT   R0,BLDWKS12                                                      
         MVI   0(R1),X'FF'                                                      
         SPACE 1                                                                
* NOW BUILD TABLE OF JULIAN DATES IF NEEDED *                                   
         SPACE 1                                                                
         OC    WBPERVRT,WBPERVRT   TEST ADDRESS OF PERVERT AVAILABLE            
         BZ    BLDWKSX                                                          
*                                                                               
         LA    R4,WBQSTB                                                        
         OC    0(3,R4),0(R4)       TEST REQUEST DATES PRESENT                   
         BNZ   *+8                                                              
         LA    R4,WBCMPST                                                       
         GOTO1 CDATCON,PARM,(3,(R4)),WORK    GET START                          
         GOTO1 (RF),(R1),(3,3(R4)),WORK+6    GET END                            
*                                                                               
         MVC   WORK+12(2),WORK             SET YEAR                             
         MVC   WORK+14(4),=C'0101'                                              
         XC    PARM(24),PARM                                                    
         GOTO1 WBPERVRT,PARM,WORK+12,WORK  GET DAY NUM OF START                 
         LH    R5,PARM+8                   SAVE JULIAN DATE                     
*                                                                               
BLDWKS20 PACK  DUB,WORK+2(2)       GET MONTH                                    
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DTBFJUL)    X WIDTH OF EACH MONTH                        
         L     RF,WBADTBF                                                       
         LA    RF,(DTBFJUL-DTBUFFD)(RF) POINT TO JULIAN DATES                   
         AR    RE,RF                    POINT TO MONTH                          
         PACK  DUB,WORK+4(2)            GET DAY                                 
         CVB   R0,DUB                                                           
         BCTR  R0,0                                                             
         AR    R0,R0               2 BYTES PER DAY                              
         AR    RE,R0               POINT TO SLOT                                
         STH   R5,0(RE)            SET JULIAN DATE IN SLOT                      
*                                                                               
         GOTO1 CADDAY,PARM,WORK,WORK,F'1'  ADVANCE DATE BY 1 DAY                
*                                                                               
         CLC   WORK(6),WORK+6     COMPARE TO CAMPAIGN END                       
         BH    BLDWKS30                                                         
*                                                                               
         LA    R5,1(R5)            ADVANCE JULIAN DATE                          
         B     BLDWKS20                                                         
         EJECT                                                                  
* NOW CONVERT MONTH & WEEK START/END DATES TO START/END QTR HRS *               
         SPACE 1                                                                
BLDWKS30 DS    0H                                                               
         L     R4,WBADTBF                                                       
         LA    R4,(DTBFWKS-DTBUFFD)(R4)  POINT TO WEEK LIST                     
*                                                                               
BLDWKS32 BAS   RE,BLDJULN                                                       
         STH   R1,4(R4)                                                         
         LA    R4,2(R4)            POINT TO END DATE                            
         BAS   RE,BLDJULN                                                       
         AH    R1,=H'95'           BUMP TO LAST QH                              
         STH   R1,4(R4)            AND SET AS END DATE                          
         LA    R4,6(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   BLDWKS32                                                         
*                                                                               
         L     R4,WBADTBF                                                       
         LA    R4,(DTBFMONS-DTBUFFD)(R4)  POINT TO MONTH LIST                   
*                                                                               
BLDWKS34 BAS   RE,BLDJULN          R4 POINTS TO MONTH START DATE                
         STH   R1,4(R4)                                                         
         LA    R4,2(R4)            R4 POINTS TO MONTH END DATE                  
         BAS   RE,BLDJULN                                                       
         AH    R1,=H'95'           BUMP TO LAST QH                              
         STH   R1,4(R4)                                                         
         LA    R4,6(R4)            ADVANCE TO NEXT ENTRY                        
         CLI   0(R4),X'FF'                                                      
         BNE   BLDWKS34                                                         
*                                                                               
BLDWKSX  B     EXIT                                                             
         SPACE 2                                                                
BLDJULN  DS    0H                                                               
         LH    R0,0(R4)                                                         
         SLL   R0,23               DROP YEAR                                    
         SRDL  R0,28               GET MONTH IN R0                              
         SRL   R1,27               AND DAY IN R1                                
         BCTR  R0,0                                                             
         MH    R0,=Y(L'DTBFJUL)                                                 
         L     RF,WBADTBF                                                       
         LA    RF,(DTBFJUL-DTBUFFD)(RF)   POINT TO SLOT FOR MONTH               
         AR    RF,R0                                                            
         BCTR  R1,0                                                             
         AR    R1,R1               DAY X 2                                      
         AR    RF,R1               POINT TO JULIAN DATE                         
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)          PICK UP JULIAN DATE                          
         BCTR  R1,0                DECREMENT                                    
         LA    R0,96               X QH PER DAY                                 
         MR    R0,R0               GIVES RESULT IN R1                           
         BR    RE                                                               
         EJECT                                                                  
*===============================*                                               
* SUBROUTINE READS GOAL RECORDS *                                               
*===============================*                                               
         SPACE 1                                                                
GETGOAL  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING GOLRECD,R2                                                       
*                                                                               
         MVI   GOLKTYPE,GOLKTYPQ                                                
         MVI   GOLKSTYP,GOLKSTPQ                                                
         MVC   GOLKAM,WBAGYMD                                                   
         MVC   GOLKCLT,WBCLT                                                    
         MVC   GOLKPRD,WBPRD                                                    
         MVC   GOLKCMP,WBCMP       MOVE CAMPAIGN TO KEY                         
*                                                                               
         GOTO1 AIO,IOSPTDIR+IOHI                                                
*                                                                               
         CLC   IOKEY(GOLKCMP+1-GOLKEY),IOKEYSAV TY/AM/CLT/PRD/CMP               
         BNE   EXIT                                                             
*                                                                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1                                           
*                                                                               
         MVI   WBMODE,WBPROCGL                                                  
         BAS   RE,GO                                                            
         B     EXIT                                                             
         EJECT                                                                  
* HOOK TO USER *                                                                
         SPACE 1                                                                
GO       NTR1                                                                   
         MVC   WBIOADDR,IOADDR     SET CURRENT REC FOR USER                     
         MVC   WBKEY,IOKEY                                                      
         ICM   RF,15,WBIOHOOK                                                   
         BZ    GOX                                                              
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
*                                                                               
GOX      XIT1                                                                   
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         EJECT                                                                  
*===================================================*                           
* SUBROUTINE MOVES REC AT R2 TO R1 FOR LENGTH IN R0 *                           
* RETURN ON RE (R0-R2 AND RF ARE DESTROYED)         *                           
*===================================================*                           
         SPACE 1                                                                
MOVEREC  LA    RF,256                                                           
         CR    R0,RF                                                            
         BNH   MOVEREC2                                                         
         MVC   0(256,R1),0(R2)                                                  
         AR    R1,RF                                                            
         AR    R2,RF                                                            
         SR    R0,RF                                                            
         B     MOVEREC                                                          
*                                                                               
MOVEREC2 LTR   RF,R0                                                            
         BZR   RE                                                               
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R1),0(R2) *EXECUTED*                                         
         SPACE 2                                                                
*==================================================                             
* SUBROUTINE CLEARS BUFFER AT R1 FOR LENGTH IN R0 *                             
*==================================================                             
         SPACE 1                                                                
CLRBUFF  LA    RF,256                                                           
         CR    R0,RF                                                            
         BNH   CLRBUFF2                                                         
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BP    CLRBUFF                                                          
*                                                                               
CLRBUFF2 LTR   RF,R0                                                            
         BZR   RE                                                               
         BCTR  RF,0                                                             
         EX    RF,*+6                                                           
         BR    RE                                                               
         XC    0(0,R1),0(R1) *EXECUTED*                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES (LOW ORDER 2 BYTES) SET FROM IO EQUATES *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSAV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)           *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSAV *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
         SPACE 1                                                                
IOEX     NTR1  WORK=(R7,IOWORKX-IOWORKD),LABEL=NO                               
         B     IOEX2                                                            
         SPACE 1                                                                
IOEX1    NTR1  BASE=SAVERB,WORK=(R7,IOWORKX-IOWORKD),LABEL=NO                   
         LM    R8,RC,SAVER8                                                     
         B     IOEX2                                                            
         SPACE 1                                                                
         USING IOWORKD,R7                                                       
         USING COMFACSD,R8                                                      
IOEX2    XC    IOWORKD(IOWORKX-IOWORKD),IOWORKD                                 
         ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         TM    IOCTRL+3,IOLOCK     TEST READ-FOR-UPDATE                         
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         TM    IOCTRL+3,IORDEL     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
         LA    R1,IO1+IO2          ESTABLISH I/O AREA ADDRESS                   
         N     R1,IOCTRL                                                        
         BZ    IOEX4                                                            
         SRL   R1,6                R1=I/O AREA NUMBER                           
         CLM   R1,1,IONUM                                                       
         BNH   *+6                                                              
         DC    H'0'                I/O AREA NUMBER INVALID                      
         SLL   R1,2                                                             
         L     R1,AIO1-4(R1)                                                    
         STCM  R1,15,IOADDR        SET ADDRESS OF I/O AREA                      
IOEX4    LA    R1,IOFILES          ESTABLISH FILE                               
         N     R1,IOCTRL                                                        
         BNZ   IOEX6                                                            
         OC    IOFILE,IOFILE                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         OC    IOCMND,IOCMND       FILE GIVEN - SO MUST COMMAND BE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         B     IOEX20                                                           
IOEX6    SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AFILNTRY         POINT TO LOCAL SYSTEM FILES                  
         LA    R0,10                                                            
         CR    R1,R0                                                            
         BNH   *+8                                                              
         L     RE,ASYSTAB          POINT TO GLOBAL SYSTEM FILES                 
         USING FILTABD,RE                                                       
IOEX8    CLI   FILNUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,FILNUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,FILTABL(RE)                                                   
         B     IOEX8                                                            
         MVC   IOFILV,FILNUM       EXTRACT FILE VALUES                          
         L     RE,ACMDTAB          RE=A(I/O COMMAND TABLE)                      
         SR    RF,RF                                                            
         LA    R1,IOCMNDS          ESTABLISH COMMAND                            
         N     R1,IOCTRL                                                        
         BNZ   IOEX10                                                           
         OC    IOCMND,IOCMND       NOT GIVEN - TEST COMMAND NAMED               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     IOEX20                                                           
IOEX10   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)                                                   
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     *+16                                                             
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX10                                                           
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
IOEX12   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX12                                                           
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
*                                                                               
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX14                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX16                                                           
         OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX14                                                           
         TM    IOFILI,FILIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,FILIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(4),IOCTRL                                                  
         NI    IODUB+2,X'F0'       TURN-OFF FILE INDICATORS                     
         L     R0,IODUB                                                         
         ZIC   R1,IOFILN2                                                       
         SLL   R1,8                                                             
         OR    R1,R0                                                            
         BAS   RE,IOEX                                                          
         BE    IOEX14              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
IOEX14   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK             
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         B     IOEXX               EXIT TO CALLER                               
IOEX16   TM    IOFILI,FILIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX20                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,FILIID      TEST I/S FILE HAS D/A ATTACHED               
         BZ    *+12                YES - MUST READ INTO IOAREA                  
         TM    IOFILI,FILIVL                                                    
         BZ    *+14                                                             
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   *+6                                                              
         DC    H'0'                                                             
IOEX18   GOTO1 CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                   
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS        TEST ANY ERRORS FOUND                        
         BZ    *+12                                                             
         TM    IOERR,IOEDEL        TEST DELETED RECORD FOUND                    
         BZ    IOEXX               NO - EXIT WITH ERROR                         
         TM    IOFILI2,FILIID      TEST D/A FILE ATTCHED TO THIS FILE           
         BZ    IOEXX               NO - EXIT                                    
         ZIC   R1,IOFILKL          YES - EXTRACT DISK ADDRESS                   
         ZIC   R0,IOFILCL                                                       
         AR    R1,R0                                                            
         LA    R1,IOKEY(R1)                                                     
         MVC   IODA,0(R1)                                                       
         B     IOEXX                                                            
IOEX20   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 CDATAMGR,PARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                    
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    L     RE,SAVER7           GET POINTER TO WBSBLOCK                      
         CLI   WBQTRACE-WBSBLKD(RE),C'Y'                                        
         BNE   *+8                                                              
         BAS   RE,IOTRACE                                                       
         MVI   IOQ,1                                                            
         TM    IOERR,IOERRS                                                     
         BZ    IOEXXX                                                           
         MVI   IOQ,2                                                            
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   IOEXXX                                                           
         MVI   IOQ,0                                                            
IOEXXX   CLI   IOQ,1               SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         SPACE 2                                                                
IOTRACE  NTR1                      ** IO TRACE ROUTINE **                       
         CLC   IOFILNM,=C'SPTDIR '                                              
         BE    *+14                                                             
         CLC   IOFILNM,=C'SPTFIL '                                              
         BNE   ITX                                                              
         CLI   IOCMDNO,IOHI                                                     
         BNE   IT2                                                              
         LA    RE,IOKEYSAV                                                      
         ST    RE,IOTR1                                                         
         MVI   IOTR1,13                                                         
         LA    RE,IOKEY                                                         
         ST    RE,IOTR2                                                         
         MVI   IOTR2,18                                                         
         B     IT8                                                              
IT2      CLI   IOCMDNO,IOSQ                                                     
         BNE   IT4                                                              
         LA    RE,IOKEY                                                         
         ST    RE,IOTR1                                                         
         MVI   IOTR1,13                                                         
         LA    RE,IOKEY                                                         
         ST    RE,IOTR2                                                         
         MVI   IOTR2,18                                                         
         B     IT8                                                              
IT4      CLI   IOCMDNO,IOGET                                                    
         BNE   IT6                                                              
         LA    RE,IOKEY+14                                                      
         ST    RE,IOTR1                                                         
         MVI   IOTR1,4                                                          
         L     RE,IOADDR                                                        
         ST    RE,IOTR2                                                         
         MVI   IOTR2,16                                                         
         B     IT8                                                              
IT6      CLI   IOCMDNO,IOGET                                                    
         BNE   ITX                                                              
         LA    RE,IOKEY                                                         
         ST    RE,IOTR1                                                         
         MVI   IOTR1,13                                                         
         L     RE,IOADDR                                                        
         ST    RE,IOTR2                                                         
         MVI   IOTR2,16                                                         
IT8      OC    IOPRINT,IOPRINT                                                  
         BZ    ITX                                                              
         MVC   P,BLANKS                                                         
         MVC   P(6),IOCMDNM                                                     
         MVC   P+8(6),IOFILNM                                                   
         LA    R3,P+16                                                          
         ZIC   R0,IOTR1                                                         
         GOTO1 CHEXOUT,PARM,IOTR1,(R3),(R0),=C'TOG'                             
         A     R3,PARM+16                                                       
         LA    R3,2(R3)                                                         
         ZIC   R0,IOTR2                                                         
         GOTO1 CHEXOUT,PARM,IOTR2,(R3),(R0),=C'TOG'                             
         GOTO1 IOPRINT,PARM,PASA,=C'BL01'                                       
*                                                                               
ITX      B     EXIT                                                             
         EJECT                                                                  
IOPRINT  DS    A                   A(PRINT)                                     
         LTORG                                                                  
         SPACE 2                                                                
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
*                                                                               
SAVER7   DS    F                                                                
SAVER8   DS    F                                                                
SAVER9   DS    F                                                                
SAVERA   DS    F                                                                
SAVERB   DS    F                                                                
SAVERC   DS    F                                                                
         EJECT                                                                  
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSPOOL)                                                      
PHASESN  EQU   *-PHASES                                                         
         SPACE 1                                                                
CONADDRS DS    0F                  ** WBSIO FACILITIES **                       
         DC    A(FILTAB)                                                        
         DC    A(SYSTAB)                                                        
         DC    A(CMDTAB)                                                        
         DC    A(IOEX)                                                          
         DC    A(IOEX1)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         EJECT                                                                  
* SYSTEM FILE NAMES TABLE (FILE NUMBERS 1 THRU 9)                               
*                                                                               
FILTAB   DS    0X                                                               
*                                  ** SPOT SYSTEM FILES **                      
FILSPT   DC    X'02',AL2(FILSPTX-*)                                             
*                                                                               
         DC    AL1(1),C'SPTDIR '   MUST BE FIRST TABLE ENTRY                    
         DC    AL1(FILIIS,FILIID)                                               
         DC    AL1(2,13,01),AL2(18)                                             
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(2),C'SPTFIL '   MUST BE SECOND TABLE ENTRY                   
         DC    AL1(FILIDA,FILIDI)                                               
         DC    AL1(1,13,24),AL2(4000)                                           
         DC    XL5'00'                                                          
*                                                                               
         DC    AL1(3),C'STATION'                                                
         DC    AL1(FILIIS,0)                                                    
         DC    AL1(0,17,01),AL2(117)                                            
         DC    XL5'00'                                                          
*                                                                               
FILSPTX  DC    AL1(EOT)                                                         
*                                                                               
FILTABX  DC    AL1(EOT)                                                         
         SPACE 1                                                                
* TABLE OF GLOBAL SYSTEM FILES (FILE NUMBERS 10 THRU 15)                        
*                                                                               
SYSTAB   DS    0X                                                               
*                                                                               
         DC    AL1(11),C'CTFILE '  CONTROL FILE                                 
         DC    AL1(FILIVL+FILIIS,0)                                             
         DC    AL1(0,25,29),AL2(2000)                                           
         DC    XL5'00'                                                          
*                                                                               
SYSTABX  DS    AL1(EOT)                                                         
         EJECT                                                                  
* SYSTEM FILE COMMANDS TABLE                                                    
*                                                                               
CMDTAB   DS    0X                                                               
*                                  INDEX SEQUENTIAL COMMANDS                    
CMDIS    DC    AL1(FILIVL+FILIIS,0),AL2(CMDISX-*)                               
         DC    C'DMRDHI ',AL1(IOHI,0,0)                                         
         DC    C'DMREAD ',AL1(IORD,0,0)                                         
         DC    C'DMRSEQ ',AL1(IOSQ,0,0)                                         
         DC    C'DMADD  ',AL1(IOADD,0,0)                                        
         DC    C'DMWRT  ',AL1(IOWRITE,0,0)                                      
CMDISX   DC    AL1(EOT)                                                         
*                                  DIRECT ACCESS COMMANDS                       
CMDDA    DC    AL1(FILIDA,0),AL2(CMDDAX-*)                                      
         DC    C'GETREC ',AL1(IOHI,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IORD,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOSQ,CMDIDARQ+CMDIDAXC,0)                         
         DC    C'GETREC ',AL1(IOGET,CMDIDARQ,0)                                 
         DC    C'ADDREC ',AL1(IOADDREC,CMDIDADD,0)                              
         DC    C'PUTREC ',AL1(IOPUTREC,CMDIDARQ,0)                              
CMDDAX   DC    AL1(EOT)                                                         
*                                                                               
CMDTABX  DC    AL1(EOT)                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
IOWORKD  DSECT                     ** IO S/R LOCAL W/S **                       
IODUB    DS    D                   GENERAL WORK AREA                            
IOCTRL   DS    XL4                 I/O COMMAND WORD                             
IOTR1    DS    F                   TRACE WORD 1                                 
IOTR2    DS    F                   TRACE WORD 2                                 
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
IOFILV   DS    0XL15               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    X                   FILE NUMBER                                  
IOFILNM  DS    CL7                 COMMAND NAME                                 
IOFILI   DS    X                   FILE INDICATORS - 1                          
IOFILI2  DS    X                   FILE INDICATORS - 2                          
IOFILN2  DS    X                   FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    X                   KEY LENGTH                                   
IOFILCL  DS    X                   CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOWORKX  EQU   *                                                                
         EJECT                                                                  
*                                  ** GLOBAL EQUATES **                         
EOT      EQU   0                   END-OF-TABLE INDICATOR                       
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
*                                  ** I/O FILE EQUATES **                       
IOFILES  EQU   X'0F00'             RESERVED FOR CONTROLLER USE                  
IOSPTDIR EQU   X'0100'             I/O TO SPTDIR                                
IOSPTFIL EQU   X'0200'             I/O TO SPTFILE                               
IOSTAFIL EQU   X'0300'             I/O TO STAFILE                               
IOCTFILE EQU   X'0B00'             I/O TO CONTROL FILE                          
         SPACE 1                                                                
*                                  ** I/O AREA EQUATES **                       
IO1      EQU   X'0040'             I/O AREA 1 TO BE USED FOR I/O                
IO2      EQU   X'0080'             I/O AREA 2 TO BE USED FOR I/O                
***IO3   EQU   X'00C0'             I/O AREA 3 TO BE USED FOR I/O                
         SPACE 1                                                                
*                                  ** I/O COMMAND EQUATES **                    
IOCMNDS  EQU   X'000F'             RESERVED FOR CONTROLLER USE                  
IOLOCK   EQU   X'0010'             READ FOR UPDATE                              
IORDEL   EQU   X'0020'             READ DELETED RECORDS                         
IOHI     EQU   X'0001'             DMRDHI                                       
IOHID    EQU   IOHI+IORDEL         DMRDHI (FOR DELETES)                         
IOHIUP   EQU   IOHI+IOLOCK         DMRDHI (FOR UPDATE)                          
IOHIUPD  EQU   IOHI+IOLOCK+IORDEL  DMRDHI (FOR UPDATE & DELETES)                
IORD     EQU   X'0002'             DMREAD                                       
IORDD    EQU   IORD+IORDEL         DMREAD (FOR DELETES)                         
IORDUP   EQU   IORD+IOLOCK         DMREAD (FOR UPDATE)                          
IORDUPD  EQU   IORD+IOLOCK+IORDEL  DMREAD (FOR UPDATE & DELETES)                
IOSQ     EQU   X'0003'             DMRSEQ                                       
IOSQD    EQU   IOSQ+IORDEL         DMRSEQ (FOR DELETES)                         
IOSQUP   EQU   IOSQ+IOLOCK         DMRSEQ (FOR UPDATE)                          
IOSQUPD  EQU   IOSQ+IOLOCK+IORDEL  DMRSEQ (FOR UPDATE & DELETES)                
IOGET    EQU   X'0004'             GETREC                                       
IOGETRUP EQU   IOGET+IOLOCK        GETREC (FOR UPDATE)                          
IOADD    EQU   X'0005'             DMADD                                        
IOADDREC EQU   X'0005'             ADDREC                                       
IOWRITE  EQU   X'0006'             DMWRT                                        
IOPUTREC EQU   X'0006'             PUTREC                                       
IOUNLOCK EQU   X'0007'             DMUNLK                                       
         EJECT                                                                  
FILTABD  DSECT                     ** I/O FILE TABLE **                         
FILNUM   DS    XL1                 FILE NUMBER (IOFILES EQUATE)                 
FILNAME  DS    CL7                 FILE NAME                                    
FILINDS  DS    XL1                 FILE INDICATORS - 1                          
FILIVL   EQU   X'80'               FILE RECORDS ARE VARIABLE LENGTH             
FILIDA   EQU   X'40'               FILE IS A DIRECT ACCESS FILE                 
FILIIS   EQU   X'20'               FILE IS AN INDEX SEQUENTIAL FILE             
FILINDS2 DS    XL1                 FILE INDICATORS - 2                          
FILIDI   EQU   X'80'               FILE IS D/A WITH I/S INDEX                   
FILIID   EQU   FILIDI              FILE IS I/S INDEX TO A D/A FILE              
FILNUM2  DS    XL1                 D/A OR I/S FILE NUMBER                       
FILKEYL  DS    XL1                 D/A OR I/S KEY LENGTH                        
FILCTLL  DS    XL1                 FIXED LENGTH I/S CONTROL LENGTH              
FILDISP  EQU   FILCTLL             DISPLACEMENT TO FIRST RECORD ELEMENT         
FILMAXL  DS    XL2                 MAXIMUM RECORD LENGTH                        
         DS    XL5                 SPARE                                        
FILTABL  EQU   *-FILTABD                                                        
         SPACE 1                                                                
CMDTABD  DSECT                     ** I/O COMMAND TABLE **                      
CMDNAME  DS    CL7                 COMMAND NAME                                 
CMDNUMB  DS    XL1                 COMMAND NUMBER (SEE IOCMNDS)                 
CMDINDS  DS    XL1                 COMMAND INDICATORS - 1                       
CMDIDARQ EQU   X'80'               DISK ADDRESS REQUIRED FOR I/O                
CMDIDAXC EQU   X'40'               CLEAR DISK ADDRESS BEFORE I/O                
CMDIDADD EQU   X'20'               DISK ADDRESS RETURNED FROM I/O               
CMDINDS2 DS    XL1                 COMMAND INDICATORS - 2                       
CMDTABL  EQU   *-CMDTABD                                                        
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
RELO     DS    A                                                                
USERRD   DS    A                   USER REGISTER 13                             
PARM     DS    8F                                                               
WORK     DS    XL64                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
*                                                                               
AFILNTRY DS    A                                                                
ACLTREC  DS    A                                                                
*                                                                               
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
BYTE3    DS    X                                                                
ELCODE   DS    X                                                                
FLAG     DS    X                                                                
INDS     DS    X                                                                
IONUM    DS    X                                                                
BLANKS   DS    CL133                                                            
XFF      DS    XL16                                                             
*                                                                               
PASA     DS    X                                                                
P        DS    CL132                                                            
*                                                                               
SVMTCHEL DS    CL12                                                             
SVBUYKEY DS    XL13                CURRENT BUY KEY                              
SVBYRBFA DS    A                                                                
SVCLTYPE DS    C                   I(NCL)/E(XCL)                                
*                                                                               
IOAREAST DS    0D                  ** I/O CONTROLLER VARIABLES **               
IOKEYEQ  DS    D                                                                
IOKEY    DS    CL32                ACTUAL KEY VALUE                             
IOKEYSAV DS    CL32                SAVED ACTUAL KEY VALUE (BEFORE I/O)          
IOADDR   DS    A                   I/O AREA ADDRESS                             
IODA     DS    XL4                 DISK ADDRESS OF D/A RECORD                   
IOWORK   DS    XL96                D/A LINKED FILES WORK AREA                   
         SPACE 1                                                                
IOFILE   DS    CL7                 FILE NAME                                    
IOCMND   DS    CL7                 DATAMGR COMMAND                              
IOERR    DS    XL1                 I/O ERROR RETURN BYTE                        
IOEEOF   EQU   X'80'               END-OF-FILE                                  
IOEDSK   EQU   X'40'               NON-RECOVERABLE DISK ERROR                   
IOEDUP   EQU   X'20'               DUPLICATE KEY ON ADD                         
IOERNF   EQU   X'10'               RECORD NOT FOUND                             
IOEDEL   EQU   X'02'               RECORD IS DELETED                            
IOERRS   EQU   X'FF'               RESERVED FOR CONTROLLER USE                  
IOAREAND EQU   *                                                                
         SPACE 1                                                                
COREFACS DS    0A                  ** T00A PHASE ROUTINES **                    
VSPOOL   DS    V                                                                
         SPACE 1                                                                
CONFACS  DS    0F                  ** WBSIO FACILITIES **                       
AFILTAB  DS    A                                                                
ASYSTAB  DS    A                                                                
ACMDTAB  DS    A                                                                
AIO      DS    A                                                                
VIO      DS    A                                                                
         SPACE 1                                                                
AIOAREAS DS    0F                  ** IO AREA ADDRESSES **                      
AIO1     DS    A                                                                
AIO2     DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE WBSBLOCK                                                       
         EJECT                                                                  
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
* AGYHDR                                                                        
         SPACE 1                                                                
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
* CLTHDR                                                                        
         SPACE 1                                                                
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
* PRDGRP                                                                        
         SPACE 1                                                                
PRDGRPD  DSECT                                                                  
       ++INCLUDE SPGENPRG                                                       
* PRDHDR                                                                        
         SPACE 1                                                                
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
* MKTREC                                                                        
         SPACE 1                                                                
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
         PRINT ON                                                               
* WUNDERMAN RECORDS *                                                           
         SPACE 1                                                                
       ++INCLUDE SPGENWBS                                                       
         EJECT                                                                  
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042WBSIOS    05/01/02'                                      
         END                                                                    
