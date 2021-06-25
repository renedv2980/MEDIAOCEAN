*          DATA SET SPREPBT02  AT LEVEL 115 AS OF 09/23/19                      
*PHASE SPBT02A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE DDUCOM                                                                 
*INCLUDE NETCOM                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'SPBT02 - SPOTPAK BILLING INTERFACE TAPES'                       
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-37827  08/28/19 AGENCY JS (GEOMETRY GLOBAL) FOR MAZDA     *         
* SMUR SPEC-24801  06/25/18 EBILLING FOR LMA - OMIT ZERO INVOICE 18.3 *         
* AKAT SPEC-24974  09/19/18 CARAT - EDI billing for Smuckers          *         
* SMUR SPEC-7724   02/08/18 MEDIABRANDS CANADA - INCLUDE BILL TAX CODE*         
* AKAT SPEC-15712  12/05/17 EBILLING FOR CARAT - GM - UPDATE FTP INFO *         
* YKVA SPEC-15712  11/15/17 EBILLING FOR CARAT - GM (LEVEL 109)       *         
* AKAT SPEC-5364   03/15/17 EBILLING FOR HAVAS - PHILIPS              *         
***********************************************************************         
*                                                                               
*  NOTE - AGYTAB ENTRIES FOR COLGATE AND AMGEN NO-OPED WITH                     
*         *ON HLD*                                                              
*                                                                               
***********************************************************************         
*  QOPT1 -   N = NO TAPE                                                        
*  QOPT2 -   TAPE SPEC CODE                                                     
*  QOPT3 -   Y = PRINT TAPE RECORD TRACE (ALSO NO MQ NOTIFICATION)              
*            T = MQ NOTIFICATION TO TEST  (DDS TESTING ONLY)                    
*            N = NO MQ NOTIFICATION (DDS TESTING ONLY)                          
*                (ALSO PUT TEST IN FILE NAME)                                   
*  QOPT4 -   M = START-END DATES ARE MOS                                        
*  QOPT5 -   C = DO ONLY COMMISSION ONLY BILLS, AND PRESERVE NET                
*            A = AOR ONLY, B=AOR AND AOR/CLIENT, X = NON-AOR ONLY               
*            2 = USE COS2 BILLS, N = NON-COMMISSION-ONLY BILLS                  
*            S = SOON BILLS ONLY                                                
*  QOPT5+1 - * = SHOW NETPAK SUB-MED INSTEAD OF CLIENT NUMBER                   
*            X = FILTER ON NETPAK SUB-MED=X                                     
*                                                                               
*                                                                               
*  BELOW FIELDS IN QAREA2 - Q2USER                                              
*                                                                               
* QINVNO1      COL 21(4) START INVOICE NUMBER                                   
* QINVNO2      COL 24(4) END INVOICE NUMBER                                     
*                                                                               
***********************************************************************         
         PRINT NOGEN                                                            
SPBT02   CSECT                                                                  
         NMOD1 0,SPBT02,R6,R8                                                   
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING SPBTWRKD,RC                                                      
*                                                                               
APLNP    EQU   7846                APL/WESTERN IDS- WESTERN MODE                
APLND    EQU   7847                               - WESTERN MODE                
APLNP1   EQU   7991                               - APL MODE                    
APLND1   EQU   7990                               - APL MODE                    
WIAPLP1  EQU   7996                               - QA MODE                     
WIAPLD2  EQU   7997                               - QA MODE                     
WAPLBT   EQU   8060                               - ?SUPPLANTS QA?              
*                                                                               
         SPACE 2                                                                
         CLI   MODE,PROCBILL                                                    
         BE    PRBL                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,OFCFRST                                                     
         BE    FBILO                                                            
         CLI   MODE,CLTFRST                                                     
         BE    FBILC                                                            
         CLI   MODE,PRDFRST                                                     
         BE    FBILP                                                            
         CLI   MODE,PRDLAST                                                     
         BE    LBILP                                                            
         CLI   MODE,CLTLAST                                                     
         BE    LBILC                                                            
         CLI   MODE,OFCLAST                                                     
         BE    LBILO                                                            
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNFRST                                                     
         BNE   SBT1                                                             
         RELOC RELO                                                             
         L     RF,=A(RUNF)                                                      
         ST    RF,ARUNF                                                         
         GOTO1 ARUNF                                                            
         B     EXIT                                                             
*                                                                               
SBT1     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         GOTO1 ARUNL                                                            
         B     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        REQUEST FIRST                                                          
         SPACE 2                                                                
REQF     DS    0H                                                               
*                                                                               
         XC    LOWINV,LOWINV     CLEAR LOW INVOICE NUMBER FILTER                
         MVC   HIINV,=X'FFFF'    SET HIGH INVOICE NUMBER FILTER TO MAX          
         CLC   QINVNO1,SPACES                                                   
         BE    REQF0A                                                           
         LA    R2,QINVNO1                                                       
         LA    R3,LOWINV                                                        
         LA    RE,REQF0A                                                        
         CLI   QINVNO1,C'0'                                                     
         BL    REQFA1                                                           
         PACK  DUB,QINVNO1                                                      
         CVB   R0,DUB                                                           
         STH   R0,LOWINV                                                        
         B     REQF0A                                                           
*                                                                               
REQFA1   DS    0H               ALPHA 1ST CHARACTER                             
         MVC   WORK+2(3),1(R2)   NUMERIC PART                                   
         MVC   WORK(2),=C'10'    OVER TEN THOUSAND                              
         CLI   0(R2),C'A'                                                       
         BE    REQFA1X                                                          
         MVC   WORK(2),=C'11'    OVER ELEVEN THOUSAND                           
         CLI   0(R2),C'B'                                                       
         BE    REQFA1X                                                          
         MVC   WORK(2),=C'12'    OVER TWELVE THOUSAND                           
         CLI   0(R2),C'C'                                                       
         BE    REQFA1X                                                          
         MVC   WORK(2),=C'13'    OVER THIRTEEN THOUSAND                         
         CLI   0(R2),C'D'                                                       
         BE    REQFA1X                                                          
         MVC   WORK(2),=C'14'    OVER FOURTEEN THOUSAND                         
         CLI   0(R2),C'E'                                                       
         BE    REQFA1X                                                          
         MVC   WORK(2),=C'15'    OVER FIFTEEN THOUSAND                          
         CLI   0(R2),C'F'                                                       
         BE    REQFA1X                                                          
         DC    H'0'             SHOULD NEVER REALLY HAPPEN                      
*                                                                               
REQFA1X  PACK  DUB,WORK(5)                                                      
         CVB   R0,DUB                                                           
         STH   R0,0(R3)                                                         
         BR    RE               RETURN                                          
*                                                                               
REQF0A   DS    0H                                                               
         CLC   QINVNO2,SPACES                                                   
         BE    REQF0B                                                           
         LA    R2,QINVNO2                                                       
         LA    R3,HIINV                                                         
         LA    RE,REQF0B                                                        
         CLI   QINVNO2,C'0'    CHECK IF ALPHA                                   
         BL    REQFA1                                                           
*                                                                               
         PACK  DUB,QINVNO2                                                      
         CVB   R0,DUB                                                           
         STH   R0,HIINV                                                         
         B     REQF0B                                                           
*                                                                               
REQF0B   DS    0H                                                               
         MVI   TESTMQ,C'P'         SET TO PROD MQ NOTIFICATION                  
         CLI   QOPT3,C'N'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT3,C'Y'          TRACING OUTPUT RECORDS                       
         BNE   *+8                 ALSO SUPPRESS MQ                             
         MVI   TESTMQ,C'N'         SUPPRESS MQ                                  
         CLI   QOPT3,C'T'                                                       
         BNE   *+8                                                              
         MVI   TESTMQ,C'T'         TEST MQ                                      
*                                                                               
         CLC   QAGY,=C'G7'       GSTX?                                          
         BNE   *+14                                                             
         MVI   G7SW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'M$'       OR MULLEN                                      
         BNE   *+14                                                             
         MVI   G7SW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'MC'       MCCANN - TYPE C                                
         BNE   *+22                                                             
         CLI   QOPT2,C'C'                                                       
         BNE   *+14                                                             
         MVI   MCCSW,1                                                          
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'UB'       CARAT  - TYPE D - DIAGEO                       
         BNE   REQF0C                                                           
         CLI   QOPT2,C'D'                                                       
         BNE   *+14                                                             
         MVI   DGSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
         CLI   QOPT2,C'S'        CARAT - SMUCKERS?                              
         BNE   *+8               NO                                             
         MVI   SMSW,1            YES - SET SMSW TO 1                            
*                                                                               
REQF0C   CLC   QAGY,=C'H7'       MSNY - TYPE S  SC JOHNSON                      
         BNE   *+22                                                             
         CLI   QOPT2,C'S'                                                       
         BNE   *+14                                                             
         MVI   SCSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'BN'       PHDNY- TYPE S  SC JOHNSON                      
         BNE   *+22                                                             
         CLI   QOPT2,C'S'                                                       
         BNE   *+14                                                             
         MVI   SCSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'OO'      OPHDNA- TYPE S  SC JOHNSON                      
         BNE   *+22                                                             
         CLI   QOPT2,C'S'                                                       
         BNE   *+14                                                             
         MVI   SCSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'H7'       MSNY - TYPE M - MAZDA                          
         BNE   *+22                                                             
         CLI   QOPT2,C'M'                                                       
         BNE   *+14                                                             
         MVI   MZSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'JS'       JACTAK TYPE M - MAZDA                          
         BNE   *+22                                                             
         CLI   QOPT2,C'M'                                                       
         BNE   *+14                                                             
         MVI   MZSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'BN'      PHDNY - TYPE B  STARBUCK                        
         BNE   *+22                                                             
         CLI   QOPT2,C'B'                                                       
         BNE   *+14                                                             
         MVI   SBSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
         CLC   QAGY,=C'OO'      OMDUSEC - TYPE B  STARBUCK                      
         BNE   REQF0X                                                           
         CLI   QOPT2,C'B'                                                       
         BNE   REQF0X                                                           
         MVI   SBSW,1                                                           
         MVC   SVQOPT3,QOPT3                                                    
*                                                                               
REQF0X   L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+8                                                              
         MVI   NETPAKSW,C'N'                                                    
         DROP  RF                                                               
*                                  SET OFF VARIOUS WESTERN/APL SWITCHES         
         MVI   WIQASW,C'N'         QA MODE                                      
         MVI   WIAPLSW,C'N'        SET WESTERN/APL WESTERN MODE                 
         MVI   WAPLBTSW,C'N'       WA MODE                                      
*                                                                               
         CLC   RCORIGID,=Y(WAPLBT) SPECIAL WA ID                                
         BNE   *+8                                                              
         MVI   WAPLBTSW,C'Y'                                                    
*                                                                               
         CLI   NETPAKSW,C'Y'       ON ONLY FOR NETPAK                           
         BNE   REQF1H              BUT QA AND WA STUFF ONLY FOR SPOT            
*                                                                               
         MVI   WIAPLSW,C'W'                                                     
         CLC   RCORIGID,=Y(APLNP)                                               
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(APLND)                                               
         BE    REQF1J                                                           
         MVI   WIAPLSW,C'A'        SET WESTERN/APL APL MODE                     
         CLC   RCORIGID,=Y(APLNP1)                                              
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(APLND1)                                              
         BE    REQF1J                                                           
         MVI   WIAPLSW,C'N'                                                     
         B     REQF1J                                                           
*                                  SPOT, NOT NET                                
REQF1H   DS    0H                                                               
         MVI   WIQASW,C'Y'                                                      
         CLC   RCORIGID,=Y(WIAPLP1)   2 SPECIAL QA ID'S                         
         BE    REQF1J                                                           
         CLC   RCORIGID,=Y(WIAPLD2)                                             
         BE    REQF1J                                                           
         MVI   WIQASW,C'N'                                                      
*                                                                               
REQF1J   DS    0H                                                               
         XC    REQINVS,REQINVS   CLEAR REQUEST                                  
         XC    OFFINVS,OFFINVS   OFFICE                                         
         XC    CLTINVS,CLTINVS   AND CLIENT INVOICE TOTALS                      
*                                                                               
         L     RF,ADAGY                                                         
         MVC   CNTRY,AGYPROF+7-AGYHDR(RF)   SET COUNTRY                         
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
         L     RF,=A(HHROUT)                                                    
         ST    RF,HEADHOOK                                                      
         L     RF,=A(TPFMT)                                                     
         ST    RF,ATPFMT                                                        
         L     RF,=A(RUNL)                                                      
         ST    RF,ARUNL                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVQST,QSTART        SAVE QSTART AND END                          
         CLC   QEND,SPACES         ANY END DATE PROVIDED?                       
         BNE   *+10                                                             
         MVC   QEND,QSTART         NO -- USE QSTART (SINGLE DATE RQST)          
         MVC   SVQEND,QEND                                                      
*                                                                               
*Y2K*                                                                           
         MVC   QSTART,=C'700101'   SET LONG RANGE START                         
         MVI   QEND,X'FF'            AND END FOR SPONSOR                        
         MVC   QEND+1(5),=C'91231'                                              
         MVC   DUB,SVQST                                                        
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQSTART)                                      
         MVC   DUB,SVQEND                                                       
         OC    DUB(6),=6C'0'                                                    
         GOTO1 DATCON,DMCB,DUB,(3,BQEND)                                        
*                                 SET VALUES FOR RUN-DATE FILTERING             
         GOTO1 DATCON,DMCB,TODAY,(2,TODAYC)      COMPRESSED                     
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,TODAYB)                                     
         ZIC   R1,TODAYB                                                        
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         M     R0,=F'10'                                                        
         STC   R1,DECADE           1980,1990,2000,2010, ETC.                    
*                                  HEX 50,5A,64,6E...                           
         MVC   YEARDIG,TODAY+1     GET YEAR WITHIN DECADE                       
         NI    YEARDIG,X'FF'-X'F0' ISOLATE YEAR DIGIT                           
*                                                                               
         XC    STARTMOS,STARTMOS   SET START MOS TO LOWEST POSSIBLE             
         MVC   ENDMOS,=X'FFFF'     SET END MOS TO HIGHEST POSSIBLE              
         CLC   QMOSSTRT,SPACES                                                  
         BE    REQF1N              NO START MOS FILTER                          
         MVC   DUB(4),QMOSSTRT                                                  
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   STARTMOS,THREE                                                   
         CLC   QMOSEND,SPACES                                                   
         BE    REQF1N              NO END MOS FILTER                            
         MVC   DUB(4),QMOSEND                                                   
         MVC   DUB+4(2),=C'01'     FOR DATCON (COMPLETE DATE REQUIRED)          
         GOTO1 DATCON,DMCB,DUB,(3,THREE)                                        
         MVC   ENDMOS,THREE                                                     
*                                                                               
REQF1N   DS    0H                                                               
         XC    ASPECS,ASPECS                                                    
         CLI   QOPT1,C'N'          SKIP TAPE                                    
         BE    REQF16                                                           
*                                                                               
         CLI   WIAPLSW,C'W'        FOR WESTERN/APL                              
         BNE   *+12                                                             
         L     R4,=A(WIAPAGY)      USE WESTERN TAPE SPEC                        
         B     REQF12                                                           
*                                                                               
         USING AGYTABD,R4                                                       
*                                                                               
         CLI   WIQASW,C'Y'         FOR QA SPECIAL                               
         BNE   *+12                                                             
         L     R4,=A(APAGY)        USE AP TAPE, NOT WESTERN                     
         B     REQF12                                                           
*                                                                               
         CLI   WAPLBTSW,C'Y'       FOR WA SPECIAL                               
         BNE   *+12                                                             
         L     R4,=A(APAGY)        USE AP TAPE, NOT WESTERN                     
         B     REQF12                                                           
*                                                                               
         L     R4,=A(AGYTAB)       ELSE GET TAPE SPEC FROM AGYTAB               
*                                                                               
REQF2    DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    REQF10                                                           
         CLC   QAGY,AGYTAGY        AGY (2)                                      
         BNE   REQF4                                                            
         CLC   QOPT2,AGYTAPE       MUST MATCH REQ TYPE                          
         BNE   REQF4               NOTE-BLANK IS NORMAL                         
         CLI   AGYTMED,C'Z'        ALL MEDIA                                    
         BE    *+14                                                             
         CLC   QMED,AGYTMED        ONE MEDIA                                    
         BNE   REQF4                                                            
         CLC   AGYTCLT,SPACES      ALL CLIENTS?                                 
         BE    REQF12                                                           
         CLC   QCLT,AGYTCLT        ONE CLIENT                                   
         BE    REQF12                                                           
*                                                                               
REQF4    DS    0H                                                               
         LA    R4,AGYTABL(R4)                                                   
         B     REQF2                                                            
*                                                                               
REQF10   DS    0H                                                               
         MVC   P(38),=C'**NO TAPE SPECS FOUND - REQ BYPASSED**'                 
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQF12   DS    0H                                                               
         MVC   RECLEN,AGYTLRCL     SAVE RECORD LENGTH                           
         MVC   CONTROLS,AGYTCTRL   SAVE CONTROLS                                
         L     R5,=A(SBITAPE)                                                   
         CLI   OPENSW,C'N'                                                      
         BNE   REQF13                                                           
         USING IHADCB,R5                                                        
         OI    DCBRECFM,DCBRECBR   BLOCKED RECORDS                              
         CLI   AGYRECFM,C'F'       'F' = FIXED, 'V' = VARIABLE                  
         BNE   *+12                                                             
         OI    DCBRECFM,DCBRECF    FIXED BLOCK                                  
         B     *+8                                                              
         OI    DCBRECFM,DCBRECV    VARIABLE BLOCK                               
         MVC   DCBBLKSI,AGYTBLKS   BLOCK SIZE                                   
         MVC   DCBLRECL,AGYTLRCL   RECORD SIZE                                  
*                                                                               
REQF13   DS    0H                                                               
         CLC   DCBBLKSI,AGYTBLKS   BLOCK SIZE                                   
         BNE   REQF13X                                                          
         CLC   DCBLRECL,AGYTLRCL   RECORD SIZE                                  
         BNE   REQF13X                                                          
         DROP  R5                                                               
         CLI   OPENSW,C'N'                                                      
         BNE   REQF14                                                           
         MVI   OPENSW,C'Y'                                                      
         CLI   QOPT1,C'N'          TEST DOING TAPE                              
         BE    REQF14                                                           
         MVI   OPENSW,C'T'         SET TAPE REALLY OPEN                         
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         LA    R3,NEDYNDSN         NETPAK                                       
         MVI   NETPAKSW,C'Y'                                                    
         CLI   MCNETPAK,C'Y'                                                    
         BE    *+12                                                             
         MVI   NETPAKSW,C'N'                                                    
         LA    R3,SPDYNDSN         OR SPOT                                      
         DROP  RF                                                               
*                                                                               
         CLC   QAGY,=C'H7'          MSNYA                                       
         BNE   FBLR1A                                                           
         CLI   QOPT4,C'A'           AMGEN FILE?  OR CHECK ID?                   
         BNE   *+8                                                              
         MVI   15(R3),C'2'       USE SUFFIX 2                                   
*                                                                               
         CLI   QOPT4,C'C'           COLGATE FILE?  OR CHECK ID?                 
         BNE   *+8                                                              
         MVI   15(R3),C'3'       USE SUFFIX 2                                   
*                                                                               
         B     FBLR1C               ELSE LEAVE AS 1                             
*                                                                               
FBLR1A   CLC   QAGY,=C'MC'                                                      
         BNE   FBLR1B                                                           
         CLC   RCORIGID,=X'2D33'     MCBT  # 11571                              
         BNE   *+12                                                             
         MVI   15(R3),C'2'      USE SUFFIX 2 INSTEAD OF 1                       
         B     FBLR1C                                                           
         CLC   RCORIGID,=X'2C95'     MCTT  # 11413                              
         BNE   *+12                                                             
         MVI   15(R3),C'3'      USE SUFFIX 3 INSTEAD OF 1                       
         B     FBLR1C                                                           
         CLC   RCORIGID,=X'39F0'    UMCHR  # 14832                              
         BNE   *+12                                                             
         MVI   15(R3),C'4'      USE SUFFIX 4 INSTEAD OF 1                       
         B     FBLR1C                                                           
         CLC   RCORIGID,=X'2D21'    UMLOU  # 11553                              
         BNE   *+12                                                             
         MVI   15(R3),C'5'      USE SUFFIX 5 INSTEAD OF 1                       
         B     FBLR1C                                                           
         CLC   RCORIGID,=X'038D'    UMLO   # 909                                
         BNE   *+12                                                             
         MVI   15(R3),C'5'      USE SUFFIX 5 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
         B     FBLR1C                                                           
*                                                                               
*        FOR ANY OTHER MCCANN ID'S LEAVE SUFFIX AS 1                            
*                                                                               
FBLR1B   CLC   QAGY,=C'U#'                                                      
         BNE   FBLR1B3                                                          
         CLC   RCORIGID,=X'39E6'     UMTOA  # 14822                             
         BNE   *+12                                                             
         MVI   15(R3),C'2'      USE SUFFIX 2 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
         CLC   RCORIGID,=X'3E71'     INMETO # 15985                             
         BNE   *+12                                                             
         MVI   15(R3),C'3'      USE SUFFIX 3 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
         CLC   RCORIGID,=X'3FB6'     CITO   # 16310                             
         BNE   *+12                                                             
         MVI   15(R3),C'5'      USE SUFFIX 5 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
         CLC   RCORIGID,=X'3FB7'     RMTO   # 16311                             
         BNE   *+12                                                             
         MVI   15(R3),C'6'      USE SUFFIX 6 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
         CLC   RCORIGID,=X'3E66'     INMETO # 15974                             
         BNE   *+12                                                             
         MVI   15(R3),C'4'      USE SUFFIX 4 INSTEAD OF 1                       
         B     FBLR1C                                                           
*                                                                               
FBLR1B3  CLC   QAGY,=C'WI'        ALTER NAME FOR SOME WILA IDS                  
         BNE   FBLR1B4                                                          
         CLC   RCORIGID,=H'15634'   IPGSAPWI                                    
         BNE   *+12                                                             
         MVI   15(R3),C'2'                                                      
         B     FBLR1C                                                           
*                                                                               
         CLC   RCORIGID,=H'15635'   WILAUSA                                     
         BNE   *+12                                                             
         MVI   15(R3),C'3'                                                      
         B     FBLR1C                                                           
         B     FBLR1C                                                           
*                                                                               
FBLR1B4  CLC  QAGY,=C'BN'       PHD                                             
         BNE  FBLR1B5                                                           
         CLI  QOPT2,C'B'        SEE IF STARBUCKS FORMAT                         
         BNE  FBLR1C            IF NOT LEAVE SUFFIX ALONE                       
         MVI  15(R3),C'2'       IF SO ALTER SUFFIX TO 2                         
         B    FBLR1C                                                            
*                                                                               
FBLR1B5  CLC  QAGY,=C'OO'       OMD                                             
         BNE  FBLR1B6                                                           
         CLI  QOPT2,C'B'        SEE IF STARBUCKS FORMAT                         
         BNE  FBLR1C            IF NOT LEAVE SUFFIX ALONE                       
         MVI  15(R3),C'2'       IF SO ALTER SUFFIX TO 2                         
         B    FBLR1C                                                            
*                                                                               
FBLR1B6  CLC  QAGY,=C'UB'       CARAT?                                          
         BNE  FBLR1C            NO                                              
         CLI  QOPT2,C'G'        GM FORMAT?                                      
         BNE  *+8               IF NOT LEAVE SUFFIX ALONE                       
         MVI  15(R3),C'2'       ALTER SUFFIX TO 2 FOR GM                        
         CLI  QOPT2,C'S'        SMUCKERS FORMAT?                                
         BNE  *+8               IF NOT LEAVE SUFFIX ALONE                       
         MVI  15(R3),C'3'       ALTER SUFFIX TO 3 FOR SMUCKERS                  
         B    FBLR1C                                                            
*                                                                               
FBLR1C   DS    0H                                                               
         MVC   13(2,R3),QAGY                                                    
*                                                                               
         CLC   QAGY,=C'G+'      WINGLATINO                                      
         BNE   REQF13B                                                          
         MVC   13(2,R3),=C'G@'       USE G@ SINCE + SIGN CAN'T BE               
         B     REQF13M                                                          
*                                                                               
REQF13B  DS    0H                                                               
         CLC   QAGY,=C'SJ'      SJR TESTING FOR SC JOHNSON                      
         BE    REQF13B2                                                         
         CLC   QAGY,=C'BN'      PHDNY SC JOHNSON                                
         BE    REQF13B2                                                         
         CLC   QAGY,=C'OO'      OPHDNA SC JOHNSON                               
         BE    REQF13B2                                                         
         CLC   QAGY,=C'JS'      AGENCY JS?                                      
         BE    REQF13B2                                                         
         CLC   QAGY,=C'H7'      MINDSHARE?                                      
         BNE   REQF13M                                                          
REQF13B2 CLI   QOPT2,C'S'      FILE TYPE S - SC JOHNSON                         
         BNE   REQF13B3                                                         
         MVI   SCSFTP,C'Y'     DOING SC JOHNSON SFTP                            
         B     REQF13B7                                                         
*                                                                               
REQF13B3 CLC   QAGY,=C'BN'     PHDNY - IF NOT SCJ                               
         BE    REQF13M         NOT SFTP STYLE                                   
         CLC   QAGY,=C'OO'     OPHDNA- IF NOT SCJ                               
         BE    REQF13M         NOT SFTP STYLE                                   
*                              MUST BE H7 OR SJ (FOR NOW)                       
*                              CONTINUE CHECKING OTHER FORMATS                  
*                                                                               
REQF13B5 CLI   QOPT2,C'B'      FILE TYPE B - BURGER KING                        
         BNE   REQF13B6                                                         
         MVI   BKSFTP,C'Y'      DOING BURGER KING SFTP                          
         B     REQF13B7                                                         
*                                                                               
REQF13B6 CLI   QOPT2,C'M'      FILE TYPE M - MAZDA                              
         BNE   REQF13M                                                          
         MVI   MZSFTP,C'Y'      DOING MAZDA SFTP                                
*                                                                               
REQF13B7 MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         MVC   DSNAME+4(3),0(R3)      SPT OR NET                                
         MVI   DSNAME+7,C'.'                                                    
         CLI   QOPT2,C'B'             SEE IF BURGER KING                        
         BNE   REQF13B8                                                         
*                                                                               
         MVC   DSNAME+8(4),=C'H7BK'   MINDSHARE-BURGER KING                     
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+10                                                             
         MVC   DSNAME+8(4),=C'H7SK'    ALTER FOR SPOT                           
         B     REQF13B9                                                         
*                                                                               
REQF13B8 L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   DSNAME+8(4),MCAGYCOD-MCEXTRA(R1)                                 
         CLI   QOPT2,C'M'          MINDSHARE - MAZDA                            
         BNE   REQF13B9            FOR SCJ (S) USE AGENCY ID                    
         MVC   DSNAME+8(4),=C'H7MZ'                                             
         CLC   QAGY,=C'JS'         AGENCY JS?                                   
         BNE   REQF13B9            NO                                           
         MVC   DSNAME+8(4),=C'JSMZ'                                             
         DROP  RF                                                               
*                                                                               
REQF13B9 MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM,=C'SFTPDISK.PROD.'                                       
         CLI   QOPT3,C'Y'   PDUMPING OUTPUT - SET TO TEST                       
         BE    REQF13D                                                          
         CLI   QOPT3,C'N'   NO MQ NOTIFICATION                                  
         BE    REQF13D      ALSO PUT 'TEST' IN MQMAPNM                          
         CLI   TESTMQ,C'T'  PUTTING TO TEST BROKER?                             
         BNE   *+10                                                             
REQF13D  MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
         MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'SBITAPE '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
         OPEN  (SBITAPE,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     REQF14                                                           
*                                                                               
* PREVENT DYNALLOC FAILURES BY SERIALIZING ON THE DSN                           
*                                                                               
REQF13M  DS    0H                                                               
         ENQ   (MAJORNAM,(3),E,DSNLENQ,SYSTEM)                                  
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'SBITAPE '),(0,0(R3))                         
         OPEN  ((R5),OUTPUT)                                                    
         B     REQF14                                                           
REQF13X  DS    0H                                                               
         L     RE,=A(MULTMSG)                                                   
         MVC   P(52),0(RE)                                                      
         BRAS  RE,PRNT                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
REQF14   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,7,AGYTSPCS       A(SPECS)                                     
         ST    RF,ASPECS                                                        
         DROP  R4                                                               
*                                                                               
REQF16   DS    0H                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CLRTOTS                                                       
         XC    LSTBLKY,LSTBLKY                                                  
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
*                                                                               
         MVI   OFFICE,0            SET NO OFFICE                                
*                                  CLEAR SAVED TAPE REC AREA                    
*                                                                               
*        NOTE - SVTPREC CAN'T BE USED FOR FILES WHOSE                           
*               LRECL IS OVER 600                                               
*                                                                               
         LA    RE,SVTPREC                                                       
         LHI   R1,L'SVTPREC-1                                                   
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         XC    SVBILL,SVBILL       AND SAVED BILL AREA                          
         LA    R3,SVBAMTS          AND SAVED BILL VALUES                        
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR OFFICE                                                       
         SPACE 2                                                                
FBILO    DS    0H                                                               
*                                                                               
         XC    CLTINVS,CLTINVS     CLEAR CLIENT                                 
         XC    OFFINVS,OFFINVS     AND OFFICE INVOICE TOTALS                    
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CLRTOTS                                                       
         L     RF,ADCLT            SAVE OFFICE CODE                             
         MVC   OFFICE,COFFICE-CLTHDR(RF)                                        
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 3                                                                
*        FIRST FOR CLI                                                          
         SPACE 2                                                                
FBILC    DS    0H                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         MVC   SAVCOFF(1),0(RF)      SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'N'       NETPAK                                    
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+8                                                              
         MVI   OFFD.OFCSYS,C'S'       ELSE SPOT                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGY                                                 
         MVC   OFFD.OFCPMED,QMED                                                
         MVC   OFFD.OFCOFC,0(RF)     FROM CLIENT HEADER                         
*                                                                               
         GOTO1 MVOFFICE,DMCB,(C'2',WORK),(0,ACOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   TBCLTF0                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
TBCLTF0  DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
*                                                                               
*                                                                               
         CLC   =C'*BILLING',QUESTOR  IF GENERATED BY B1/BU                      
         BNE   FBILC2                                                           
         CLI   PROGPROF+2,C'Y'     DOES THIS CLIENT WANT BT?                    
         BNE   FBILC3              NO, SKIP IT                                  
*                                                                               
FBILC2   CLC   AGY,=C'PU'          FOR AMMARATTI/PURIS                          
         BNE   FBILC3D                                                          
         CLI   QOPT1,C'N'          IF DOING TAPE                                
         BE    FBILC3D                                                          
         CLC   CLT,=C'COM'         SKIP THESE CLIENTS                           
         BE    FBILC3                                                           
         CLC   CLT,=C'CDI'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CMI'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'MIN'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'MLO'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CDM'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'LCO'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'CCW'                                                      
         BE    FBILC3                                                           
         CLC   CLT,=C'BAC'                                                      
         BE    FBILC3                                                           
         BNE   FBILC3D                                                          
*                                                                               
FBILC3   DS    0H                                                               
         MVI   MODE,CLTLAST        SKIP CLIENT                                  
         B     EXIT                                                             
*                                                                               
FBILC3D  DS    0H                                                               
         CLI   QOPT2,C'T'          IF IT IS THE TOYOTA SPECIAL                  
         BNE   FBILC3G                                                          
         CLC   AGY,=C'DF'          FOR SAATCHI                                  
         BE    FBILC3F                                                          
         CLC   AGY,=C'TH'          OR ZENITH                                    
         BNE   FBILC3G                                                          
*                                                                               
FBILC3F  DS    0H                                                               
         CLC   CLT,=C'TM '         DO ONLY THESE CLIENTS                        
         BE    FBILC3G                                                          
         CLC   CLT,=C'TMS'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LEX'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LDA'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'LDL'                                                      
         BE    FBILC3G                                                          
         CLC   CLT,=C'TOC'                                                      
         BE    FBILC3G                                                          
         B     FBILC3                                                           
*                                                                               
FBILC3G  DS    0H                                                               
         CLI   WIAPLSW,C'N'        FOR WESTERN/APL EITHER MODE                  
         BE    FBILC7                                                           
*                                                                               
         BAS   RE,WIAPLCT          DO ONLY WI/APL CLIENTS                       
         BE    FBILC7                                                           
         MVI   MODE,CLTLAST        SKIP THIS CLENT                              
         B     EXIT                                                             
*                                                                               
FBILC7   DS    0H                                                               
         CLI   WAPLBTSW,C'N'       FOR WESTERN/APL WA ID                        
         BE    FBILC7B                                                          
*                                                                               
         BAS   RE,WAPLBTCT         DO ONLY WI/APL CLIENTS                       
         BE    FBILC7B                                                          
         MVI   MODE,CLTLAST        SKIP THIS CLENT                              
         B     EXIT                                                             
*                                                                               
FBILC7B  DS    0H                                                               
         SR    R0,R0               SET INVOICE LIST BINSRCH PARS                
         L     R1,=A(INVTAB)                                                    
         SR    R2,R2                                                            
         LHI   R3,3                                                             
         LHI   R4,3                                                             
         L     R5,=A(INVMAX)                                                    
         STM   R0,R5,INVPARS                                                    
         XC    CLTINVS,CLTINVS                                                  
*                                                                               
         ZAP   OBCOUNT,=P'0'       OB10  RECORD COUNT                           
         ZAP   OBTOTAL,=P'0'       OB10 $ TOTAL                                 
         MVI   OBISKIP,C'N'                                                     
*                                                                               
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         XC    SV00APRF,SV00APRF   READ 00A PROFILE                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),AGY                                                    
         GOTO1 GETPROF,DMCB,WORK,SV00APRF,DATAMGR                               
*                                                                               
         XC    B1PROF,B1PROF       READ B1 PROFILE                              
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING PROFKD,RE                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B1'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
*                                                                               
         XC    B1XPROF,B1XPROF     READ B1X PROFILE                             
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         MVI   PROFKSYS,C'S'-X'40' MAKE SYSTEM LOWER CASE                       
         MVC   PROFKPGM,=C'B1X'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         DROP  RE                                                               
*                                                                               
         CLI   PROGPROF+0,C'Y'     NEW PAGE PER CLIENT?                         
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                  SET CLIENT DATA IN SVMID                     
         MVI   NEWCLT,C'Y'                                                      
         MVC   SVMID,SPACES                                                     
         MVC   SVMID(7),=C'CLIENT='                                             
         MVC   SVMID+8(3),CLT                                                   
         MVC   SVMID+12(24),CLTNM                                               
*                                                                               
         LA    RF,SVMID+36                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         L     RE,ADCLT                                                         
         CLI   CCLTIFC-CLTHDR(RE),C' '                                          
         BNH   FBILC9                                                           
         MVI   2(RF),C'('                                                       
         MVC   3(8,RF),CCLTIFC-CLTHDR(RE)                                       
         LA    RF,11(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
FBILC9   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
*        FIRST FOR PRODUCT                                                      
         SPACE 2                                                                
FBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CLRTOTS                                                       
*                                                                               
         CLI   PROGPROF+1,C'Y'     NEW PAGE PER PRODUCT?                        
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*        LAST FOR PRODUCT                                                       
         SPACE 2                                                                
LBILP    DS    0H                                                               
         LA    R3,PAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,PAMTS                                                         
         LA    R4,CAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+10(20),=C'** PRODUCT TOTALS **'                                
         LA    R3,PAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST  FOR CLIENT                                                       
         SPACE 2                                                                
LBILC    DS    0H                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,CAMTS                                                         
         LA    R4,OAMTS            ROLL TO OFFICE TOTS                          
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+8                                                              
         LA    R4,RQAMTS           ELSE TO REQUEST TOTS                         
         BAS   RE,PBROLL                                                        
*                                                                               
         MVC   CLTINVS,INVPARS+8   INVOICE COUNT                                
         L     R0,OFFINVS                                                       
         LA    RE,OFFINVS                                                       
         CLI   QCLT,C'$'           IF IN OFFICE LIST MODE                       
         BE    *+12                                                             
         L     R0,REQINVS                                                       
         LA    RE,REQINVS                                                       
         A     R0,CLTINVS                                                       
         ST    R0,0(RE)                                                         
*                                                                               
         MVI   ALLOWLIN,2                                                       
         MVC   P+10(19),=C'** CLIENT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  CLTINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,CAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    CLTINVS,CLTINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST  FOR OFFICE                                                       
         SPACE 2                                                                
LBILO    DS    0H                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,OAMTS                                                         
         LA    R4,RQAMTS                                                        
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,REQINVS                                                       
         A     R0,OFFINVS                                                       
         ST    R0,REQINVS                                                       
*                                                                               
         BRAS  RE,PRNT                                                          
         MVC   P+7(2),SAVCOFF                                                   
         MVC   P+10(19),=C'** OFFICE TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  OFFINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,OAMTS                                                         
         BAS   RE,TOTPRNT                                                       
         BAS   RE,CLRTOTS                                                       
         XC    OFFINVS,OFFINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST FOR REQ                                                           
REQL     DS    0H                                                               
         L     RF,ADBILL           CLEAR BILL RECORD                            
         XC    0(L'BILLREC,RF),0(RF)                                            
         CLC   QAGY,=C'JW'         FOR JWT                                      
         BNE   *+8                                                              
         BRAS  RE,JWPROC           MUST FINISH UP SPECIALS                      
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                                                             
         LA    R3,RQAMTS                                                        
         LA    R4,RAMTS                                                         
         BAS   RE,PBROLL                                                        
*                                                                               
         L     R0,RUNINVS                                                       
         A     R0,REQINVS                                                       
         ST    R0,RUNINVS                                                       
*                                                                               
         MVC   P+10(20),=C'** REQUEST TOTALS **'                                
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  REQINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RQAMTS                                                        
         BAS   RE,TOTPRNT                                                       
         XC    REQINVS,REQINVS                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*        LAST FOR RUN                                                           
         SPACE 2                                                                
                                                                                
         EJECT                                                                  
*        PROCESS BILL  **NOTE- BILL RECORD NOT READ YET**                       
         SPACE 2                                                                
PRBL     DS    0H                                                               
*                                                                               
         CLC   KEY+BKEYINV-BKEY(2),LOWINV                                       
         BL    EXIT                                                             
         CLC   KEY+BKEYINV-BKEY(2),HIINV                                        
         BH    EXIT                                                             
*                                                                               
         MVI   SKIPBILL,0                                                       
         CLI   KEY+BKEYEST-BKEY,0  SKIP EST 0 BILLS (BILLING BUG)               
         BE    EXIT                                                             
         CLC   KEY(10),LSTBLKY     IF FIRST FOR EST/MOS                         
         BE    PRB1                                                             
         MVI   REVSW,C' '          SET NOT A REVISION                           
*                                                                               
         ZAP   EATOTGRS,=P'0'                                                   
         ZAP   EATOTAC,=P'0'                                                    
         ZAP   EATOTACT,=P'0'                                                   
         ZAP   EATOTNET,=P'0'                                                   
         B     PRB1D                                                            
*                                                                               
PRB1     DS    0H                                                               
         MVI   REVSW,C'R'          SET IS A REVISION                            
*                                                                               
PRB1D    DS    0H                                                               
         CLI   WIAPLSW,C'N'        FOR WESTERN/APL EITHER MODE                  
         BE    PRB1F                                                            
*                                                                               
         BAS   RE,WIAPLCMT         CHECK CLIENT/MONTH                           
         BNE   EXIT                SKIP THIS BILL                               
         B     PRB1H                                                            
*                                                                               
PRB1F    DS    0H                  IF NOT WESTERN/APL                           
         CLC   AGY,=C'PU'          AND AGY IS PU                                
         BNE   PRB1H                                                            
*                                                                               
         BAS   RE,WIAPLCMT         CHECK CLIENT/MONTH                           
         BE    EXIT                SKIP THIS BILL                               
*                                                                               
PRB1H    DS    0H                                                               
         MVC   LSTBLKY,KEY                                                      
         TM    CONTROL1,X'80'      TEST NEED 'ESTIMATE' AMOUNTS                 
         BNZ   PRB3                YES- MUST READ ALL BILLS                     
*                                                                               
         CLC   KEY+BKEYYSRV-BKEY(2),STARTMOS  MONTH-OF-SERVICE FILTERS          
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),ENDMOS                                      
         BH    EXIT                                                             
*                                                                               
         CLI   QOPT4,C'M'          TEST MOS FILTERING                           
         BNE   PRB2D                                                            
         CLC   KEY+BKEYYSRV-BKEY(2),BQSTART                                     
         BL    EXIT                                                             
         CLC   KEY+BKEYYSRV-BKEY(2),BQEND                                       
         BH    EXIT                                                             
         B     PRB3                                                             
*                                                                               
PRB2D    DS    0H                  RUN DATE FILTERING                           
         ZIC   R3,KEY+BKEYMBIL-BKEY                                             
         SRL   R3,4                YEAR DIGIT OF BILL                           
         ZIC   RE,DECADE                                                        
         CLM   R3,1,YEARDIG        COMPARE TO YEAR OF TODAY                     
         BNH   *+8                 IF NOT HIGH, OK                              
         SH    RE,=H'10'           ELSE BACK UP TO PREV DECADE                  
         AR    RE,R3                                                            
         STC   RE,FULL             CALCULATED YEAR OF BILL                      
*                                                                               
         MVC   FULL+1(1),KEY+BKEYMBIL-BKEY                                      
         NI    FULL+1,X'FF'-X'F0'  ISOLATE MONTH                                
*                                                                               
         CLC   FULL(2),BQSTART                                                  
         BL    EXIT                                                             
         CLC   FULL(2),BQEND                                                    
         BH    EXIT                                                             
*                                                                               
PRB3     DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R2,ADBILL                                                        
         USING BILLREC,R2                                                       
*                                                                               
         CLI   QOPT5+1,C'*'        TEST NETPAK SUB MED FILT                     
         BE    PRB4                NO                                           
         CLI   QOPT5+1,C' '                                                     
         BNH   PRB4                NO                                           
         MVC   BYTE,BLMED                                                       
         CLI   BYTE,C' '           IF NO SUB MEDIA                              
         BH    *+8                                                              
         MVI   BYTE,C'N'           DEFAULT TO N                                 
         CLC   BYTE,QOPT5+1        TEST RIGHT SUB-MED                           
         BNE   EXIT                                                             
*                                                                               
PRB4     DS    0H                                                               
         TM    BILSTAT,BSTCMONQ    TEST COMMISSION ONLY BILL                    
         BO    *+16                YES                                          
         CLI   QOPT5,C'C'          NO, TEST TO SKIP OTHERS                      
         BE    EXIT                                                             
         B     *+12                                                             
         CLI   QOPT5,C'N'          EXCLUDE COMMISSION-ONLY BILLS?               
         BE    EXIT                YES                                          
*                                                                               
         CLI   QOPT5,C'A'          AOR BILLS ONLY?                              
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'B'          AOR AND AOR/CLIENT BILLS                     
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ+BSTCAORQ                                        
         BZ    EXIT                                                             
*                                                                               
         CLI   QOPT5,C'X'          NON-AOR BILLS ONLY?                          
         BNE   *+12                                                             
         TM    BILSTAT,BSTTAORQ                                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   QOPT5,C'S'          SOON BILLS ONLY?                             
         BNE   *+12                                                             
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BNO   EXIT                                                             
*                                                                               
         CLC   =C'*SOON',QUESTOR   IF WAS AUTOREQUESTED BY SOON                 
         BNE   PB6                                                              
         TM    BILSTAT3,BSTSOONQ   WAS REC GENERATED BY SOON ?                  
         BZ    EXIT                                                             
         CLC   BILLUID,RCORIGID    PROCESS ONLY FOR REQUESTING USER ID          
         BNE   EXIT                                                             
*                                  NB- POST TO 'ESTIMATE' AMTS                  
*                                  WHETHER PASSES DATE FILTERS OR NOT           
PB6      DS    0H                  SET BILL AMOUNTS                             
*                                                                               
*        SPECIAL HANDLING OF GSTX (G7) BILLS FOR CLIENTS LLB AND MCI            
*        FOR THESE COST 2 CLIENTS SET BNETP TO BNET2P (COST2 NET)               
*        SO THAT NO COMMISSION WILL BE REPORTED                                 
*                                                                               
         CLC   AGY,=C'G7'    GSTX                                               
         BNE   PB6AX                                                            
         CLC   CLT,=C'LLB'          MUST BE EITHER CLIENT LLB OR MCI            
         BE    PB6A                                                             
         CLC   CLT,=C'MCI'                                                      
         BNE   PB6AX                                                            
*                                                                               
PB6A     TM    BILSTAT2,BSTC2Q     MUST BE COST 2 BILL                          
         BZ    PB6AX                                                            
         MVC   BNETP,BNET2P        SET BNET TO COST 2 VALUE                     
*                                                                               
PB6AX    GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
*                                                                               
         LA    R3,BAMTS            CLEAR BILL AMOUNTS                           
         BRAS  RE,CLRTOTS                                                       
*                                                                               
         USING AMOUNTSD,R3                                                      
         ZAP   AMTGRS,SPBVGRSP     EFFECTIVE GROSS                              
         ZAP   AMTACT,SPBVACTP     ACTUAL                                       
*                                                                               
         TM    BILSTAT3,X'02'      SEE IF EXCHANGE BILL                         
         BNO   PB6AX4              EQU WILL BE BSTTRCNQ                         
         L     R0,BINVSEQ+1        WILL BE CALLED BCLDNET                       
         CVD   R0,DUB                                                           
         ZAP   AMTNET,DUB                                                       
         B     PB6AX5                                                           
*                                                                               
PB6AX4   ZAP   AMTNET,SPBVNETP     EFFECTIVE NET                                
*                                                                               
PB6AX5   L     R0,SPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTGST,DUB          GST                                          
         ZAP   MYGST,AMTGST        SAVE TO USE IN SPEC                          
         L     R0,SPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTPST,DUB          PST'S (INCL HSTS)                            
         ZAP   MYPST,AMTPST                                                     
         L     R0,SPBVHST                                                       
         CVD   R0,DUB                                                           
         ZAP   AMTHST,DUB          HST'S ALONE                                  
         ZAP   MYHST,AMTHST                                                     
*                                                                               
         CLI   QOPT5,C'2'          IF COS2 REQUEST                              
         BE    *+12                                                             
         CLI   QOPT5,C'P'          (PW = COS2)                                  
         BNE   PB6C4                                                            
*                                                                               
         TM    BILSTAT2,BSTC2Q     AND COS2 BILL, OK                            
         BZ    EXIT                ELSE, SKIP                                   
*                                                                               
         ZAP   AMTGRS,BGRS2P       USE COS2 GROSS                               
         ZAP   AMTNET,BNET2P       NET                                          
         ZAP   AMTACT,BACT2P       AND ACTUAL                                   
*                                                                               
PB6C4    DS    0H                                                               
         ZAP   DUB,AMTACT                                                       
         TM    BILSTAT,BSTCMONQ    IF COMM ONLY BILL                            
         BZ    PB7                                                              
         CLI   QOPT5,C'C'          TEST LEAVE NET                               
         BE    PB7                 YES                                          
         CLC   =C'WI',QAGY         ALSO LEAVE NET FOR WI, TAPE CODE "A"         
         BNE   *+12                                                             
         CLI   QOPT2,C'A'                                                       
         BE    PB7                                                              
         ZAP   AMTNET,=P'0'        ELSE CLEAR NET (AC = RCVBL)                  
*                                                                               
PB7      DS    0H                                                               
         TM    BILSTAT,BSTTAORQ    FOR AOR BILLS                                
         BZ    *+10                'NET' IS ORIGINAL BILLS COMMISION            
         ZAP   AMTNET,=P'0'        SO SIT MUST BE CLEARED HERE                  
*                                                                               
         SP    DUB,AMTNET                                                       
         ZAP   AMTAC,DUB           AC                                           
*                                                                               
         AP    EATOTGRS,AMTGRS     ADD TO EATOTS                                
         AP    EATOTAC,AMTAC                                                    
         AP    EATOTACT,AMTACT                                                  
         AP    EATOTNET,AMTNET                                                  
*                                                                               
         CLI   QOPT4,C'M'          UNLESS FILTERING ON MOS                      
         BE    PB8                                                              
         CLC   BDATE,SVQST         TEST BILL WITHIN REQ PERIOD                  
         BL    EXIT                                                             
         CLC   BDATE,SVQEND                                                     
         BH    EXIT                                                             
*                                                                               
PB8      DS    0H                                                               
         MVI   RETAIL,C'N'                                                      
         CLI   BRETAIL,0                                                        
         BE    PB30                                                             
*                                  RETAIL BILLS                                 
         CLI   BRETAIL,X'81'       SKIP ALL CORP CONTROL                        
         BE    EXIT                                                             
*                                                                               
         BAS   RE,CHKTOTS                                                       
         BE    EXIT                SKIP ZERO BILLS                              
*                                                                               
         MVI   RETAIL,C'Y'                                                      
*                                  BUILD PRINT LINE                             
PB30     DS    0H                                                               
*        FINALLY - PROCESS THIS BILL                                            
*                                  USE BINSRCH TO ADD TO INVTAB                 
         MVC   WORK(1),KEY+BKEYMBIL-BKEY     BILL MONTH                         
         MVC   WORK+1(2),KEY+BKEYINV-BKEY    AND INVOICE NUMBER                 
         GOTO1 BINSRCH,INVPARS,(1,WORK)                                         
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
         CLI   NEWCLT,C'Y'         IF NEW CLIENT                                
         BNE   PB30B                                                            
         MVC   P,SVMID             PRINT CLIENT DATA                            
         MVI   ALLOWLIN,7                                                       
         MVI   NEWCLT,C'N'                                                      
*                                  MVI   SPACING,2 REMOVED                      
         BRAS  RE,PRNT                                                          
*                                                                               
PB30B    DS    0H                                                               
         LHI   RF,1                SET BILL COUNT                               
         ST    RF,AMTCNT                                                        
         LA    R7,P                                                             
         USING BLINED,R7                                                        
*                                                                               
         CLI   QOPT5+1,C' '        TEST TO SHOW NETPAK SUB-MED                  
         BNH   PB31                                                             
         CLI   BLMED,C' '          TEST ANY                                     
         BNH   PB31                                                             
         MVI   P,C'*'                                                           
         MVC   P+1(1),BLMED                                                     
*                                                                               
PB31     DS    0H                                                               
         MVC   BLPRD,BKEYPRD       PRD                                          
*                                                                               
         L     RF,ADPRD            PRD NUMBER                                   
         LA    RF,PACCT-PRDHDR(RF)                                              
         MVC   BLPNUM+1(4),0(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *+20                                                             
         ZAP   DUB,1(3,RF)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  BLPNUM,DUB                                                       
*                                                                               
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BLEST,DUB                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(6,BLPER)                               
         CLI   BKEYYSRV+1,12       SPECIAL PERIOD                               
         BNH   PB33                                                             
         ZIC   R1,BKEYYSRV+1                                                    
         EDIT  (R1),(2,BLPER+1)                                                 
         MVI   BLPER,C' '                                                       
*                                                                               
PB33     DS    0H                                                               
         GOTO1 DATCON,DMCB,BDATE,(5,BLRUND)                                     
         GOTO1 DATCON,DMCB,BQDATE,(5,BLINVD)                                    
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(5,BLDUED)                              
*                                                                               
         MVC   BLTYPE(2),BTYPE     BILLING TYPE                                 
         MVI   BLTYPE+2,C' '                                                    
*                                                                               
         TM    BILSTAT3,X'02'      SEE IF EXCHANGE BILL                         
         BNO   *+8                 EQU WILL BE BSTTRCNQ                         
         MVI   BLTYPE+2,C'X'                                                    
*                                                                               
         TM    BILSTAT3,X'01'      SEE IF MIDAS BARTER                          
         BNO   *+8                 EQU WILL BE BSTMBARQ                         
         MVI   BLTYPE+2,C'T'                                                    
*                                                                               
         TM    BILSTAT,BSTSCOMQ    IF UPFRONT COMM                              
         BZ    *+8                                                              
         MVI   BLTYPE,C'U'         U4-U7 = UPFRONT                              
         TM    BILSTAT,BSTSNETQ    IF NET BILL (AFTER UPFRONT)                  
         BZ    *+8                                                              
         MVI   BLTYPE,C'N'         N4-N7 = UPFRONT                              
         TM    BILSTAT,BSTMANQ     MANUAL?                                      
         BZ    *+10                                                             
         MVC   BLTYPE,=C'MAN'                                                   
         TM    BILSTAT,BSTTAORQ    AOR?                                         
         BZ    *+10                                                             
         MVC   BLTYPE,=C'AOR'                                                   
*                                                                               
         CLC   QAGY,=C'OM'         SPECIAL FOR OM                               
         BNE   PB33D                                                            
         TM    BILSTAT,BSTTAORQ    AOR BILLS                                    
         BZ    PB33D                                                            
         MVC   BINVNO,SVBILL+BINVNO-BILLREC  USE CLT INV BILL NUMBER            
         CLC   BILLREC(11),SVBILL  SHOULD ALWAYS BE RIGHT BILL                  
         BE    *+10                                                             
         MVC   BINVNO,=6C'000000'  IF NOT CLEAR BILL NUMBER                     
*                                                                               
PB33D    DS    0H                                                               
         MVC   DINVNO,SPACES                                                    
         LA    R5,BILLREC                                                       
         GOTO1 VSPFMTIN,DMCB,(C'B',BDATE),(6,BINVNO),(QMED,B1PROF),B1XPX        
               ROF,ADBILL                                                       
**OLD**  GOTO1 VSPFMTIN,DMCB,BDATE,(6,BINVNO),(QMED,B1PROF),B1XPROF             
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL FORMAT INVOICE NUMBER                   
*                                                                               
         L     RF,DMCB+12                                                       
         MVC   DINVMED,0(RF)       MEDIA PART OF INVOICE                        
*                                                                               
         L     RF,DMCB+4           FORMAT MN-NNNN (SHORT INVOICE NO.)           
         LA    RE,DINVNO                                                        
         LHI   R0,7                                                             
*                                                                               
PB34     DS    0H                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,PB34                                                          
*                                                                               
         MVC   BLINVNO(2),DINVNO                                                
         MVI   BLINVNO+2,C'-'                                                   
         MVC   BLINVNO+3(4),DINVNO+2                                            
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   PB34B                                                            
*                                                                               
         ZAP   DOUBLE,AMTACT       ACTUAL                                       
         AP    DOUBLE,AMTGST       PLUS GST                                     
         AP    DOUBLE,AMTPST       PLUS PST (INCL HST)                          
         EDIT  (P8,DOUBLE),BLACTUAL,2,COMMAS=YES,MINUS=YES                      
*                                                                               
         EDIT  AMTGRS,BLGROSS,2,COMMAS=YES,MINUS=YES                            
         EDIT  AMTNET,BLNET,2,COMMAS=YES,MINUS=YES                              
         EDIT  AMTAC,BLAC,2,COMMAS=YES,MINUS=YES                                
         EDIT  AMTGST,BLGST,2,COMMAS=YES,MINUS=YES                              
*                                                                               
         CP    AMTPST,=P'0'        PST ((INCL HST)                              
         BE    PB34D                                                            
         EDIT  AMTPST,BLPST,2,COMMAS=YES,MINUS=YES                              
         B     PB34D                                                            
*                                                                               
PB34B    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
*                                                                               
PB34D    DS    0H                                                               
         BRAS  RE,PRNT                                                          
         LA    R4,PAMTS            ROLL TO PRODUCT TOTALS                       
         BAS   RE,PBROLL                                                        
         DROP  R3                                                               
*                                                                               
*                                  CLEAR TAPE RECORD                            
         LA    RE,TPREC                                                         
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         CLI   QOPT1,C'N'                                                       
         BE    PSX                 SKIP TAPE                                    
*                                                                               
         ICM   R2,15,ASPECS                                                     
         BZ    PS40                NO SPECS                                     
*                                                                               
         USING DSELEM,R2                                                        
         MVI   ELCODE,X'05'                                                     
         CLC   ELCODE,0(R2)                                                     
         BE    PS4B                                                             
*                                                                               
PS4      DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   PS40                                                             
*                                                                               
PS4B     DS    0H                                                               
         CLI   DSEMEDF,C' '        IF MEDIA FILTER PRESENT                      
         BNH   *+14                                                             
         CLC   DSEMEDF,QMED        TEST IF OK                                   
         BNE   PS4                                                              
*                                                                               
         CLI   CLFILT,C' '         TEST CLIENT FILTER ACTIVE                    
         BNH   *+14                                                             
         CLC   CLT,CLFILT          YES DO ONLY FOR THIS CLIENT                  
         BNE   PS4                                                              
*                                                                               
         CLI   MEFILT,C' '         TEST MEDIA FILTER ACTIVE                     
         BNH   *+14                                                             
         CLC   MED,MEFILT          YES-DO ONLY FOR THIS MEDIA                   
         BNE   PS4                                                              
*                                                                               
         ZIC   RE,DSECODE                                                       
         L     RF,=A(PSTRT)                                                     
         AR    RF,RE                                                            
         ZIC   R3,0(RF)                                                         
         MHI   R3,4                                                             
         B     PSBRTAB(R3)                                                      
PSRETURN DS    0H                  *** RETURN ADDRESS ***                       
         B     PS4                                                              
*                                                                               
PS40     DS    0H                                                               
         CLC   QAGY,=C'MC'         MCCANN AND TYPE=C                            
         BNE   PS40G                                                            
         CLI   QOPT2,C'C'          CHRYLSER                                     
         BNE   PS40G                                                            
         BRAS  RE,MCHPROC                                                       
         B     PS900                                                            
*                                                                               
PS40G    CLC   QAGY,=C'G7'         GSTX SPECIAL ROUTINE                         
         BNE   *+12                                                             
         BRAS  RE,G7PROC           COLLAPSE RECORDS                             
         B     PS900                                                            
*                                                                               
         CLC   QAGY,=C'M$'         MULLEN - LIKE GSTX SPECIAL                   
         BNE   *+12                                                             
         BRAS  RE,G7PROC           COLLAPSE RECORDS                             
         B     PS900                                                            
*                                                                               
         CLC   QAGY,=C'JW'         JWT SPECIAL                                  
         BNE   *+12                                                             
         BRAS  RE,JWPROC           MUST FINISH UP SPECIALS                      
         B     PS900                                                            
*                                                                               
         CLC   QAGY,=C'BN'         PHDNY?                                       
         BNE   PS40O                                                            
         CLI   QOPT2,C'S'          SC JOHNSON FILE?                             
         BNE   PS45                                                             
         BRAS  RE,SCPROC           YES: DO SPECIAL                              
         B     PS900                                                            
*                                                                               
PS40O    CLC   QAGY,=C'OO'         OPHDNA                                       
         BNE   PS40X                                                            
         CLI   QOPT2,C'S'          SC JOHNSON FILE?                             
         BNE   PS45                                                             
         BRAS  RE,SCPROC           YES: DO SPECIAL                              
         B     PS900                                                            
*                                                                               
PS40X    CLC   QAGY,=C'H7'         STARCOM?                                     
         BNE   PS41                                                             
         CLI   QOPT2,C'S'          SC JOHNSON FILE?                             
         BNE   PS40S                                                            
         BRAS  RE,SCPROC           YES: DO SPECIAL                              
         B     PS900                                                            
*                                                                               
PS40S    CLI   QOPT2,C'M'          MAZDA FILE?                                  
         BNE   PS45                                                             
         MVI   MZTYPE,C'1'                                                      
         MVI   TPREC,C'1'          OVERRIDES 'L'                                
         XC    MZMOS,MZMOS         CLEAR MOS                                    
         BRAS  RE,MZPROC           YES: DO SPECIAL                              
         MVI   MZTYPE,C'2'                                                      
         MVI   TPREC,C'2'          OVERRIDES 'L'                                
         BRAS  RE,MZPROC           YES: DO SPECIAL                              
         B     PS900                                                            
*                                                                               
PS41     CLC   QAGY,=C'JS'         JACTAK - ALSO MAY BE MAZDA                   
         BNE   PS45                                                             
         B     PS40S               CODE SAME AS FOR H7                          
*                                                                               
PS45     CLC   QAGY,=C'H9'         STARCOM?                                     
         BNE   PS50                                                             
         CLI   QOPT2,C'P'          PHILIP MORRIS TAPE?                          
         BNE   PS50                                                             
         BRAS  RE,PMPROC           YES: DO SPECIAL                              
         B     PS900                                                            
PS50     DS    0H                                                               
         CLC   QAGY,=C'UB'                                                      
         BNE   PS50D                                                            
         CLI   QOPT2,C'S'         SMUCKERS FORMAT?                              
         BE    PS900              YES - RECORDS ALREADY PUT TO TAPE             
         CLI   QOPT2,C'D'         DIAGEO FORMAT?                                
         BNE   PS50D                                                            
*                                                                               
         MVI   DGTYPE,C'0'        FILE HEADER                                   
         BRAS  RE,DGPROC                                                        
         MVI   DGTYPE,C'1'        INVOICE HEADER                                
         BRAS  RE,DGPROC                                                        
         MVI   DGTYPE,C'2'        INVOICE DETAIL                                
         BRAS  RE,DGPROC                                                        
*                              3  FILE TOTAL RECORD A RUNLAST                   
         B     PS900                                                            
*                                                                               
PS50D    CLC   QAGY,=C'FM'        HAVAS MEDIA?                                  
         BNE   PS51               NO                                            
         BRAS  RE,PHPROC          PHILIPS                                       
         B     PS900                                                            
*                                                                               
PS51     TM    SKIPBILL,X'80'      TEST THIS BILL TO TAPE                       
         BNZ   PSX                                                              
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   PS052                                                            
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PS052    DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         CLC   QAGY,=C'U#'         IF M2TO MIGHT NEED TO ADD                    
         BE    PS053               ANOTHER RECORD W/ HST                        
         CLC   QAGY,=C'WT'         IF WITO MIGHT NEED TO ADD                    
         BNE   PS054               ANOTHER RECORD W/ HST                        
PS053    CLC   TPREC(32),SVTPREC   COMPARE UNTIL AMOUNT FIELDS                  
         BNE   PS054                                                            
         L     R1,=A(SBITAPE)      ADD RECORD                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
         B     PS900                                                            
*                                                                               
PS054    DS    0H                                                               
         CLC   QAGY,=C'OO'   MORE RECORDS MAY BE NEEDED  OMDSEC                 
         BE    PS054O                                                           
         CLC   QAGY,=C'OU'   MORE RECORDS MAY BE NEEDED  OMDTO                  
         BNE   PS060                                                            
*                                                                               
PS054O   DS    0H                                                               
         CLI   QOPT2,C'B'          STARBUCK'S FORMAT?                           
         BE    PS071                                                            
         CLI   QOPT2,C'E'          EDWARDS JONES FORMAT?                        
         BNE   PS900                                                            
*                                  MUST READ ESTIMATE                           
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    OOP5                                                             
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
OOP5     DS    0H                                                               
*                                  CLEAR TAPE RECORD                            
         LA    RE,TPREC                                                         
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*        DETAIL RECORD                                                          
*                                                                               
         MVC   TPREC(2),=C'01'               SPECIAL IDENTIFER                  
         MVC   TPREC+2(3),=C'USA'            COUNTRY                            
         CLC   QAGY,=C'OO'                                                      
         BE    *+10                                                             
         MVC   TPREC+2(3),=C'CAN'            OU IS CANADIAN                     
*                                                                               
         L     RF,ADPRD                                                         
         LA    RF,PUSER1-PRDHDR(RF)  USE PUSER1 AS CLIENT CODE                  
         CLI   0(RF),C' '            IF PRESENT                                 
         BNH   OOP8                                                             
         MVC   TPREC+10(5),0(RF)                                                
         OC    TPREC+10(5),SPACES   UPPERCASE                                   
*                                                                               
OOP8     DS    0H                                                               
         MVI   TPREC+16,C'H'                 HEADQUARTER/ IR IDENTIFER          
*                                            H=HEADQUATERS,I= IR                
         LA    R3,BAMTS                                                         
         USING AMOUNTSD,R3                                                      
         ZAP   MYDUB,AMTNET                  NET-CD                             
         EDIT  MYDUB,(15,TPREC+62),2,FILL=0,ZERO=NOBLANK                        
         CP    MYDUB,=P'0'                                                      
         BNL   *+8                                                              
         MVI   TPREC+62,C'-'                                                    
*                                                                               
         DROP  R3                                                               
*                                                                               
OOP10    DS    0H                                                               
*                                                                               
         CLC   QAGY,=C'OU'     CANADIAN TAXES NEEDED?                           
         BNE   OOP10K                                                           
         ZAP   MYDUB,MYGST                                                      
         EDIT  MYDUB,(15,TPREC+77),2,FILL=0,ZERO=NOBLANK                        
         CP    MYDUB,=P'0'                                                      
         BNL   *+8                                                              
         MVI   TPREC+77,C'-'                                                    
*                                                                               
OOP10B   DS    0H                                                               
*                                                                               
         ZAP   MYDUB,MYHST                                                      
         EDIT  MYDUB,(15,TPREC+92),2,FILL=0,ZERO=NOBLANK                        
         CP    MYDUB,=P'0'                                                      
         BNL   *+8                                                              
         MVI   TPREC+92,C'-'                                                    
*                                                                               
OOP10D   DS    0H                                                               
*                                                                               
         ZAP   MYDUB,MYPST                                                      
         SP    MYDUB,MYHST      REMOVE HST                                      
         EDIT  MYDUB,(15,TPREC+107),2,FILL=0,ZERO=NOBLANK                       
         CP    MYDUB,=P'0'                                                      
         BNL   *+8                                                              
         MVI   TPREC+107,C'-'                                                   
*                                                                               
OOP10F   DS    0H                                                               
         B     OOP20                                                            
*                                                                               
*        FOR US AGENCY JUST DO TAX INSTEAD                                      
*                                                                               
OOP10K   DS    0H                                                               
         L     R7,ADBILL                                                        
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,MYDUB                                                         
         EDIT  MYDUB,(15,TPREC+77),2,FILL=0,ZERO=NOBLANK                        
         CP    MYDUB,=P'0'                                                      
         BNL   *+8                                                              
         MVI   TPREC+77,C'-'                                                    
*                                                                               
         MVC   TPREC+92(15),=C'000000000000000' FREIGHT AMOUNT                  
         MVI   TPREC+104,C'.'          DECIMAL                                  
         MVC   TPREC+107(15),=C'000000000000000' MISCELLANEOUS AMOUNT           
         MVI   TPREC+119,C'.'          DECIMAL                                  
*                                                                               
OOP20    DS    0H                                                               
         L     RF,ADEST                                                         
         MVC   TPREC+122(15),EUSER2-ESTHDR(RF)    PROJECT CODE                  
         OC    TPREC+122(15),SPACES UPPERCASE                                   
                                                                                
         MVC   TPREC+315(10),EUSER1-ESTHDR(RF)    ITEM NUMBER                   
         OC    TPREC+315(15),SPACES UPPERCASE                                   
         MVC   TPREC+250(7),=C'GENERAL'           ACTIVITY ID#                  
         MVC   TPREC+265(3),=C'ACT'               ANALYSIS TYPE                 
         MVC   TPREC+268(5),=C'PRGRM'             RESOURCE TYPE                 
         MVC   TPREC+273(5),=C'ADVRT'             RESOURCE CATEGORY             
*                                                                               
         MVC   TPREC+137(6),DINVNO          SHORT INVOICE #                     
*                                                                               
         MVC   TPREC+192(15),=C'000000000000000'   ITEM COUNT                   
*                                                                               
         MVI   TPREC+231,C'A'      INVOICE/ALLOCATE INDICATOR                   
*                                                                               
         MVC   TPREC+291(10),=C'0000000000'   PO LINE NUMBER                    
*                                                                               
         MVI   TPREC+349,C'0'            SO RECORD WON'T BE COMPRESSED          
*                                        WHEN TRANSMITTED                       
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   OOP30                                                            
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
OOP30    L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
*        NOW DO TRAILER RECORD                                                  
*                                                                               
         MVC   TPREC(2),=C'99'                                                  
         MVC   TPREC+2(60),SPACES                                               
*                                                                               
*        NET AND TAX AMOUNTS - SAME AS DETAIL RECORD                            
*                                                                               
*                              CLEAR REST OF RECORD                             
         LA    RE,TPREC+122                                                     
         LHI   R1,227                                                           
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   TPREC+192(15),=C'000000000000000'   ITEM COUNT                   
*                                                                               
         MVI   TPREC+349,C'0'            SO RECORD WON'T BE COMPRESSED          
*                                        WHEN TRANSMITTED                       
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   OOP40                                                            
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
OOP40    L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
         B     PS900                                                            
*                                                                               
PS060    CLC   QAGY,=C'H7'         MINDSHARE?                                   
         BNE   PS070                                                            
         CLI   QOPT2,C'B'          BURGER KING FORMAT?                          
         BNE   PS070                                                            
*                                                                               
*                                  MUST NOW CREATE DETAIL RECORD                
*                                                                               
         DROP  R7                                                               
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLRECD,R7                                                      
*                                                                               
         MVC   SVTPREC(150),TPREC    SAVE HEAD DATA                             
*                                                                               
         LA    RE,TPREC              CLEAR TPREC                                
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   TPREC(2),=C'D1'                                                  
         MVC   TPREC+2(L'DINVFULL),DINVFULL                                     
*                                                                               
*        CODE BELOW WILL PUT CAMPAIGN (PUSER1) INSTEAD OF INV. NO.              
*        IF NOT NETPAK                                                          
*                                                                               
****     CLI   NETPAKSW,C'Y'          USE INVOICE # FOR NETPAK                  
****     BE    PS062                                                            
****     MVC   TPREC+2(L'DINVFULL),SPACES                                       
****     L     RF,ADPRD                                                         
****     LA    RF,PUSER1-PRDHDR(RF)  PRODUCT USER1                              
****     MVC   TPREC+2(5),0(RF)      SHOULD BE CAMPAIGN NUMBER                  
****     OC    TPREC+2(5),SPACES      JUST IN CASE                              
*                                                                               
PS062    MVC   TPREC+18(10),BKACCT    G/L ACCOUNT                               
         MVC   TPREC+28(10),BKCOSTC   COST CENTER                               
*                                                                               
PSBILL   USING AMOUNTSD,BAMTS                                                   
*                                                                               
         ZAP   BKCOM,PSBILL.AMTACT    COMMISSION = ACTUAL MINUS NET             
         SP    BKCOM,PSBILL.AMTNET                                              
*                                                                               
*        IF TRADE BILL CREATE A NET OF 85% OF GROSS                             
*        FORMULA SHOULD BE N + 85% OF GROSS?                                    
*                                                                               
         TM    BILSTAT3,X'02'      SEE IF EXCHANGE BILL                         
         BO    PSBILL5             EQU WILL BE BSTTRCNQ                         
*                                  DON'T CALULATE NET HERE                      
*                                                                               
         TM    BILSTAT3,BSTTRDQ      SEE IF TRADE BILL                          
         BNO   PSBILL5                                                          
*   NET DOWN VALUE IN AMTGRS (GROSS) AND SAVE RESULT IN BKCOM                   
*                                                                               
         ZAP   PL16,PSBILL.AMTGRS                                               
         MP    PL16,=P'8500'                                                    
         DP    PL16,=P'10000'                                                   
         CP    PL16+13(3),=P'5000' REMAINDER                                    
         BL    PSBILL2                                                          
         AP    PL16(13),=P'1'      ROUND UP                                     
         B     PSBILL3                                                          
*                                                                               
PSBILL2  CP    PL16+13(3),=P'-5000' REMAINDER                                   
         BH    PSBILL3                                                          
         SP    PL16(13),=P'1'      ROUND DOWN                                   
*                                                                               
PSBILL3  DS    0H                                                               
         ZAP   BKCOM,PSBILL.AMTACT    COMMISSION = ACTUAL MINUS NET             
         SP    BKCOM,PL16+5(8)        (CALCULATED NET)                          
*                                                                               
PSBILL5  ZAP   MYDUB,PSBILL.AMTACT                                              
         SP    MYDUB,BKCOM     FIRST DO ACTUAL MINUS COMMISSION                 
         DROP  PSBILL                                                           
*                                                                               
         EDIT  MYDUB,(11,TPREC+38),2,FILL=0,ZERO=NOBLANK                        
*                                                                               
         MVI   TPREC+38,C'-'                                                    
         CP    MYDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   TPREC+38,C'+'                                                    
*                                                                               
         OC    BTAXAMT,BTAXAMT                                                  
         BZ    *+10                                                             
         MVC   TPREC+49(2),=C'P1'                                               
*                                                                               
         MVC   TPREC+101(23),BKMAR#   MAR NUMBER                                
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   PS060C                                                           
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PS060C   L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         CP    BKCOM,=P'0'       ANY COMMISSION?                                
         BE    PS900             NO - THEN ONLY ONE DETAIL RECORD               
*                                                                               
         EDIT  BKCOM,(11,TPREC+38),2,FILL=0,ZERO=NOBLANK                        
*                                                                               
         MVI   TPREC+38,C'-'                                                    
         CP    BKCOM,=P'0'                                                      
         BL    *+8                                                              
         MVI   TPREC+38,C'+'                                                    
         MVC   TPREC+18(10),BKCOMACC  G/L ACCOUNT FOR COMMISSION                
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   PS065C                                                           
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PS065C   L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         B     PS900                                                            
*                                                                               
*                                                                               
PS070    CLC   QAGY,=C'BN'         PHDNY?                                       
         BNE   PS900                                                            
         CLI   QOPT2,C'B'          STARBUCK FORMAT?                             
         BNE   PS900                                                            
*                                  MUST NOW CREATE DETAIL RECORD                
         DROP  R7                                                               
*                                                                               
PS071    L     R7,ADBILL                                                        
         USING BILLRECD,R7                                                      
*                                                                               
         MVC   SVTPREC(250),TPREC    SAVE HEAD DATA                             
*                                                                               
         LA    RE,TPREC              CLEAR TPREC                                
         LHI   R1,130                100 BYTES                                  
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   TPREC(2),SVTPREC                                                 
         MVI   TPREC,C'D'                                                       
         MVC   TPREC+2(6),=C'127326'   SUPPLIER (NO PIPE)                       
         MVI   TPREC+8,CARET       DELIMITER                                    
         MVC   TPREC+9(29),SVTPREC+13    NEWYORK01 + DELIMITER                  
         LA    RE,TPREC+38                                                      
PS070A   CLI   0(RE),CARET         WAS IT THE DELIMITER                         
         BE    PS070B                                                           
         BCT   RE,PS070A           KEEP LOOKING                                 
*                                                                               
PS070B   LA    RE,1(RE)            PAST DELIMITER                               
*                                                                               
*  NOTE: ASSUMES PUSER1 AND EUSER1 ARE STILL THE SAME LENGTH - 32               
*                                                                               
         MVC   0(L'PUSER1,RE),SVTPREC+200   EST USER1 DATA (SBSPECS)            
         CLC   SVTPREC+200(10),SPACES                                           
         BH    *+10                         USE IF PRESENT                      
         MVC   0(L'PUSER1,RE),SVTPREC+160  ELSE PRD USER1 DATA-SBSPECS          
*                                                                               
         LA    RE,+L'PUSER1(RE)                                                 
SBPR29B  CLI   0(RE),C' '           FLOAT DELIMITER                             
         BH    SBPR29C                                                          
         BCT   RE,SBPR29B                                                       
*                                                                               
SBPR29C  MVI   1(RE),CARET          DELIMITER                                   
         LA    RE,2(RE)                                                         
         ST    RE,MYFULL                                                        
         LR    R2,RE                                                            
         EDIT  BACTP,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT               
*                                                                               
         AP    SBCOUNT,=P'1'                                                    
         AP    SBTOTAL,BACTP                                                    
         L     RE,MYFULL                                                        
         AR    RE,R0                                                            
         MVI   0(RE),CARET     DELIMITER                                        
         LA    RE,1(RE)                                                         
         MVC   0(4,RE),=C'ITEM'   YES 2 DELIMITERS-EMPTY FIELD                  
         MVI   4(RE),CARET                                                      
         MVI   5(RE),CARET                                                      
         LA    RE,6(RE)                                                         
         ST    RE,MYFULL                                                        
*                                                                               
         MVC   WORK(50),SPACES                                                  
         MVC   WORK(3),BKEYPRD                                                  
         L     RF,ADEST                                                         
         USING ESTHDRD,RF                                                       
         MVC   WORK+4(20),EDESC                                                 
         DROP  RF                                                               
*                                                                               
         OC    WORK+4(20),SPACES     JUST IN CASE                               
         LA    RF,WORK+24                                                       
SBPR29E  CLI   0(RF),C' '                                                       
         BH    SBPR29G                                                          
         BCT   RF,SBPR29E                                                       
*                                                                               
SBPR29G  DS    0H                                                               
         LA    R2,2(RF)                                                         
*                                                                               
         MVC   WORK+50(4),SVTPREC+150      YYMM  FROM SBSPECS                   
         MVC   WORK+54(2),=C'01'       SET DAY                                  
         GOTO1 DATCON,DMCB,(0,WORK+50),(6,0(R2))                                
         LA    R2,6(R2)                                                         
         MVC   0(09,R2),CARET9        DELIMITER + EMPTIES                       
         L     RE,MYFULL                                                        
         MVC   0(50,RE),WORK                                                    
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   PS075C                                                           
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PS075C   L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
         B     PS900                                                            
*                                                                               
         DROP  R7                                                               
*                                                                               
PS900    DS    0H                                                               
         L     RF,ADBILL           SAVE THIS BILL RECORD                        
         MVC   SVBILL,0(RF)                                                     
*                                                                               
         LA    RE,SVBAMTS          AND BILL VALUES                              
         LA    RF,BAMTS                                                         
         LHI   R0,NAMTS                                                         
         ZAP   0(6,RE),0(6,RF)                                                  
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         LA    RE,TPREC            AND THIS TAPE RECORD                         
         LHI   R1,L'TPREC                                                       
         LA    RF,SVTPREC                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
PSX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*    INDIVIDUAL FIELD ROUTINES                                                  
***********************************************************************         
         SPACE 1                                                                
PSBR01   DS    0H                  AGENCY CODE                                  
         LA    R4,QAGY                                                          
         B     PSSET                                                            
*                                                                               
PSBR02   DS    0H                  MEDIA CODE                                   
         LA    R4,MED                                                           
         B     PSSET                                                            
*                                                                               
PSBR03   DS    0H                  OFFICE CODE                                  
         L     R7,ADCLT                                                         
         LA    R4,COFFICE-CLTHDR(R7)                                            
         B     PSSET                                                            
*                                                                               
PSBR04   DS    0H                  MKT GROUP                                    
         L     R7,ADBILL                                                        
         LA    R4,BLMGR-BILLREC(R7)                                             
         B     PSSET                                                            
*                                                                               
PSBR05   DS    0H                  RETAIL ACCOUNT                               
         LA    R4,SPACES                                                        
         CLI   RETAIL,C'Y'         ONLY FOR RETAIL                              
         BNE   PSSET                                                            
         L     R7,ADBILL                                                        
         LA    R4,BRETACCT-BILLREC(R7)                                          
         B     PSSET                                                            
*                                                                               
PSBR06   DS    0H                  NETPAK SUB-MEDIA (BLMED)                     
         LA    R4,=C' '            ONLY IF NETPAK                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   PSSET                                                            
         L     R7,ADBILL                                                        
         LA    R4,BLMED-BILLREC(R7)                                             
         CLI   0(R4),C' '          DEFAULT IS NET                               
         BH    *+8                                                              
         LA    R4,=C'N'                                                         
         B     PSSET                                                            
*                                                                               
PSBR07   DS    0H                  NETPAK - COST TYPE                           
         L     R7,ADBILL                                                        
         LA    R4,BILCTYP-BILLREC(R7)                                           
         B     PSSET                                                            
*                                                                               
PSBR11   DS    0H                  CLT CODE                                     
         LA    R4,CLT                                                           
         B     PSSET                                                            
*                                                                               
PSBR12   DS    0H                  CLT NAME (UPPERCASE)                         
         LA    R4,CLTNM                                                         
         OC    CLTNM,SPACES                                                     
         B     PSSET                                                            
*                                                                               
PSBR13   DS    0H                  CLT NUMBER                                   
         L     R7,ADCLT                                                         
         LA    R4,CCLTIFC-CLTHDR(R7)                                            
         CLI   0(R4),C' '          TEST PRESENT                                 
         BH    PSSET                                                            
         LA    R4,=8C'0'                                                        
         B     PSSET                                                            
*                                                                               
PSBR21   DS    0H                  PRD CODE                                     
         LA    R4,PRD                                                           
         B     PSSET                                                            
*                                                                               
PSBR22   DS    0H                  PRD NAME (UPPERCASE)                         
         LA    R4,PRDNM                                                         
         OC    PRDNM,SPACES                                                     
         B     PSSET                                                            
*                                                                               
PSBR23   DS    0H                  PRD NUMBER                                   
         L     R7,ADPRD                                                         
         LA    R4,PACCT-PRDHDR(R7)                                              
         CLI   0(R4),X'FF'                                                      
         BNE   PSSET                                                            
         ZAP   DUB,1(3,R4)                                                      
         B     PSNUM                                                            
*                                                                               
PSBR24   DS    0H                  DIVISION (FROM PRD HDR)                      
         L     R7,ADPRD                                                         
         LA    R4,PDIV-PRDHDR(R7)                                               
         OC    0(3,R4),=3C'0'                                                   
         B     PSSET                                                            
*                                                                               
PSBR25   DS    0H                  PUSER1 (UPPERCASE)                           
         L     R7,ADPRD                                                         
         LA    R4,PUSER1-PRDHDR(R7)                                             
         OC    0(L'PUSER1,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR26   DS    0H                  PUSER2 (UPPERCASE)                           
         L     R7,ADPRD                                                         
         LA    R4,PUSER2-PRDHDR(R7)                                             
         OC    0(L'PUSER2,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR31   DS    0H                  EST CODE                                     
         L     R7,ADBILL                                                        
         ZIC   R0,BKEYEST-BILLREC(R7)                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         LA    R4,WORK                                                          
         B     PSSET                                                            
*                                                                               
PSBR32   DS    0H                  EST NAME (UPPERCASE)                         
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBR32B                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBR32B  DS    0H                                                               
         LA    R4,EDESC-ESTHDR(R7)                                              
         OC    0(L'EDESC,R4),SPACES                                             
         B     PSSET                                                            
*                                                                               
PSBR33   DS    0H                  ESTIMATE FILERS                              
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBR33B                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBR33B  DS    0H                                                               
         LA    R4,EPROF-ESTHDR(R7)                                              
         B     PSSET                                                            
*                                                                               
PSBR35   DS    0H                  EUSER1 (UPPERCASE)                           
         L     R7,ADEST                                                         
         LA    R4,EUSER1-ESTHDR(R7)                                             
         OC    0(L'EUSER1,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR36   DS    0H                  EUSER2 (UPPERCASE)                           
         L     R7,ADEST                                                         
         LA    R4,EUSER2-ESTHDR(R7)                                             
         OC    0(L'EUSER2,R4),SPACES                                            
         B     PSSET                                                            
*                                                                               
PSBR41   DS    0H                  BILL MONTH (2)                               
         L     R7,ADBILL                                                        
         LA    R4,DINVNO                                                        
         B     PSSET                                                            
*                                                                               
PSBR42   DS    0H                  INVOICE NUMBER (4)                           
         L     R7,ADBILL                                                        
         LA    R4,DINVNO+2                                                      
         B     PSSET                                                            
*                                                                               
PSBR43   DS    0H                  INVOICE NUMBER (6)                           
         L     R7,ADBILL                                                        
         LA    R4,DINVNO                                                        
         B     PSSET                                                            
*                                                                               
PSBR44   DS    0H                  BILL RUN DATE                                
         L     R7,ADBILL                                                        
         LA    R4,BDATE-BILLREC(R7)                                             
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
PSBR45   DS    0H                  INVOICE DATE                                 
         L     R7,ADBILL                                                        
         LA    R4,BQDATE-BILLREC(R7)                                            
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
PSBR46   DS    0H                  DUE DATE                                     
         L     R7,ADBILL                                                        
         LA    R4,BDUEDATE-BILLREC(R7)                                          
         MVI   BYTE,3                                                           
         B     PSDTE                                                            
*                                                                               
PSBR47   DS    0H                  MONTH OF SERVICE                             
         L     R7,ADBILL                                                        
         LA    R4,BKEYYSRV-BILLREC(R7)                                          
         MVI   BYTE,3                                                           
         B     PSDTE                                                            
*                                                                               
PSBR48   DS    0H                  TODAY'S DATE                                 
         LA    R4,TODAY                                                         
         MVI   BYTE,0                                                           
         B     PSDTE                                                            
*                                                                               
BILL     USING AMOUNTSD,BAMTS                                                   
PSBR51   DS    0H                  GROSS                                        
         ZAP   DUB,BILL.AMTGRS                                                  
         B     PSNUM                                                            
*                                                                               
PSBR52   DS    0H                  NET                                          
         ZAP   DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR53   DS    0H                  ACTUAL                                       
         ZAP   DUB,BILL.AMTACT                                                  
         B     PSNUM                                                            
*                                                                               
PSBR54   DS    0H                  AGYCOM - (GROSS - NET)                       
         ZAP   DUB,BILL.AMTGRS                                                  
         SP    DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR55   DS    0H                  AGYCOM (ACTUAL - NET)                        
         ZAP   DUB,BILL.AMTACT                                                  
         SP    DUB,BILL.AMTNET                                                  
         B     PSNUM                                                            
*                                                                               
PSBR56   DS    0H                  TAX                                          
         L     R7,ADBILL                                                        
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,DUB                                                           
         B     PSNUM                                                            
*                                                                               
PSBR57   DS    0H                  GROSS LESS TAX                               
         L     R7,ADBILL                                                        
         ZAP   DUB,BILL.AMTGRS                                                  
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
         B     PSNUM                                                            
*                                                                               
PSBR58   DS    0H                  FEE AMOUNT                                   
         ZAP   DUB,=P'0'           0 IF NOT AOR BILL                            
         L     R7,ADBILL                                                        
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BZ    PSNUM                                                            
         ZAP   DUB,BILL.AMTACT     ELSE COMPLEMENT OF ACTUAL                    
         MP    DUB,=P'-1'                                                       
         B     PSNUM                                                            
*                                                                               
PSBR59   DS    0H                  MEDIA ACTUAL                                 
         ZAP   DUB,=P'0'           0 IF AOR BILL                                
         L     R7,ADBILL                                                        
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BNZ   PSNUM                                                            
         ZAP   DUB,BILL.AMTACT     ELSE ACTUAL                                  
         B     PSNUM                                                            
*                                                                               
PSBR5A   DS    0H                  GST                                          
         ZAP   DUB,BILL.AMTGST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5B   DS    0H                  PST (INCL HST)                               
         ZAP   DUB,BILL.AMTPST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5C   DS    0H                  GST + PST (INCL HST)                         
         ZAP   DUB,BILL.AMTGST                                                  
         AP    DUB,BILL.AMTPST                                                  
         B     PSNUM                                                            
*                                                                               
PSBR5E   DS    0H                  PST ALONE                                    
         ZAP   DUB,BILL.AMTPST     IS PST                                       
         SP    DUB,BILL.AMTHST     LESS HST                                     
         B     PSNUM                                                            
*                                                                               
PSBR5F   DS    0H                  HST ALONE                                    
         ZAP   DUB,BILL.AMTHST                                                  
         B     PSNUM                                                            
         DROP  BILL                                                             
*                                                                               
PSBR5D   DS    0H                  MARKET                                       
         L     R7,ADBILL                                                        
         LA    R4,BLMKT-BILLREC(R7)                                             
         B     PSSET                                                            
*                                                                               
PSBR61   DS    0H                  GROSS   (ESTIMATE AMOUNTS)                   
         ZAP   DUB,EATOTGRS                                                     
         B     PSNUM                                                            
*                                                                               
PSBR62   DS    0H                  NET                                          
         ZAP   DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR63   DS    0H                  ACTUAL                                       
         ZAP   DUB,EATOTACT                                                     
         B     PSNUM                                                            
*                                                                               
PSBR64   DS    0H                  AGYCOM - (GROSS - NET)                       
         ZAP   DUB,EATOTGRS                                                     
         SP    DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR65   DS    0H                  AGYCOM (ACTUAL - NET)                        
         ZAP   DUB,EATOTACT                                                     
         SP    DUB,EATOTNET                                                     
         B     PSNUM                                                            
*                                                                               
PSBR71   DS    0H                  REVISION STATUS                              
         LA    R4,REVSW                                                         
         B     PSSET                                                            
*                                                                               
PSBR72   DS    0H                  AOR STATUS                                   
         L     R7,ADBILL                                                        
         LA    R4,=C'A'            AOR                                          
         TM    BILSTAT-BILLREC(R7),BSTTAORQ                                     
         BNZ   PSSET                                                            
         LA    R4,=C'C'            CLIENT BILL IN AOR SITUATION                 
         TM    BILSTAT-BILLREC(R7),BSTCAORQ                                     
         BNZ   PSSET                                                            
         LA    R4,SPACES           REGULAR                                      
         B     PSSET                                                            
*                                                                               
PSBR73   DS    0H                  BILL TYPE - B4...B7                          
         L     R7,ADBILL                                                        
         USING BILLRECD,R7                                                      
         MVC   FULL,=C'    '       NEW STYLE BILLS                              
         MVC   FULL(2),BTYPE                                                    
*                                                                               
         TM    BILSTAT,X'40'       TEST MANUAL BILL                             
         BZ    *+16                                                             
         MVC   FULL(3),=C'MAN'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,X'20'       TEST AOR BILL                                
         BZ    *+16                                                             
         MVC   FULL(3),=C'AOR'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,BSTSCOMQ    TEST SEP COMM BILL                           
         BZ    *+16                                                             
         MVC   FULL(3),=C'UFC'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         TM    BILSTAT,BSTSNETQ    TEST SEP NET BILL                            
         BZ    *+16                                                             
         MVC   FULL(3),=C'NET'                                                  
         MVC   FULL+3(1),BTYPE+1                                                
*                                                                               
         LA    R4,FULL                                                          
         B     PSSET                                                            
*                                                                               
PSBR74   DS    0H                  KETCHUM BILL TYPE (DEBIT/CREDIT)             
         L     R7,ADBILL                                                        
         LA    R4,=C'SI'           STANDARD INVOICE                             
         CP    BACTP,=P'0'                                                      
         BNL   *+8                                                              
         LA    R4,=C'CI'           CREDIT INVOICE                               
         B     PSSET                                                            
         DROP  R7                                                               
*                                                                               
PSBRF0   DS    0H                  ZEROS                                        
         ZAP   DUB,=P'0'                                                        
         B     PSNUM                                                            
*                                                                               
PSBRF1   DS    0H                  LITERAL                                      
         LA    R4,DSEDATA                                                       
         B     PSSET                                                            
*                                                                               
PSBRF2   DS    0H                  SPACES                                       
         LA    R4,SPACES                                                        
         B     PSSET                                                            
*                                                                               
PSBRFE   DS    0H                  AGENCY SPECIAL                               
         SR    RF,RF                                                            
         ICM   RF,7,DSEPROC                                                     
         BASR  RE,RF                                                            
*                                                                               
         CLI   MODE,CLTLAST        ROUNTINE MIGHT HAVE SET THIS                 
         BE    EXIT                TO SKIP THIS CLIENT                          
*                                                                               
         B     PSRETURN                                                         
*                                                                               
PSBRB1   DS    0H                  FILTERS                                      
         CLI   DSEFTYP,C'C'        CLIENT FILTER                                
         BNE   *+14                                                             
         MVC   CLFILT,DSEFILT                                                   
         B     PSRETURN                                                         
*                                                                               
         CLI   DSEFTYP,C'M'        MEDIA FILTER                                 
         BNE   *+10                                                             
         MVC   MEFILT,DSEFILT                                                   
         B     PSRETURN                                                         
*                                                                               
PSBRC0   DS    0H                  SPECIAL (JW - LAST 5 PRD CHARS)              
         L     R7,ADPRD                                                         
         LA    R4,PNAME+19-PRDHDR(R7)                                           
*                                                                               
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         SH    R4,=H'4'            LAST 5 CHARS OF PRD NAME                     
         B     PSSET                                                            
*                                                                               
PSBRC2   DS    0H                  SPECIAL (COMPTON - NETWORK VS CABLE)         
         LA    R4,=C'05'           NETWORK                                      
         L     R7,ADBILL                                                        
         CLI   BKEYEST-BILLREC(R7),200                                          
         BL    *+8                                                              
         LA    R4,=C'09'           CABLE                                        
         B     PSSET                                                            
*                                                                               
PSBRC3   DS    0H                  SPECIAL MEDIA CODES                          
         CLC   QAGY,=C'WW'         WUNDERMAN                                    
         BNE   PSBRC3A                                                          
         LA    R4,=C'240'          TV = 240                                     
         CLI   QMED,C'T'                                                        
         BE    PSSET                                                            
         LA    R4,=C'210'          RADIO = 210                                  
         CLI   QMED,C'R'                                                        
         BE    PSSET                                                            
         LA    R4,=C'999'          OTHER = 999 ??                               
         B     PSSET                                                            
*                                                                               
PSBRC3A  DS    0H                                                               
         CLC   QAGY,=C'YN'         YNR                                          
         BNE   PSBRC3B                                                          
         LA    R4,=C'281'          TV = 281                                     
         CLI   QMED,C'T'                                                        
         BE    PSSET                                                            
         LA    R4,=C'271'          RADIO = 271                                  
         CLI   QMED,C'R'                                                        
         BE    PSSET                                                            
         LA    R4,=C'111'          X= 111                                       
         CLI   QMED,C'X'                                                        
         BE    PSSET                                                            
         B     PSBRC3N             ELSE NETWORK SPECIAL                         
*                                                                               
PSBRC3B  DS    0H                                                               
         CLC   QAGY,=C'NE'         DDB NEEDHAM                                  
         BNE   PSBRC3C                                                          
         CLI   QMED,C'N'           NETWORK ONLY                                 
         BE    PSBRC3N                                                          
         B     PSBRC3X                                                          
*                                                                               
PSBRC3C  DS    0H                                                               
         B     PSBRC3X                                                          
*                                  ELSE NETWORK TYPE FROM ESTIMATE              
PSBRC3N  DS    0H                                                               
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC3P                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC3P  DS    0H                                                               
         LA    RF,EPROF-ESTHDR(R7)                                              
*                                                                               
         CLC   QAGY,=C'YN'         YNR                                          
         BNE   PSBRC3Q                                                          
         LA    R4,=C'231'                                                       
         CLI   0(RF),C'C'          CABLE                                        
         BE    PSSET                                                            
         LA    R4,=C'261'                                                       
         CLI   0(RF),C'S'          SYNDICATION                                  
         BE    PSSET                                                            
         LA    R4,=C'241'                                                       
         CLI   0(RF),C'U'          CUT INS                                      
         BE    PSSET                                                            
         LA    R4,=C'211'          NETWORK (AND OTHER)                          
         B     PSSET                                                            
*                                                                               
PSBRC3Q  DS    0H                                                               
         CLC   QAGY,=C'NE'         DDB-NEEDHAM                                  
         BNE   PSBRC3R                                                          
         LA    R4,=C'17'                                                        
         CLI   0(RF),C'C'          CABLE                                        
         BE    PSSET                                                            
         LA    R4,=C'16'                                                        
         CLI   0(RF),C'S'          SYNDICATION                                  
         BE    PSSET                                                            
         LA    R4,=C'15'           NETWORK                                      
         B     PSSET                                                            
*                                                                               
PSBRC3R  DS    0H                                                               
PSBRC3X  DS    0H                  EXIT WITHOUT SETTING ANY VALUE               
         B     PSRETURN                                                         
*                                                                               
PSBRC4   DS    0H                  DDB- SPECIAL AGENCY CODES                    
         LA    R4,=C'RET'          DNRFO = RET                                  
         CLC   =C'NR',QAGY                                                      
         BE    PSBRC4D                                                          
*                                                                               
         L     R7,ADCLT            ELSE BASED ON OFFICE                         
         LA    R7,COFFICE-CLTHDR(R7)                                            
*                                                                               
         LA    R4,=C'DDB'                                                       
         CLI   0(R7),C'Y'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'2'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'1'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'C'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'W'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'4'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'5'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'T'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'I'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'F'                                                       
         BE    PSBRC4D                                                          
*                                                                               
         LA    R4,=C'DDW'                                                       
         CLI   0(R7),C'L'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'S'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'Z'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'V'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'7'                                                       
         BE    PSBRC4D                                                          
         CLI   0(R7),C'8'                                                       
         BE    PSBRC4D                                                          
*                                                                               
         LA    R4,=C'XXX'          UNKNOWN AGENCY                               
*                                                                               
PSBRC4D  DS    0H                                                               
         B     PSSET                                                            
*                                                                               
PSBRC5   DS    0H                  DDB- SPECIAL CLIENT/ESTIMATE NUMBERS         
         L     R7,ADBILL               JUST USE ESTIMATE NUMBER                 
         ZIC   R0,BKEYEST-BILLREC(R7)  UNLESS SPECIAL CLIENT CODE               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         LA    R4,WORK                                                          
*                                                                               
         CLC   CLT(2),=C'GT'                                                    
         BE    PSBRC5D                                                          
         CLC   CLT(2),=C'VX'                                                    
         BE    PSBRC5D                                                          
         B     PSSET                                                            
*                                                                               
PSBRC5D  DS    0H                  EST NAME EXTRACT                             
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC5G                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC5G  DS    0H                                                               
         LA    R4,EDESC+2-ESTHDR(R7)    ESTNAME+2(4)                            
         B     PSSET                                                            
*                                                                               
PSBRC6   DS    0H                  GROUP W - AGENT                              
*                                  (UP TO 6 NUMERICS AT START OF ESTN)          
         L     RF,ADBILL                                                        
         L     R7,ADEST                                                         
         CLC   0(8,RF),0(R7)       TEST HAVE EST REC                            
         BE    PSBRC6G                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(RF)                                                     
         GOTO1 READ                                                             
         GOTO1 GETEST                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
*                                                                               
PSBRC6G  DS    0H                                                               
         LA    R4,WORK                                                          
         MVC   WORK(6),=C'000000'                                               
         LA    R5,EDESC-ESTHDR(R7)                                              
         LHI   R3,6                                                             
*                                                                               
PSBRC6J  DS    0H                                                               
         CLI   0(R5),C'0'          CHECK FOR NUMERICS                           
         BL    *+12                                                             
         LA    R5,1(R5)            NEXT POS                                     
         BCT   R3,PSBRC6J                                                       
*                                                                               
         LCR   R3,R3                                                            
         AHI   R3,5                R3 HAS LENGTH - 1                            
         BM    PSSET                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,EDESC-ESTHDR(0,R7)                                           
         UNPK  WORK(6),DUB                                                      
         B     PSSET                                                            
*                                                                               
PSSET    DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,DSEPOS         POSITION IN OUTPUT                           
         LA    RF,TPREC-1(RF)                                                   
         ZIC   R5,DSELEN           LENGTH                                       
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)   *EXECUTED*                                       
*                                                                               
         CLI   DSEFMT,4            TEST EBCDIC OUTPUT (UPPERCASE)               
         BNE   PSRETURN                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),SPACES  *EXECUTED*                                       
         B     PSRETURN                                                         
*                                                                               
PSNUM    DS    0H                                                               
         CLI   DSEFMT,1                                                         
         BNE   PSNUM2                                                           
*                                  PACKED                                       
         LA    R4,DUB+8                                                         
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM2   DS    0H                                                               
         CLI   DSEFMT,2                                                         
         BNE   PSNUM3                                                           
*                                  BINARY                                       
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         LA    R4,FULL+4                                                        
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM3   DS    0H                                                               
         CLI   DSEFMT,3                                                         
         BNE   PSNUM4                                                           
*                             EBCDIC - SIGN OVERPUNCH IN LOW POSITION           
PSNUM3B  DS    0H                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         B     PSSET                                                            
*                                                                               
PSNUM4   DS    0H                                                               
         CLI   DSEFMT,4                                                         
         BNE   PSNUM5                                                           
*                                  EBCDIC - NO SIGN OVERPUNCH                   
         OI    DUB+7,X'0F'                                                      
         B     PSNUM3B                                                          
*                                                                               
PSNUM5   DS    0H                  LEADING MINUS SIGN - FOR NEGATIVES           
         CLI   DSEFMT,5                                                         
         BNE   PSNUM6                                                           
*                                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         TM    WORK+14,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+8                                                              
         MVI   0(R4),C'-'          ADD MINUS SIGN                               
         OI    WORK+14,X'F0'       REMOVE OVERPUNCH                             
         B     PSSET                                                            
*                                                                               
PSNUM6   DS    0H                  TRAILING MINUS SIGN - FOR NEGATIVES          
         CLI   DSEFMT,6                                                         
         BNE   PSNUM7                                                           
*                                                                               
         UNPK  WORK(15),DUB                                                     
         LA    R4,WORK+15                                                       
         ZIC   R5,DSELEN                                                        
         SR    R4,R5                                                            
         TM    WORK+14,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+12                                                             
         LA    R4,1(R4)                                                         
         MVI   WORK+15,C'-'        ADD MINUS SIGN AT END                        
         OI    WORK+14,X'F0'       REMOVE OVERPUNCH                             
         B     PSSET                                                            
*                                                                               
PSNUM7   DS    0H                                                               
         DC    H'0'                UNKNOWN NUMERIC FORMAT                       
*                                                                               
PSDTE    DS    0H                  DATES                                        
         MVC   BYTE2,DSEFMT        DATE FORMAT FROM FORMAT= PARAMETER           
         CLI   DSEFMT,32           PREDEFINED DATCON OUTPUT TYPE?               
         BNL   PSDTE2              NO                                           
         OI    BYTE2,X'20'         DON'T RETURN FUNNY YEARS                     
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(BYTE2,WORK)                            
         B     PSDTEX                                                           
*                                                                               
PSDTE2   DS    0H                                                               
         CLI   DSEFMT,32           MMDDYY                                       
         BNE   PSDTE3                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK)                            
         MVC   WORK+6(6),WORK                                                   
         MVC   WORK(4),WORK+6+2    MMDD                                         
         MVC   WORK+4(2),WORK+6    YY                                           
         B     PSDTEX                                                           
*                                                                               
PSDTE3   DS    0H                                                               
         CLI   DSEFMT,33           MMYY                                         
         BNE   PSDTE4                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK)                            
         MVC   WORK+6(6),WORK                                                   
         MVC   WORK(2),WORK+6+2    MM                                           
         MVC   WORK+2(2),WORK+6    YY                                           
         B     PSDTEX                                                           
*                                                                               
PSDTE4   DS    0H                                                               
         CLI   DSEFMT,34           CYYMMDD                                      
         BNE   PSDTE5                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(15,WORK)                               
         UNPK  DUB,WORK(4)         X'0CYYDDDF' --> C'00CYYDDD'                  
         MVC   WORK(1),DUB+2       CENTURY                                      
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',WORK+1) YYMMDD                   
*                                                                               
PSDTE5   DS    0H                                                               
         CLI   DSEFMT,35           CMMYY                                        
         BNE   PSDTEX                                                           
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(15,WORK)                               
         UNPK  DUB,WORK(4)         X'0CYYDDDF' --> C'00CYYDDD'                  
         MVC   WORK(1),DUB+2       CENTURY                                      
         GOTO1 DATCON,DMCB,(BYTE,0(R4)),(X'20',DUB) YYMMDD                      
         MVC   WORK+1(2),DUB+2     MM                                           
         MVC   WORK+3(2),DUB       YY                                           
*                                                                               
PSDTEX   DS    0H                                                               
         LA    R4,WORK                                                          
         B     PSSET                                                            
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NXTEL+2                                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
PBROLL   NTR1                                                                   
         LHI   R0,NAMTS                                                         
PBROLL2  DS    0H                                                               
         AP    0(6,R4),0(6,R3)                                                  
         LA    R3,6(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R0,PBROLL2                                                       
         B     EXIT                                                             
         SPACE 3                                                                
CLRTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CLRT2    DS    0H                                                               
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R0,CLRT2                                                         
         B     EXIT                                                             
         SPACE 3                                                                
CHKTOTS  NTR1                                                                   
         LHI   R0,NAMTS                                                         
CHKT2    DS    0H                                                               
         CP    0(6,R3),=P'0'                                                    
         BNE   NO                  EXIT WITH CC NOT =                           
         LA    R3,6(R3)                                                         
         BCT   R0,CHKT2                                                         
         B     YES                 EXIT WITH CC =                               
         ANSR                                                                   
         EJECT                                                                  
TOTPRNT  NTR1                                                                   
         SPACE 2                                                                
         ST    R3,ATOTS                                                         
         L     R4,ATOTS                                                         
         GOTO1 ATPFMT                                                           
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
WIAPLCT  DS    0H                  FOR WESTERN/APL - TEST CLIENT                
*                                  (CALLED FROM CLTFRST)                        
         LA    RF,WIAPLLST                                                      
*                                                                               
WIAPLC4  DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WIAPNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BER   RE                                                               
*                                                                               
         LA    RF,5(RF)                                                         
         B     WIAPLC4                                                          
*                                                                               
*                                                                               
         SPACE 2                                                                
WAPLBTCT DS    0H                  FOR WESTERN/APL - TEST WA CLT                
*                                  (CALLED FROM CLTFRST)                        
         LA    RF,WAPLLSTS                                                      
         CLI   NETPAKSW,C'Y'                                                    
         BNE   WAPLC4                                                           
         LA    RF,WAPLLSTN         USE NET CLIENT LIST, NOT SPOT LIST           
*                                                                               
WAPLC4   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WAPLNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BER   RE                                                               
*                                                                               
         LA    RF,3(RF)                                                         
         B     WAPLC4                                                           
*                                                                               
WAPLNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
WAPLOK   DS    0H                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
         SPACE 2                                                                
WIAPLCMT DS    0H                  FOR WESTERN/APL - TEST CLIENT/MONTH          
*                                  (CALLED FROM PROC BILL)                      
         LA    RF,WIAPLLST                                                      
*                                                                               
WIAPLCM4 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    WIAPNO                                                           
*                                                                               
         CLC   CLT,0(RF)                                                        
         BNE   *+14                                                             
         CLC   KEY+8(2),3(RF)                                                   
         BNL   WIAPOK                                                           
*                                                                               
         LA    RF,5(RF)                                                         
         B     WIAPLCM4                                                         
*                                                                               
WIAPNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
WIAPOK   DS    0H                                                               
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
WIAPLLST DS    0C                                                               
         DC    C'BKK',X'6304'                                                   
         DC    C'LAB',X'6304'                                                   
         DC    C'LGO',X'6304'                                                   
         DC    C'RCA',X'6304'                                                   
         DC    C'UPS',X'6304'                                                   
         DC    X'FF'                                                            
*                                                                               
*        CLIENT LIST FOR WAPLBT                                                 
*                                                                               
WAPLLSTS DS    0C                  FOR SPOT                                     
         DC    C'LAB'                                                           
         DC    C'LEV'                                                           
         DC    C'LIP'                                                           
         DC    C'RCA'                                                           
         DC    C'HYG'                                                           
         DC    C'JMD'                                                           
         DC    C'MSK'                                                           
         DC    C'JJO'                                                           
         DC    C'GOR'                                                           
         DC    C'CHB'                                                           
         DC    C'AMI'                                                           
         DC    C'UPS'                                                           
         DC    C'ATK'                                                           
         DC    X'FF'                                                            
*                                                                               
WAPLLSTN DS    0C                  FOR NET                                      
         DC    C'AMI'                                                           
         DC    C'BKK'                                                           
         DC    C'LAB'                                                           
         DC    C'LGO'                                                           
         DC    C'UPS'                                                           
         DC    X'FF'                                                            
*                                                                               
MAJORNAM DC    C'SBTTAPE '         MAJOR RESOURCE NAME FOR ENQ                  
CARET9   DC    X'B0B0B0B0B0B0B0B0B0'                                            
*                                                                               
SPDYNDSN DC    CL(DSNLENQ)'SPTTAPE.SP0BTAG1'                                    
NEDYNDSN DC    CL(DSNLENQ)'NETTAPE.NE0BTAG1'                                    
         LTORG                                                                  
*                                                                               
PSBRTAB  DS    F'0'                (TABLE IS ZERO-BASED)                        
         B     PSBR01       #01    AGENCY                                       
         B     PSBR02       #02    MEDIA                                        
         B     PSBR03       #03    OFFICE                                       
         B     PSBR04       #04    MKT GROUP                                    
         B     PSBR11       #05    CLT CODE                                     
         B     PSBR12       #06    CLT NAME                                     
         B     PSBR13       #07    CLT NUMBER                                   
         B     PSBR21       #08    PRD CODE                                     
         B     PSBR22       #09    PRD NAME                                     
         B     PSBR23       #10    PRD NUMBER                                   
         B     PSBR31       #11    EST CODE                                     
         B     PSBR32       #12    EST NAME                                     
         B     PSBR41       #13    BILL MONTH (2)                               
         B     PSBR42       #14    INV. NO (4)                                  
         B     PSBR43       #15    INV. NO (6)                                  
         B     PSBR44       #16    BILL RUN DATE                                
         B     PSBR45       #17    INVOICE DATE                                 
         B     PSBR46       #18    DUE DATE                                     
         B     PSBR47       #19    MONTH OF SERVICE                             
         B     PSBR51       #20    GROSS                                        
         B     PSBR52       #21    NET                                          
         B     PSBR53       #22    ACTUAL                                       
         B     PSBR54       #23    AGYCOM (GROSS-NET)                           
         B     PSBR55       #24    AGYCOM (ACTUAL - NET)                        
         B     PSBRF0       #25    ZEROS                                        
         B     PSBRF1       #26    LITERAL                                      
         B     PSBRF2       #27    SPACES                                       
         B     PSBRB1       #28    FILTER                                       
         B     PSBRC0       #29    SPECIAL (JW)                                 
         DC    F'0'         #30                                                 
         B     PSBR24       #31    DIVISION (FROM PRD HEADER)                   
         B     PSBR71       #32    REVISION STATUS                              
         B     PSBR33       #33    ESTIMATE FILTERS                             
         B     PSBR61       #34    GROSS   (ESTIMATE AMOUNTS)                   
         B     PSBR62       #35    NET                                          
         B     PSBR63       #36    ACTUAL                                       
         B     PSBR64       #37    AGYCOM (GROSS-NET)                           
         B     PSBR65       #38    AGYCOM (ACTUAL - NET)                        
         B     PSBRC2       #39    SPECIAL (COMPTON)                            
         B     PSBRC3       #40    SPECIAL (Y&R NETWORK MEDIA)                  
         B     PSBRC4       #41    SPECIAL (DDB AGENCY CODES)                   
         B     PSBR05       #42    RETAIL ACCOUNT                               
         B     PSBR48       #43    TODAY'S DATE                                 
         B     PSBRC5       #44    SPECIAL (DDB CLIENT ESTIMATES)               
         B     PSBR56       #45    TAX                                          
         B     PSBR57       #46    GROSS LESS TAX                               
         B     PSBRC6       #47    SPECIAL - GROUP W AGENT                      
         B     PSBR58       #48    FEE AMOUNT   (0 FOR NON-AOR BILLS)           
         B     PSBR59       #49    MEDIA ACTUAL (0 FOR AOR BILLS)               
         B     PSBR72       #50    AOR STATUS                                   
         B     PSBR73       #51    BILL TYPE                                    
         B     PSBR74       #52    KETCHAM BILL TYPE (DEBIT/CREDIT)             
         B     PSBR06       #53    NETPAK SUB-MEDIA (BLMED)                     
         B     PSBRFE       #54    AGENCY SPECIAL ROUTINE                       
         B     PSBR5A       #55    GST AMOUNT                                   
         B     PSBR07       #56    COST TYPE (NETPAK)                           
         B     PSBR25       #57    PUSER1                                       
         B     PSBR26       #58    PUSER2                                       
         B     PSBR35       #59    EUSER1                                       
         B     PSBR36       #60    EUSER2                                       
         B     PSBR5B       #61    PST AMOUNT (INCL HST)                        
         B     PSBR5C       #62    GST+PST (INCL HST)                           
         B     PSBR5D       #63    MARKET                                       
         B     PSBR5E       #64    PST ALONE                                    
         B     PSBR5F       #65    HST ALONE                                    
         SPACE  3                                                               
*        NOTE X'41' (65) IS HIGHEST USED SO FAR (AT 5F)                         
         SPACE 2                                                                
SBITAPE  DCB   DDNAME=SBITAPE,DSORG=PS,MACRF=PM                                 
*                                                                               
MULTMSG  DC    C'**MULTIPLE TAPE DESCRIPTIONS IN ONE JOB - BYPASSED**'          
*                                                                               
         DROP  RB                                                               
*        RUN FIRST                                                              
         SPACE 2                                                                
RUNF     NMOD1 0,RUNF                                                           
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,VMASTC            USE MASTC'S AGYID                           
         USING MASTD,RF                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         DROP  RF                                                               
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY - YYYYMMDD                   
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R4,DUB                                                           
         EDIT  (R4),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
         XC    RUNINVS,RUNINVS     CLEAR RUN INVOICE TOTALS                     
         MVI   BKSFTP,C'N'         SET OFF DOING BURGER SFTP                    
*                                                                               
         MVI   G7SW,0              WILL BE SET TO 1 FOR GSTX REQ                
         MVI   SCSW,0              WILL BE SET TO 1 FOR H7 TYPE S               
*                                  AND FOR BN TYPE S                            
         MVI   MZSW,0              WILL BE SET TO 1 FOR H7 TYPE M               
         MVI   SBSW,0              WILL BE SET TO 1 FOR BN TYPE B               
         MVI   DGSW,0              WILL BE SET TO 1 FOR UB TYPE D               
         MVI   SMSW,0              WILL BE SET TO 1 FOR UB TYPE S               
         MVI   SMHDSW,C'N'         Y = HEADER CREATED                           
         XC    SCLMOS,SCLMOS       CLEAR MOS DATEES FOR HEADER                  
         XC    SCHMOS,SCHMOS                                                    
         MVI   MCCSW,0             WILL BE SET TO 1 FOR MC TYPE C               
         MVI   PHHDSW,0            INIT PHHDSW                                  
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   VSPFMTIN,VSPFMINO   A(SPFMTINO)                                  
         MVC   MVOFFICE,VOFFICER  A(OFFICER)                                    
         DROP  RF                                                               
*                                                                               
         MVI   OPENSW,C'N'                                                      
         LA    R3,RAMTS                                                         
         BAS   RE,FCLRTOTS                                                      
*                                                                               
         ZAP   SBCOUNT,=P'0'      STARBUCKS COUNTERS                            
         ZAP   SBTOTAL,=P'0'                                                    
         ZAP   DGCOUNT,=P'0'      DIAGEO COUNTERS                               
         ZAP   DGTOTAL,=P'0'                                                    
*                                                                               
*        SET GBINVALS (FOR GSTX BINSRCH)                                        
*                                                                               
         L     R1,=A(G7TABLE)     STORE SOEM ADDRESSES FIRST                    
         ST    R1,AOFG7T                                                        
         LA    R1,G7MED                                                         
         ST    R1,AG7MED                                                        
         ZAP   G7COUNT,=P'0'                                                    
         MVC   GBPAR1,AG7MED                                                    
         MVI   GBPAR1,X'01'      SET TO ADD RECORD                              
         MVC   GBPAR2,AOFG7T                                                    
         XC    G7RECNT,G7RECNT   ZERO RECORD COUNTER                            
         MVC   GBPAR4,=F'212'    LENGTH OF RECORD                               
         MVC   GBPAR5,=F'12'     LENGTH OF KEY                                  
         MVC   GBPAR6,=F'3000'   MAX NUMBER OF RECORDS                          
*                                                                               
*        SET SCINVALS (FOR MSNY TYPE S FILE)                                    
*                                                                               
         L     R1,=A(SCTABLE)     STORE SOME ADDRESSES FIRST                    
         ST    R1,AOFSCT                                                        
         LA    R1,SCMED                                                         
         ST    R1,ASCMED                                                        
         ZAP   SCCOUNT,=P'0'                                                    
         MVC   SCPAR1,ASCMED                                                    
         MVI   SCPAR1,X'01'      SET TO ADD RECORD                              
         MVC   SCPAR2,AOFSCT                                                    
         XC    SCRECNT,SCRECNT   ZERO RECORD COUNTER                            
         MVC   SCPAR4,=F'212'    LENGTH OF RECORD                               
         MVC   SCPAR5,=F'12'     LENGTH OF KEY                                  
         MVC   SCPAR6,=F'2000'   MAX NUMBER OF RECORDS                          
*                                                                               
*        SET MZINVALS (FOR MAZDA)                                               
*                                                                               
         L     R1,=A(MZTABLE)     STORE SOME ADDRESSES FIRST                    
         ST    R1,AOFMZT                                                        
         LA    R1,MZKEY                                                         
         ST    R1,AMZKEY                                                        
         MVC   MZPAR1,AMZKEY                                                    
         MVI   MZPAR1,X'01'      SET TO ADD RECORD                              
         MVC   MZPAR2,AOFMZT                                                    
         XC    MZRECNT,MZRECNT   ZERO RECORD COUNTER                            
         MVC   MZPAR4,=F'390'    LENGTH OF RECORD                               
         MVC   MZPAR5,=F'15'     LENGTH OF KEY                                  
         MVC   MZPAR6,=F'1000'   MAX NUMBER OF RECORDS                          
*                                                                               
*        SET MCINVALS (FOR GSTX BINSRCH)                                        
*                                                                               
         L     R1,=A(MCTABLE)     STORE SOEM ADDRESSES FIRST                    
         ST    R1,AOFMCT                                                        
         LA    R1,MCMED                                                         
         ST    R1,AMCMED                                                        
         ZAP   MCCOUNT,=P'0'                                                    
         MVC   MCPAR1,AMCMED                                                    
         MVI   MCPAR1,X'01'      SET TO ADD RECORD                              
         MVC   MCPAR2,AOFMCT                                                    
         XC    MCRECNT,MCRECNT   ZERO RECORD COUNTER                            
         MVC   MCPAR4,=F'353'    LENGTH OF RECORD                               
         MVC   MCPAR5,=F'12'     LENGTH OF KEY                                  
         MVC   MCPAR6,=F'2000'   MAX NUMBER OF RECORDS                          
         PRINT GEN                                                              
RFEXIT   XIT1                                                                   
         SPACE 3                                                                
FCLRTOTS NTR1                                                                   
         LHI   R0,NAMTS                                                         
FCLRT2   DS    0H                                                               
         ZAP   0(6,R3),=P'0'                                                    
         LA    R3,6(R3)                                                         
         BCT   R0,FCLRT2                                                        
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
         EJECT                                                                  
RUNL     NMOD1 0,RUNL                                                           
         LA    RC,SPACEND                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(19),=C'** REPORT TOTALS **'                                 
*                                                                               
         MVC   P+32(9),=C'INVOICES='                                            
         EDIT  RUNINVS,(7,P+41),0,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         LA    R3,RAMTS                                                         
         BAS   RE,RTOTPRNT                                                      
*                                                                               
         CLI   OPENSW,C'T'                                                      
         BNE   RLEXIT                                                           
*                                                                               
*                                                                               
         CLI   BKSFTP,C'Y'       AM I DOING A BURGER KING SFTP?                 
         BE    RUNLBK                                                           
*                                                                               
         CLI   G7SW,1              GSTX?                                        
         BNE   *+8                                                              
         BRAS  RE,G7OUT            FINALLY PRODUCE RECORDS                      
*                                                                               
         CLI   DGSW,1              DIAGEO FILE                                  
         BNE   *+12                                                             
         MVI   DGTYPE,C'3'                                                      
         BRAS  RE,DGPROC           TOTAL RECORD                                 
*                                                                               
         CLI   SMSW,1              SMUCKERS FILE?                               
         BNE   *+16                NO                                           
         CLI   SMHDSW,C'Y'         HAVE AT LEAST ONE INVOICE?                   
         BNE   *+8                 NO - WE DON'T HAVE ANY DATA                  
         BRAS  RE,SMIPROC          YES - CLOSE CXML TAGS                        
*                                                                               
         CLI   SCSW,1              SC JOHNSON                                   
         BNE   RUNL2                                                            
         BRAS  RE,SCOUT            FINALLY PRODUCE RECORDS                      
         CLI   SCSFTP,C'Y'       AM I DOING A SC JOHNSON SFTP?                  
         BE    RUNLBK              MOSTLY LIKE BURGER KING                      
         B     RLEXIT                                                           
*                                                                               
RUNL2    CLI   MZSW,1              MAZDA INTERFACE?                             
         BNE   RUNL5                                                            
         BRAS  RE,MZOUT            FINALLY PRODUCE RECORDS                      
         CLI   MZSFTP,C'Y'       AM I DOING A MAZDA SFTP?                       
         BE    RUNLBK              MOSTLY LIKE BURGER KING                      
         B     RLEXIT                                                           
*                                                                               
RUNL5    CLI   MCCSW,1             MCCANN TYPE C                                
         BNE   RUNL7                                                            
         BRAS  RE,MCCOUT            FINALLY PRODUCE RECORDS                     
         B     RUNL20                                                           
*                                                                               
RUNL7    CLI   SBSW,1             STARBUCK INTERFACE                            
         BNE   RUNL20                                                           
         BRAS  RE,SBOUT           PRODUCE BATCH RECORD                          
         B     RUNL20                                                           
*                                                                               
RUNL20   DS    0H                                                               
         LA    R3,NEDYNDSN         NETPAK                                       
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+8                                                              
         LA    R3,SPDYNDSN         OR SPOT                                      
*                                                                               
         CLOSE (SBITAPE)                                                        
*                                                                               
         DEQ   (RMAJORNM,(3),DSNLENQ,SYSTEM)                                    
*                                                                               
*                                                                               
         CLI   DGERRSW,C'Y'          WERE DIAGEO ERRORS ECOUNTERED?             
         BNE   RLEXIT                                                           
         MVC   P(34),=C'*** WARNING-ERRORS ENCOUNTERED ***'                     
         MVC   P2(54),=C'*** PRODUCTS WITH MISSING UCOMM DATA NOT PROCEX        
               SSED ***'                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     RLEXIT                                                           
         EJECT                                                                  
RUNLBK   DS    0H      HERE IF DOING BURGER KING SFTP (MINDSHARE)               
*                                                                               
         CLOSE (SBITAPE)                                                        
*                                                                               
         CLI   TESTMQ,C'N'       SEE IF SUPRESSING MQ NOTIFICATION              
         BE    RUNLBK10                                                         
* SEND MQ MESSAGE WITH FILE NAME                                                
         LA    R5,ELEM                                                          
         USING MQMSGD,R5                                                        
         MVI   ELEM,C' '                                                        
         MVC   ELEM+1(MQMSGLNQ-1),ELEM                                          
         LA    R1,MQMAPNM                                                       
         MVC   MQFILE(34),14(R1)   BIL.SYS.AGID.DYYMMDD.THHMMSS                 
*                                  SYS=SYSTEM,AGID= 4 CHARACTER AGY ID          
*                          14(R1) TO GET PAST SPTPDISK.PROD (OR TEST)           
         MVC   MQDATE(6),28(R1)    YYMMDD OF FILE NAME                          
         MVC   MQTIME(6),36(R1)    HHMMSS OF FILE NAME                          
*                                                                               
         BRAS  RE,MQOPEN                                                        
*                                                                               
         MVC   MQHID,=CL6'DANOT1'                                               
         MVC   MQSYS,MQFILE+4  SYSTEM (+4 PAST BIL.)                            
         CLI   BKSFTP,C'Y'           BURGER KING?                               
         BNE   RUNLBK5                                                          
*                                                                               
         MVC   MQAGYID,=C'H7NY'      AGENCY ID-MINDSHARE  BURGER KIING          
*                                                                               
         CLI   NETPAKSW,C'Y'         SEE IF NETPAK                              
         BE    *+10                                                             
         MVC   MQAGYID,=C'H7SY'      H7SY FOR SPOT FILE                         
         B     RUNLBK7                                                          
*                                                                               
RUNLBK5  DS    0H                                                               
*                                                                               
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   MQAGYID,MCAGYCOD-MCEXTRA(R1)                                     
         DROP  RF                                                               
         CLI   SCSFTP,C'Y'          SC JOHNSON?                                 
         BNE   RUNLBK5M                                                         
         MVC   MQQUAL(10),=C'SC BILLING'                                        
         B     RUNLBK8                                                          
*                                                                               
RUNLBK5M CLI   MZSFTP,C'Y'          MAZDA?                                      
         BNE   RUNLBK5X                                                         
         MVC   MQQUAL(10),=C'MZ BILLING'                                        
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         L     R1,MCAEXTRA                                                      
         MVC   MQAGYID,MCAGYCOD-MCEXTRA(R1)                                     
         DROP  RF                                                               
         B     RUNLBK8                                                          
*                                                                               
*****    MVC   MQAGYID,=C'H7MZ'                                                 
*****    B     RUNLBK8                                                          
*                                                                               
RUNLBK5X DC    H'0'          UNKNOW H7 SFTP                                     
*                                                                               
RUNLBK7  MVC   MQQUAL(7),=C'BILLING'                                            
*                                                                               
RUNLBK8  GOTO1 AMQRPT,DMCB,(0,=C'PUT'),ELEM,MQMSGLNQ,0                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    RUNLBK10                                                         
         DCHO                                                                   
*                                                                               
RUNLBK10 MVC   P+1(49),MQMAPNM       WHOLE FILE NAME                            
         BRAS  RE,PRNT                                                          
         B     RLEXIT                                                           
*                                                                               
         DROP  R5                                                               
RLEXIT   XIT1                                                                   
RMAJORNM  DC    C'SBTTAPE '         MAJOR RESOURCE NAME FOR ENQ                 
         EJECT                                                                  
RTOTPRNT NTR1                                                                   
         SPACE 2                                                                
         ST    R3,ATOTS                                                         
         L     R4,ATOTS                                                         
         GOTO1 ATPFMT                                                           
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MQOPEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
*                                                                               
* IF WE'RE RUNNING A TEST, SEND TO TEST MQ BROKER                               
         CLI   TESTMQ,C'T'         IS THIS A MQ TEST RUN                        
         BNE   *+8                  NO                                          
         OI    DMCB+8,X'01'         YES -PUT TO TEST MQ BROKER                  
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'MEDIACOMSFTP****'),,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DCHO                                                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
TPFMT    NMOD1 0,TPFMT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         LA    R7,P                                                             
         USING BLINED,R7                                                        
         USING AMOUNTSD,R4                                                      
*                                                                               
         CLI   CNTRY,C'C'          IF CANADA                                    
         BNE   TPF02                                                            
*                                                                               
         ZAP   DUB(6),AMTACT       ACTUAL                                       
         AP    DUB(6),AMTGST       PLUS GST                                     
         AP    DUB(6),AMTPST       PLUS PST                                     
         LA    R1,DUB                                                           
         LA    R5,BLACTUAL                                                      
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTNET           NET                                          
         LA    R5,BLNET                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGRS           GROSS                                        
         LA    R5,BLGROSS                                                       
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTAC            ACTUAL COMMISSION                            
         LA    R5,BLAC                                                          
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTGST           DO GST                                       
         LA    R5,BLGST                                                         
         BAS   RE,TPEDT                                                         
*                                                                               
         LA    R1,AMTPST           AND PST                                      
         ZAP   DUB(6),AMTPST                                                    
         BZ    TPF04                                                            
         LA    R1,DUB                                                           
         LA    R5,BLPST                                                         
         BAS   RE,TPEDT                                                         
         B     TPF04                                                            
*                                                                               
TPF02    DS    0H                  NON-CANADIAN GETS WIDER COLUMNS              
         EDIT  AMTGRS,BLGRSWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTACT,BLACTWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTNET,BLNETWID,2,COMMAS=YES,MINUS=YES                           
         EDIT  AMTAC,BLACWIDE,2,COMMAS=YES,MINUS=YES                            
         DROP  R4                                                               
*                                                                               
TPF04    DS    0H                                                               
         BRAS  RE,PRNT                                                          
         XIT1                                                                   
         SPACE 3                                                                
TPEDT    DS    0H                                                               
         EDIT  (P6,0(R1)),(15,WORK),2,COMMAS=YES,MINUS=YES                      
         LR    RF,R5                                                            
         SH    RF,=H'2'                                                         
         CLI   WORK,C' '                                                        
         BNE   TPEDT6                                                           
         CLI   WORK+1,C' '                                                      
         BE    *+12                                                             
         CLI   1(RF),C' '                                                       
         BNE   TPEDT6                                                           
*                                                                               
         OC    1(15,RF),WORK                                                    
         BR    RE                                                               
*                                                                               
TPEDT6   DS    0H                                                               
         MVC   133(15,RF),WORK                                                  
         BR    RE                                                               
         LTORG                                                                  
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
*        TRACY LOCKE SPECIAL                                                    
TLPROC   NMOD1 0,TLPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         TM    BILSTAT,BSTCMONQ                                                 
         BZ    *+8                                                              
         MVI   TPREC+58,C'2'       COMMISSION ONLY                              
         DROP  R7                                                               
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        DDB PROC - SPECIAL CLIENT/PRODUCT CODE FOR SOME OFFICES                
DBPROC   NMOD1 0,DBPROC                                                         
         LA    RC,SPACEND                                                       
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C'L'                                                       
         BE    DBP04                                                            
         CLI   0(RF),C'7'                                                       
         BE    DBP04                                                            
         CLI   0(RF),C'8'                                                       
         BNE   DBP06                                                            
*                                                                               
DBP04    DS    0H                  CLIENT/PRODUCT                               
         MVC   TPREC+6(6),SPACES                                                
         MVC   TPREC+6(3),CLT                                                   
         LA    RE,TPREC+9          CCCPPP, CCPPP_, OR CCPP__                    
         CLI   TPREC+8,C' '                                                     
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVC   0(3,RE),PRD                                                      
*                                                                               
DBP06    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        DONER-SCHUR  - SPECIAL OFFICE CODES                                    
DAPROC   NMOD1 0,DAPROC                                                         
         LA    RC,SPACEND                                                       
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),C'1'                                          
         BNE   *+14                                                             
         MVC   TPREC(8),=CL8'DETROIT'                                           
         B     DAX                                                              
         CLI   COFFICE-CLTHDR(RF),C'7'                                          
         BNE   *+14                                                             
         MVC   TPREC(8),=CL8'TORONTO'                                           
         B     DAX                                                              
         CLI   COFFICE-CLTHDR(RF),C'9'                                          
         BNE   *+14                                                             
         MVC   TPREC(8),=CL8'MONTREAL'                                          
         B     DAX                                                              
         MVC   TPREC(8),=CL8'OTHER'                                             
DAX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        M2TO AND WITO                                                          
M2PROC   NMOD1 0,M2PROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         LA    RE,SVTPREC          CLEAR SAVE AREA, MIGHT USE IT L8R            
         LHI   R1,L'SVTPREC-1                                                   
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC   TPREC+121(2),=C'01'    USED IF NOT ONE OF THE ID'S BELOW         
*                                                                               
         CLC   RCORIGID,=X'39E6'      UMTOA - # 14822                           
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'07'    CHANGE COMPANY CODE                       
         CLC   RCORIGID,=X'3E71'      INMETO  # 15985                           
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'05'    CHANGE COMPANY CODE                       
         CLC   RCORIGID,=X'3E66'      ORTO    # 15974                           
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'04'    CHANGE COMPANY CODE                       
         CLC   RCORIGID,=X'3FB6'      CITO    # 16310                           
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'09'    CHANGE COMPANY CODE                       
         CLC   RCORIGID,=X'3FB7'      RMTO    # 16311                           
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'08'    CHANGE COMPANY CODE                       
*                                                                               
         CLC   QAGY,=C'WT'       WITO?                                          
         BNE   *+10                                                             
         MVC   TPREC+121(2),=C'05'    CHANGE COMPANY CODE                       
*                                                                               
         L     RF,ADCLT                                                         
         MVC   TPREC+123(2),CACCOFC-CLTHDR(RF)                                  
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         GOTO1 DATCON,DMCB,BQDATE,(20,WORK)         BILL DATE                   
         MVC   TPREC+24(4),WORK+4       MMDD                                    
         MVC   TPREC+28(4),WORK         YYYY                                    
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK)   INVOICE DATE                
         MVC   TPREC+84(4),WORK+4       MMDD                                    
         MVC   TPREC+88(4),WORK         YYYY                                    
         MVC   TPREC+92(2),TPREC+24     MM          ACCOUNTING PERIOD           
         MVC   TPREC+94(4),TPREC+28     YYYY                                    
*                                                                               
         TM    BILSTAT,BSTTAORQ    IF TRUE AOR BILL                             
         BZ    M2PR6                                                            
         BRAS  RE,GETAOR                                                        
         MVC   TPREC+11(3),=C'AOR'                                              
         MVC   TPREC+72(12),WORK  AOR NAME - ONLY ROOM FOR 12 CHARS             
*                                                                               
M2PR6    DS    0H                                                               
*                                                                               
*                                                                               
         CP    MYHST,=P'0'              ANY HST?                                
         BNE   M2HST                                                            
         CP    MYPST,=P'0'              QST?                                    
         BNE   M2QST                                                            
         CP    MYGST,=P'0'              ANY GST?                                
         BNE   M2GST                                                            
*                                                                               
*        NO TAXES PRESENT - JUST SEND $0 TAXES                                  
*                                                                               
         MVI   TPREC+101,C'+'                                                   
         MVC   TPREC+109(2),=C'00'     NO QST EITHER                            
         MVI   TPREC+111,C'+'                                                   
         MVC   TPREC+119(2),=C'00'     NO GST EITHER                            
         B     M2P20                                                            
*                                                                               
M2GST    DS    0H                      ONLY GST                                 
         MVC   TPREC+98(3),=C'GST'                                              
         MVI   TPREC+101,C'+'                                                   
         MVC   TPREC+109(2),=C'00'     NO QST                                   
         CP    MYGST,=P'0'                                                      
         BL    *+8                                                              
         MVI   TPREC+111,C'+'                                                   
         EDIT  MYGST,(9,TPREC+112),ZERO=NOBLANK                                 
         CLC   TPREC+119(2),=C' 0'                                              
         BNE   *+8                                                              
         MVI   TPREC+119,C'0'          ALTER 0 TO 00                            
         B     M2P20                                                            
*                                                                               
M2QST    DS    0H               QST PRESENT WITHOUT HST                         
         MVC   TPREC+98(3),=C'QST'                                              
         CP    MYPST,=P'0'                                                      
         BL    *+8                                                              
         MVI   TPREC+101,C'+'                                                   
         EDIT  MYPST,(9,TPREC+102),ZERO=NOBLANK                                 
         CLC   TPREC+109(2),=C' 0'                                              
         BNE   *+8                                                              
         MVI   TPREC+109,C'0'          ALTER 0 TO 00                            
         CP    MYGST,=P'0'                                                      
         BL    *+8                                                              
         MVI   TPREC+111,C'+'                                                   
         EDIT  MYGST,(9,TPREC+112),ZERO=NOBLANK                                 
         CLC   TPREC+119(2),=C' 0'                                              
         BNE   *+8                                                              
         MVI   TPREC+119,C'0'          ALTER 0 TO 00                            
         B     M2P20                                                            
*                                                                               
M2HST    DS    0H               HST PRESENT                                     
         SP    MYPST,MYHST                                                      
         CP    MYPST,=P'0'      ANY QST?                                        
         BNE   M2QST            PROCESS QST (WITH GST) FIRST                    
         CP    MYGST,=P'0'      GST PRESENT?                                    
         BNE   M2GST            PROCESS GST                                     
*                               IF NEITHER SEND $0 FOR THOSE TAXES              
*                               SEEMS THEY WANT WITHOUT CODES                   
         MVI   TPREC+101,C'+'                                                   
         MVC   TPREC+102(9),=C'       00'    NO QST EITHER                      
         MVI   TPREC+111,C'+'                                                   
         MVC   TPREC+112(9),=C'       00'    NO GST EITHER                      
*                                                                               
*                                                                               
M2P20    DS    0H                                                               
         CP    TPREC+33(6),=P'0'          ACTUAL                                
         BL    *+8                                                              
         MVI   TPREC+32,C'+'                                                    
         EDIT  (P6,TPREC+33),(9,TPREC+33),ZERO=NOBLANK                          
         CLC   =C' 0',TPREC+40                                                  
         BNE   *+8                                                              
         MVI   TPREC+40,C'0'              ALTER 0 TO 00                         
*                                                                               
         CP    TPREC+43(6),=P'0'          NET                                   
         BL    *+8                                                              
         MVI   TPREC+42,C'+'                                                    
         EDIT  (P6,TPREC+43),(9,TPREC+43),ZERO=NOBLANK                          
         CLC   =C' 0',TPREC+50                                                  
         BNE   *+8                                                              
         MVI   TPREC+50,C'0'              ALTER 0 TO 00                         
*                                                                               
         CP    TPREC+53(6),=P'0'          AGY COMM                              
         BL    *+8                                                              
         MVI   TPREC+52,C'+'                                                    
         EDIT  (P6,TPREC+53),(9,TPREC+53),ZERO=NOBLANK                          
         CLC   =C' 0',TPREC+60                                                  
         BNE   *+8                                                              
         MVI   TPREC+60,C'0'              ALTER 0 TO 00                         
*                                                                               
         CLI   RETAIL,C'Y'        IF RETAIL ACC, PAD WITH ZEROES                
         BNE   M2P30                                                            
*                                                                               
         MVC   WORK(12),TPREC+129        STORE RETAIL ACCT                      
         MVC   TPREC+129(12),SPACES      CLEAR RETAIL ACCT                      
         LA    R2,TPREC+140              LAST BYTE OF RET ACC                   
         LA    R3,WORK+11                                                       
         LA    R4,12         FOR BCT                                            
M2P20B   CLI   0(R3),C' '    ANY CHARACTER THERE?                               
         BH    M2P20C                                                           
         BCTR  R3,0           BACK UP R3                                        
         BCT   R4,M2P20B                                                        
         B     M2P20X                                                           
*                                                                               
M2P20C   MVC   0(1,R2),0(R3)                                                    
         BCTR  R2,0          BACK UP BOTH R2 AND R3                             
         BCTR  R3,0                                                             
         BCT   R4,M2P20B                                                        
         B     M2P20X                                                           
*                                                                               
M2P20X   OC    TPREC+129(12),=12C'0'                                            
*                                                                               
M2P30    CP    MYHST,=P'0'         ANY HST PRESENT ?                            
         BE    M2PRX                                                            
         MVC   SVTPREC(141),TPREC  COPY RECORD                                  
*                                                                               
*        CLEAR BILL AMOUNTS AND SEND HST IN ANOTHER RECORD                      
*                                                                               
         LA    RE,SVTPREC                                                       
         USING SVTPREC,RE                                                       
*                                                                               
         MVI   SVTPREC+32,C'+'                                                  
         MVC   SVTPREC+33(9),=C'       00'                                      
         MVI   SVTPREC+42,C'+'                                                  
         MVC   SVTPREC+43(9),=C'       00'                                      
         MVI   SVTPREC+52,C'+'                                                  
         MVC   SVTPREC+53(9),=C'       00'                                      
         MVC   SVTPREC+98(3),=C'HST'                                            
         MVI   SVTPREC+101,C'+'                                                 
         MVC   SVTPREC+102(9),=C'       00'                                     
*                                                                               
         MVI   SVTPREC+111,C' '       CLEAR POSITIVE SIGN                       
*                                                                               
         CP    MYHST,=P'0'                                                      
         BL    *+8                                                              
         MVI   SVTPREC+111,C'+'                                                 
         EDIT  MYHST,(9,SVTPREC+112),ZERO=NOBLANK                               
         CLC   SVTPREC+119(2),=C' 0'                                            
         BNE   *+8                                                              
         MVI   SVTPREC+119,C'0'     ALTER 0 TO 00                               
*                                                                               
         DROP  RE                                                               
         DROP  R7                                                               
M2PRX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
M2GPROC  NMOD1 0,M2GPROC                                                        
         LA    RC,SPACEND                                                       
*                                                                               
*        MVC   TPREC+0(2),DINVFULL (MEDIA CHARACTERS)                           
*                                                                               
***      LHI   R0,L'DINVFULL                                                    
***      LA    R1,DINVFULL                                                      
***      LA    R2,TPREC                                                         
***                                                                             
***      CLI   0(R1),C'-'                                                       
***      BE    *+14                                                             
***      MVC   0(1,R2),0(R1)                                                    
***      LA    R2,1(R2)                                                         
***      LA    R1,1(R1)                                                         
***      BCT   R0,*-22                                                          
*                                                                               
         MVC   TPREC(2),DINVMED      MEDIA PART                                 
         CLI   DINVMED,C' '   SEE IF BEGINS WITH BLANK                          
         BH    M2G2A                                                            
         MVC   TPREC(1),DINVMED+1                                               
*                                                                               
M2G2A    LA    R1,TPREC+1        MEDIA CODE COULD BE 1 OR 2 CHARACTERS          
         CLI   TPREC+1,C' '      SOMETHING THERE?                               
         BNH   *+8                                                              
         LA    R1,TPREC+2                                                       
*                                                                               
         MVC   0(6,R1),DINVNO    SHORT INVOICE #   YMNNNN                       
*                                                                               
M2G4     DS    0H                                                               
         L     RF,ADCLT                                                         
         LA    R4,M2BRTAB                                                       
         CLC   QAGY,=C'M2'                                                      
         BE    M2G4C                                                            
         LA    R4,WGBRTAB                                                       
         CLC   QAGY,=C'G+'      WINGLATINO                                      
         BE    M2G4C                                                            
         LA    R4,H7BRTAB       MSNYA BRANCH TAB                                
         CLC   QAGY,=C'H7'                                                      
         BE    M2G4C                                                            
         LA    R4,FRBRTAB       FDMJW BRANCH TAB                                
         CLC   QAGY,=C'FR'                                                      
         BE    *+6                                                              
         DC    H'0'             UNKNOWN AGENCY                                  
*                                                                               
M2G4C    CLI   0(R4),X'FF'      END OF TABLE                                    
         BNE   *+6                                                              
         DC    H'0'       UNKNOWN OFFICE                                        
         CLC   0(1,R4),COFFICE-CLTHDR(RF)   MIGHT BE HEX CODE                   
*                                           CHARACTER FOR G+?                   
         BE    M2G4E                                                            
         LA    R4,7(R4)                                                         
         B     M2G4C                                                            
*                                                                               
M2G4E    MVC   TPREC+18(4),3(R4)    SET RANCH FROM OFFICE TABLE                 
*                                                                               
         L     RF,ADPRD                                                         
         LA    RF,PUSER1-PRDHDR(RF)  USE PUSER1 AS CLT/PRD CODE                 
         CLI   0(RF),C' '            IF PRESENT                                 
         BNH   M2GP2                                                            
         MVC   TPREC+22(6),0(RF)                                                
         OC    TPREC+22(6),SPACES   UPPERCASE                                   
*                                                                               
M2GP2    DS    0H                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         CLI   NETPAKSW,C'Y'      NETPAK                                        
         BNE   M2GP10                                                           
         MVC   TPREC+42(2),=C'02'                                               
         CLI   BLMED,C' '         NETWORK                                       
         BE    M2GP10                                                           
         CLI   BLMED,C'N'         NETWORK                                       
         BE    M2GP10                                                           
         MVC   TPREC+42(2),=C'04'                                               
         CLI   BLMED,C'C'         CABLE                                         
         BE    M2GP10                                                           
         MVC   TPREC+42(2),=C'06'                                               
         CLI   BLMED,C'S'         SYNDICATION                                   
         BE    M2GP10                                                           
         MVC   TPREC+42(2),=C'08'                                               
         CLI   BLMED,C'D'         NETPAK RADIO                                  
         BE    M2GP10                                                           
         MVC   TPREC+42(2),=C'10'    OTHER (UNWIRED?)                           
         CLI   BLMED,C'O'                                                       
         BE    M2GP10                                                           
         DC    H'0'             UNKNOWN SUB-MEDIA                               
*                                                                               
M2GP10   DS    0H                                                               
         GOTO1 DATCON,DMCB,BQDATE,(20,TPREC+28)   INVOICE DATE                  
*                                                                               
         MVC   WORK(2),BKEYYSRV               MONTH OF SERVICE CCYYMM           
         MVI   WORK+2,X'01'     DAY TO 1                                        
         GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+3)                                 
         MVC   TPREC+36(6),WORK+3                                               
*                                                                               
M2BILL   USING AMOUNTSD,BAMTS                                                   
*                                                                               
         TM    BILSTAT,BSTTAORQ    AOR BILL?                                    
         BNZ   M2GPAOR                                                          
         UNPK  TPREC+44(10),M2BILL.AMTNET                                       
         OI    TPREC+53,X'F0'                                                   
         MVI   TPREC+54,C'+'                                                    
         CP    M2BILL.AMTNET,=P'0'                                              
         BNL   *+8                                                              
         MVI   TPREC+54,C'-'                                                    
*                                                                               
         MVC   TPREC+55(11),=C'0000000000+'     CD?                             
*                                                                               
         ZAP   DUB,M2BILL.AMTACT                                                
         SP    DUB,M2BILL.AMTNET                                                
*                                                                               
         UNPK  TPREC+66(10),DUB      ACTUAL-NET = COMMISSION                    
         OI    TPREC+75,X'F0'                                                   
         MVI   TPREC+76,C'+'                                                    
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   TPREC+76,C'-'                                                    
*                                                                               
         MVC   TPREC+77(11),=C'0000000000+'     NO AOR                          
*                                                                               
         B     M2GP30                                                           
*                                                                               
M2GPAOR  DS    0H                              AOR INVOICES                     
         MVC   TPREC+44(11),=C'0000000000+'                                     
         MVC   TPREC+55(11),=C'0000000000+'                                     
         MVC   TPREC+66(11),=C'0000000000+'                                     
*                                                                               
         UNPK  TPREC+77(10),M2BILL.AMTACT      ONLY SEND ACTUAL                 
         OI    TPREC+86,X'F0'                                                   
         MVI   TPREC+87,C'+'                                                    
         CP    M2BILL.AMTACT,=P'0'                                              
         BNL   *+8                                                              
         MVI   TPREC+87,C'-'                                                    
         MVC   TPREC+92(2),=C'IC'    STANDS FOR INTERCOMPANY                    
*                                                                               
         DROP  M2BILL                                                           
         DROP  R7                                                               
*                                                                               
M2GP30   MVI   TPREC+145,C'0'           LAST BYTE                               
*                                                                               
M2GROCX  XIT1                                                                   
*                                                                               
*        INTERNAL HEX CODE, 2 CHAR, GFMS BRANCH CODE                            
*                                                                               
M2BRTAB  DC    X'43',C'GG',C'0001'                                              
         DC    X'45',C'G1',C'0013'                                              
         DC    X'46',C'JB',C'0008'                                              
         DC    X'44',C'WL',C'0053'                                              
         DC    X'4A',C'G5',C'0045'                                              
         DC    X'49',C'G2',C'0138'                                              
         DC    X'4C',C'G3',C'0017'                                              
         DC    X'FFFF'        END OF TABLE                                      
*                                                                               
*        INTERNAL HEX CODE, 2 CHAR, FDMJW BRANCH CODE                           
*                                                                               
FRBRTAB  DC    X'45',C'X3',C'0001'                                              
         DC    X'44',C'X2',C'0045'                                              
         DC    X'55',C'RX',C'0001'                                              
         DC    X'FFFF'        END OF TABLE                                      
*                                                                               
*        INTERNAL HEX CODE, 2 CHAR, MSNYA BRANCH CODE                           
*                                                                               
H7BRTAB  DC    X'42',C'X3',C'0001'                                              
         DC    X'44',C'X2',C'0045'                                              
         DC    X'52',C'RX',C'0001'                                              
         DC    X'6F',C'RM',C'0001'                                              
         DC    X'FFFF'        END OF TABLE                                      
*                                                                               
*        OFFICE CODE, 2 CHAR, GFMS BRANCH CODE                                  
*                                                                               
WGBRTAB  DC    C'M',C'M ',C'0152'                                               
         DC    C'N',C'N ',C'0053'                                               
         DC    C'L',C'L ',C'0057'                                               
         DC    C'S',C'S ',C'0045'                                               
         DC    X'FFFF'        END OF TABLE                                      
         LTORG                                                                  
         EJECT                                                                  
OOPROC   NMOD1 0,OOPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         GOTO1 DATCON,DMCB,BQDATE,(20,WORK)         BILL DATE                   
         MVC   TPREC+16(4),WORK+4       MMDD                                    
         MVC   TPREC+20(4),WORK         YYYY                                    
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,WORK)   INVOICE DATE                
         MVC   TPREC+26(4),WORK+4       MMDD                                    
         MVC   TPREC+30(4),WORK         YYYY                                    
         CLC   QAGY,=C'OU'        CANADIAN - OMDTO?                             
         BNE   OOPROCX                                                          
         MVC   TPREC+36(5),=C'PRMSC'   SPOT                                     
         CLI   QMED,C'N'               SEE IF CANADIAN NETWORK                  
         BNE   OOPROCX                                                          
         MVC   TPREC+36(5),=C'PRMNC'                                            
*                                                                               
OOPROCX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        PHDDE                                                                  
BNCPROC  NMOD1 0,BNPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,ADCLT                                                         
         MVC   TPREC(2),CACCOFC-CLTHDR(RF)                                      
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         GOTO1 DATCON,DMCB,BQDATE,(5,TPREC+101)                                 
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(5,TPREC+109)                           
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(6,TPREC+117)                           
         EDIT  (P6,TPREC+123),(12,TPREC+123),2,MINUS=YES                        
         EDIT  (P6,TPREC+135),(12,TPREC+135),2,MINUS=YES                        
         EDIT  (P6,TPREC+151),(12,TPREC+151),2,MINUS=YES                        
         EDIT  (P6,TPREC+163),(12,TPREC+163),2,MINUS=YES                        
         TM    BILSTAT-BILLREC(R7),BSTTAORQ     IS IT AOR BILL ?                
         BZ    BNCPRX                                                           
         EDIT  BACTP,(12,TPREC+271),2,MINUS=YES                                 
*                                                                               
         DROP  R7                                                               
BNCPRX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        BBDO CHRYSLER SPECIAL - USER FIELDS BY COST TYPE                       
BDCPROC  NMOD1 0,BDCPR                                                          
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   TPREC+0(2),=C'PC'   SPECIAL AGENCY CODE                          
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   TPREC+2,C'N'                                                     
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         L     RF,ADEST                                                         
*                                                                               
         MVC   TPREC+55(20),SPACES                                              
         MVC   TPREC+55(11),EUSER1-ESTHDR(RF)                                   
         OC    TPREC+55(11),SPACES UPPERCASE                                    
*                                                                               
         MVC   TPREC+75(20),SPACES                                              
         MVC   TPREC+75(15),EUSER2-ESTHDR(RF)                                   
         OC    TPREC+75(15),SPACES UPPERCASE                                    
*                                                                               
         CLI   NETPAKSW,C'Y'       DONE IF NOT NETPAK                           
         BNE   BDCX                                                             
*                                                                               
         MVC   TPREC+75(20),SPACES                                              
         MVC   TPREC+75(15),EUSER1+11-ESTHDR(RF)                                
         OC    TPREC+75(15),SPACES UPPERCASE                                    
*                                                                               
         CLI   BILCTYP,C'T'        TIME CHARGES                                 
         BE    BDCX                                                             
*                                                                               
         MVC   TPREC+55(20),SPACES                                              
         MVC   TPREC+55(16),EUSER2-ESTHDR(RF)                                   
         OC    TPREC+55(16),SPACES UPPERCASE                                    
*        **NOTE- ALL OTHERS TREATED LIKE INTEGRATION                            
*                                                                               
BDCX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        STARBUCK SPECIAL CODE                                                  
*                                                                               
SBPROC   NMOD1 0,SBPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         MVC   WORK(11),SPACES                                                  
         LA    R1,WORK                                                          
         LA    R2,DINVFULL                                                      
         LA    R3,10                                                            
SBIP5A   CLI   0(R2),C' '              ELIMINATE SPACES                         
         BE    SBIP5B                                                           
         CLI   0(R2),C'-'              ELIMINATE DASHES                         
         BNE   SBIP5C                                                           
SBIP5B   LA    R2,1(R2)                                                         
         BCT   R3,SBIP5A                                                        
         MVI   0(R1),CARET             DELIMITER                                
         B     SBIP5X                                                           
*                                                                               
SBIP5C   MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         B     SBIP5B                                                           
*                                                                               
SBIP5X   DS    0H                                                               
         MVI   TPREC+1,CARET          DELIMITER                                 
         MVI   TPREC+12,CARET          DELIMITER                                
         MVI   TPREC+22,CARET          DELIMITER                                
*                                                                               
         MVC   TPREC+23(9),WORK       INV. NUMBER NO DASHES + DELIMITER         
         LA    R2,TPREC+31                                                      
         CLI   0(R2),CARET                                                      
         BE    SBIP7                                                            
         LA    R2,TPREC+30                                                      
*                                                                               
SBIP7    GOTO1 DATCON,DMCB,BQDATE,(20,1(R2))  INVOICE DATE YYYYMMDD             
         MVI   9(R2),CARET         DELIMITER                                    
         LA    R2,10(R2)                                                        
*                                                                               
         EDIT  BACTP,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT               
         AR    R2,R0                                                            
         MVI   0(R2),CARET             DELIMITER                                
         LA    R2,1(R2)                                                         
*                                                                               
*        MEDIA GOES NEXT                                                        
*                                                                               
         CLI   NETPAKSW,C'Y'                                                    
         BNE   SBIP8                                                            
         MVC   0(08,R2),=C'CABLE TV'                                            
         MVI   08(R2),CARET         DELIMITER                                   
         CLC   DINVFULL(2),=C'NC'                                               
         BE    SBPROCX                                                          
         MVC   0(10,R2),=C'NETWORK TV'                                          
         MVI   10(R2),CARET         DELIMITER                                   
         CLC   DINVFULL(2),=C'NT'                                               
         BE    SBPROCX                                                          
         MVC   0(11,R2),=C'SYNDICATION'                                         
         MVI   11(R2),CARET         DELIMITER                                   
         CLC   DINVFULL(2),=C'N-'                                               
         BE    SBPROCX                                                          
         CLC   DINVFULL(2),=C'N '                                               
         BE    SBPROCX                                                          
         DC    H'0'        UNKNOWN OTHER SUBMEDIA                               
***      MVC   0(13,R2),=C'NETWORK OTHER'                                       
***      MVI   13(R2),CARET         DELIMITER                                   
***      B     SBPROCX              UNKOWN SUB-MEDIA                            
*                                                                               
SBIP8    MVC   0(07,R2),=C'SPOT TV'                                             
         MVI   07(R2),CARET         DELIMITER                                   
         CLI   QMED,C'T'                                                        
         BE    SBPROCX                                                          
         MVC   0(10,R2),=C'SPOT RADIO'   SPOT MEDIA                             
         MVI   10(R2),CARET         DELIMITER                                   
         CLI   QMED,C'R'                                                        
         BE    SBPROCX                                                          
         MVC   0(10,R2),=C'NTWK RADIO'   SPOT MEDIA                             
         MVI   10(R2),CARET         DELIMITER                                   
         CLI   QMED,C'X'                                                        
         BE    SBPROCX                                                          
         DC     H'0'       UNKNOWN MEDIA                                        
*                                                                               
SBPROCX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
                                                                                
***********************************************************************         
*   GSTX SPECIAL                                                                
***********************************************************************         
*                                                                               
G7PROC   NMOD1 0,G7PROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
G7LAR1   DS    0H                                                               
*                                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         MVC    G7MED,MEDIA                                                     
         MVC    G7CLI,CLT                                                       
         MVC    G7PRO,BKEYPRD                                                   
         MVC    G7EST(1),BKEYEST                                                
         MVC    G7INVMO,BKEYMBIL    BILLING Y/M BYTE                            
         MVC    G7INVN,BKEYINV                                                  
*                                                                               
         DROP   R7                                                              
*                                                                               
         MVC    G7REC,TPREC                                                     
*                                                                               
         L      R2,AOFG7T         ADDRESS OF G7TAB                              
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),GBINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    GBINVALS,1         RECORD INSERTED                              
         BE     G7TOXIT                                                         
         OC     GBINVALS+1(3),GBINVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L       RF,GBINVALS              ADDRESS OF FOUND RECORD               
         LA      RF,L'G7KEY(RF)           PAST KEY                              
*                                                                               
         CLC     128(6,RF),TPREC+128      CHECK IF START DATE                   
         BL      *+10                     NEEDS TO BE LOWERED                   
         MVC     128(6,RF),TPREC+128                                            
         CLC     134(6,RF),TPREC+134      CHECK IF END DATE                     
         BH      *+10                     NEEDS TO BE RAISED                    
         MVC     134(6,RF),TPREC+134                                            
*                                                                               
         AP      57(8,RF),TPREC+57(8)   ACTUAL                                  
         AP      68(8,RF),TPREC+68(8)   NET                                     
         AP      79(8,RF),TPREC+79(8)   CD                                      
         AP      88(8,RF),TPREC+88(8)   COMMISSION                              
         AP      99(8,RF),TPREC+99(8)   GROSS                                   
         AP      160(8,RF),TPREC+160(8)   SALES TAX                             
*                                                                               
G7TOXIT  DS    0H                                                               
         MVC   GBINVALS,AG7MED                                                  
         MVI   GBINVALS,1                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GSTX SPECIAL OUTPUT FROM A SPECIAL BINSRCH TABLE                            
***********************************************************************         
*                                                                               
G7OUT    NMOD1 0,G7OUT                                                          
         LA    RC,SPACEND                                                       
*                                                                               
*                      FINALLY PRODUCE RECORDS                                  
G7PRL    DS    0H                                                               
*                                                                               
*                                                                               
G7PRL5   DS    0H                                                               
         L     R2,G7RECNT          FOR BCT RECORD COUNT                         
         CH    R2,=H'0'            NO RECORDS                                   
         BE    G7ZERO                                                           
         L     R3,AOFG7T                                                        
         LA    R3,L'G7KEY(R3)      BUMP PAST PSUEDO KEY                         
G7AGN    MVC   MYDUB,57(R3)                                                     
*                                                                               
         LA    R5,57(R3)                                                        
         EDIT  (P8,MYDUB),(11,0(R5)),0,FILL=0                                   
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,68(R3)                                                     
         LA     R5,68(R3)                                                       
         EDIT  (P8,MYDUB),(11,0(R5)),0,FILL=0                                   
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,79(R3)                                                     
         LA    R5,79(R3)                                                        
         EDIT  (P8,MYDUB),(9,0(R5)),0,FILL=0                                    
         CP     MYDUB,=P'0'            SEE IF NEGATIVE                          
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,88(R3)                                                     
         LA    R5,88(R3)                                                        
         EDIT  (P8,MYDUB),(11,0(R5)),0,FILL=0                                   
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,99(R3)                                                     
         LA    R5,99(R3)                                                        
         EDIT  (P8,MYDUB),(11,0(R5)),0,FILL=0                                   
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,160(R3)                                                    
         LA    R5,160(R3)                                                       
         EDIT  (P8,MYDUB),(11,0(R5)),0,FILL=0                                   
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
*                                                                               
G7AGN5   DS    0H                                                               
         CLI   SVQOPT3,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    G7AGN7                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         PUT   (1),(3)                                                          
*                                                                               
G7AGN7   DS    0H                                                               
         CLI   SVQOPT3,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   G7AGNX                                                           
         CP    G7COUNT,=P'100'                                                  
         BH    G7AGNX                                                           
         AP    G7COUNT,=P'1'                                                    
*                                                                               
         MVC   P(13),=C'***OUTPUT HEX'                                          
         BRAS  RE,PRNT                                                          
         LR    R5,R3                                                            
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,60(R3)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,120(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,180(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,20,0                                          
         BRAS  RE,PRNT                                                          
*                                                                               
G7AGN9   MVC   P(13),=C'***OUTPUT CHAR'                                         
         BRAS  RE,PRNT                                                          
         MVC   P(100),0(R3)                                                     
         BRAS  RE,PRNT                                                          
         MVC   P(100),100(R3)                                                   
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
G7AGNX   LA    R3,212(R3)   TO NEXT RECORD IN TABLE                             
         BCT   R2,G7AGN                                                         
*                                                                               
G7ZERO   DS    0H                                                               
*                                                                               
         MVC   GBINVALS,AG7MED                                                  
         MVI   GBINVALS,1                                                       
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SCPROC   NMOD1 0,SCPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
SCLAR1   DS    0H                                                               
*                                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         MVC    SCMED,MEDIA                                                     
         MVC    SCCLI,CLT                                                       
         MVC    SCPRO,BKEYPRD                                                   
         MVC    SCEST(1),BKEYEST                                                
         MVC    SCINVMO,BKEYMBIL    BILLING Y/M BYTE                            
         MVC    SCINVN,BKEYINV                                                  
*                                                                               
         DROP   R7                                                              
*                                                                               
         MVC    SCREC,TPREC                                                     
*                                                                               
         L      R2,AOFSCT         ADDRESS OF SCTABLE                            
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),SCINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    SCINVALS,1         RECORD INSERTED                              
         BE     SCTOXIT                                                         
         OC     SCINVALS+1(3),SCINVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L       RF,SCINVALS              ADDRESS OF FOUND RECORD               
         LA      RF,L'SCKEY(RF)           PAST KEY                              
*                                                                               
***      CLC     128(6,RF),TPREC+128      CHECK IF START DATE                   
***      BL      *+10                     NEEDS TO BE LOWERED                   
***      MVC     128(6,RF),TPREC+128                                            
***      CLC     134(6,RF),TPREC+134      CHECK IF END DATE                     
***      BH      *+10                     NEEDS TO BE RAISED                    
***      MVC     134(6,RF),TPREC+134                                            
*                                                                               
         AP      91(8,RF),TPREC+91(8)   ACTUAL                                  
         AP      107(8,RF),TPREC+107(8) GROSS                                   
         AP      123(8,RF),TPREC+123(8) NET                                     
         AP      139(8,RF),TPREC+139(8)   SALES TAX                             
*                                                                               
SCTOXIT  DS    0H                                                               
         MVC   SCINVALS,ASCMED                                                  
         MVI   SCINVALS,1                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   SC JOHNSON SPECIAL OUTPUT FROM A SPECIAL BINSRCH TABLE                      
***********************************************************************         
*                                                                               
SCOUT    NMOD1 0,SCOUT                                                          
         LA    RC,SPACEND                                                       
*                                                                               
*                      FINALLY PRODUCE RECORDS                                  
SCPRL    DS    0H                                                               
*                                                                               
*                                                                               
SCPRL5   DS    0H                                                               
         ZAP   SCCOUNT,=P'0'                                                    
         MVI   SCRECSW,0                                                        
         L     R2,SCRECNT          FOR BCT RECORD COUNT                         
         CH    R2,=H'0'            NO RECORDS                                   
         BE    SCZERO                                                           
         ZAP   SCTOTAL,=P'0'       TOTAL OF INVOICES                            
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         XC    WORK,WORK           READ B3 PROFILE                              
         LA    RE,WORK                                                          
         USING PROFKD,RE                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B3'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                                
         DROP  RE                                                               
         MVC   SPOTPROF+2(1),WORK+20     REPLACE CALENDAR VALUES                
         MVC   SPOTPROF+6(3),WORK+21                                            
*                                                                               
*                                                                               
         MVC   WORK(4),TODAY        GET MOS OF CREATION DATE (TODAY)            
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,40  ADD MORE THEN A MONTH TO ...          
*                                     ... BE SURE I GOT INTO NXT PERIOD         
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK,-1     SUBTRACT 1 YEAR               
*                                                                               
         CLC   WORK+2(2),=C'01'    IF MONTH IS JAN, GO TO FEB                   
         BNE   *+10                                                             
         MVC   WORK+2(2),=C'02'    DOESN'T ALWAYS GET RIGHT YEAR                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,SPOTPROF+2                                                  
         GOTO1 MOBILE,DMCB,(20,WORK),((R0),ADBUY)                               
*                                                                               
         L     R3,ADBUY            LIST OF PERIODS                              
         SR    RE,RE               CHECK IF STARTING FROM JAN                   
         SR    RF,RF                                                            
         ICM   RF,12,0(R3)         GET THE PERIOD IN HIGH NIBBLE                
         SLDL  RE,7                PUT YEAR IN RE                               
         SR    RE,RE               CLEAR IT                                     
         SLDL  RE,4                NOW GET MONTH                                
         CHI   RE,1                IS IT JAN MOS ?                              
         BNE   *+8                 TRY TO MAKE SURE THERE'S ONLY ONE            
         LA    R3,4(R3)            PERIOD OF NEW YEAR                           
*                                  SINCE JAN CAN BE BEGINNING OF                
*                                  NEW YEAR, GET NEXT PER WHICH                 
SCPRL5B  DS    0H                  FIND FIRST PERIOD OF A NEW YEAR              
         BAS   RE,SCKNEWYR         NEW YEAR?                                    
         BE    *+12                YES                                          
         LA    R3,4(R3)                                                         
         B     SCPRL5B                                                          
*                                  GO THROUGH LIST OF PERIODS                   
*        FIND THE PERIOD THAT CONTAINS TODAY                                    
*                                                                               
SCPRL5D  DS    0H                                                               
         CLC   0(2,R3),TODAYC                                                   
         BL    SCPRL5F                                                          
         BE    SCPRL5H            USE THIS PERIOD                               
         DC    H'0'               COULD NOT FIND THE PERIOD                     
*                                                                               
SCPRL5F  CLC   2(2,R3),TODAYC                                                   
         BNL   SCPRL5H             USE THIS PERIOD                              
         LA    R3,4(R3)                                                         
SCPRL5G  CLI   0(R3),X'FF'                                                      
         BNE   SCPRL5D                                                          
         DC    H'0'                SHOULD NEVER REACH EOL                       
*                                                                               
SCPRL5H  GOTO1 DATCON,DMCB,(2,0(R3)),(20,TPREC+38)                              
         GOTO1 DATCON,DMCB,(2,2(R3)),(20,TPREC+46)                              
*                                                                               
*                                                                               
*        FIRST PUT OUT HEADER RECORD                                            
*                                                                               
         MVC   TPREC(6),=C'HEADER'                                              
         MVC   TPREC+10(13),=C'SPOT INVOICES'                                   
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+10                                                             
         MVC   TPREC+10(16),=C'NETWORK INVOICES'                                
         GOTO1 DATCON,DMCB,TODAY,(20,TPREC+30) YYYYMMDD                         
*                                                                               
*                                                                               
*        NO-OPED CODE BELOW LOOKS AT THE MOS RANGE OF                           
*        BILLS IN THE FILE                                                      
*                                                                               
****     MVC   WORK(2),SCLMOS                                                   
****     MVI   WORK+2,X'01'     SET DAY TO 1                                    
****     GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+6) YYYYMMDD                        
****     MVC   TPREC+38(6),WORK+6       YYYYMM  (NO DAY)                        
****     MVC   WORK(2),SCHMOS                                                   
****     MVI   WORK+2,X'01'     SET DAY TO 1                                    
****     GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+6) YYYYMMDD                        
****     MVC   TPREC+46(6),WORK+6       YYYYMM  (NO DAY)                        
*                                                                               
*        FISCAL START AND END DAYS MAY BE NEEDED                                
*        MAY NOT BE EASY TO ASCERTAIN                                           
*                                                                               
         LA    R3,TPREC            OUTPUT HEADER                                
         MVI   SCRECSW,C'H'                                                     
         B     SCAGN5                                                           
*                                                                               
SCPRL8   L     R3,AOFSCT                                                        
         LA    R3,L'SCKEY(R3)      BUMP PAST PSUEDO KEY                         
SCAGN    MVC   MYDUB,91(R3)                                                     
         AP    SCTOTAL,MYDUB        FOR FOOTER RECORD                           
*                                                                               
         LA    R5,91(R3)                                                        
         EDIT  (P8,MYDUB),(15,1(R5)),2,FILL=0                                   
         MVI    0(R5),C'+'                                                      
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,107(R3)                                                    
         LA     R5,107(R3)                                                      
         EDIT  (P8,MYDUB),(15,1(R5)),2,FILL=0                                   
         MVI    0(R5),C'+'                                                      
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,123(R3)                                                    
         LA    R5,123(R3)                                                       
         EDIT  (P8,MYDUB),(15,1(R5)),2,FILL=0                                   
         MVI    0(R5),C'+'                                                      
         CP     MYDUB,=P'0'            SEE IF NEGATIVE                          
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         MVC   MYDUB,139(R3)                                                    
         LA    R5,139(R3)                                                       
         EDIT  (P8,MYDUB),(15,1(R5)),2,FILL=0                                   
         MVI    0(R5),C'+'                                                      
         CP     MYDUB,=P'0'             SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         CLI   NETPAKSW,C'Y'         SEE IF NETWORK                             
         BNE   SCAGN5                                                           
         MVC   139(16,R3),SPACES     NO TAX FOR NET                             
*                                                                               
SCAGN5   DS    0H                                                               
         CLI   SVQOPT3,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    SCAGN7                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         PUT   (1),(3)                                                          
*                                                                               
SCAGN7   DS    0H                                                               
         AP    SCCOUNT,=P'1'                                                    
         CLI   SVQOPT3,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   SCAGNX                                                           
         CP    SCCOUNT,=P'100'                                                  
         BH    SCAGNX                                                           
*                                                                               
         MVC   P(13),=C'***OUTPUT HEX'                                          
         BRAS  RE,PRNT                                                          
         LR    R5,R3                                                            
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,60(R3)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,120(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,180(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,20,0                                          
         BRAS  RE,PRNT                                                          
*                                                                               
SCAGN9   MVC   P(13),=C'***OUTPUT CHAR'                                         
         BRAS  RE,PRNT                                                          
         MVC   P(100),0(R3)                                                     
         BRAS  RE,PRNT                                                          
         MVC   P(100),100(R3)                                                   
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
SCAGNX   DS    0H                                                               
         CLI   SCRECSW,C'H'          DID I JUST DO THE HEADER                   
         BNE   SCAGNX3       GO DO THE DETAILS                                  
         MVI   SCRECSW,0     CLEAR                                              
         B     SCPRL8        GO DO THE DETAILS                                  
*                                                                               
SCAGNX3  CLI   SCRECSW,C'F'          DID I JUST DO THE FOOTER                   
         BE    SCZERO        DONE                                               
*                                                                               
SCAGNXX  LA    R3,212(R3)   TO NEXT RECORD IN TABLE                             
         BCT   R2,SCAGN                                                         
*                                                                               
*        LAST  PUT OUT FOOTER RECORD                                            
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVC    TPREC(6),=C'FOOTER'                                             
         AP     SCCOUNT,=P'1'       FOR THE FOOTER RECORD                       
         EDIT   (P3,SCCOUNT),(10,TPREC+10),0,FILL=0                             
         LA     R5,TPREC+20                                                     
         EDIT   (P8,SCTOTAL),(15,1(R5)),2,FILL=0                                
         MVI    0(R5),C'+'                                                      
         CP     SCTOTAL,=P'0'           SEE IF NEGATIVE                         
         BNL    *+8                                                             
         MVI    0(R5),C'-'                                                      
*                                                                               
         LA    R3,TPREC                                                         
         MVI   SCRECSW,C'F'                                                     
         B     SCAGN5                                                           
*                                                                               
SCZERO   DS    0H                                                               
*                                                                               
         MVC   SCINVALS,ASCMED                                                  
         MVI   SCINVALS,1                                                       
         XIT1                                                                   
         SPACE 2                                                                
SCKNEWYR NTR1                                                                   
*                                                                               
         MVC   DUB(4),0(R3)                                                     
         NI    DUB,X'FF'-X'FE'     STRIP YEAR                                   
         CLC   DUB(2),SNEWYRLO                                                  
         BL    SCKNYYES                                                         
*                                                                               
         CLC   DUB(2),SPDDEC                                                    
         BNH   SCKNYNO                                                          
*                                                                               
         NI    DUB+2,X'FF'-X'FE'                                                
         CLC   DUB+2(2),SPDDEC                                                  
         BH    SCKNYNO                                                          
*                                                                               
         NI    DUB+1,X'FF'-X'E0'   ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LHI   R0,30                                                            
         SR    R0,RF                                                            
         BNP   SCKNYYES            STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'FF'-X'E0'   ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   SCKNYYES                                                         
*                                                                               
SCKNYNO  LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
SCKNYYES CR    RE,RE                                                            
         XIT1                                                                   
         SPACE 2                                                                
SNEWYRLO DC    X'002E'             JAN14                                        
SPDDEC   DC    X'0180'             DEC00                                        
         EJECT                                                                  
         LTORG                                                                  
SCTOTAL  DS    PL8                                                              
SCRECSW  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
MZPROC   NMOD1 0,MZPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
MZLAR1   DS    0H                                                               
*                                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         CLI    MZTYPE,C'2'         SECOND DETAIL?                              
         BNE    *+10                                                            
         MVC    MZMOS,BKEYYSRV                                                  
*                                                                               
MZLAR3   MVC    MZMED,MEDIA         GO HERE TO TYPE 2 REC                       
         MVC    MZCLI,CLT                                                       
         MVC    MZPRO,BKEYPRD                                                   
         MVC    MZEST(1),BKEYEST                                                
         MVC    MZINVMO,BKEYMBIL    BILLING Y/M BYTE                            
         MVC    MZINVN,BKEYINV                                                  
*                                                                               
         MVC    MZREC(250),TPREC                                                
         MVC    MZREC+250(125),TPREC+250                                        
*                                                                               
MZLAR5   L      R2,AOFMZT         ADDRESS OF MZTABLE                            
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),MZINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    MZINVALS,1         RECORD INSERTED                              
         BE     MZTOXIT                                                         
         OC     MZINVALS+1(3),MZINVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
         CLI     MZTYPE,C'1'              SHOULD ONLY HAPPEN FOR                
         BE      *+6                      INVOICE HEADER                        
         DC      H'0'                     SOMETHING'S WRONG                     
*                                                                               
         L       RF,MZINVALS              ADDRESS OF FOUND RECORD               
         LA      RF,L'MZKEY(RF)           PAST KEY                              
*                                                                               
         AP      28(8,RF),TPREC+28(8)   ACTUAL (AMOUNT DUE)                     
*                                                                               
MZTOXIT  DS      0H                                                             
         MVC     MZINVALS,AMZKEY                                                
         MVI     MZINVALS,1                                                     
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DGPROC   NMOD1 0,DGPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         CLI   DGTYPE,C'0'        FILE HEADER                                   
         BNE   DG1PR                                                            
         CLI   DGHDSW,C'Y'        SEE IF I ALREADY CREATED                      
         BE    DGPRX                                                            
*                                                                               
DG0PR    TM    SKIPBILL,X'80'      TEST THIS BILL TO TAPE                       
         BNZ   DGPRX                                                            
         MVI   DGHDSW,C'Y'                                                      
         AP    DGCOUNT,=P'1'                                                    
*                                                                               
DG0PR2   CLI   SVQOPT3,C'Y'          TEST TO DUMP RECORDS                       
         BNE   DG0PR5                                                           
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
DG0PR5   DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
         B     DGPRX                                                            
*                                                                               
DG1PR    CLI   DGTYPE,C'1'          INVOICE HEADER                              
         BNE   DG2PR                                                            
         TM    SKIPBILL,X'80'      TEST THIS BILL TO TAPE                       
         BNZ   DGPRX                                                            
         AP    DGCOUNT,=P'1'                                                    
         AP    DGTOTAL,BACTP                                                    
         MVI   TPREC,C'1'                                                       
         MVI   TPREC+1,C','          DELIMITER                                  
         MVC   TPREC+02(10),=C'1000084655'                                      
         MVI   TPREC+12,C','          DELIMITER                                 
         MVC   TPREC+13(4),TPREC+364     PRD UCOMM 3                            
         MVI   TPREC+17,C','          DELIMITER                                 
         GOTO1 DATCON,DMCB,BQDATE,(20,TPREC+18)   INVOICE DATE                  
         MVI   TPREC+26,C','          DELIMITER                                 
*                                                                               
         MVC   WORK(11),SPACES                                                  
         LA    R1,WORK                                                          
         LA    R2,DINVFULL                                                      
         LA    R3,10                                                            
DG1PRA   CLI   0(R2),C' '              ELIMINATE SPACES                         
         BE    DG1PRB                                                           
         CLI   0(R2),C'-'              ELIMINATE DASHES                         
         BNE   DG1PRC                                                           
DG1PRB   LA    R2,1(R2)                                                         
         BCT   R3,DG1PRA                                                        
         B     DG1PRD                                                           
*                                                                               
DG1PRC   MVC   0(1,R1),0(R2)                                                    
         LA    R1,1(R1)                                                         
         B     DG1PRB                                                           
*                                                                               
DG1PRD   MVC   TPREC+27(8),WORK                                                 
         MVI   TPREC+35,C','          DELIMITER                                 
*                                                                               
         EDIT  BACTP,(13,TPREC+36),2,FLOAT=-,FILL=0                             
         B     DG0PR2                     OUTPUT RECORD                         
*                                                                               
DG2PR    CLI   DGTYPE,C'2'          INVOICE DETAIL RECORD                       
         BNE   DG3PR                                                            
         TM    SKIPBILL,X'80'      TEST THIS BILL TO TAPE                       
         BNZ   DGPRX                                                            
         AP    DGCOUNT,=P'1'                                                    
         MVI   TPREC,C'2'                                                       
         MVI   TPREC+01,C','          DELIMITER                                 
         MVC   TPREC+2(10),TPREC+300    PRD UCOMM 1                             
         MVI   TPREC+12,C','          DELIMITER                                 
         MVC   TPREC+13(5),TPREC+332    PRD UCOMM 2 (FIRST 5)                   
         MVI   TPREC+18,C','          DELIMITER                                 
         EDIT  BACTP,(13,TPREC+19),2,FLOAT=-,FILL=0                             
         MVC   TPREC+32(18),SPACES      CLEAR END                               
         B     DG0PR2                   OUTPUT RECORD                           
*                                                                               
DG3PR    DS    0H                                                               
         CLI   DGTYPE,C'3'              TOTAL RECORD                            
         BE    *+6                                                              
         DC    H'0'                    SOMETHING VERY WRONG                     
         CP    DGCOUNT,=P'0'           ANY RECORDS?                             
         BE    DGPRX                                                            
         MVI   TPREC,C'3'                                                       
         MVI   TPREC+01,C','          DELIMITER                                 
         MVC   TPREC+2(48),SPACES                                               
         EDIT  DGCOUNT,(10,TPREC+2),FILL=0                                      
         MVI   TPREC+12,C','          DELIMITER                                 
         EDIT  DGTOTAL,(13,TPREC+13),2,FLOAT=-,FILL=0                           
         B     DG0PR2                   OUTPUT RECORD                           
         DROP  R7                                                               
DGPRX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PHPROC   NMOD1 0,PHPROC                PHILIPS (AGENCY FM)                      
*                                                                               
         LA    RC,SPACEND              SPBTWRKD WORK AREA                       
*                                                                               
         CLI   PHHDSW,C'Y'             HEADER ALREADY WRITTEN TO TAPE?          
         BE    PH0PR02                 YES                                      
*                                                                               
         LA    R2,TPREC                R2 = TAPE RECORD LINE TO OPUTPUT         
         LA    R3,PHEADTAB             HEADLINE TABLE                           
         XR    R4,R4                   CLEAR R4                                 
*                                                                               
PH0PR00  IC    R4,0(R3)                ENTRY LENGTH                             
         BCTR  R4,0                    -1 FOR EX                                
         EX    R4,*+8                  EXECUTE MVC                              
         B     *+10                    FOR IDF                                  
         MVC   0(0,R2),1(R3)           HEADLINE                                 
         LA    R3,2(R3,R4)             BUMP TO NEXT HEADLINE                    
         CLI   0(R3),X'FF'             END OF TABLE?                            
         BE    PH0PR01                 YES - NO DELIMITER ON LAST ENTRY         
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
         B     PH0PR00                 PROCESS NEXT ENTRY                       
*                                                                               
PH0PR01  BAS   RE,PHTAPE               PUT TPREC TO TAPE                        
*                                                                               
         LA    RE,TPREC                RE = TPREC                               
         LHI   R1,L'TPREC-1            R1 = L'TPREC-1                           
         LA    RF,1(RE)                RF = LAST BYTE OF TPREC                  
         MVI   0(RE),C' '              INIT T0 A SPACE                          
         MOVE  ((RF),(R1)),(RE)        INIT TPREC TO SPACES                     
*                                                                               
         MVI   PHHDSW,C'Y'             FLAG HEADER WRITTEN TO TAPE              
*                                                                               
PH0PR02  L     R7,ADBILL               R7 = A(BILL RECORD)                      
         USING BILLREC,R7              BILL RECORD DSECT                        
*                                                                               
         LA    R2,TPREC                R2 = TAPE RECORD LINE TO OPUTPUT         
*                                                                               
         MVC   0(2,R2),=C'CR'          CREDIT INVOICE                           
         CP    BACTP,=P'0'             INVOICE AMOUNT NEGATIVE?                 
         BL    *+10                    YES                                      
         MVC   0(3,R2),=C'INV'         STANDARD INVOICE                         
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(1,R2),QMED            MEDIA                                    
         CLI   NETPAKSW,C'Y'           NETWORK?                                 
         BNE   PH0PR03                 NO                                       
         MVC   0(1,R2),BLMED           MEDIA                                    
         CLI   BLMED,C' '              BLMED BLANK?                             
         BNE   *+8                     NO                                       
         MVI   0(R2),C'N'              YES - MOVE IN AN "N"                     
         MVC   1(6,R2),BINVNO          BILL INVOICE NUMBER                      
PH0PR03  BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         GOTO1 DATCON,DMCB,BDATE,(10,0(R2))                                     
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         L     R3,ADEST                A(ESTIMATE RECORD)                       
         USING ESTHDRD,R3              ESTIMATE RECORD DSECT                    
         MVC   0(L'EUSER1,R2),EUSER1   EST UDEF1 = PO NUMBER                    
         OC    0(L'EUSER1,R2),SPACES   SPACE PAD IN CASE BINARY ZEROS           
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
         DROP  R3                      DROP ESTIMATE RECORD USING               
*                                                                               
         MVC   0(4,R2),=C'OB10'        BILL TO CUSTOMER ID NUMBER               
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(11,R2),=C'HAVAS MEDIA'     SHIP TO NAME                        
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(13,R2),=C'200 HUDSON ST'   SHIP TO ADDRESS                     
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(08,R2),=C'NEW YORK'   SHIP TO CITY                             
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(02,R2),=C'NY'         SHIP TO STATE                            
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVC   0(05,R2),=C'10013'      SHIP TO ZIP CODE                         
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         L     R3,ADEST                A(ESTIMATE RECORD)                       
         USING ESTHDRD,R3              ESTIMATE RECORD DSECT                    
         MVC   0(L'EUSER1,R2),EUSER1   EST UDEF1 = PO NUMBER                    
         OC    0(L'EUSER1,R2),SPACES   SPACE PAD IN CASE BINARY ZEROS           
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
         MVC   0(L'EDESC,R2),EDESC     EST NAME/DESCRIPTION                     
         OC    0(L'EDESC,R2),SPACES    SPACE PAD IN CASE BINARY ZEROS           
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
         DROP  R3                      DROP ESTIMATE RECORD USING               
*                                                                               
         EDIT  BACTP,(13,0(R2)),2,ZERO=NOBLANK,ALIGN=LEFT,MINUS=YES             
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         MVI   0(R2),C'1'              QUANTITY                                 
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         EDIT  BACTP,(13,0(R2)),2,ZERO=NOBLANK,ALIGN=LEFT,MINUS=YES             
         BAS   RE,FINDLST              FIND END AND SET DELIMITER               
*                                                                               
         L     R3,ADPRD                A(PRODUCT RECORD)                        
         USING PRDHDRD,R3              PRODUCT RECORD DSECT                     
         MVC   0(32,R2),PUSER1         PRODUCT UDEF 1 (EMAIL ADDRESS)           
         OC    0(32,R2),SPACES         SPACE PAD IN CASE BINARY ZEROS           
         DROP  R3                      DROP PRODUCT RECORD USING                
*                                                                               
         BAS   RE,PHTAPE               PUT TPREC TO TAPE                        
*                                                                               
PHPRX    J     EXIT                    EXIT                                     
         DROP  R7                      DROP BILL RECORD USING                   
*                                                                               
FINDLST  LA    R2,TPREC+PHTLEN         PAST END OF DATA                         
         CLI   0(R2),X'40'             HAVE DATA HERE?                          
         BH    *+8                     YES - DELIMITER GOES 1 AFTER             
         BCT   R2,*-8                  NO - GO BACK 1 CHAR & TRY AGAIN          
         MVI   1(R2),C','              DELIMITER                                
         LA    R2,2(R2)                BUMP R2                                  
         BR    RE                      RETURN                                   
*                                                                               
PHTAPE   NTR1                                                                   
*                                                                               
         CLI   SVQOPT3,C'Y'            PRINT TPREC?                             
         BNE   PH0PR5                  NO                                       
*                                                                               
         LH    R0,RECLEN               PRINT TPREC                              
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
PH0PR5   L     R1,=A(SBITAPE)          R1 = A(SBITAPE)                          
         LA    R0,TPREC                R0 = TPREC                               
         PUT   (1),(0)                 PUT RECORD TO TAPE                       
*                                                                               
         J     EXIT                    RETURN                                   
*                                                                               
PHEADTAB DC   X'13',C'INVOICE CREDIT NOTE'                                      
         DC   X'0E',C'INVOICE NUMBER'                                           
         DC   X'0C',C'INVOICE DATE'                                             
         DC   X'15',C'PURCHASE ORDER NUMBER'                                    
         DC   X'1A',C'BILL TO CUSTOMER ID NUMBER'                               
         DC   X'0C',C'SHIP TO NAME'                                             
         DC   X'0F',C'SHIP TO ADDRESS'                                          
         DC   X'0C',C'SHIP TO CITY'                                             
         DC   X'0D',C'SHIP TO STATE'                                            
         DC   X'10',C'SHIP TO ZIP CODE'                                         
         DC   X'09',C'PO NUMBER'                                                
         DC   X'0B',C'DESCRIPTION'                                              
         DC   X'0A',C'UNIT PRICE'                                               
         DC   X'07',C'QUANITY'                                                  
         DC   X'11',C'TOTAL LINE AMOUNT'                                        
         DC   X'0D',C'EMAIL ADDRESS'                                            
         DC   X'FF'                                                             
*                                                                               
         LTORG                                                                  
***********************************************************************         
*   MZ MAZDA SPECIAL OUTPUT FROM A SPECIAL BINSRCH TABLE                        
***********************************************************************         
*                                                                               
MZOUT    NMOD1 0,MZOUT                                                          
         LA    RC,SPACEND                                                       
*                                                                               
MZPRL5   ZAP   MZHCNT,=P'0'            INIT INVOICE HEADER COUNT                
         ZAP   MZLCNT,=P'0'            INIT INVOICE DETAIL COUNT                
         ZAP   MZCOUNT,=P'0'           INIT TOTAL RECORD COUNT                  
         MVI   MZRECSW,0               INIT MZRECSW                             
*                                                                               
         L     R2,MZRECNT              FOR BCT RECORD COUNT                     
         CH    R2,=H'0'                HAVE ANY RECORDS?                        
         BE    MZEXIT                  NO - DONE                                
*                                                                               
         LA    RE,TPREC                RE = TPREC                               
         LHI   R1,L'TPREC-1            R1 = L'TPREC                             
         LA    RF,1(RE)                1 BYTE PAST TPREC                        
         MVI   0(RE),C' '              CLEAR TPREC WITH SPACES                  
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVI   TPREC,C'A'              FILE HEADER                              
         MVC   TPREC+1(3),=C'TMG'      SOURCE IDENTIFER                         
         CLC   AGY,=C'JS'              AGENCY JS?                               
         BNE   *+10                    NO                                       
         MVC   TPREC+1(6),=C'GTMGEO'   YES - SOURCE IDENTIFER FOR JS            
         MVC   TPREC+11(8),=C'MEDIA_AP'  INTERFACE IDENTIFER                    
         CLC   AGY,=C'JS'              AGENCY JS?                               
         BNE   *+10                    NO                                       
         MVC   TPREC+11(10),=C'CO-OPMEDIA' INTERFACE IDENTIFER FOR JS           
         MVC   TPREC+21(8),CTODAY   USE TODAY AS SEQUENCE # CCYYMMDD            
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(20,TPREC+29) YYYYMMDD                         
*                                                                               
         CLC   AGY,=C'JS'                 AGENCY JS?                            
         BNE   MZPRL6                     NO                                    
         MVC   TPREC+37(17),=C'CO-OP MEDIA BUY -'                               
         MVC   TPREC+55(2),TODAY+2        MONTH                                 
         MVI   TPREC+57,C'/'                                                    
         MVC   TPREC+58(2),=C'20'         CENTURY                               
         MVC   TPREC+60(2),TPREC+31       YEAR                                  
         B     MZPRL6A                    DONE WITH BATCH DESCRIPTION           
*                                                                               
MZPRL6   MVC   TPREC+37(31),=C'TEAM MAZDA MEDIA PAYMENT FILE -'                 
         MVC   TPREC+70(2),TODAY+2        MONTH                                 
         MVI   TPREC+72,C'/'                                                    
         MVC   TPREC+73(2),=C'20'         CENTURY                               
         MVC   TPREC+75(2),TPREC+31       YEAR                                  
*                                                                               
MZPRL6A  MVI   MZRECSW,C'A'        FILE HEADER                                  
         AP    MZLCNT,=P'1'                                                     
         B     MZAGN5                                                           
*                                                                               
MZPRL8   L     R3,AOFMZT                                                        
         LA    R3,L'MZKEY(R3)      BUMP PAST PSUEDO KEY                         
*                                                                               
MZAGN    DS    0H                                                               
         CLI   0(R3),C'1'          FIRST INVOICE DETAIL                         
         BNE   MZAGN2                                                           
*                                                                               
*        DO INVOICE HEADER (H) RECORD FIRST                                     
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         MVI   TPREC,C'H'                                                       
         MVC   TPREC+1(8),347(R3)    INVOICE DATE                               
         MVC   TPREC+9(2),=C'YI'     DEFAULT/FIXED                              
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+9(2),=C'ZI'     DEFAULT/FIXED                              
         MVC   TPREC+11(4),=C'US10'  FIXED                                      
*******  MVC   TPREC+15(8),355(R3)    RUN DATE                                  
         MVC   TPREC+15(8),CTODAY    FILE CREATION DATE                         
         MVC   TPREC+23(3),=C'USD'   CURRENCY                                   
*                                                                               
*        TPREC+28(8) TO BE INVOICE # WITHOUT DASHES                             
*                                                                               
         MVC   TPREC+28(2),364(R3)   MEDIA PART                                 
         MVC   TPREC+30(2),367(R3)   MONTH PART                                 
         MVC   TPREC+32(4),370(R3)   INV # PART                                 
         MVC   TPREC+44(12),=C'TEAM MAZDA -'                                    
         MVC   TPREC+57(2),351(R3)   MONTH OF BILLING                           
         MVI   TPREC+59,C'/'                                                    
         MVC   TPREC+60(2),349(R3)   YEAR OF BILLING                            
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   MZOUT10               NO                                         
         MVC   TPREC+44(18),=C'CO-OP MEDIA BUY - '                              
         MVC   TPREC+62(2),351(R3)   MONTH OF BILLING                           
         MVI   TPREC+64,C'/'                                                    
         MVC   TPREC+65(2),349(R3)   YEAR OF BILLING                            
MZOUT10  MVI   MZRECSW,C'H'          INVOICE HEADER                             
         AP    MZHCNT,=P'1'                                                     
         AP    MZLCNT,=P'1'                                                     
         B     MZAGN5                                                           
*                                                                               
MZAGN1   ZAP   MYDUB,28(8,R3)        FIRST DETAIL RECORD                        
*                                                                               
         MVI   TPREC,C'L'                                                       
         MVC   TPREC+1(2),=C'31'                                                
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+1(2),=C'01'                                                
         CP    28(8,R3),=P'0'                                                   
         BNL   MZOUT20                                                          
         MVC   TPREC+1(2),=C'21'     CREDIT MEMO                                
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+1(2),=C'11'     CREDIT MEMO                                
*                                                                               
MZOUT20  MVC   TPREC+7(6),=C'110641'       HARD CODED                           
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+7(6),304(R3)    YES - PUSER1/DEALER NUMBER                 
         MVC   TPREC+28(12),=C'000000000000'   SINCE $ FIELD IS 27 LONG         
         LA    R5,TPREC+40                                                      
         EDIT  (P8,MYDUB),(15,0(R5)),2,FILL=0                                   
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BE    *+8                   YES - LEAVE TPREC+126 BLANK!               
         MVI   TPREC+126,C'M'        AP-MANUAL WIRES?                           
         MVC   TPREC+211(3),=C'DUE'                                             
         MVC   TPREC+215(2),339(R3)  DUE MONTH                                  
         MVI   TPREC+217,C'/'                                                   
         MVC   TPREC+218(2),341(R3)  DUE DAY                                    
         MVI   TPREC+220,C'/'                                                   
         MVC   TPREC+221(2),343(R3)                                             
         MVC   TPREC+223(4),223(R3)   COULD BE MKT GROUP                        
*                                                                               
         MVC   TPREC+273(30),273(R3) MEDIA AND MOS                              
         LA    R5,TPREC+303                                                     
MZAGN1A  CLI   0(R5),C' '       FIND FIRST NON-SPACE                            
         BNE   MZAGN1B                                                          
         SH    R5,=H'1'         BACK-UP R5                                      
         B     MZAGN1A                                                          
MZAGN1B  SH    R5,=H'5'         MUST HAVE HIT MOS                               
         MVC   0(6,R5),SPACES   CLEAR MOS FOR THIS RECORD                       
         AP    MZLCNT,=P'1'                                                     
         B     MZAGN5                                                           
*                                                                               
MZAGN2   ZAP   MYDUB,28(8,R3)        NON-FIRST DETAIL RECORD                    
*                                                                               
         MVI   TPREC,C'L'                                                       
         MVC   TPREC+1(2),=C'40'                                                
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+1(2),=C'50'                                                
         CP    28(8,R3),=P'0'                                                   
         BNL   MZOUT30                                                          
         MVC   TPREC+1(2),=C'50'     CREDIT MEMO                                
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BNE   *+10                  NO                                         
         MVC   TPREC+1(2),=C'40'     CREDIT MEMO                                
*                                                                               
MZOUT30  MVC   TPREC+7(16),7(R3)     GL ACCOUNT - PRD USER 1?                   
         MVC   TPREC+28(12),=C'000000000000'                                    
         LA    R5,TPREC+40                                                      
         EDIT  (P8,MYDUB),(15,0(R5)),2,FILL=0                                   
         MVC   TPREC+159(4),159(R3) COST CENTER - EST USER 1?                   
         MVC   TPREC+169(5),169(R3) INTERNAL ORDER - EST USER 2?                
         CLC   AGY,=C'JS'            AGENCY JS?                                 
         BE    *+10                  YES - LEAVE TPREC+211 BLANK!               
         MVC   TPREC+211(3),211(R3)  PRODUCT CODE                               
         MVC   TPREC+223(4),223(R3)   COULD BE MKT GROUP                        
         MVC   TPREC+273(30),273(R3) MEDIA AND MOS                              
         AP    MZLCNT,=P'1'                                                     
         B     MZAGN5                                                           
*                                                                               
MZAGN5   DS    0H                                                               
         LA    R4,TPREC                                                         
         CLI   SVQOPT3,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    MZAGN7                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         PUT   (1),(4)                                                          
*                                                                               
MZAGN7   DS    0H                                                               
         AP    MZCOUNT,=P'1'                                                    
         CLI   SVQOPT3,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   MZAGNX                                                           
         CP    MZCOUNT,=P'50'                                                   
         BH    MZAGNX                                                           
*                                                                               
         MVC   P(13),=C'***OUTPUT HEX'                                          
         BRAS  RE,PRNT                                                          
         LR    R5,R4                                                            
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,60(R4)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,120(R4)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,180(R4)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,240(R4)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,300(R4)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,75,0                                          
         BRAS  RE,PRNT                                                          
*                                                                               
MZAGN9   MVC   P(13),=C'***OUTPUT CHAR'                                         
         BRAS  RE,PRNT                                                          
         MVC   P(100),0(R4)                                                     
         BRAS  RE,PRNT                                                          
         MVC   P(100),100(R4)                                                   
         BRAS  RE,PRNT                                                          
         MVC   P(100),200(R4)                                                   
         BRAS  RE,PRNT                                                          
         MVC   P(75),300(R4)                                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
MZAGNX   DS    0H                                                               
         CLI   MZRECSW,C'A'  DID I JUST DO THE FILE HEADER                      
         BNE   MZAGNX2       GO DO THE DETAILS                                  
         MVI   MZRECSW,0     CLEAR                                              
         MVC   TPREC(100),SPACES   CLEAR TPREC                                  
         B     MZPRL8        GO DO THE H RECORD                                 
*                                                                               
MZAGNX2  DS    0H                                                               
         CLI   MZRECSW,C'H'  DID I JUST DO THE INVOICE HEADER                   
         BNE   MZAGNX3       GO DO THE DETAILS                                  
         MVI   MZRECSW,0     CLEAR                                              
         MVC   TPREC(100),SPACES   CLEAR TPREC                                  
         B     MZAGN1        GO DO FIRST DETAIL RECORD                          
*                            SAME BINSEARCH RECORD AS INV. HEADER               
*                                                                               
MZAGNX3  CLI   MZRECSW,C'F'          DID I JUST DO THE TRAILER                  
         BE    MZEXIT        DONE                                               
*                                                                               
MZAGNXX  LA    R3,390(R3)   TO NEXT RECORD IN TABLE                             
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         BCT   R2,MZAGN                                                         
*                                                                               
*        LAST  PUT OUT TRAILER RECORD                                           
*                                                                               
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         AP    MZLCNT,=P'1'        ADD ONE FOR TRAILER                          
         MVI   TPREC,C'T'          TRAILER RECORD                               
         EDIT  (P3,MZHCNT),(6,TPREC+1),0,FILL=0                                 
         EDIT  (P3,MZLCNT),(8,TPREC+7),0,FILL=0                                 
         LA    R3,TPREC                                                         
         MVI   MZRECSW,C'F'                                                     
         B     MZAGN5                                                           
*                                                                               
MZEXIT   MVC   MZINVALS,AMZKEY                                                  
         MVI   MZINVALS,1                                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
MZRECSW  DS    CL1                RECORD TYPE SWITCH A/H/F                      
MZHCNT   DS    PL3                INVOICE HEADER COUNT                          
MZLCNT   DS    PL3                INVOICE DETAIL COUNT                          
MZCOUNT  DS    PL3                RECORD COUNT                                  
*                                                                               
         EJECT                                                                  
MCCOUT   NMOD1 0,MCCOUT                                                         
         LA    RC,SPACEND                                                       
*                                                                               
*                      FINALLY PRODUCE RECORDS                                  
MCPRL    DS    0H                                                               
*                                                                               
*                                                                               
MCPRL5   DS    0H                                                               
         L     R2,MCRECNT          FOR BCT RECORD COUNT                         
         CH    R2,=H'0'            NO RECORDS                                   
         BE    MCZERO                                                           
         L     R3,AOFMCT                                                        
         LA    R3,L'MCKEY(R3)      BUMP PAST PSUEDO KEY                         
*                                                                               
MCAGN    ZAP   BIGDUB,101(12,R3)                                                
         CP    BIGDUB,=P'0'       IS IT NEGATIVE?                               
         BL    MCP5                                                             
         EDIT  BIGDUB,(12,101(R3)),0,FILL=0,ZERO=NOBLANK                        
         B     MCP10                                                            
*                                                                               
MCP5     EDIT  BIGDUB,(12,101(R3)),0,FILL=0,ZERO=NOBLANK,MINUS=YES              
*                                                                               
MCP10    DS    0H                                                               
         ZAP   BIGDUB,113(12,R3)                                                
         CP    BIGDUB,=P'0'       IS IT NEGATIVE?                               
         BL    MCP15                                                            
         EDIT  BIGDUB,(12,113(R3)),0,FILL=0,ZERO=NOBLANK                        
         B     MCP20                                                            
*                                                                               
MCP15    EDIT  BIGDUB,(12,113(R3)),0,FILL=0,ZERO=NOBLANK,MINUS=YES              
*                                                                               
MCP20    DS    0H                                                               
         ZAP   BIGDUB,125(12,R3)                                                
         CP    BIGDUB,=P'0'       IS IT NEGATIVE?                               
         BL    MCP25                                                            
         EDIT  BIGDUB,(12,125(R3)),0,FILL=0,ZERO=NOBLANK                        
         B     MCP30                                                            
*                                                                               
MCP25    EDIT  BIGDUB,(12,125(R3)),0,FILL=0,ZERO=NOBLANK,MINUS=YES              
*                                                                               
MCP30    DS    0H                                                               
         ZAP   BIGDUB,137(12,R3)                                                
         CP    BIGDUB,=P'0'       IS IT NEGATIVE?                               
         BL    MCP35                                                            
         EDIT  BIGDUB,(12,137(R3)),0,FILL=0,ZERO=NOBLANK                        
         B     MCP40                                                            
*                                                                               
MCP35    EDIT  BIGDUB,(12,137(R3)),0,FILL=0,ZERO=NOBLANK,MINUS=YES              
*                                                                               
MCP40    DS    0H                                                               
         ZAP   BIGDUB,186(12,R3)                                                
         CP    BIGDUB,=P'0'       IS IT NEGATIVE?                               
         BL    MCP45                                                            
         EDIT  BIGDUB,(12,186(R3)),0,FILL=0,ZERO=NOBLANK                        
         B     MCP50                                                            
*                                                                               
MCP45    EDIT  BIGDUB,(12,186(R3)),0,FILL=0,ZERO=NOBLANK,MINUS=YES              
*                                                                               
MCP50    DS    0H                                                               
*                                                                               
MCAGN5   DS    0H                                                               
         CLI   SVQOPT3,C'Y'          TEST RUN - NO RECORDS TO FILE              
         BE    MCAGN7                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         PUT   (1),(3)                                                          
*                                                                               
MCAGN7   DS    0H                                                               
         CLI   SVQOPT3,C'Y'          DISPLAY FIRST 100 RECORDS                  
         BNE   MCAGNX                                                           
         CP    MCCOUNT,=P'100'                                                  
         BH    MCAGNX                                                           
         AP    MCCOUNT,=P'1'                                                    
*                                                                               
         MVC   P(13),=C'***OUTPUT HEX'                                          
         BRAS  RE,PRNT                                                          
         LR    R5,R3                                                            
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,60(R3)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,120(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,180(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,240(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,60,0                                          
         BRAS  RE,PRNT                                                          
         LA    R5,300(R3)                                                       
         GOTO1 HEXOUT,DMCB,(R5),P,41,0                                          
         BRAS  RE,PRNT                                                          
*                                                                               
MCAGN9   MVC   P(14),=C'***OUTPUT CHAR'                                         
         BRAS  RE,PRNT                                                          
         MVC   P(100),0(R3)                                                     
         BRAS  RE,PRNT                                                          
         MVC   P(100),100(R3)                                                   
         BRAS  RE,PRNT                                                          
         MVC   P(100),200(R3)                                                   
         BRAS  RE,PRNT                                                          
         MVC   P(41),300(R3)                                                    
         BRAS  RE,PRNT                                                          
         BRAS  RE,PRNT                                                          
*                                                                               
MCAGNX   LA    R3,353(R3)   TO NEXT RECORD IN TABLE                             
         BCT   R2,MCAGN                                                         
*                                                                               
MCZERO   DS    0H                                                               
*                                                                               
         MVC   MBINVALS,AMCMED                                                  
         MVI   MBINVALS,1                                                       
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
SBOUT    NMOD1 0,SBOUT            STARBUCKS BATCH RECORD                        
         LA    RC,SPACEND                                                       
*                              FILE TOTAL RECORD                                
         LA    RE,TPREC            CLEAR TPREC                                  
         LHI   R1,L'TPREC-1                                                     
         LA    RF,1(RE)                                                         
         MVI   0(RE),C' '                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
*******  AP    SBCOUNT,=P'1'          DON'T COUNT THIS RECORD                   
         MVI   TPREC,C'B'                                                       
         MVI   TPREC+1,CARET          DELIMITER                                 
*                                                                               
*        RECORD COUNTS AND TOTAL $                                              
*                                                                               
         LA    R2,TPREC+2                                                       
         EDIT  SBCOUNT,(4,0(R2)),0,ALIGN=LEFT                                   
         AR    R2,R0                                                            
         MVI   0(R2),CARET          DELIMITER                                   
         LA    R2,1(R2)                                                         
         EDIT  SBCOUNT,(4,0(R2)),0,ALIGN=LEFT                                   
         AR    R2,R0                                                            
         MVI   0(R2),CARET          DELIMITER                                   
         LA    R2,1(R2)                                                         
         EDIT  SBTOTAL,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT             
         AR    R2,R0                                                            
         MVI   0(R2),CARET          DELIMITER                                   
*                                                                               
         CLI   SVQOPT3,C'Y'          TEST TO DUMP RECORDS                       
         BNE   SBOUT5                                                           
*                                                                               
         LH    R0,RECLEN                                                        
         GOTO1 PRNTBL,DMCB,0,TPREC,C'DUMP',(R0),=C'1D'                          
*                                                                               
SBOUT5   L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
SBPRL5   DS    0H                                                               
*                                                                               
SBPRL15  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*   MCCANN SPECIAL FOR CHRYSLER TYPE=C                                          
***********************************************************************         
*                                                                               
MCHPROC  NMOD1 0,MCHPROC                                                        
         LA    RC,SPACEND                                                       
*                                                                               
MCLAR1   DS    0H                                                               
*                                                                               
*                                                                               
*  AT THIS POINT MUST ADD TAPE RECORD TO TABLE AND IF THERE IS A                
*   DUPLICATE ADD THEM TOGETHER                                                 
*       CREATE KEY                                                              
*                                                                               
         L      R7,ADBILL                                                       
         USING  BILLREC,R7                                                      
         MVC    MCMED,MEDIA                                                     
         MVC    MCCLI,CLT                                                       
         MVC    MCPRO,BKEYPRD                                                   
         MVC    MCEST(1),BKEYEST                                                
         MVC    MCINVMO,BKEYMBIL    BILLING Y/M BYTE                            
         MVC    MCINVN,BKEYINV                                                  
*                                                                               
         DROP   R7                                                              
*                                                                               
         MVC    MCREC(250),TPREC                                                
         MVC    MCREC+250(341-250),TPREC+250                                    
*                                                                               
         L      R2,AOFMCT         ADDRESS OF MCTAB                              
         PRINT  GEN                                                             
         GOTO1 =V(BINSRCH),MBINVALS                                             
         PRINT  NOGEN                                                           
*                                                                               
         CLI    MBINVALS,1         RECORD INSERTED                              
         BE     MCTOXIT                                                         
         OC     MBINVALS+1(3),MBINVALS+1 IF ZERO TABLE IS FULL                  
         BNZ    *+6                                                             
         DC     H'0'                                                            
*                                                                               
*   HAVE DUPLICATE MUST ADD FIELDS                                              
*                                                                               
         L       RF,MBINVALS              ADDRESS OF FOUND RECORD               
         LA      RF,L'MCKEY(RF)           PAST KEY                              
*                                                                               
         MVC     82(5,RF),TPREC+82        SAVE MOS                              
*                                                                               
         AP      101(12,RF),TPREC+101(12) ACTUAL                                
         AP      113(12,RF),TPREC+113(12) NET                                   
         AP      125(12,RF),TPREC+125(12)                                       
         AP      137(12,RF),TPREC+137(12) COMMISSION                            
         AP      186(12,RF),TPREC+186(12) GROSS                                 
*                                                                               
MCTOXIT  DS    0H                                                               
         MVC   MBINVALS,AMCMED                                                  
         MVI   MBINVALS,1                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*    JWT SPECIAL                                                                
***********************************************************************         
*                                                                               
JWPROC   NMOD1 0,JWPROC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         CLI   QOPT5,C'A'          FOR JWT TAPE CANNOT LIMIT TO                 
         BE    *+12                AOR OR NON-AOR ONLY                          
         CLI   QOPT5,C'X'                                                       
         BNE   JW07B                                                            
*                                                                               
         MVC   P(80),QPROG                                                      
         MVC   P+82(15),=C'INVALID REQUEST'                                     
         GOTO1 AENDREQ                                                          
*                                                                               
JW07B    DS    0H                                                               
         CLC   CLT,=C'WL '         FOR CLIENT WL                                
         BNE   JW07D                                                            
         L     RF,ADPRD                                                         
         LA    RF,PUSER1-PRDHDR(RF)  USE PUSER1 AS CLIENT CODE                  
         CLI   0(RF),C' '            IF PRESENT                                 
         BNH   JW07D                                                            
         MVC   TPREC+1(4),0(RF)                                                 
         OC    TPREC+1(4),SPACES   UPPERCASE                                    
*                                                                               
JW07D    DS    0H                                                               
         CLI   QMED,C'N'           FOR NETWORK                                  
         BNE   JW10                                                             
         CLC   CLT,=C'OA '         SPECIAL FOR CLIENT OA                        
         BE    JW08                                                             
         CLC   CLT,=C'FD '         AND FD                                       
         BE    JW08                                                             
         CLC   CLT,=C'EK '         AND EK                                       
         BNE   JW10                                                             
JW08     DS    0H                                                               
         L     RF,ADPRD                                                         
         MVC   TPREC+1(4),PADDR1-PRDHDR(RF)  CLT CODE IS BILLADDR(4)            
*                                                                               
JW10     DS    0H                                                               
         CLC   CLT,=C'BAP'         SPECIAL FOR CLIENT BAP                       
         BNE   *+14                                                             
         L     RF,ADPRD                                                         
         MVC   TPREC+1(4),PUSER1-PRDHDR(RF)  CLT CODE IS USER1 FIELD            
*                                                                               
         LA    R7,SVBILL           **NOTE-SVBILL IS 'BILLREC'                   
         USING BILLREC,R7                                                       
*                                                                               
         CLI   QMED,C'N'           FOR NETPAK                                   
         BNE   *+18                                                             
         CLI   BLMED,C'O'          AND SUB-MED OF O                             
         BNE   *+10                                                             
         MVC   SVTPREC+10(2),=C'U '   SPECIAL MEDIA CODE                        
*                                                                               
         CLI   SVBILL+1,0          ANY PREVIOUS BILL                            
         BE    JWPX                NO, DO NOTHING NOW                           
         TM    BILSTAT,BSTTAORQ    OR IF PREV WAS AOR                           
         BNZ   JWPX                                                             
*                                                                               
         CLI   NETPAKSW,C'Y'       NO OA SPECIALS UNLESS NETPAK                 
         BNE   JW12                                                             
         MVI   JWOASW,C'Y'         SET CLIENT OA TYPE POSTINGS SWITCH           
         LA    RE,SVTPREC                                                       
         USING SVTPREC,RE                                                       
         CLC   SVTPREC+123(3),=C'OA '  QUAKER                                   
         BE    JW12B                                                            
         CLC   SVTPREC+123(3),=C'UNI'  UNILEVER                                 
         BNE   JW12                                                             
         DROP  RE                                                               
*Y2K*                                                                           
         CLC   BMONSERV,=C'9210'   MOS OCT92+                                   
         BL    JW12                                                             
         B     JW12B                                                            
*                                                                               
JW12     DS    0H                                                               
         MVI   JWOASW,C'N'                                                      
*                                                                               
JW12B    DS    0H                                                               
         LA    R3,BAMTS            BILL AMOUNTS                                 
         USING AMOUNTSD,R3                                                      
         LA    R4,SVBAMTS          SAVED BILL AMOUNTS                           
SVB      USING AMOUNTSD,R4                                                      
*                                                                               
         L     R8,ADBILL                                                        
         TM    BILSTAT-BILLREC(R8),BSTTAORQ  IS THIS BILL AOR                   
         BZ    JW200                                                            
*                                                                               
         CLI   JWOASW,C'Y'         SPECIAL CLIENT OA TYPE MODE                  
         BE    JW44                                                             
*                                  ELSE WRITE 3 RECORDS                         
*                                  1)SAVED REC (ORIG BILL) AS TYPE 1            
         MVI   SVTPREC+00,C'1'     SET BILL TYPE                                
         UNPK  SVTPREC+34(11),SVB.AMTACT SET ACTUAL IN GROSS                    
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW20                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
*                                                                               
JW20     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                  2)SVTPREC AS TYPE 4 WITH AMTS REVRSD         
*                                                                               
         MVI   SVTPREC+00,C'4'     SET BILL TYPE                                
         ZAP   DUB,SVB.AMTACT      SET ACTUAL                                   
         MP    DUB,=P'-1'                                                       
         UNPK  SVTPREC+34(11),DUB    IN GROSS                                   
         ZAP   DUB,SVB.AMTNET      SET NET                                      
         MP    DUB,=P'-1'                                                       
         UNPK  SVTPREC+54(11),DUB    IN COST FIELD                              
         ZAP   DUB,SVB.AMTAC       AND COMMISSION                               
         MP    DUB,=P'-1'                                                       
         UNPK  SVTPREC+65(11),DUB    IN REVENUE                                 
         MVC   FULL,SVTPREC+12     SAVE PREVIOUS INVOICE NUM                    
         MVC   SVTPREC+12(4),TPREC+12   NEW INVOICE NUMBER                      
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW30                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
*                                                                               
JW30     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVC   SVTPREC+12(4),FULL  RESTORE PREVIOUS INV NUM                     
*                                  3)TPREC (AOR BILL) AS TYPE 4                 
         MVI   TPREC+00,C'4'       SET BILL TYPE                                
         ZAP   DUB,SVB.AMTAC       PREVIOUS BILL COMM                           
         AP    DUB,AMTACT          PLUS AOR ACTUAL = RESULTING COMMIS.          
         UNPK  TPREC+87(9),DUB    IN SPECIAL AOR FIELD                          
         MVC   TPREC+34(11),=11C'0' CLEAR OTHERS                                
         MVC   TPREC+54(11),=11C'0'                                             
         MVC   TPREC+65(11),=11C'0'                                             
         MVC   TPREC+5(3),=C'088'  SET PRODUCT AS 088                           
*                                                                               
         CLI   QMED,C'N'           FOR NETPAK                                   
         BNE   *+18                                                             
         CLI   BLMED,C'O'          AND SUB-MED OF O                             
         BNE   *+10                                                             
         MVC   TPREC+10(2),=C'U '  SPECIAL MEDIA CODE                           
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW40                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,TPREC,P,(R0),=C'N'                                   
         BRAS  RE,PRNT                                                          
*                                                                               
JW40     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         B     JW400        ???  IF AOR SITUATION, SKIP COMM ONLY CODE          
*                                                                               
JW44     DS    0H                  SPECIAL FOR CLIENT OA TYPE POSTS             
*                                  TYPE 1                                       
         MVI   TPREC+00,C'1'       SET BILL TYPE                                
         UNPK  TPREC+34(11),AMTACT AOR ACTUAL IN GROSS                          
         UNPK  TPREC+65(11),AMTACT AND REVENUE                                  
         MVC   TPREC+54(11),=11C'0'  CLEAR COST                                 
         MVC   TPREC+5(3),=C'088'  SET PRODUCT AS 088                           
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW45                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,TPREC,P,(R0),=C'N'                                   
         BRAS  RE,PRNT                                                          
JW45     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
         MVI   SVTPREC+00,C'5'     DO A TYPE 4 NOW                              
         UNPK  SVTPREC+34(11),BNETP  NET (OF SAVED BILLREC) IN GROSS            
         UNPK  SVTPREC+54(11),BNETP  AND COST                                   
         MVC   SVTPREC+65(11),=11C'0' CLEAR REVENUE                             
*                                                                               
         CLI   QMED,C'N'           FOR NETPAK                                   
         BNE   *+18                                                             
         CLI   BLMED,C'O'          AND SUB-MED OF O                             
         BNE   *+10                                                             
         MVC   TPREC+10(2),=C'U '  SPECIAL MEDIA CODE                           
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW47                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
*                                                                               
JW47     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                                                               
         MVI   SVTPREC+00,C'4'     DO A TYPE 4 NOW                              
         ZAP   DUB,BNETP           NET (OF SAVED BILL RECORD)                   
         MP    DUB,=P'-1'          COMPLEMENT                                   
         UNPK  SVTPREC+34(11),DUB    IN GROSS                                   
         UNPK  SVTPREC+54(11),DUB    AND COST                                   
         MVC   SVTPREC+65(11),=11C'0' CLEAR REVENUE                             
*                                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW48                                                             
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
*                                                                               
JW48     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     JW400        ???  IF AOR SITUATION, SKIP COMM ONLY CODE          
*                                                                               
JW200    DS    0H                  THIS BILL NOT AOR                            
         TM    BILSTAT,BSTCMONQ    WAS LAST BILL COMM ONLY                      
         BNZ   JW210               YES, SPECIAL TREATMENT                       
         TM    BILSTAT-BILLREC(R8),BSTTAORQ NO, JUST WRITE IT AS TYPE 1         
         BNZ   JW400               UNLESS THIS BILL IS AOR                      
         CLI   JWOASW,C'Y'         BUT FOR CLIENT OA TYPE POSTS                 
         BE    JW230               DO A TYPE 5, NOT TYPE 1                      
         B     JW280                                                            
*                                  YES, WRITE 2 RECORDS                         
JW210    DS    0H                  1)SAVED REC (ORIG BILL) AS TYPE 1            
         MVI   SVTPREC+00,C'1'     SET BILL TYPE                                
         UNPK  SVTPREC+34(11),SVB.AMTACT SET ACTUAL IN GROSS                    
         UNPK  SVTPREC+65(11),SVB.AMTACT AND REVENUE                            
         MVC   SVTPREC+54(11),=11C'0' CLEAR COST                                
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW220                                                            
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
         DROP  SVB                                                              
         DROP  R3                                                               
*                                                                               
JW220    DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                                                               
         CLI   JWOASW,C'Y'         SKIP 5 POSTING FOR COMM ONLY                 
         BNE   JW400               UNLESS OA SPECIAL                            
*                                  2)SVTPREC AS TYPE 5 WITH NET                 
JW230    DS    0H                                                               
         MVI   SVTPREC+00,C'5'     SET BILL TYPE                                
         UNPK  SVTPREC+34(11),BNETP  NET (OF SAVED BILLREC) IN GROSS            
         UNPK  SVTPREC+54(11),BNETP  AND COST                                   
         MVC   SVTPREC+65(11),=11C'0' CLEAR REVENUE                             
*                                                                               
JW280    DS    0H                                                               
         CLI   QOPT3,C'Y'          TEST TO DUMP RECORDS                         
         BNE   JW282                                                            
         LH    R0,RECLEN                                                        
         GOTO1 HEXOUT,DMCB,SVTPREC,P,(R0),=C'N'                                 
         BRAS  RE,PRNT                                                          
*                                                                               
JW282    DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,SVTPREC                                                       
         PUT   (1),(0)                                                          
*                                                                               
JW400    DS    0H                                                               
JWPX     DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        AMMARATTI AND PURIS SPECIAL                                            
*        ALSO USED FOR GSDM AND MULLEN M$                                       
*                                                                               
PUPROC   NMOD1 0,**AMPU**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         MVC   TPREC+174(2),=C'NE'                                              
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+10                                                             
         MVC   TPREC+174(2),=C'SP'                                              
*                                                                               
         MVC   TPREC+176(2),BDATE+2   RUN DATE MONTH                            
*                                                                               
         OC    BILPOST,BILPOST     DATE POSTED                                  
         BZ    PUP4                                                             
         GOTO1 DATCON,DMCB,(2,BILPOST),(X'20',WORK)                             
         MVC   TPREC+41(2),WORK+2  MONTH                                        
         MVC   TPREC+43(2),WORK    YEAR                                         
*                                                                               
PUP4     DS    0H                                                               
         MVC   WORK(4),BMONSERV                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,WORK,WORK+6,35                                        
         SR    R0,R0                                                            
         ICM   R0,1,BCALNDR                                                     
         BNZ   *+8                                                              
         LHI   R0,2                DEFAULT TO CALENDAR MONTHS                   
         GOTO1 MOBILE,DMCB,(1,WORK),((R0),WORK+20)                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK+20),(X'20',WORK)                             
         MVC   TPREC+128(4),WORK+2 MMDD                                         
         MVC   TPREC+132(2),WORK    YY                                          
         GOTO1 DATCON,DMCB,(2,WORK+22),(X'20',WORK)                             
         MVC   TPREC+134(4),WORK+2 MMDD                                         
         MVC   TPREC+138(2),WORK    YY                                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
SCIPROC  NMOD1 0,**SCIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         OC    SCLMOS,SCLMOS  DO I HAVE ONE YET?                                
         BNZ   SCIP1                                                            
         MVC   SCLMOS,BKEYYSRV   SAVE THIS MOS AS LOWEST START MOS              
         MVC   SCHMOS,BKEYYSRV   AND AS HIGEST END  MOS                         
         B     SCIP1X                                                           
*                                                                               
SCIP1    CLC   BKEYYSRV,SCLMOS                                                  
         BNL   *+10                                                             
         MVC   SCLMOS,BKEYYSRV  IF LOWER, SAVE IT                               
         CLC   BKEYYSRV,SCHMOS                                                  
         BNH   *+10                                                             
         MVC   SCHMOS,BKEYYSRV  IF HIGHER, SAVE IT                              
SCIP1X   DS    0H                                                               
*                                                                               
         CLC   QAGY,=C'BN'      PHDNY                                           
         BE    SCIP4BN          THEY WILL USE EST UDEF                          
*                                                                               
         CLC   QAGY,=C'OO'      OPHDNA                                          
         BE    SCIP4BN          THEY WILL USE EST UDEF - LIKE PHDNY             
*                                                                               
*        CODE BELOW WAS FOR MINDSHARE (H7) AND SJR                              
*                                                                               
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEY2,KEY            SAVE KEY                                     
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO SPOT                                 
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO NET                                  
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         MVC   UCSEST,BKEYEST    ESTIMATE                                       
         OI    UCOPT,UCOEST      RETURN ESTIMATE UCOMMS                         
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   SCIP4        ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    SCIP4        NO EST DATA                                         
*                                                                               
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,TPREC+10    START OF 1ST EST UCOMM                            
         LHI   R0,1           JUST ONE                                          
SCIP2    CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(08,RE),0(R1)  ONLY 8 CHARACTERS                                
         OC    0(08,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,SCIP2                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
SCIP4    MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         B     SCIP4X                                                           
*                                  BELOW FOR BN                                 
SCIP4BN  LA    R4,WORK                                                          
         L     RF,ADEST                                                         
         MVC   WORK(8),=C'00000000'                                             
         LA    R5,EUSER1-ESTHDR(RF)                                             
         LHI   R3,8                                                             
*                                                                               
SCIP4BN2 DS    0H                                                               
         CLI   0(R5),C'0'          CHECK FOR NUMERICS                           
         BL    *+12                                                             
         LA    R5,1(R5)            NEXT POS                                     
         BCT   R3,SCIP4BN2                                                      
*                                                                               
         LCR   R3,R3                                                            
         AHI   R3,7                R3 HAS LENGTH - 1                            
         BM    SCIP4BN4                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,EUSER1-ESTHDR(0,RF)                                          
         UNPK  WORK(8),DUB                                                      
         MVC   TPREC+10(8),WORK                                                 
         B     SCIP4X                                                           
*                                                                               
SCIP4BN4 DS    0H                                                               
         MVC   TPREC+10(8),SPACES     NO VALID URN #                            
*                                                                               
SCIP4X   MVC   TPREC+21(2),DINVNO      MTH PART OF INV #                        
         MVC   TPREC+23(2),DINVMED     MEDIA PART OF INV #                      
         MVC   TPREC+25(4),DINVNO+2                                             
         GOTO1 DATCON,DMCB,BDATE,(20,TPREC+45)    BILL DATE                     
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,TPREC+53) DUE DATE                  
*                                                                               
         MVC   TPREC+61(3),BKEYPRD                                              
         MVC   WORK(2),TPREC+64            YYMM FROM SPSPECS                    
         MVC   WORK+4(2),=C'01'            DAY TO 01                            
         MVC   WORK+2(2),TPREC+66          MM                                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   TPREC+67(4),WORK+6      YYYY                                     
         GOTO1 DATCON,DMCB,(0,WORK),(4,WORK+6)                                  
         MVC   TPREC+64(3),WORK+6      MMM                                      
*                                                                               
         MVC   TPREC+71(2),DINVMED    MEDIA PART OF INV. NO.                    
         CLI   NETPAKSW,C'Y'                                                    
         BNE   SCIP5                  USE IT FOR SPOT                           
         MVC   TPREC+71(2),=C'NT'                                               
         CLI   BLMED,C'N'                                                       
         BE    SCIP5                                                            
         CLI   BLMED,C' '              NOT PRESENT                              
         BNH   SCIP5                                                            
         MVC   TPREC+71(2),=C'CA'      CABLE                                    
         CLI   BLMED,C'C'                                                       
         BE    SCIP5                                                            
         MVC   TPREC+71(2),=C'SY'      SYNDICATION                              
         CLI   BLMED,C'S'                                                       
         BE    SCIP5                                                            
         MVC   TPREC+71(2),=C'ND'      NETWORK RADIO                            
         CLI   BLMED,C'D'                                                       
         BE    SCIP5                                                            
         MVC   TPREC+71(2),=C'NO'      OTHER                                    
         CLI   BLMED,C'O'                                                       
         BE    SCIP5                                                            
*                                     FOR OTHER NETWORK MEDIA                   
         MVC   TPREC+71(2),DINVMED    USE MEDIA PART OF INV. NO.                
SCIP5    DS    0H                                                               
         DROP  R7                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
MZIPROC  NMOD1 0,**MZIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
*        AT THIS POINT TPREC+159 HAS WHOLE EST USER1                            
*                                                                               
         MVC   TPREC+169(5),TPREC+164  INTERNAL# PART OF EST USER1              
         MVC   TPREC+163(6),SPACES     CLEAR OUT OF COST CENTER FIELD           
*                                                                               
         CLC   QAGY,=C'JS'      SEE IF JACTAK                                   
         BNE   *+10                                                             
         MVC   TPREC+223(4),BLMGR        MARKET GROUP = MAZDA MARKET            
*                                                                               
         MVC   TPREC+364(L'DINVFULL),DINVFULL                                   
*                                                                               
         MVC   WORK(4),TPREC+273      YYMM  FROM MZSPECS                        
         MVC   WORK+4(2),=C'01'       SET DAY                                   
         GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+6)                                  
*                                     AND FLOAT IT PAST MEDIA                   
*                                                                               
         MVC   TPREC+273(13),=C'SPOT TV MEDIA'                                  
         LA    RE,TPREC+288                                                     
         CLI   QMED,C'T'                                                        
         BE    MZIP5                                                            
         MVC   TPREC+273(16),=C'SPOT RADIO MEDIA'                               
         LA    RE,TPREC+291                                                     
         CLI   QMED,C'R'                                                        
         BE    MZIP5                                                            
         CLI   NETPAKSW,C'Y'                                                    
         BNE   MZIP5                                                            
         MVC   TPREC+273(16),=C'NETWORK TV MEDIA'                               
         LA    RE,TPREC+291                                                     
         CLI   BLMED,C'N'                                                       
         BE    MZIP5                                                            
         CLI   BLMED,C' '              NOT PRESENT                              
         BNH   MZIP5                                                            
         MVC   TPREC+273(25),SPACES                                             
         MVC   TPREC+273(14),=C'CABLE TV MEDIA'                                 
         LA    RE,TPREC+289                                                     
         CLI   BLMED,C'C'                                                       
         BE    MZIP5                                                            
         MVC   TPREC+273(25),SPACES                                             
         MVC   TPREC+273(11),=C'SYNDICATION'                                    
         LA    RE,TPREC+286                                                     
         CLI   BLMED,C'S'                                                       
         BE    MZIP5                                                            
         MVC   TPREC+273(13),=C'NETWORK RADIO'                                  
         LA    RE,TPREC+288                                                     
         MVC   TPREC+288(6),WORK+6                                              
         CLI   BLMED,C'D'                                                       
         BE    MZIP5                                                            
         MVC   TPREC+273(13),=C'NETWORK OTHER'                                  
         LA    RE,TPREC+288                                                     
         MVC   TPREC+288(6),WORK+6                                              
         CLI   BLMED,C'O'                                                       
         BE    MZIP5                                                            
*                                     FOR OTHER NETWORK MEDIA                   
MZIP5    DS    0H                                                               
         MVC   0(6,RE),WORK+6      FLOAT MOS AFTER MEDIA                        
         DROP  R7                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*        DIAGEO INTERFACE - CARAT TYPE D                                        
DGIPROC  NMOD1 0,**DGIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CP    BACTP,=P'0'        SKIP ZERO INVOICES                            
         BE    DGSKIP                                                           
*                                                                               
         LA    R2,UCOMBLK                SET-UP UCOM CONTROL BLOCK              
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R2                                                       
         MVC   KEY2,KEY         SAVE KEY                                        
         MVC   UCACOMF,ACOMFACS    COMFACS                                      
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         OI    UCOPT,UCOPRD      RETURN PRODUCT UCOMMS                          
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   DGPRERR      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOPRD                                                  
         BO    DGPRERR      NO PRD DATA                                         
*                                                                               
         L     R1,UCPDATA     PRD DATA                                          
         LA    RF,UCPLENS     LENGTHS                                           
         LA    RE,TPREC+300   SAVE DATA HERE                                    
         LHI   R0,3           JUST FIRST 3 USED                                 
DGI05    CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    DGPRERR        MISSING UCOMM                                     
         MVC   0(32,RE),0(R1)                                                   
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RE,32(RE)      NEXT SAVE LOCATION                                
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,DGI05                                                         
         B     DGI05X                                                           
*                                                                               
DGPRERR  DS    0H                                                               
         MVC   P(42),=C'*** MISSING UCOMM DATA FOR PRODUCT     ***'             
         MVC   P+35(3),BKEYPRD                                                  
         MVC   P2(23),=C'*** INVOICE SKIPPED ***'                               
         OI    SKIPBILL,X'80'      SO IT WON'T GET ON TAPE                      
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         MVI   DGERRSW,C'Y'        SET ERRORS ENCOUNTERED                       
DGSKIP   MVI   DGISKIP,C'Y'                                                     
         OI    SKIPBILL,X'80'      SO IT WON'T GET ON TAPE                      
         B     DGX                                                              
*                                  I NEED THE FIRST 3                           
*                                                                               
DGI05X   MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         GOTO1 DATCON,DMCB,TODAY,(20,TPREC+8)   YYYYMMDD                        
         MVC   TPREC+16(2),TIMEOFD         WITHOUT .'S                          
         MVC   TPREC+18(2),TIMEOFD+3                                            
         MVC   TPREC+20(2),TIMEOFD+6                                            
         B     DGX                                                              
*                                                                               
         DROP  R7                                                               
         DROP  R2                                                               
DGX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*        GM LMA (LOCAL DEALERS) AGENCY CARAT                                    
GMIPROC  NMOD1 0,**GMIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CLC   TPREC(12),SPACES                                                 
         BH    GMPR10                                                           
*                                                                               
         MVC   P(39),=C'*** MISSING UCOMM DATA FOR ESTIMATE ***'                
         MVC   P2(23),=C'*** INVOICE SKIPPED ***'                               
         OI    SKIPBILL,X'80'      SO IT WON'T GET ON TAPE                      
         BRAS  RE,PRNT                                                          
         B     GMX                                                              
*                                                                               
GMPR10   GOTO1 DATCON,DMCB,BQDATE,(20,TPREC+22) INVOICE DATE CCYYMMDD           
                                                                                
         MVC   WORK(2),BKEYYSRV               MONTH OF SERVICE CCYYMM           
         MVI   WORK+2,X'01'     DAY TO 1                                        
         GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+3)                                 
         MVC   TPREC+30(6),WORK+3                                               
*                                                                               
         EDIT  BACTP,(16,TPREC+36),2,FILL=0,ZERO=NOBLANK                        
         BNE   *+12                                                             
         OI    SKIPBILL,X'80'      SO IT WON'T GET ON TAPE                      
         B     GMX                 SKIP ZERO INVOICE                            
         BH    *+8                                                              
         MVI   TPREC+36,C'-'           LEADING MINUS SIGN                       
*                                                                               
         LA    RE,TPREC+66             PREFIL EST NAME WITH ZEROES              
         LHI   R1,20                                                            
         LA    RF,1(RE)                                                         
         MVI   0(RE),C'0'                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         LA    RE,TPREC+118            PREFIL MED NAME WITH ZEROES              
         LHI   R1,39                                                            
         LA    RF,1(RE)                                                         
         MVI   0(RE),C'0'                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
***      MVC   TPREC+154(5),=C'RADIO'                                           
         MVC   TPREC+153(5),=C'RADIO'                                           
         CLI   QMED,C'R'                                                        
         BE    GMX                                                              
***      MVC   TPREC+150(9),=C'NET RADIO'                                       
         MVC   TPREC+149(9),=C'NET RADIO'                                       
         CLI   QMED,C'X'                                                        
         BE    GMX                                                              
***      MVC   TPREC+149(10),=C'TELEVISION'                                     
         MVC   TPREC+148(10),=C'TELEVISION'                                     
         CLI   QMED,C'T'                                                        
         BE    GMX                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R7                                                               
GMX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*        CARAT - SMUCKERS - SPEC-24974                                          
*                                                                               
SMIPROC  NMOD1 0,**SMIP**          SMUCKERS ROUTINE                             
*                                                                               
         LA    RC,SPACEND          SPBTWRKD WORK AREA                           
*                                                                               
         CLI   MODE,RUNLAST        MODE RUNLAST?                                
         BNE   SMI05               NO                                           
         LAY   R2,SMFTAB           SMUCKERS CXML FOOTER TABLE                   
         BAS   RE,SMTABLE          PROCESS HARDCODED SMUCKERS TABLE             
         B     SMIX                DONE                                         
*                                                                               
SMI05    CLI   SMHDSW,C'Y'         HEADER CREATED?                              
         BE    SMI10               YES - ONLY DONE ONCE                         
*                                                                               
         LA    R2,SMDSTAMP         UPDATE DATESTAMP YYYY-MM-DD                  
         MVC   0(4,R2),CTODAY      YYYY                                         
         MVC   5(2,R2),CTODAY+4    MM                                           
         MVC   8(2,R2),CTODAY+6    DD                                           
*                                                                               
         LA    R2,SMTSTAMP         UPDATE TIMSTAMP HH:MM:SS                     
         MVC   0(2,R2),TIMEOFD     HH                                           
         MVC   3(2,R2),TIMEOFD+3   MM                                           
         MVC   6(2,R2),TIMEOFD+6   SS                                           
*                                                                               
         LA    R2,SMHTAB           SMUCKERS CXML HEADER TABLE                   
         BAS   RE,SMTABLE          PROCESS HARDCODED SMUCKERS TABLE             
         MVI   SMHDSW,C'Y'         FLAG HEADER CREATED                          
*                                                                               
SMI10    L     R7,ADBILL           A(BILL RECORD)                               
         USING BILLREC,R7          BILL RECORD DSECT                            
*                                                                               
         LA    R5,TPREC+200        R5 = TPREC+200                               
         USING SMUCKRSD,R5         SMUCKERS DSECT                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(20,WORK) YYYYMMDD                        
*                                                                               
         MVC   SMINVDT(4),WORK     YYYY                                         
         MVI   SMINVDT+4,C'-'      -                                            
         MVC   SMINVDT+5(2),WORK+4 MM                                           
         MVI   SMINVDT+7,C'-'      -                                            
         MVC   SMINVDT+8(2),WORK+6 DD                                           
*                                                                               
         MVC   WORK(2),BKEYYSRV    MOS YY/MM                                    
         MVI   WORK+2,X'01'        SET DAY TO 1                                 
         GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+3)                                 
*                                                                               
         MVC   SMINVMOS(4),WORK+3  YYYY                                         
         MVI   SMINVMOS+4,C'-'     -                                            
         MVC   SMINVMOS+5(2),WORK+7 MM                                          
         MVI   SMINVMOS+7,C'-'     -                                            
         MVC   SMINVMOS+8(2),WORK+9 DD                                          
*                                                                               
         MVC   SMINVNUM,DINVFULL+1 INVOICE NUMBER (L-83-5149)                   
*                                                                               
         LA    R3,BAMTS            BILL AMOUNTS                                 
         USING AMOUNTSD,R3         BILL AMOUNTS DSECT                           
         EDIT  AMTACT,SMBACT,2,FLOAT=-,ALIGN=LEFT,COMMAS=YES                    
         DROP  R3                  DROP BILL AMOUNTS USING                      
                                                                                
         MVC   SMIPURP(21),=C'standard">           '                            
         CLI   SMBACT,C'-'         NEGATIVE AMOUNT?                             
         BNE   *+10                NO                                           
         MVC   SMIPURP(21),=C'lineLevelCreditMemo">'                            
*                                                                               
         L     RF,ADEST            A(ESTIMATE RECORD)                           
         USING ESTHDRD,RF          ESTIMATE RECORD DSECT                        
         MVC   SMEDESC,EDESC       ESTIMATE DESCRIPTION                         
         DROP  RF                  DROP ESTIMATE RECORD USING                   
*                                                                               
         BAS   RE,SADDRESS         GET ADDRESS FOR SMUCKERS                     
*                                                                               
         MVC   SMIDAT(10),SMINVDT  INVOICE DATE (YYYY-MM-DD)                    
         MVC   SMINUM(9),SMINVNUM  INVOICE NUMBER (L-83-5149)                   
         DROP  R5                  DROP SMUCKERS DSECT                          
*                                                                               
         LA    R2,SMBTAB           SMUCKERS CXML BODY TABLE                     
         LA    R4,TPREC            R4 = TPREC                                   
         XR    R3,R3               CLEAR R3                                     
*                                                                               
SMI20    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SMIX                YES - DONE                                   
         BAS   RE,SMSPFLD          PROCESSED SPECIAL FIELD?                     
         BE    SMI20               YES, FIELD PROCESSED & R2 BUMPED             
         BAS   RE,CLRTPREC         INIT TPREC TO SPACES FOR 150 CHARS           
         IC    R3,1(R2)            LENGTH OF LITERAL                            
         BCTR  R3,0                DECREMENT FOR EX                             
         EX    R3,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R2)       MOVE LITERAL TO TPREC                        
*                                                                               
         L     R1,=A(SBITAPE)      A(SBITAPE)                                   
         LA    R0,TPREC            LINE WE'RE ADDING TO TAPE                    
         PUT   (1),(0)             ADD THE LINE TO TAPE                         
*                                                                               
         LA    R2,3(R3,R2)         BUMP TO NEXT ENTRY IN THE TABLE              
         B     SMI20               PROCESS NEXT TABLE ENTRY                     
*                                                                               
SMIX     XIT1                                                                   
         DROP  R7                  DROP BILL RECORD USING                       
*                                                                               
SMTABLE  NTR1                                                                   
*                                                                               
         LA    R4,TPREC            R4 = TPREC                                   
         XR    R3,R3               CLEAR R3                                     
*                                                                               
SMTAB05  CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SMIX                YES - DONE                                   
         BAS   RE,CLRTPREC         INIT TPREC TO SPACES FOR 150 CHARS           
         IC    R3,0(R2)            LENGTH OF LITERAL                            
         BCTR  R3,0                DECREMENT FOR EX                             
         EX    R3,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),1(R2)       MOVE LITERAL TO TPREC                        
*                                                                               
         L     R1,=A(SBITAPE)      A(SBITAPE)                                   
         LA    R0,TPREC            LINE WE'RE ADDING TO TAPE                    
         PUT   (1),(0)             ADD THE LINE TO TAPE                         
*                                                                               
         LA    R2,2(R3,R2)         BUMP TO NEXT TABLE ENTRY                     
         B     SMTAB05             PROCESS NEXT TABLE ENTRY                     
*                                                                               
CLRTPREC LR    R0,RE               SAVE OFF RE                                  
         LA    RE,TPREC            RE = TPREC                                   
         LHI   R1,150              LENGTH OF SMUCKERS FILELINE                  
         LA    RF,1(RE)            POINT TO TPREC+1                             
         MVI   0(RE),C' '          INIT SPACES                                  
         MOVE  ((RF),(R1)),(RE)    INIT 150 BYTES TO SPACES                     
         LR    RE,R0               RESTORE RE                                   
         BR    RE                  RETURN TO CALLER                             
*                                                                               
SMSPFLD  NTR1                                                                   
*                                                                               
         LAY   R3,SMSPCL           SMUCKERS SPECIAL FIELDS TABLE                
*                                                                               
SPFLD00  CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    SPFLDNEQ            YES - SET CC NEQ                             
         CLC   0(1,R2),0(R3)       IS THIS A SPECIAL FIELD?                     
         BE    SPFLD10             YES                                          
         LA    R3,3(R3)            BUMP TO NEXT TABLE ENTRY                     
         B     SPFLD00             PROCESS NEXT TABLE ENTRY                     
*                                                                               
SPFLD10  LA    R4,TPREC            R4 = TPREC                                   
         BAS   RE,CLRTPREC         INIT TPREC TO SPACES FOR 150 CHARS           
         LLC   RF,1(R2)            LENGTH OF LITERAL                            
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),2(R2)       MOVE LITERAL TO TPREC                        
         LA    R4,1(RF,R4)         MOVE SOFT DATA HERE                          
         LA    R2,3(RF,R2)         BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         LA    R5,TPREC+200        SPECIAL DATA BUILT HERE                      
         USING SMUCKRSD,R5         SMUCKERS DSECT                               
         LLC   RE,1(R3)            DISPLACEMENT INTO DATA                       
         LLC   RF,2(R3)            LENGTH OF DATA                               
         BCTR  RF,0                DECREMENT FOR EX                             
         AR    R5,RE               INDEX INTO DATA                              
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   0(0,R4),0(R5)       MOVE SOFT DATA                               
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         OC    0(0,R4),SPACES      SPACE PAD SOFT DATA                          
         LA    R4,1(RF,R4)         LAST BYTE OF SOFT DATA                       
*                                                                               
         CLI   0(R4),X'40'         LAST CHAR OF DATA?                           
         BH    *+8                 YES                                          
         BCT   R4,*-8              NO, CHECK PREVIOUS BYTE                      
*                                                                               
         LLC   RF,1(R2)            LENGTH OF LITERAL                            
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,*+8              EXECUTE THE MVC                              
         B     *+10                BRANCH OVER MVC                              
         MVC   1(0,R4),2(R2)       MOVE LITERAL TO TPREC                        
         LA    R2,3(RF,R2)         BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         L     R1,=A(SBITAPE)      A(SBITAPE)                                   
         LA    R0,TPREC            LINE WE'RE ADDING TO TAPE                    
         PUT   (1),(0)             ADD THE LINE TO TAPE                         
*                                                                               
         CR    RE,RE               SET CC EQU                                   
         B     SPFLDXIT            AND EXIT                                     
*                                                                               
SPFLDNEQ LTR   RE,RE               SET CC NEQ                                   
*                                                                               
SPFLDXIT XIT1  REGS=(R2)           EXIT BUT KEEP R2 INTACT                      
*                                                                               
SADDRESS NTR1                      GET ADDRESS FOR SMUCKERS                     
*                                                                               
         USING SMUCKRSD,R5         SMUCKERS DSECT                               
         XC    SMADD,SMADD         CLEAR ADDRESS                                
         XC    SMCITY,SMCITY       CLEAR CITY                                   
         XC    SMSTATE,SMSTATE     CLEAR STATE                                  
         XC    SMZIP,SMZIP         CLEAR ZIP CODE                               
*                                                                               
         MVC   KEY1(13),KEY        SAVE BILLING KEY                             
         LA    R6,KEY              R6 = KEY                                     
         USING PRDHDRD,R6          PRODUCT RECORD DSECT                         
         XC    PKEY,PKEY           CLEAR THE PRODUCT KEY                        
         MVC   PKEYAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   PKEYCLT,BCLT        CLIENT                                       
         MVC   PKEYPRD,=C'AAA'     PRODUCT AAA                                  
*                                                                               
         L     RE,ADPRD            A(PRODUCT RECORD IN CORE)                    
         MVC   KEY2(13),0(RE)      SAVE PRODUCT KEY                             
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   PKEY,KEYSAVE        FOUND THE RECORD?                            
         BNE   SADDX               NO - PRD AAA NOT ON FILE                     
*                                                                               
         GOTO1 GETPRD              READ PRODUCT RECORD IN ADPRD                 
*                                                                               
SADD10   L     R6,ADPRD            A(PRODUCT RECORD IN CORE)                    
                                                                                
         LA    RE,SMADD            ADDRESS                                      
         CLC   PADDR1,SPACES       HAVE AN ADDRESS LINE 1?                      
         BNH   SADD20              NO                                           
         MVC   0(L'PADDR1,RE),PADDR1                                            
         AHI   RE,L'PADDR1         BUMP PAST ADDRESS LINE 1                     
         CLI   0(RE),X'40'         GREATER THAN A SPACE?                        
         BH    *+8                 YES                                          
         BCT   RE,*-8              FIND LAST CHAR OF ADDRESS LINE 1             
         LA    RE,2(RE)            ADDRESS LINE 2 GOES HERE                     
*                                                                               
SADD20   CLC   PADDR2,SPACES       HAVE AN ADDRESS LINE 2?                      
         BNH   SADD30              NO                                           
         MVC   0(L'PADDR1,RE),PADDR2                                            
*                                                                               
SADD30   LA    RE,PADDR3           FIND END OF CITY                             
         LA    RF,L'PADDR3         DON'T GO PAST END                            
         LR    R1,RF               TO CALCULATE LENGTH OF CITY                  
*                                                                               
SADD40   CLI   0(RE),C','          COMMA?                                       
         BE    SADD50              YES - CITY ALWAYS FOLLOWED BY COMMA          
         LA    RE,1(RE)            BUMP TO NEXT BYTE                            
         BCT   RF,SADD40           CHECK NEXT BYTE                              
         B     SADDX               CANNOT FIND CITY/STATE/ZIP                   
*                                                                               
SADD50   SR    R1,RF               LENGTH OF CITY                               
         BCTR  R1,0                -1 FOR EX                                    
         EX    R1,*+8              EXECUTE MVC                                  
         B     *+10                SO IDF DOESN'T COMPLAIN                      
         MVC   SMCITY(0),PADDR3    CITY                                         
*                                                                               
         LA    RE,1(RE)            BUMP PAST COMMA                              
         BCTR  RF,0                DECREMENT REMAINING LENGTH                   
*                                                                               
SADD60   CLI   0(RE),C' '          FIRST BYTE OF STATE?                         
         BH    SADD70              YES                                          
         LA    RE,1(RE)            BUMP TO NEXT BYTE                            
         BCT   RF,SADD60           CHECK NEXT BYTE                              
         B     SADDX               CANNOT FIND STATE/ZIP                        
*                                                                               
SADD70   MVC   SMSTATE,0(RE)       STATE                                        
         LA    RE,2(RE)            BUMP PAST STATE                              
         SHI   RF,2                DECREMENT REMAINING LENGTH                   
*                                                                               
SADD80   CLI   0(RE),C' '          FIRST BYTE OF ZIP?                           
         BH    SADD90              YES                                          
         LA    RE,1(RE)            BUMP TO NEXT BYTE                            
         BCT   RF,SADD80           CHECK NEXT BYTE                              
         B     SADDX               CANNOT FIND ZIP                              
*                                                                               
SADD90   MVC   SMZIP,0(RE)         ZIP CODE IS 10 DIGITS                        
*                                                                               
SADDX    MVC   KEY(13),KEY2        RESTORE PRODUCT KEY                          
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND THE PRODUCT KEY?                       
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING WENT WRONG                    
*                                                                               
         GOTO1 GETPRD              READ PRODUCT RECORD IN ADPRD                 
*                                                                               
         MVC   KEY(13),KEY1        RESTORE BILL KEY                             
*                                                                               
         GOTO1 HIGH                RESTORE SEQUENCE                             
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND THE BILL KEY?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - SOMETHING WENT WRONG                    
*                                                                               
         B     SMIX                DONE                                         
         DROP  R6                  DROP PRODUCT RECORD USING                    
*                                                                               
         LTORG                                                                  
***                                                                             
* SMUCKERS XML HEADER TABLE                                                     
* FORMAT IS XL1              TABLE ENTRY LENGTH                                 
*           CL(ENTRY LENGTH) TABLE DATA                                         
***                                                                             
SMHTAB   DC    X'26',C'<?xml version="1.0" encoding="UTF-8"?>'                  
         DC    X'53',C'<!DOCTYPE cXML SYSTEM "http://xml.cxml.org/sche'         
         DC          C'mas/cXML/1.2.023/InvoiceDetail.dtd">'                    
         DC    X'63',C'<cXML payloadID="28642627BGS20180208T18:55" tim'         
         DC          C'estamp="'                                                
SMDSTAMP DC          C'2018-02-08T'                                             
SMTSTAMP DC          C'17:44:46-05:00" version="1.2.023"'                       
         DC    X'11',C'xml:lang="en-US">'                                       
         DC    X'08',C'<Header>'                                                
         DC    X'06',C'<From>'                                                  
         DC    X'1F',C'<Credential domain="NetworkID">'                         
         DC    X'23',C'<Identity> AN01001348934</Identity>'                     
         DC    X'0D',C'</Credential>'                                           
         DC    X'07',C'</From>'                                                 
         DC    X'04',C'<To>'                                                    
         DC    X'1F',C'<Credential domain="NetworkID">'                         
         DC    X'22',C'<Identity>AN01014693236</Identity>'                      
         DC    X'0D',C'</Credential>'                                           
         DC    X'05',C'</To>'                                                   
         DC    X'08',C'<Sender>'                                                
         DC    X'1F',C'<Credential domain="NetworkID">'                         
         DC    X'22',C'<Identity>AN01001348934</Identity>'                      
         DC    X'2B',C'<SharedSecret>BBSharedSecret</SharedSecret>'             
         DC    X'0D',C'</Credential>'                                           
         DC    X'29',C'<UserAgent>Ariba Network V1.1</UserAgent>'               
         DC    X'09',C'</Sender>'                                               
         DC    X'09',C'</Header>'                                               
         DC    X'25',C'<Request deploymentMode="production">'                   
         DC    X'FF'                                                            
***                                                                             
* SMUCKERS XML INVOICE DETAIL TABLE                                             
* FORMAT IS XL1              FIELD NUMBER (USED IN SMSPCL)                      
*           XL1              TABLE ENTRY LENGTH                                 
*           CL(ENTRY LENGTH) TABLE DATA                                         
***                                                                             
SMBTAB   DC    X'19',X'16',C'<InvoiceDetailRequest>'                            
         DC    X'1A',X'88',C'<InvoiceDetailRequestHeader invoiceDate="'         
SMIDAT   DC                C'2018-03-12T00:00:00-05:00" invoiceID="'            
SMINUM   DC                C'L-83-5149" operation="new" purpose="'              
SMIPURP  DC                C'lineLevelCreditMemo">'                             
         DC    X'1B',X'1F',C'<InvoiceDetailHeaderIndicator/>'                   
         DC    X'1C',X'1D',C'<InvoiceDetailLineIndicator/>'                     
         DC    X'1D',X'10',C'<InvoicePartner>'                                  
         DC    X'1E',X'37',C'<Contact addressID="BILL-TO-AP-DOMESTIC" '         
         DC                C'role="billTo">'                                    
         DC    X'1F',X'17',C'<Name xml:lang="en-US">'                           
         DC    X'20',X'07',C'</Name>'                                           
         DC    X'21',X'15',C'<PostalAddress name="'                             
         DC    X'22',X'02',C'">'                                                
         DC    X'23',X'08',C'<Street>'                                          
         DC    X'24',X'09',C'</Street>'                                         
         DC    X'25',X'06',C'<City>'                                            
         DC    X'26',X'07',C'</City>'                                           
         DC    X'27',X'07',C'<State>'                                           
         DC    X'28',X'08',C'</State>'                                          
         DC    X'29',X'0C',C'<PostalCode>'                                      
         DC    X'2A',X'0D',C'</PostalCode>'                                     
         DC    X'2B',X'34',C'<Country isoCountryCode="US">United State'         
         DC                C's</Country>'                                       
         DC    X'2C',X'10',C'</PostalAddress>'                                  
         DC    X'2D',X'13',C'<Phone name="work">'                               
         DC    X'2E',X'11',C'<TelephoneNumber>'                                 
         DC    X'2F',X'30',C'<CountryCode isoCountryCode="US">1</Count'         
         DC                C'ryCode>'                                           
         DC    X'30',X'21',C'<AreaOrCityCode></AreaOrCityCode>'                 
         DC    X'31',X'11',C'<Number></Number>'                                 
         DC    X'32',X'12',C'</TelephoneNumber>'                                
         DC    X'33',X'08',C'</Phone>'                                          
         DC    X'34',X'0A',C'</Contact>'                                        
         DC    X'35',X'11',C'</InvoicePartner>'                                 
         DC    X'36',X'10',C'<InvoicePartner>'                                  
         DC    X'37',X'37',C'<Contact addressID="BILL-TO-AP-DOMESTIC" '         
         DC                C'role="soldTo">'                                    
         DC    X'38',X'17',C'<Name xml:lang="en-US">'                           
         DC    X'39',X'07',C'</Name>'                                           
         DC    X'3A',X'15',C'<PostalAddress name="'                             
         DC    X'3B',X'02',C'">'                                                
         DC    X'3C',X'08',C'<Street>'                                          
         DC    X'3D',X'09',C'</Street>'                                         
         DC    X'3E',X'06',C'<City>'                                            
         DC    X'3F',X'07',C'</City>'                                           
         DC    X'40',X'07',C'<State>'                                           
         DC    X'41',X'08',C'</State>'                                          
         DC    X'42',X'0C',C'<PostalCode>'                                      
         DC    X'43',X'0D',C'</PostalCode>'                                     
         DC    X'44',X'34',C'<Country isoCountryCode="US">United State'         
         DC                C's</Country>'                                       
         DC    X'45',X'10',C'</PostalAddress>'                                  
         DC    X'46',X'13',C'<Phone name="work">'                               
         DC    X'47',X'11',C'<TelephoneNumber>'                                 
         DC    X'48',X'30',C'<CountryCode isoCountryCode="US">1</Count'         
         DC                C'ryCode>'                                           
         DC    X'49',X'21',C'<AreaOrCityCode></AreaOrCityCode>'                 
         DC    X'4A',X'11',C'<Number></Number>'                                 
         DC    X'4B',X'12',C'</TelephoneNumber>'                                
         DC    X'4C',X'08',C'</Phone>'                                          
         DC    X'4D',X'0A',C'</Contact>'                                        
         DC    X'4E',X'11',C'</InvoicePartner>'                                 
         DC    X'4F',X'10',C'<InvoicePartner>'                                  
         DC    X'50',X'2B',C'<Contact addressID="289479" role="remitTo'         
         DC                C'">'                                                
         DC    X'51',X'27',C'<Name xml:lang="en-US">Carat USA</Name>'           
         DC    X'52',X'0F',C'<PostalAddress>'                                   
         DC    X'53',X'1F',C'<Street>P.O. Box 28004</Street>'                   
         DC    X'54',X'15',C'<City>New York</City>'                             
         DC    X'55',X'11',C'<State>NY</State>'                                 
         DC    X'56',X'23',C'<PostalCode>10087-8004</PostalCode>'               
         DC    X'57',X'34',C'<Country isoCountryCode="US">United State'         
         DC                C's</Country>'                                       
         DC    X'58',X'10',C'</PostalAddress>'                                  
         DC    X'59',X'0A',C'</Contact>'                                        
         DC    X'5A',X'11',C'</InvoicePartner>'                                 
         DC    X'5B',X'17',C'<InvoiceDetailShipping>'                           
         DC    X'5C',X'14',C'<Contact addressID="'                              
         DC    X'5D',X'10',C'" role="shipTo">'                                  
         DC    X'5E',X'1E',C'<Name xml:lang="en-US"></Name>'                    
         DC    X'5F',X'2F',C'<PostalAddress name="SMUCKER SERVICES COM'         
         DC                C'PANY">'                                            
         DC    X'60',X'17',C'<DeliverTo></DeliverTo>'                           
         DC    X'61',X'11',C'<Street></Street>'                                 
         DC    X'62',X'09',C'<Street/>'                                         
         DC    X'63',X'0D',C'<City></City>'                                     
         DC    X'64',X'0F',C'<State></State>'                                   
         DC    X'65',X'19',C'<PostalCode></PostalCode>'                         
         DC    X'66',X'27',C'<Country isoCountryCode="US"></Country>'           
         DC    X'67',X'10',C'</PostalAddress>'                                  
         DC    X'68',X'0A',C'</Contact>'                                        
         DC    X'69',X'19',C'<Contact role="shipFrom">'                         
         DC    X'6A',X'24',C'<Name xml:lang="en">Carat USA</Name>'              
         DC    X'6B',X'0F',C'<PostalAddress>'                                   
         DC    X'6C',X'24',C'<Street>500 Woodward Avenue</Street>'              
         DC    X'6D',X'09',C'<Street/>'                                         
         DC    X'6E',X'14',C'<City>Detroit</City>'                              
         DC    X'6F',X'11',C'<State>MI</State>'                                 
         DC    X'70',X'23',C'<PostalCode>48226-3416</PostalCode>'               
         DC    X'71',X'34',C'<Country isoCountryCode="US">United State'         
         DC                C's</Country>'                                       
         DC    X'72',X'10',C'</PostalAddress>'                                  
         DC    X'73',X'0A',C'</Contact>'                                        
         DC    X'74',X'18',C'</InvoiceDetailShipping>'                          
         DC    X'75',X'0A',C'<Comments>'                                        
         DC    X'76',X'0C',C'<Attachment>'                                      
         DC    X'77',X'0B',C'<URL></URL>'                                       
         DC    X'78',X'0D',C'</Attachment>'                                     
         DC    X'79',X'0B',C'</Comments>'                                       
         DC    X'7A',X'1D',C'<Extrinsic name="invoicePDF">'                     
         DC    X'7B',X'0C',C'<Attachment>'                                      
         DC    X'7C',X'0B',C'<URL></URL>'                                       
         DC    X'7D',X'0D',C'</Attachment>'                                     
         DC    X'7E',X'0C',C'</Extrinsic>'                                      
         DC    X'7F',X'1D',C'</InvoiceDetailRequestHeader>'                     
         DC    X'80',X'14',C'<InvoiceDetailOrder>'                              
         DC    X'81',X'18',C'<InvoiceDetailOrderInfo>'                          
         DC    X'82',X'19',C'<OrderReference orderID="'                         
         DC    X'83',X'02',C'">'                                                
         DC    X'84',X'21',C'<DocumentReference payloadID=""/>'                 
         DC    X'85',X'11',C'</OrderReference>'                                 
         DC    X'86',X'19',C'</InvoiceDetailOrderInfo>'                         
         DC    X'87',X'33',C'<InvoiceDetailItem invoiceLineNumber="1" '         
         DC                C'quantity="'                                        
         DC    X'88',X'02',C'">'                                                
         DC    X'89',X'21',C'<UnitOfMeasure>EA</UnitOfMeasure>'                 
         DC    X'8A',X'0B',C'<UnitPrice>'                                       
         DC    X'8B',X'22',C'<Money currency="USD">1.00</Money>'                
         DC    X'8C',X'0C',C'</UnitPrice>'                                      
         DC    X'8D',X'2B',C'<InvoiceDetailItemReference lineNumber="1'         
         DC                C'">'                                                
         DC    X'8E',X'08',C'<ItemID>'                                          
         DC    X'8F',X'10',C'<SupplierPartID>'                                  
         DC    X'8F',X'11',C'</SupplierPartID>'                                 
         DC    X'90',X'09',C'</ItemID>'                                         
         DC    X'91',X'1B',C'<Description xml:lang="en">'                       
         DC    X'92',X'0E',C'</Description>'                                    
         DC    X'93',X'1D',C'</InvoiceDetailItemReference>'                     
         DC    X'94',X'10',C'<SubtotalAmount>'                                  
         DC    X'95',X'16',C'<Money currency="USD">'                            
         DC    X'96',X'08',C'</Money>'                                          
         DC    X'97',X'11',C'</SubtotalAmount>'                                 
         DC    X'98',X'0D',C'<GrossAmount>'                                     
         DC    X'99',X'16',C'<Money currency="USD">'                            
         DC    X'9A',X'08',C'</Money>'                                          
         DC    X'9B',X'0E',C'</GrossAmount>'                                    
         DC    X'9C',X'17',C'<InvoiceDetailDiscount>'                           
         DC    X'9D',X'17',C'<Money currency="USD"/>'                           
         DC    X'9E',X'18',C'</InvoiceDetailDiscount>'                          
         DC    X'9F',X'0B',C'<NetAmount>'                                       
         DC    X'A0',X'16',C'<Money currency="USD">'                            
         DC    X'A1',X'08',C'</Money>'                                          
         DC    X'A2',X'0C',C'</NetAmount>'                                      
         DC    X'A3',X'14',C'</InvoiceDetailItem>'                              
         DC    X'A4',X'15',C'</InvoiceDetailOrder>'                             
         DC    X'A5',X'16',C'<InvoiceDetailSummary>'                            
         DC    X'A6',X'10',C'<SubtotalAmount>'                                  
         DC    X'A7',X'16',C'<Money currency="USD">'                            
         DC    X'A8',X'08',C'</Money>'                                          
         DC    X'A9',X'11',C'</SubtotalAmount>'                                 
         DC    X'AA',X'05',C'<Tax>'                                             
         DC    X'AB',X'22',C'<Money currency="USD">0.00</Money>'                
         DC    X'AC',X'35',C'<Description xml:lang="en-US">total tax</'         
         DC                C'Description>'                                      
         DC    X'AD',X'1C',C'<TaxDetail category="sales">'                      
         DC    X'AE',X'0F',C'<TaxableAmount>'                                   
         DC    X'AF',X'22',C'<Money currency="USD">0.00</Money>'                
         DC    X'B0',X'10',C'</TaxableAmount>'                                  
         DC    X'B1',X'0B',C'<TaxAmount>'                                       
         DC    X'B2',X'22',C'<Money currency="USD">0.00</Money>'                
         DC    X'B3',X'0C',C'</TaxAmount>'                                      
         DC    X'B4',X'1F',C'<Description xml:lang="en-US"/>'                   
         DC    X'B5',X'0C',C'</TaxDetail>'                                      
         DC    X'B6',X'06',C'</Tax>'                                            
         DC    X'B7',X'10',C'<ShippingAmount>'                                  
         DC    X'B8',X'22',C'<Money currency="USD">0.00</Money>'                
         DC    X'B9',X'11',C'</ShippingAmount>'                                 
         DC    X'BA',X'0D',C'<GrossAmount>'                                     
         DC    X'BB',X'16',C'<Money currency="USD">'                            
         DC    X'BC',X'08',C'</Money>'                                          
         DC    X'BD',X'0E',C'</GrossAmount>'                                    
         DC    X'BE',X'0B',C'<NetAmount>'                                       
         DC    X'BF',X'16',C'<Money currency="USD">'                            
         DC    X'C0',X'08',C'</Money>'                                          
         DC    X'C1',X'0C',C'</NetAmount>'                                      
         DC    X'C2',X'0B',C'<DueAmount>'                                       
         DC    X'C3',X'16',C'<Money currency="USD">'                            
         DC    X'C4',X'08',C'</Money>'                                          
         DC    X'C5',X'0C',C'</DueAmount>'                                      
         DC    X'C6',X'17',C'</InvoiceDetailSummary>'                           
         DC    X'C7',X'17',C'</InvoiceDetailRequest>'                           
         DC    X'FF'                                                            
***                                                                             
* SMUCKERS XML FOOTER TABLE                                                     
* FORMAT IS XL1              TABLE ENTRY LENGTH                                 
*           CL(ENTRY LENGTH) TABLE DATA                                         
***                                                                             
SMFTAB   DC    X'0A',C'</Request>'                                              
         DC    X'07',C'</cXML>'                                                 
         DC    X'FF'                                                            
***                                                                             
* SMUCKERS XML INVOICE SPECIAL FIELDS TABLE                                     
* THIS MAPS AGAINST THE FIRST FIELD OF SMBTAB AND IF IT MATCHES                 
* WE INDEX INTO SMUCKRSD (BUILT IN TPREC+200) USING THE DISPLACEMENT            
* IN THE SECOND FIELD FOR A PRE-DEFINED LENGTH (3RD FIELD)                      
* WE DO THIS IN ORDER TO HANDLE DATA THAT WAS BOTH SOFT AND VARIABLE            
* IN LENGTH                                                                     
* FORMAT IS XL1              FIELD NUMBER (MATCHED AGAINST SMBTAB)              
*           AL1              DISPLACEMENT INTO SMUCKRSD IN TPREC+200            
*           AL(ENTRY LENGTH) MAX LENGTH OF SOFT DATA                            
***                                                                             
SMSPCL   DC    X'1F',AL1(SMCNAME-SMUCKRSD),AL1(L'SMCNAME)                       
         DC    X'21',AL1(SMCNAME-SMUCKRSD),AL1(L'SMCNAME)                       
         DC    X'23',AL1(SMADD-SMUCKRSD),AL1(L'SMADD)                           
         DC    X'25',AL1(SMCITY-SMUCKRSD),AL1(L'SMCITY)                         
         DC    X'27',AL1(SMSTATE-SMUCKRSD),AL1(L'SMSTATE)                       
         DC    X'29',AL1(SMZIP-SMUCKRSD),AL1(L'SMZIP)                           
         DC    X'38',AL1(SMCNAME-SMUCKRSD),AL1(L'SMCNAME)                       
         DC    X'3A',AL1(SMCNAME-SMUCKRSD),AL1(L'SMCNAME)                       
         DC    X'3C',AL1(SMADD-SMUCKRSD),AL1(L'SMADD)                           
         DC    X'3E',AL1(SMCITY-SMUCKRSD),AL1(L'SMCITY)                         
         DC    X'40',AL1(SMSTATE-SMUCKRSD),AL1(L'SMSTATE)                       
         DC    X'42',AL1(SMZIP-SMUCKRSD),AL1(L'SMZIP)                           
         DC    X'5C',AL1(SMEUDEF2-SMUCKRSD),AL1(L'SMEUDEF2)                     
         DC    X'82',AL1(SMEUDEF1-SMUCKRSD),AL1(L'SMEUDEF1)                     
         DC    X'87',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'91',AL1(SMEDESC-SMUCKRSD),AL1(L'SMEDESC)                       
         DC    X'95',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'99',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'A0',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'A7',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'BB',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'BF',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'C3',AL1(SMBACT-SMUCKRSD),AL1(L'SMBACT)                         
         DC    X'FF'                                                            
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*        PHILIPS - HAVAS (AGENCY FM)                                            
PHIPROC  NMOD1 0,**PHIP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         DROP  R7                                                               
PHX      DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*        STARCOM / PHILIP MORRIS SPEC                                           
*                                                                               
         DS    0H                                                               
PMPROC   NMOD1 0,**STPM**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         TM    BILSTAT,BSTTAORQ    'TRUE' AOR BILL?                             
         BO    PMX                 YES: IGNORE THIS ONE                         
*                                                                               
         XC    TPRECRDW,TPRECRDW   CLEAR RECORD DESCRIPTOR WORD                 
         MVC   TPRECRDW(2),=H'79'  VARIABLE RECORD LENGTH                       
*                                                                               
         MVC   TPREC(6),=C'310771' VENDOR CODE                                  
*                                                                               
         MVC   TPREC+6(2),=C'NT'   ASSUME NETPAK                                
         CLI   NETPAKSW,C'Y'                                                    
         BE    *+14                                                             
         MVI   TPREC+6,C'S'        IT'S SPOTPAK                                 
         MVC   TPREC+7(1),QMED     2ND CHARACTER IS SPOT MEDIA CODE             
*                                                                               
         MVI   TPREC+8,C'C'        TYPE                                         
*                                                                               
         MVC   TPREC+9(1),DINVFULL INVOICE NUMBER                               
         MVC   TPREC+10(7),DINVFULL+2                                           
*                                                                               
         L     RF,ADEST            HANDLE ESTIMATE FIELD                        
         USING ESTHDRD,RF                                                       
*&&DO                                                                           
         OC    EUSER1,EUSER1                                                    
         BNZ   *+6                                                              
         DC    H'0'                EUSER1 FIELD IS MANDATORY                    
*&&                                                                             
         CLI   EUSER1+4,C'X'       IGNORE THESE "X" ESTIMATES                   
         BE    PMX                                                              
*                                                                               
         MVI   TPREC+22,C'0'                                                    
         MVC   TPREC+23(1),EUSER1                                               
         MVI   TPREC+24,C'-'                                                    
         MVC   TPREC+25(2),TPREC+6                                              
         MVI   TPREC+27,C'-'                                                    
         MVC   TPREC+28(4),EUSER1                                               
         DROP  RF                                                               
*                                                                               
         MVC   TPREC+43(2),=C'50'  RECORD TYPE                                  
*                                                                               
         ZAP   TPREC+45(6),BACTP   CGA- CASH GROSS AMOUNT                       
         ZAP   TPREC+51(6),=P'0'   CDS- CASH DISCOUNT                           
         ZAP   TPREC+57(6),BNETP   NCA- NET COST AMOUNT                         
*                                                                               
         TM    BILSTAT,BSTCAORQ    CLIENT AOR BILL?                             
         BO    PM40                YES: LOOK FOR ASSOCIATED 'TRUE' BILL         
*                                                                               
         ZAP   TPREC+63(6),=P'0'                                                
         ZAP   TPREC+69(6),=P'0'                                                
*                                                                               
PM10     DS    0H                                                               
         CLI   QOPT3,C'Y'          DUMP TRACE OF RECORD?                        
         BNE   PM20                                                             
*                                                                               
         LH    R0,TPRECRDW                                                      
         GOTO1 PRNTBL,DMCB,0,TPRECRDW,C'DUMP',(R0),=C'1D'                       
*                                                                               
PM20     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPRECRDW                                                      
         PUT   (1),(0)                                                          
*                                                                               
         MVC   TPREC+43(2),=C'55'  RECORD TYPE                                  
*                                                                               
         L     R1,ADPRD                                                         
         MVC   TPREC+40(3),PACCT-PRDHDR(R1)  CLIENT/PRODUCT CODE                
*                                                                               
         CLI   QOPT3,C'Y'          DUMP TRACE OF RECORD?                        
         BNE   PM30                                                             
*                                                                               
         LH    R0,TPRECRDW                                                      
         GOTO1 PRNTBL,DMCB,0,TPRECRDW,C'DUMP',(R0),=C'1D'                       
*                                                                               
PM30     DS    0H                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPRECRDW                                                      
         PUT   (1),(0)                                                          
         B     PMX                                                              
*                                                                               
PM40     DS    0H                                                               
         MVC   DUB(6),BDATE                                                     
*                                                                               
         MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
PM50     DS    0H                                                               
         GOTO1 SEQ                                                              
         GOTO1 GETBILL                                                          
         CLC   KEY(11),WORK                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HIGH                                                             
*                                                                               
         CLC   DUB(6),BDATE                                                     
         BNE   PM50                                                             
*                                                                               
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    PM50                                                             
*                                                                               
         ZAP   TPREC+63(6),BACTP                                                
         ZAP   TPREC+69(6),TPREC+45(6)                                          
         SP    TPREC+69(6),TPREC+57(6)                                          
         AP    TPREC+69(6),TPREC+63(6)                                          
         MP    TPREC+69(6),=P'-1'                                               
         B     PM10                                                             
*                                                                               
PMX      DS    0H                                                               
         DROP  R7                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        MCCANN SPECIAL (ADWARE)                                                
*                                                                               
MCMPROC  NMOD1 0,**MCCNM*                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDRD,RF                                                       
*                                                                               
         CLC   QAGY,=C'B$'         BARKLEY?                                     
         BNE   MCMP5                                                            
*                                                                               
         MVI   TPREC+37,C'S'       S FOR SPOT                                   
         CLI   NETPAKSW,C'Y'                                                    
         BNE   MCMP40                                                           
         MVI   TPREC+37,C'K'       K FOR NET                                    
         B     MCMP40                                                           
*                                                                               
MCMP5    DS    0H                                                               
         CLI   QMED,C'N'           FOR NETWORK...                               
         BNE   MCMP10                                                           
         CLI   COFFICE,C'S'        ...IF CLIENT IS COKE...                      
         BE    *+12                                                             
         CLI   COFFICE,C'J'        ...OR J&J...                                 
         BNE   MCMP40                                                           
         MVI   TPREC+37,C'O'       ...THEN "SYSTEM CODE" IS 'O'                 
         B     MCMP40                                                           
*                                                                               
MCMP10   DS    0H                                                               
         CLI   COFFICE,C'G'        IF SPOT, AND CLIENT IS GM...                 
         BNE   MCMP40                                                           
         MVI   TPREC+37,C'C'       ...THEN "SYSTEM CODE" IS 'C'                 
         DROP  RF                                                               
*                                                                               
MCMP40   DS    0H                                                               
         CLI   QOPT2,C'C'          CHRYSLER FORMAT (IPG)                        
         BNE   MCMP50                                                           
*                                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEY2,KEY            SAVE KEY                                     
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         MVC   UCSEST,BKEYEST    ESTIMATE                                       
         OI    UCOPT,UCOEST      RETURN ESTIMATE UCOMMS                         
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   MCMP40X      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    MCMP40X      NO EST DATA                                         
*                                                                               
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,TPREC+301   START OF 1ST EST UCOMM                            
         LHI   R0,1           JUST ONE                                          
MCMP40C  CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,MCMP40C                                                       
*                                                                               
         DROP  RF                                                               
         DROP  R3                                                               
*                                                                               
MCMP40X  MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         B     MCMPX                                                            
*                                                                               
MCMP50   L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         CLI   QMED,C'N'           FOR NETWORK...                               
         BNE   MCMP60                                                           
         OC    BILWKNO,BILWKNO                                                  
         BZ    MCMPX                                                            
         ZIC   RE,BILWKNO          PUT IN WEEK# IF IT'S THERE                   
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPREC+301(2),DUB+6(2)                                            
         B     MCMPX                                                            
         DROP  RF                                                               
*                                                                               
MCMP60   L     RF,ADEST                                                         
         USING ESTHDRD,RF                                                       
         MVC   TPREC+301(20),EDESC                                              
         DROP  RF                                                               
*                                                                               
MCMPX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*        MCCANN SPECIAL (CHRYLSER)   TYPE=C                                     
*                                                                               
MCCPROC  NMOD1 0,**MCCHR*                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,ADCLT                                                         
         USING CLTHDRD,RF                                                       
*                                                                               
MCCP5    DS    0H                                                               
         CLI   QMED,C'N'           FOR NETWORK...                               
         BNE   MCCP10                                                           
         CLI   COFFICE,C'S'        ...IF CLIENT IS COKE...                      
         BE    *+12                                                             
         CLI   COFFICE,C'J'        ...OR J&J...                                 
         BNE   MCCP40                                                           
         MVI   TPREC+37,C'O'       ...THEN "SYSTEM CODE" IS 'O'                 
         B     MCCP40                                                           
*                                                                               
MCCP10   DS    0H                                                               
         CLI   COFFICE,C'G'        IF SPOT, AND CLIENT IS GM...                 
         BNE   MCCP40                                                           
         MVI   TPREC+37,C'C'       ...THEN "SYSTEM CODE" IS 'C'                 
         DROP  RF                                                               
*                                                                               
MCCP40   DS    0H                                                               
*                                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         LA    R3,BAMTS                                                         
         USING AMOUNTSD,R3                                                      
*                                                                               
         ZAP   TPREC+101(12),AMTACT                                             
         ZAP   TPREC+113(12),AMTNET                                             
         ZAP   TPREC+125(12),AMTAC                                              
         ZAP   TPREC+137(12),AMTGRS                                             
         ZAP   TPREC+186(12),AMTGRS                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEY2,KEY            SAVE KEY                                     
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         MVC   UCSEST,BKEYEST    ESTIMATE                                       
         OI    UCOPT,UCOEST      RETURN ESTIMATE UCOMMS                         
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   MCCP40X      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BO    MCCP40X      NO EST DATA                                         
*                                                                               
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,TPREC+301   START OF 1ST EST UCOMM                            
         LHI   R0,1           JUST ONE                                          
MCCP40C  CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,MCCP40C                                                       
*                                                                               
         DROP  RF                                                               
         DROP  R3                                                               
*                                                                               
MCCP40X  MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         B     MCCPX                                                            
*                                                                               
MCCPX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*        WILA SPECIAL                                                           
WTPROC   NMOD1 0,**WTPROC                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         MVI   TPREC+162,C' '                                                   
         CLI   QMED,C'N'                                                        
         BNE   WTP8                                                             
*                                                                               
         MVI   TPREC+162,C'1'                                                   
         CLI   BLMED,C'N'                                                       
         BE    WTP8                                                             
         CLI   BLMED,C' '                                                       
         BE    WTP8                                                             
         MVI   TPREC+162,C'2'                                                   
         CLI   BLMED,C'S'                                                       
         BE    WTP8                                                             
         MVI   TPREC+162,C'3'                                                   
         CLI   BLMED,C'C'                                                       
         BE    WTP8                                                             
*                                                                               
WTP8     DS    0H                                                               
         MVC   TPREC+163(09),=X'F0F0F0F0F0F0F0F0C0'  NO CLIENT $                
         MVC   TPREC+172(09),=X'F0F0F0F0F0F0F0F0C0'                             
         MVC   TPREC+181(09),=X'F0F0F0F0F0F0F0F0C0'                             
         TM    BILSTAT2,BSTC2Q     IF COS2 BILL DO CLIENT $                     
         BZ    WTP9                                                             
*                                                                               
         UNPK  TPREC+163(9),BACT2P ACTUAL                                       
         UNPK  TPREC+172(9),BNET2P NET                                          
         ICM   RF,15,BCCTAX        TAX                                          
         CVD   RF,DUB                                                           
         UNPK  TPREC+181(9),DUB                                                 
         MVI   TPREC+190,C'P'      SAY IT'S A PW BILL                           
*                                                                               
WTP9     DS    0H                                                               
         TM    BILSTAT,BSTTAORQ    IF TRUE AOR BILL                             
         BZ    *+14                                                             
         BRAS  RE,GETAOR                                                        
         MVC   TPREC+191(30),WORK  AOR NAME (LINE 1) IN WORK                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*        MINDSHARE - BURGER KING                                                
H7PROC   NMOD1 0,**H7PROC                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
*        BK FISCAL YEAR GOES IN COL 5-8                                         
*                                                                               
         MVC   WORK(1),BKEYYSRV         YEAR OF SERVICE                         
         MVC   WORK+1(1),BKEYMSRV       MONTH OF SERVICE                        
         MVI   WORK+2,X'01'             SET DAY TO 1                            
         GOTO1 DATCON,DMCB,(3,WORK),(20,WORK+6)                                 
         MVC   BKBYR(4),WORK+6     YEAR OF SERVICE                              
         MVC   BKBMTH,WORK+10                                                   
         MVC   BKFISCAL,WORK+6                                                  
*                                                                               
***      CLI   NETPAKSW,C'Y'       SEE IF NETPAK                                
***      BNE   H7P2                IF NOT SET FISCAL THE OLD WAY                
***                                NOW DO THE SAME FOR SPOT                     
*                                                                               
         CLC   WORK+6(4),=C'2011'                                               
         BNL   H7P5                IF IN 2011 OR LATER                          
*                                  LEAVE FISCAL YEAR ALONE                      
H7P2     CLC   WORK+10(2),=C'06'   FISCAL YEAR START JULY                       
         BNH   H7P5                                                             
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK+8),WORK+12,F'1'                            
         GOTO1 DATCON,DMCB,(0,WORK+12),(20,WORK+6)                              
         MVC   BKFISCAL,WORK+6       CCYY FOR NEXT YEAR                         
*                                                                               
H7P5     MVC   TPREC+4(4),BKFISCAL                                              
*                                                                               
         GOTO1 DATCON,DMCB,BQDATE,(20,WORK)         BILL DATE                   
         MVC   TPREC+8(4),WORK+4       MMDD                                     
         MVC   TPREC+12(4),WORK         YYYY                                    
*                                                                               
         MVC   TPREC+28(L'DINVFULL),DINVFULL                                    
*                                                                               
         EDIT  BACTP,(11,TPREC+47),2,FILL=0,ZERO=NOBLANK                        
*                                                                               
         ICM   R0,15,BTAXAMT-BILLREC(R7)                                        
         CVD   R0,MYDUB                                                         
         EDIT  MYDUB,(11,TPREC+58),2,FILL=0,ZERO=NOBLANK                        
*                                                                               
         MVI   TPREC+77,C'.'       DECIMAL POINT IN DISCOUNT FIELD              
*                                                                               
         MVC   TPREC+80(2),=C'DI'                                               
         CP    BACTP,=P'0'                                                      
         BNL   *+16                                                             
         MVC   TPREC+80(2),=C'CN'                                               
         MVC   TPREC+82(11),=C'CREDIT MEMO'                                     
*                                                                               
         MVC   BKACCT,SPACES     SET ACCOUNT AND COST CENTER TO SPACES          
         MVC   BKCOSTC,SPACES                                                   
         MVC   BKCOMACC,=C'0000000000'                                          
         MVC   BKCOMACC+5(5),=C'76523'  ACCOUNT FOR COMMISSION                  
*                                                                               
         MVC   BKMAR#,SPACES     ALSO MAR#                                      
*                                                                               
         LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEY2,KEY            SAVE KEY                                     
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         MVC   UCSEST,BKEYEST    ESTIMATE                                       
         OI    UCOPT,UCOEST      RETURN ESTIMATE UCOMMS                         
         CLI   QMED,C'N'         NETPAK                                         
         BE    H7PR7                                                            
         MVI   UCOPT,0                                                          
         OI    UCOPT,UCOMKT      NEED MARKET UCOMM FOR SPOT                     
         CLI   BLMKT,C' '        DO I HAVE A MARKET IN THE BILL?                
         BNH   H7PRNOM                                                          
         PACK  DUB,BLMKT                                                        
         CVB   R0,DUB                                                           
         STH   R0,UCMKT                                                         
*                                                                               
H7PR7    DS    0H                                                               
         GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   H7PR15E      ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCOPT,UCOEST       DID I ASK FOR ESTIMATE DATA?                  
         BZ    H7PR33                                                           
         TM    UCDATA,UCDNOEST                                                  
         BO    H7PR15E                                                          
*                                                                               
*   TRY AND MATCH LAST 2 DIGITS OF FISCAL TO FIRST 2 OF AN EST UCOMM            
*   AND USE ITS MARKET TO FIND ACCOUNT AND COST CENTER FROM BKTABLE             
*                                                                               
*   FORMAT OF ESTIMATE UCOMM SHOULD BE YY-80008001-1234567890                   
*   YY= LAST 2 DIGITS OF FISCAL YEAR, 8000=MEDIA CODE,                          
*   8001=MARKET, 1234567890=10 DIGIT NUMBER                                     
*                                                                               
*   LENGTH MUST BE 22                                                           
*                                                                               
*        NEW  UCOMM FORMAT FOR MAR#                                             
*        LENGTH MAY BE 22 0R 23                                                 
*        11 (FOR 2011) MAY BE SUFFIXED BY A B FOR NOV-DEC/2010                  
*        OR A C FOR JAN-DEC/2011                                                
*        THIS OF COURSE ALTERS DISPLACEMENT OF LATER DATA                       
*                                                                               
         MVC   WORK(4),=C'  --'                                                 
         MVC   WORK(2),BKFISCAL+2   LAST 2 DIGITS OF THE YEAR                   
         CLC   WORK(2),=C'11'    2011?                                          
         BNE   H7PR05                                                           
         MVI   WORK+2,C'C'       WORK=11C-                                      
         CLC   BKBYR,=C'2010'    MOS IN 2010?                                   
         BNE   H7PR05                                                           
         MVI   WORK+2,C'B'       WORK=11B-                                      
         CLC   BKBMTH,=C'11'     SEE IF NOV OR DEC                              
         BNL   H7PR05                                                           
         MVI   WORK+2,C'-'       WORK=11-                                       
*                                                                               
H7PR05   L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LHI   R0,4                                                             
*                                                                               
H7PR10   ST    R1,SAVER1      SAVE ADDRESS OF UCOMM ENTRY                       
         CLI   0(RF),22       CHECK DATA LENGTH -SHOULD BE AT LEAST 22          
         BL    H7PR30         ELSE IGNORE                                       
         CLC   WORK(3),0(R1)     DO FISCAL YEARS MATCH?                         
         BNE   H7PR30         NO - THEN SKIP                                    
         CLI   WORK+2,C'-'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)        BUMP R1 FOR 11B AND 11C                          
*                                                                               
         CLC   2(5,R1),=C'-8000' NEXT SHOULD BE MEDIA CODE                      
         BNE   H7PR30         SKIP IF NOT THIS                                  
*                                                                               
*        TRY AND FIND THE MARKET IN BKTABLE                                     
*                                                                               
         LA    RE,BKTABLE                                                       
H7PR15   CLC   7(4,R1),0(RE)                                                    
         BE    H7PR20                                                           
         CLC   0(2,RE),=X'FFFF'       END OF TABLE                              
         BE    H7PR15E                                                          
         LA    RE,24(RE)                                                        
         B     H7PR15                                                           
*                                                                               
H7PR15E  MVC   BKMAR#(09),=C'NOT FOUND'                                         
*                                                                               
         MVC   P+1(22),=C'VALID MAR# - NOT FOUND'                               
         BRAS  RE,PRNT                                                          
         B     H7PRX                                                            
*                                                                               
*                                                                               
H7PR20   MVC   BKACCT,4(RE)   SAVE ACCOUNT NUMBER                               
         MVC   BKCOSTC,14(RE)  AND COST CENTER                                  
*                                                                               
         MVC   BKMAR#(4),WORK                                                   
         LA    RE,BKMAR#+3                                                      
         CLI   WORK+2,C'-'       IF SO - THEN NO SUFFIX PRESENT                 
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
*                                                                               
******   MVC   BKMAR#(2),FISCAL+2                                               
*******  MVC   BKMAR#(3),0(R1)   FISCAL PLUS DASH FROM UCOMM                    
*******  MVC   BKMAR#+3(4),7(R1) MARKET NUMBER FROM UCOMM                       
*******  MVI   BKMAR#+7,C'-'                                                    
*******  MVC   BKMAR#+8(3),BKCOSTC+4   FIRST 3 DIGITS OF COST CENTER            
*******  MVI   BKMAR#+11,C'-'                                                   
*******  MVC   BKMAR#+12(10),12(R1)  10 DIGIT NUMBER FROM UCOMM                 
         MVC   0(4,RE),7(R1) MARKET NUMBER FROM UCOMM                           
         MVI   4(RE),C'-'                                                       
         MVC   5(3,RE),BKCOSTC+4   FIRST 3 DIGITS OF COST CENTER                
         MVI   8(RE),C'-'                                                       
         MVC   9(10,RE),12(R1)  10 DIGIT NUMBER FROM UCOMM                      
         B     H7PRX                                                            
*                                                                               
H7PR30   L     R1,SAVER1      ADDRESS OF LAST ENTRY CHECKED                     
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,H7PR10                                                        
         B     H7PR15E                                                          
*                                                                               
H7PR33   DS    0H                  FOR SPOT - CHECK FOR MARKET UCOMM            
         TM    UCOPT,UCOMKT        DID I LOOK FOR ANY?                          
         BZ    H7PRX                                                            
         TM    UCDATA,UCDNOMKT                                                  
         BO    H7PRNOM                                                          
*                                                                               
         MVC   WORK(4),=C'  --'                                                 
         MVC   WORK(2),BKFISCAL+2   LAST 2 DIGITS OF THE YEAR                   
         CLC   WORK(2),=C'11'    2011?                                          
         BNE   H7PR35                                                           
         MVI   WORK+2,C'C'       WORK=11C-                                      
         CLC   BKBYR,=C'2010'    MOS IN 2010?                                   
         BNE   H7PR35                                                           
         MVI   WORK+2,C'B'       WORK=11B-                                      
         CLC   BKBMTH,=C'11'     SEE IF NOV OR DEC                              
         BNL   H7PR35                                                           
         MVI   WORK+2,C'-'       WORK=11-                                       
*                                                                               
*                                                                               
H7PR35   L     R4,UCMDATA          POINT TO FIRST MARKET COMMENT                
         LA    RF,UCMLENS                                                       
         LHI   R0,4                                                             
*                                                                               
*                                                                               
H7PR35A  ST    R4,SAVER4      SAVE ADDRESS OF UCOMM ENTRY                       
         CLI   0(RF),17       CHECK DATA LENGTH -SHOULD BE AT LEAST 17          
         BL    H7PR50         ELSE IGNORE                                       
         CLC   WORK(3),0(R4)     DO FISCAL YEARS MATCH?                         
         BNE   H7PR50         NO - THEN SKIP                                    
*                                                                               
         DROP  R3                                                               
*                                                                               
*        CHECK FOR NEW FORMAT THAT HAS FISCAL "YEAR" AT START OF UCOMM          
*        SKIP PAST IT IF FOUND                                                  
*                                                                               
*        GET HERE IF I FOUND A MATCH ON FISCAL YEAR                             
*                                                                               
         CLC   0(3,R4),=C'11-'                                                  
         BNE   H7PR35A2                                                         
         LA    R4,3(R4)                                                         
         B     H7PR35A9                                                         
*                                                                               
H7PR35A2 CLC   0(4,R4),=C'11B-'                                                 
         BNE   H7PR35A4                                                         
         LA    R4,4(R4)                                                         
         B     H7PR35A9                                                         
*                                                                               
H7PR35A4 CLC   0(4,R4),=C'11C-'                                                 
         BNE   H7PR35A6                                                         
         LA    R4,4(R4)                                                         
         B     H7PR35A9                                                         
*                                                                               
H7PR35A6 CLI   2(R4),C'-'         SHOULD PICK UP OTHER YEARS                    
         BNE   H7PR35A9           LIKE 10-,12-, ETC.                            
         LA    R4,3(R4)           BUMP PAST YEAR AND DASH                       
*                                                                               
*                                                                               
H7PR35A9 CLC   0(2,R4),=C'CO'   SKIP IF COMMENT BEGINS LIKE THIS                
         BNE   H7PR35B             DDEDIMAP HAD THIS CHECK                      
         MVC   P+1(34),=C'** MARKET UCOMM BEGINS WITH CO **'                    
         MVC   P2+1(21),=C'** INVOICE SKIPPED **'                               
         BRAS  RE,PRNT                                                          
         OI    SKIPBILL,X'80'      SO IT WON'T GET ON TAPE                      
         B     H7PRX                                                            
*                                                                               
H7PR35B  DS    0H                                                               
         CLI   0(R4),C'8'         DOES UCOM START WITH AN 8                     
         BNE   H7PR36             IF NOT GET DATA FROM OLD EDI TABLES           
*                                                                               
*        TRY AND FIND THE MARKET IN SBKTABLE (SPOT TABLE)                       
*                                                                               
         LA    RE,SBKTABLE                                                      
H7PR35C  CLC   4(4,R4),0(RE)                                                    
         BE    H7PR35D                                                          
         CLC   0(2,RE),=X'FFFF'       END OF TABLE                              
         BE    H7PR15E          SEND NOT FOUND IN MAR#                          
         LA    RE,24(RE)                                                        
         B     H7PR35C                                                          
*                                                                               
H7PR35D  MVC   BKACCT,4(RE)   SAVE ACCOUNT NUMBER                               
         MVC   BKCOSTC,14(RE)  AND COST CENTER                                  
*        NEW  UCOMM FORMAT FOR MAR#                                             
*        LENGTH MAY BE 22 0R 23                                                 
*        11 (FOR 2011) MAY BE SUFFIXED BY A B FOR NOV-DEC/2010                  
*        OR A C FOR JAN-DEC/2011                                                
*        THIS OF COURSE ALTERS DISPLACEMENT OF LATER DATA                       
*                                                                               
         MVC   WORK(4),=C'  --'                                                 
         MVC   WORK(2),BKFISCAL+2   LAST 2 DIGITS OF THE YEAR                   
         CLC   WORK(2),=C'11'    2011?                                          
         BNE   H7PR35E                                                          
         MVI   WORK+2,C'C'       WORK=11C-                                      
         CLC   BKBYR,=C'2010'    MOS IN 2010?                                   
         BNE   H7PR35E                                                          
         MVI   WORK+2,C'B'       WORK=11B-                                      
         CLC   BKBMTH,=C'11'     SEE IF NOV OR DEC                              
         BNL   H7PR35E                                                          
         MVI   WORK+2,C'-'       WORK=11-                                       
*                                                                               
H7PR35E  DS    0H                                                               
         MVC   BKMAR#(4),WORK                                                   
         LA    RE,BKMAR#+3                                                      
         CLI   WORK+2,C'-'       IF SO - THEN NO SUFFIX PRESENT                 
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
*                                                                               
*****    MVC   BKMAR#(2),BKFISCAL+2                                             
*****    MVI   BKMAR#+2,C'-'                                                    
*****    MVC   BKMAR#+3(4),4(R4) MARKET NUMBER FROM UCOMM                       
*****    MVI   BKMAR#+7,C'-'                                                    
*****    MVC   BKMAR#+8(3),BKCOSTC+4   FIRST 3 DIGITS OF COST CENTER            
*****    MVI   BKMAR#+11,C'-'                                                   
*****    MVC   BKMAR#+12(10),9(R4)  10 DIGIT NUMBER FROM UCOMM                  
         MVC   0(4,RE),4(R4)     MARKET NUMBER FROM UCOMM                       
         MVI   4(RE),C'-'                                                       
         MVC   5(3,RE),BKCOSTC+4       FIRST 3 DIGITS OF COST CENTER            
         MVI   8(RE),C'-'                                                       
         MVC   9(10,RE),9(R4)       10 DIGIT NUMBER FROM MKT UCOMM              
         B     H7PRX                                                            
*                                                                               
H7PR36   L     R2,=A(H7ACCTAB)                                                  
         USING H7ACCD,R2                                                        
H7PR38   CLC   CLT,H7CLI                                                        
         BE    H7PR40                                                           
         LA    R2,H7ACLNQ(R2)                                                   
         CLI   0(R2),EOT                                                        
         BNE   H7PR38                                                           
         MVC   P+1(20),=C'** INVALID CLIENT **'                                 
         MVC   P+25(3),CLT                                                      
         BRAS  RE,PRNT                                                          
         MVI   MODE,CLTLAST     SKIP THIS CLIENT                                
         B     H7PRX                                                            
*                                                                               
H7PR40   DS    0H                                                               
         MVC   BKCOSTC,=C'0000000000'                                           
         MVC   BKCOSTC+3(3),H7FUND      COST CENTER                             
         MVC   BKCOSTC+6(4),4(R4)       MARKET NUMBER FROM MKT UCOMM            
*                                                                               
         CLI   0(R4),C'8'               IS DOES THE UCOMM START WITH 8          
         BE    *+10                                                             
         MVC   BKCOSTC+6(4),2(R4)       IF NOT IT'S HERE                        
*                                                                               
         LA    RF,H7GLSX                       NETWORK RADIO                    
         CLI   MEDIA,C'X'                                                       
         BE    H7PR42                                                           
         LA    RF,H7GLST                       TV                               
         CLI   MEDIA,C'T'                                                       
         BE    H7PR42                                                           
         LA    RF,H7GLSR                       RADIO                            
         CLI   MEDIA,C'R'                                                       
         BE    H7PR42                                                           
         DC    H'0'               INVALID MEDIA                                 
*                                                                               
H7PR42   DS    0H                                                               
         MVC   BKACCT,=C'0000000000'                                            
         MVC   BKACCT+5(5),0(RF)        ACCOUNT FOR MEDIA                       
*                                                                               
         MVC   BKCOMACC,=C'0000000000'                                          
         MVC   BKCOMACC+5(5),H7GLCOM    ACCOUNT FOR COMMISSION                  
*                                                                               
*        NEW  UCOMM FORMAT FOR MAR#                                             
*        LENGTH MAY BE 22 0R 23                                                 
*        11 (FOR 2011) MAY BE SUFFIXED BY A B FOR NOV-DEC/2010                  
*        OR A C FOR JAN-DEC/2011                                                
*        THIS OF COURSE ALTERS DISPLACEMENT OF LATER DATA                       
*                                                                               
         MVC   WORK(4),=C'  --'                                                 
         MVC   WORK(2),BKFISCAL+2   LAST 2 DIGITS OF THE YEAR                   
         CLC   WORK(2),=C'11'    2011?                                          
         BNE   H7PR42E                                                          
         MVI   WORK+2,C'C'       WORK=11C-                                      
         CLC   BKBYR,=C'2010'    MOS IN 2010?                                   
         BNE   H7PR42E                                                          
         MVI   WORK+2,C'B'       WORK=11B-                                      
         CLC   BKBMTH,=C'11'     SEE IF NOV OR DEC                              
         BNL   H7PR42E                                                          
         MVI   WORK+2,C'-'       WORK=11-                                       
*                                                                               
H7PR42E  DS    0H                                                               
         MVC   BKMAR#(4),WORK                                                   
         LA    RE,BKMAR#+3                                                      
         CLI   WORK+2,C'-'       IF SO - THEN NO SUFFIX PRESENT                 
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
*                                                                               
*                                                                               
*******  MVC   BKMAR#(2),BKFISCAL+2  OLD CODE                                   
*******  MVI   BKMAR#+2,C'-'                                                    
*                                                                               
         CLI   0(R4),C'8'            DOES MKT UCOMM START WITH 8?               
         BNE   H7PR48                                                           
*                                                                               
*   FORMAT OF MKT UCOMM IS DIFFERENT                                            
*                                                                               
*   NOTE - NEW FORMAT WILL HAVE FISCAL YEAR AND DASH AT START                   
*          R4 IS ADJUSTED AT H7PR35B TO SKIP PAST IT                            
*                                                                               
*   IF IT STARTS WITH 8                                                         
*   FORMAT SHOULD BE 80208002-1234567890                                        
*   8020=MEDIA CODE,8002=MARKET, 1234567890=10 DIGIT NUMBER                     
*                                      NNNNMMMM-                                
         MVC   0(4,RE),4(R4)           MKT NUMBER FROM MKT UCOMM                
         MVI   4(RE),C'-'                                                       
         MVC   5(3,RE),BKCOSTC+3       FIRST 3 DIGITS OF COST CENTER            
         MVI   8(RE),C'-'                                                       
         MVC   9(10,RE),9(R4)          LAST 10 FROM MKT UCOMM                   
         B     H7PRX                                                            
*                                                                               
****     MVC   BKMAR#+3(4),4(R4)       MARKET NUMBER IN MKT UCOMM               
****     MVI   BKMAR#+7,C'-'                                                    
****     MVC   BKMAR#+8(3),BKCOSTC+3   FIRST 3 DIGITS OF COST CENTER            
****     MVI   BKMAR#+11,C'-'                                                   
****     MVC   BKMAR#+12(10),9(R4)     LAST 10 FROM MKT UCOMM                   
****     B     H7PRX                                                            
*                                                                               
H7PR48   DS    0H                                                               
         MVC   0(4,RE),2(R4)           MKT NUMBER FROM MKT UCOMM                
         MVI   4(RE),C'-'                                                       
         MVC   5(3,RE),BKCOSTC+3       FIRST 3 DIGITS OF COST CENTER            
         MVI   8(RE),C'-'                                                       
         MVC   9(10,RE),7(R4)          LAST 10 FROM MKT UCOMM                   
         B     H7PRX                                                            
*                                                                               
****48   MVC   BKMAR#+3(4),2(R4)       MARKET NUMBER MKT UCOMM                  
****     MVI   BKMAR#+7,C'-'                                                    
****     MVC   BKMAR#+8(3),BKCOSTC+3   FIRST 3 DIGITS OF COST CENTER            
****     MVI   BKMAR#+11,C'-'                                                   
****     MVC   BKMAR#+12(10),7(R4)      LAST 10 FROM MKT UCOMM                  
****     B     H7PRX                                                            
*                                                                               
H7PR50   L     R4,SAVER4      ADDRESS OF LAST ENTRY CHECKED                     
         LA    R4,32(R4)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,H7PR35A                                                       
         B     H7PR15E                                                          
*                                                                               
*                                                                               
H7PRNOM  DS    0H                  HERE IF NO MARKET UCOMM                      
*                                  OR NO BLMKT IN BILL                          
         MVC   P+1(35),=C'** MISSING UCOMM FOR MARKET XXXX **'                  
         MVC   P+29(4),BLMKT                                                    
         MVC   P2+1(21),=C'** INVOICE SKIPPED **'                               
         BRAS  RE,PRNT                                                          
         OI    SKIPBILL,X'80'                                                   
*                                                                               
H7PRX    MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
*                                                                               
         XIT1                                                                   
*                                                                               
BKTABLE  DS    0H                                                               
         DC    C'800100000764730000206000'  MKT,G/L ACCT,COST CENTER            
         DC    C'800200000764730000206000'                                      
         DC    C'801400000765020000206000'                                      
         DC    X'FFFF'                                                          
*                                                                               
SBKTABLE DS    0H                           USE FOR SPOT                        
         DC    C'800100000764730000206000'  MKT,G/L ACCT,COST CENTER            
         DC    C'800200000764730000206000'                                      
         DC    C'800300000764770000206000'                                      
         DC    C'800400000765060000206000'                                      
         DC    C'800500000764870000206000'                                      
         DC    C'800600000764850000206000'                                      
         DC    C'801100000765060000206000'                                      
         DC    C'801200000765030000206000'                                      
         DC    C'801300000765030000206000'                                      
         DC    C'801400000765020000206000'                                      
         DC    C'801500000765060000206000'                                      
         DC    C'801600000765060000206000'                                      
         DC    C'801700000765060000206000'                                      
         DC    C'801800000765060000206000'                                      
         DC    C'801900000765060000206000'                                      
         DC    C'802200000764850000206024'                                      
         DC    C'832300000765270000399575'                                      
         DC    C'832400000765270000399575'                                      
         DC    C'832500000765270000399575'                                      
         DC    C'832600000765270000399575'                                      
         DC    C'832700000765270000399575'                                      
         DC    C'832800000765270000399575'                                      
         DC    C'832900000765270000399575'                                      
         DC    C'833000000765270000399575'                                      
         DC    C'833100000765270000399575'                                      
         DC    C'833200000765270000399575'                                      
         DC    C'850200000765270000399575'                                      
         DC    C'850300000765270000399575'                                      
         DC    C'850400000765270000399575'                                      
         DC    C'850500000765270000399575'                                      
         DC    C'850600000765270000399575'                                      
         DC    C'850700000765270000399575'                                      
         DC    C'850800000765270000399575'                                      
         DC    C'850900000765270000399575'                                      
         DC    C'851000000765270000399575'                                      
         DC    C'851100000765270000399575'                                      
         DC    X'FFFF'                                                          
         EJECT                                                                  
*********************************************************************           
*  MINDSHARE BURGER KING CLIENT/GL TABLE                            *           
*********************************************************************           
H7ACCTAB DS    0XL(H7ACLNQ)        MEDIA FIRST ACCT CODES - SEE H7ACCD          
         DC    C'BI1',C'023'       CLIENT CODE/FUNDING SOURCE                   
*                                  CHANGED FROM 023 IMDB# 2407641               
*                                  CHGED FROM 020 BACK TO 023 0118493N          
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BI2',C'023'       CLIENT CODE/FUNDING IMDB# 2135841            
*                                  CHANGED FROM 023 IMDB# 2407641               
*                                  RESTORED TO 023 9/21/07 #0144031N            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
*                                                                               
         DC    C'BI3',C'023'       NEW CLIENT IMDB# 0147769N                    
*                                                                               
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BJ1',C'021'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BJ2',C'021'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BB1',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BP1',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV0',C'020'       CLIENT CODE/TICKET# 0181553N                 
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV1',C'020'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV2',C'020'       CLIENT CODE/FUNDING IMDB# 2135841            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV3',C'020'       CLIENT CODE/FUNDING IMDB# 7666351            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV4',C'020'       CLIENT CODE/FUNDING IMDB# 2817571            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV5',C'020'       CLIENT CODE/FUNDING IMDB# 2872701            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV6',C'020'       CLIENT CODE/FUNDING TICKET# 0109112N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
*  NOTE G/L FOR CLIENTS BELOW ARE SET LIKE THOSE FOR BV6                        
*       EVEN THOUGH THEY INDICATED THAT ONLY SPOT WOULD USE THE CLTS            
*       OTHER ENTRIES IN THE TABLE BELOW SHOULD CAUSE NO PROBLEMS               
*                                                                               
         DC    C'BV8',C'020'       CLIENT CODE/FUNDING TICKET# 0133745N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV9',C'020'       CLIENT CODE/FUNDING TICKET# 0133745N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG0',C'020'       CLIENT CODE/FUNDING TICKET# 0189892N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG1',C'020'       CLIENT CODE/FUNDING TICKET# 0189892N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG2',C'020'       CLIENT CODE/FUNDING TICKET# 0205126N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BK ',C'802'       CLIENT CODE/FUNDING IMDB# 2554611            
         DC    C'76489'            RADIO                                        
*        CODES NOT KNOWN FOR THE OTHER MEDIA YET                                
         DC    C'     '            NETWORK RADIO                                
         DC    C'     '            TV                                           
         DC    C'     '            NETWORK TV                                   
         DC    C'     '            OUTDOOR                                      
         DC    C'     '            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
MINDSHAR DC    CL(L'AGYNAME)'MINDSHARE USA, INC.'                               
         LTORG                                                                  
*        INITIATIVE / TRUE NORTH                                                
IMTNPROC NMOD1 0,**IMTN**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         MVI   TPREC+174,C' '                                                   
         CLI   QMED,C'N'                                                        
         BNE   IMTN10                                                           
*                                                                               
         MVI   TPREC+174,C'1'                                                   
         CLI   BLMED,C'N'                                                       
         BE    IMTN10                                                           
         CLI   BLMED,C' '                                                       
         BE    IMTN10                                                           
         MVI   TPREC+174,C'2'                                                   
         CLI   BLMED,C'S'                                                       
         BE    IMTN10                                                           
         MVI   TPREC+174,C'3'                                                   
         CLI   BLMED,C'C'                                                       
         BE    IMTN10                                                           
*                                                                               
IMTN10   DS    0H                                                               
         MVC   TPREC+175(11),=X'F0F0F0F0F0F0F0F0F0F0F0'  NO CLIENT $            
         MVC   TPREC+186(11),=X'F0F0F0F0F0F0F0F0F0F0F0'                         
         MVC   TPREC+197(11),=X'F0F0F0F0F0F0F0F0F0F0F0'                         
         TM    BILSTAT2,BSTC2Q     IF COS2 BILL DO CLIENT $                     
         BZ    IMTN20                                                           
*                                                                               
         UNPK  TPREC+175(11),BACT2P ACTUAL                                      
         TM    TPREC+185,X'10'     NEGATIVE?                                    
         BZ    *+8                                                              
         MVI   TPREC+175,C'-'                                                   
         OI    TPREC+185,X'F0'     REMOVE SIGN OVERPUNCH                        
*                                                                               
         UNPK  TPREC+186(11),BNET2P NET                                         
         TM    TPREC+196,X'10'     NEGATIVE?                                    
         BZ    *+8                                                              
         MVI   TPREC+186,C'-'                                                   
         OI    TPREC+196,X'F0'     REMOVE SIGN OVERPUNCH                        
*                                                                               
         ICM   RF,15,BCCTAX        TAX                                          
         CVD   RF,DUB                                                           
         UNPK  TPREC+197(11),DUB                                                
         TM    TPREC+207,X'10'     NEGATIVE?                                    
         BZ    *+8                                                              
         MVI   TPREC+197,C'-'                                                   
         OI    TPREC+207,X'F0'     REMOVE SIGN OVERPUNCH                        
*                                                                               
         MVI   TPREC+208,C'P'      SAY IT'S A PW BILL                           
*                                                                               
IMTN20   DS    0H                                                               
         TM    BILSTAT,BSTTAORQ    IF TRUE AOR BILL                             
         BZ    *+14                                                             
         BRAS  RE,GETAOR                                                        
         MVC   TPREC+209(30),WORK  AOR NAME (LINE 1) IN WORK                    
*                                                                               
         L     RF,ADPRD                                                         
         USING PRDHDRD,RF                                                       
         CLI   PACCT,X'FF'         CLT/PRD CODE                                 
         BNE   *+14                                                             
         UNPK  TPREC+390(5),PACCT+1(3) ACCOUNT NUMBER PACKED                    
         B     *+10                                                             
         MVC   TPREC+390(4),PACCT      ACCOUNT NUMBER NOT PACKED                
*                                                                               
         CLI   PGRP1,IMTNPGRQ      PRD MUST BELONG TO A PARTICULAR ID           
         BNE   *+18                                                             
         MVC   THREE,PGRP1                                                      
         BRAS  RE,GETPGRP                                                       
         B     IMTN30                                                           
*                                                                               
         CLI   PGRP2,IMTNPGRQ                                                   
         BNE   *+18                                                             
         MVC   THREE,PGRP2                                                      
         BRAS  RE,GETPGRP                                                       
         B     IMTN30                                                           
*                                                                               
         CLI   PGRP3,IMTNPGRQ                                                   
         BNE   *+18                                                             
         MVC   THREE,PGRP3                                                      
         BRAS  RE,GETPGRP                                                       
         B     IMTN30                                                           
*                                                                               
         CLI   PGRP4,IMTNPGRQ                                                   
         BNE   *+18                                                             
         MVC   THREE,PGRP4                                                      
         BRAS  RE,GETPGRP                                                       
         B     IMTN30                                                           
*                                                                               
         CLI   PGRP5,IMTNPGRQ                                                   
         BNE   IMTNX                                                            
         MVC   THREE,PGRP5                                                      
         BRAS  RE,GETPGRP                                                       
         B     IMTN30                                                           
         DROP  RF                                                               
*                                                                               
IMTN30   MVC   TPREC+239(4),FULL   PRODUCT GROUP CODE                           
         MVC   TPREC+243(24),WORK  PRODUCT GROUP NAME                           
*                                                                               
IMTNX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
IMTNPGRQ EQU   C'V'                IM MUST USE THIS PRDGRP SCHEME               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        IPG                                                                    
*                                                                               
IPGPROC  NMOD1 0,**IPG***                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     RF,ADCLT                                                         
         MVC   TPREC+30(2),CACCOFC-CLTHDR(RF)                                   
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         TM    BILSTAT,BSTTAORQ    IF TRUE AOR BILL                             
         BZ    *+14                                                             
         BRAS  RE,GETAOR                                                        
         MVC   TPREC+130(30),WORK  AOR NAME (LINE 1) IN WORK                    
*                                                                               
         ZIC   RE,BCALNDR          PUT IN CALENDAR VALUE FROM B3                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPREC+178(2),DUB+6(2)                                            
         GOTO1 DATCON,DMCB,(3,BKEYYSRV),(20,WORK)                               
         MVC   TPREC+172(6),WORK                   MOVE IN YYYYMM               
         GOTO1 DATCON,DMCB,BDATE,(20,TPREC+180)    BILL DATE                    
         GOTO1 DATCON,DMCB,BQDATE,(20,TPREC+190)   INVOICE DATE                 
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(20,TPREC+198) DUE DATE                 
         GOTO1 DATCON,DMCB,(2,BEDIDTE),(20,TPREC+218) TRANSMITTAL DATE          
*                                                                               
         TM    BILSTAT2,BSTRVSLQ   REVERSAL ?                                   
         BZ    IPGP3                                                            
         GOTO1 DATCON,DMCB,(2,BLREVDAT),(20,TPREC+206) DATE OF REV              
         XC    DMCB,DMCB                GET REVERSED BILL #                     
         GOTO1 VSPFMTIN,DMCB,,(C'U',BLREVINO)                                   
         L     RF,DMCB+4                                                        
         OC    0(2,RF),0(RF)       IF NULLS -> INVALID                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TPREC+214(4),0(RF)  INVOICE NUMBER                               
*                                                                               
*                                                                               
IPGP3    XC    WORK,WORK           READ B3 PROFILE                              
         LA    RE,WORK                                                          
         USING PROFKD,RE                                                        
         MVI   PROFKSYS,C'S'                                                    
         MVC   PROFKPGM,=C'0B3'                                                 
         MVC   PROFKAGN,AGY                                                     
         MVC   PROFKMED,MED                                                     
         MVC   PROFKCLI,CLIENT                                                  
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   PROFKOI2,C'*'                                                    
         MVC   PROFKOCD,0(RF)                                                   
         GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                                
         DROP  RE                                                               
         MVC   SPOTPROF+2(1),WORK+20     REPLACE CALENDAR VALUES                
         MVC   SPOTPROF+6(3),WORK+21                                            
*                                                                               
         MVC   WORK(4),BMONSERV           GET MOS                               
         MVC   WORK+4(2),=C'01'                                                 
         CLC   WORK+2(2),=C'12'    IF MONTH HIGHER THAN 12 (13)                 
         BNH   *+10                                                             
         MVC   WORK+2(2),=C'12'    SET TO 12 TO GIVE MOBILE REAL DATE           
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,40  ADD MORE THEN A MONTH TO ...          
*                                     ... BE SURE I GOT INTO NXT PERIOD         
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK,-1     SUBTRACT 1 YEAR               
*                                                                               
         CLC   WORK+2(2),=C'01'    IF MONTH IS JAN, GO TO FEB                   
         BNE   *+10                                                             
         MVC   WORK+2(2),=C'02'    DOESN'T ALWAYS GET RIGHT YEAR                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,BCALNDR                                                     
         GOTO1 MOBILE,DMCB,(20,WORK),((R0),ADBUY)                               
*                                                                               
         L     R3,ADBUY            LIST OF PERIODS                              
         SR    RE,RE               CHECK IF STARTING FROM JAN                   
         SR    RF,RF                                                            
         ICM   RF,12,0(R3)         GET THE PERIOD IN HIGH NIBBLE                
         SLDL  RE,7                PUT YEAR IN RE                               
         SR    RE,RE               CLEAR IT                                     
         SLDL  RE,4                NOW GET MONTH                                
         CHI   RE,1                IS IT JAN MOS ?                              
         BNE   *+8                 TRY TO MAKE SURE THERE'S ONLY ONE            
         LA    R3,4(R3)            PERIOD OF NEW YEAR                           
*                                  SINCE JAN CAN BE BEGINNING OF                
*                                  NEW YEAR, GET NEXT PER WHICH                 
IPGP4    DS    0H                  FIND FIRST PERIOD OF A NEW YEAR              
         BAS   RE,CKNEWYR          NEW YEAR?                                    
         BE    *+12                YES                                          
         LA    R3,4(R3)                                                         
         B     IPGP4                                                            
*                                  GO THROUGH LIST OF PERIODS                   
IPGP7    DS    0H                  TRY TO MATCH MOS                             
         ZIC   R0,2(R3)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         CLC   BYTE,BKEYYSRV       HAS TO BE THE SAME                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
         ZIC   RF,BKEYMSRV         MOS                                          
IPGP8    DS    0H                                                               
         AHI   R4,1                                                             
         CR    R4,RF                                                            
         BE    *+18                                                             
         LA    R3,4(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   IPGP8                                                            
         DC    H'0'                SHOULD NEVER REACH EOL                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R3)),(20,TPREC+242)                             
         GOTO1 DATCON,DMCB,(2,2(R3)),(20,TPREC+250)                             
*                                                                               
*                                                                               
         GOTO1 VSPFMTIN,DMCB,BDATE,(6,BINVNO),(QMED,B1PROF),B1XPROF             
         L     RF,DMCB                                                          
         MVC   TPREC+258(10),0(RF)          INVOICE NUMBER                      
         MVC   TPREC+268(2),BTYPE           BILL TYPE                           
         TM    BILSTAT,BSTMANQ                                                  
         BZ    *+8                                                              
         MVI   TPREC+268,C'M'               INDICATE IF A MANUAL                
         TM    BILSTAT,BSTTAORQ                                                 
         BZ    *+8                                                              
         MVI   TPREC+268,C'A'               INDICATE IF AOR                     
*                                                                               
         ZAP   DUB,BGRSP                    GROSS                               
         LHI   R3,272                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
         ZAP   DUB,BGRS2P                   GROSS (COS2)                        
         LHI   R3,283                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
*                                                                               
         ZAP   DUB,=P'0'                    MIGHT NEED TO CLEAR NET             
         TM    BILSTAT,BSTTAORQ             IF TRUE AOR BILL                    
         BZ    *+14                                                             
         CP    BGRS2P,=P'0'                 AND IF GROSS = $0                   
         BE    *+10                         MAKE NET ZERO TOO, ELSE IT          
*                                           WILL PRINT BAORCOMP                 
         ZAP   DUB,BNETP                    NET                                 
*                                           WILL PRINT BAORCOMP                 
         LHI   R3,305                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
         ZAP   DUB,BNET2P                   NET (COS2)                          
         LHI   R3,316                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
         ZAP   DUB,BACTP                    ACTUAL                              
*                                                                               
         CLC   QAGY,=C'T1'                  TEST                                
         BE    *+14                                                             
         CLC   QAGY,=C'U#'                  M2TOA?                              
         BNE   IPGP51                                                           
*                                                                               
         AP    DUB,MYGST                    INCLUDE CANADIAN TAXES              
         AP    DUB,MYPST                    MYPST INCLUDES HST                  
*                                                                               
IPGP51   LHI   R3,338                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
         ZAP   DUB,BACT2P                   ACTUAL (COS2)                       
         LHI   R3,349                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
*        TM    BILSTAT,BSTTAORQ             IF TRUE AOR BILL                    
*        BZ    *+18                                                             
*        ZAP   DUB,BAORCOMP                 AOR AGENCY COMMISSION               
         ZAP   DUB,BACTP                    SHOW COMM (ACT-NET)                 
         CLC   RCORIGID,=H'15634'           FOR IPGSAPWI                        
         BNE   IPGP51C                                                          
IPGP51B  TM    BILSTAT,BSTTAORQ             IF TRUE AOR BILL                    
         BNZ   IPGP51E                      SHOW ONLY ACT                       
         B     IPGP51D                                                          
*                                                                               
IPGP51C  CLC   QAGY,=C'U#'                  M2TOA                               
         BE    IPGP51B                     SAME AS FOR IPGSAPWI                 
         CLC   QAGY,=C'T1'                  TEST                                
         BE    IPGP51B                                                          
*                                                                               
IPGP51D  SP    DUB,BNETP                                                        
*                                                                               
IPGP51E  LHI   R3,393                       POSITION                            
         BAS   RE,FMTAMT                    FORMAT THE AMOUNT                   
*                                                                               
         MVI   TPREC+480,C'N'                                                   
         TM    BILSTAT,BSTSADJQ    SEP ADJ BILL?                                
         BZ    *+8                                                              
         MVI   TPREC+480,C'Y'                                                   
         MVI   TPREC+481,C'N'                                                   
         TM    BILSTAT,BSTMANQ     MANUAL BILL?                                 
         BZ    *+8                                                              
         MVI   TPREC+481,C'Y'                                                   
         MVI   TPREC+482,C'N'                                                   
         TM    BILSTAT,BSTTAORQ    TRUE AOR BILL?                               
         BZ    *+8                                                              
         MVI   TPREC+482,C'Y'                                                   
         MVI   TPREC+483,C'N'                                                   
         TM    BILSTAT,BSTCAORQ    CLIENT AOR BILL?                             
         BZ    *+8                                                              
         MVI   TPREC+483,C'Y'                                                   
         MVI   TPREC+484,C'N'                                                   
         TM    BILSTAT,BSTNTXAQ    NO TAX IN AOR CALC?                          
         BZ    *+8                                                              
         MVI   TPREC+484,C'Y'                                                   
         MVI   TPREC+485,C'N'                                                   
         TM    BILSTAT,BSTCMONQ    COMM ONLY BILL?                              
         BZ    *+8                                                              
         MVI   TPREC+485,C'Y'                                                   
         MVI   TPREC+486,C'N'                                                   
         TM    BILSTAT,BSTSCOMQ    SEP COMMISSION BILL?                         
         BZ    *+8                                                              
         MVI   TPREC+486,C'Y'                                                   
         MVI   TPREC+487,C'N'                                                   
         TM    BILSTAT,BSTSNETQ    SEP COMMISSION BILL?                         
         BZ    *+8                                                              
         MVI   TPREC+487,C'Y'                                                   
*                                                                               
         CLI   BMASPRD,0           IS THERE A MASTER PRODUCT                    
         BNH   IPGP20                                                           
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDR(RF)                                              
IPGP17   CLI   0(RF),0             EOL, PRODUCT NOT FOUND                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BMASPRD,3(RF)                                                    
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     IPGP17                                                           
*                                                                               
         MVC   TPREC+538(3),0(RF)                                               
*                                                                               
*                                                                               
IPGP20   LA    R3,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R3                                                       
         MVC   KEY2,KEY            SAVE KEY                                     
         MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   UCSYS,C'N'        SYSTEM TO PRINT (NET)                          
         MVC   UCSAM,BKEYAM      AGENCY/MEDIA                                   
         MVC   UCSCLT,BKEYCLT    PACKED CLIENT                                  
         MVC   UCPRD,BKEYPRD     PRD CODE                                       
         OI    UCOPT,UCOPRD      RETURN PRODUCT UCOMMS                          
         MVC   UCSEST,BKEYEST    ESTIMATE                                       
         OI    UCOPT,UCOEST      RETURN ESTIMATE UCOMMS                         
         CLI   QMED,C'N'         IF NETWORK, DONE                               
         BE    IPGP23                                                           
         CLI   BLMKT,C' '        IF MARKET IS ENTERED, GET UCOMM                
         BNH   IPGP23                                                           
         PACK  DUB,BLMKT                                                        
         CVB   R0,DUB                                                           
         STH   R0,UCMKT                                                         
         OI    UCOPT,UCOMKT      RETURN MARKET UCOMMS                           
IPGP23   GOTO1 =V(DDUCOM),UCOMBLK                                               
         CLI   UCERROR,0                                                        
         BNE   IPGP28       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOPRD+UCDNOEST+UCDNOMKT                                
         BO    IPGP28       NO PRD,EST,OR MKT DATA                              
         TM    UCDATA,UCDNOPRD                                                  
         BO    IPGPUCE                                                          
         L     R1,UCPDATA     PRD DATA                                          
         LA    RF,UCPLENS     LENGTHS                                           
         LA    RE,TPREC+637   START OF 1ST PRODUCT UCOMM                        
         LHI   R0,4                                                             
IPGPUCP5 CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,IPGPUCP5                                                      
*                                                                               
IPGPUCE  TM    UCDATA,UCDNOEST                                                  
         BO    IPGPUCM                                                          
         L     R1,UCEDATA     EST DATA                                          
         LA    RF,UCELENS     LENGTHS                                           
         LA    RE,TPREC+765   START OF 1ST EST UCOMM                            
         LHI   R0,4                                                             
IPGPUCE5 CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,IPGPUCE5                                                      
*                                                                               
IPGPUCM  CLI   QMED,C'N'           IF NETWORK, DONE                             
         BE    IPGP28                                                           
         TM    UCOPT,UCOMKT        DID WE ASK FOR MKT UCOMS?                    
         BZ    IPGP28              NO, DONE                                     
         TM    UCDATA,UCDNOMKT                                                  
         BO    IPGP28                                                           
         L     R1,UCMDATA     MKT DATA                                          
         LA    RF,UCMLENS     LENGTHS                                           
         LA    RE,TPREC+893   START OF 1ST MKT UCOMM                            
         LHI   R0,4                                                             
IPGPUCM5 CLI   0(RF),0        CHECK DATA LENGTH                                 
         BE    *+16                                                             
         MVC   0(32,RE),0(R1)                                                   
         OC    0(32,RE),=32C' '                                                 
         LA    RE,32(RE)                                                        
         LA    R1,32(R1)      NEXT DATA                                         
         LA    RF,1(RF)       NEXT LENGTH                                       
         BCT   R0,IPGPUCM5                                                      
         DROP  R3                                                               
*                                                                               
*                                                                               
IPGP28   MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLI   QMED,C'N'           FOR NETWORK...                               
         BNE   IPGP40                                                           
         OC    BLPKGNM,BLPKGNM                                                  
         BZ    *+10                                                             
         MVC   TPREC+123(5),BLPKGNM                                             
         OC    BLPKG,BLPKG                                                      
         BZ    IPGP30                                                           
         ZIC   RE,BLPKG            PUT IN PACKAGE# IF IT'S THERE                
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPREC+120(3),DUB+5(3)                                            
IPGP30   OC    BILWKNO,BILWKNO                                                  
         BZ    IPGP35                                                           
         ZIC   RE,BILWKNO          PUT IN WEEK# IF IT'S THERE                   
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TPREC+188(2),DUB+6(2)                                            
IPGP35   CLI   BLDPT,C' '                                                       
         BNH   *+10                                                             
         MVC   TPREC+128(1),BLDPT                                               
         OC    BILCTYP,BILCTYP                                                  
         BNZ   *+8                                                              
         MVI   TPREC+270,C' '                                                   
         CLI   BLSTATN,C' '        ANYTHING IN STATION FIELD?                   
         BNH   *+16                NO, DONE W/ NET                              
         MVC   TPREC+489(4),BLSTATN                                             
         MVC   TPREC+493(5),=5C' '                                              
         B     IPGP50                                                           
*                                                                               
*                                                                               
IPGP40   DS    0H                  FOR SPOT ....                                
         CLI   BLMKT,C' '          IF MARKET IS ENTERED, GET DESCRIPT           
         BNH   IPGP50                                                           
         MVC   KEY2,KEY            SAVE KEY                                     
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'KEY-1),KEY                                               
         LA    R3,KEY                                                           
         USING MKTRECD,R3                                                       
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,BLMKT                                                    
         MVC   MKTKAGY,QAGY                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,ADBUY                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ADBUY                                                         
         MVC   TPREC+502(24),MKTNAME                                            
         OC    TPREC+502(24),SPACES                                             
         MVC   KEY,KEY2                                                         
         GOTO1 HIGH                RESTORE SEQUENCE                             
         DROP  R3                                                               
IPGP50   MVI   TPREC+925,C'?'      PRESET TAX TO MIX OR UNKNOWN                 
         CLI   BILNPVTS,0          ANY PROVINCIAL VAT ELEMS?                    
         BE    IPGP55                                                           
         CLI   BILPVCOD,0          ANY CODE?                                    
         BE    IPGP55                                                           
         MVC   TPREC+925(L'BILPVCOD),BILPVCOD                                   
         B     IPGPX                                                            
                                                                                
IPGP55   CLI   BVATCOD,X'FF'                                                    
         BE    IPGPX                                                            
         CLI   BVATCOD,0                                                        
         BE    IPGPX                                                            
         MVC   TPREC+925(L'BVATCOD),BVATCOD                                     
IPGPX    MVI   TPREC+1149,SEMICOL                                               
         XIT1                                                                   
         DROP  R7                                                               
SEMICOL  EQU   X'5E'                                                            
         EJECT                                                                  
FMTAMT   NTR1                      DUB HAS AMOUNT, R3 POSIT IN TPREC            
         UNPK  WORK(11),DUB                                                     
         LA    R4,WORK                                                          
         TM    WORK+10,X'10'       IF AMOUNT NEGATIVE                           
         BZ    *+12                                                             
         LA    R4,1(R4)                                                         
         MVI   WORK+11,C'-'        ADD MINUS SIGN AT END                        
*                                                                               
         OI    WORK+10,X'F0'       REMOVE OVERPUNCH                             
         LA    R3,TPREC-1(R3)                                                   
         MVC   0(11,R3),0(R4)                                                   
         XIT1                                                                   
         EJECT                                                                  
CKNEWYR  NTR1                                                                   
*                                                                               
         MVC   DUB(4),0(R3)                                                     
         NI    DUB,X'FF'-X'FE'     STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'FF'-X'FE'                                                
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'FF'-X'E0'   ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LHI   R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'FF'-X'E0'   ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
CKNYYES  CR    RE,RE                                                            
         XIT1                                                                   
         SPACE 2                                                                
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SAATCHI/ZENITH TOYOTA SPECS                                            
*                                                                               
DFTPROC  NMOD1 0,**DFTP**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
         LA    R2,TPREC                                                         
         USING DFTLIND,R2                                                       
*                                  SET 1-BYTE VENDOR ID                         
         MVI   DFTVID,C'1'         SAATCHI                                      
         CLC   CLT,=C'TM '                                                      
         BE    DFP04                                                            
         CLC   CLT,=C'TMS'                                                      
         BE    DFP04                                                            
*                                                                               
         MVI   DFTVID,C'2'         TEAM ONE                                     
         CLC   CLT,=C'LEX'                                                      
         BE    DFP04                                                            
         CLC   CLT,=C'LDA'                                                      
         BE    DFP04                                                            
         CLC   CLT,=C'LDL'                                                      
         BE    DFP04                                                            
*                                                                               
         MVI   DFTVID,C'3'         CONHILL                                      
         CLC   CLT,=C'TOC'                                                      
         BE    DFP04                                                            
*                                                                               
         MVI   MODE,CLTLAST        SKIP OTHER CLIENTS                           
         B     DFPX                                                             
*                                  'VENDOR' SPECIFIC                            
DFP04    DS    0H                                                               
         CLI   DFTVID,C'1'         SAATCH1                                      
         BNE   DFP06                                                            
         MVC   DFTVENN,=C'01001539'                                             
         MVC   DFTADDR,=C'001'                                                  
         MVC   DFTSAC,=C'100139'                                                
         MVC   DFTREFT,=C'61'                                                   
         B     DFP10                                                            
*                                                                               
DFP06    DS    0H                                                               
         CLI   DFTVID,C'2'         TEAM 0NE                                     
         BNE   DFP08                                                            
         MVC   DFTVENN,=C'02019620'                                             
         MVC   DFTADDR,=C'002'                                                  
         MVC   DFTSAC,=C'100611'                                                
         MVC   DFTREFT,=C'71'                                                   
         B     DFP10                                                            
*                                                                               
DFP08    DS    0H                                                               
         CLI   DFTVID,C'3'         TEAM 0NE                                     
         BNE   DFP09                                                            
         MVC   DFTVENN,=C'01021083'                                             
         MVC   DFTADDR,=C'001'                                                  
         MVC   DFTSAC,=C'100139'                                                
         MVC   DFTREFT,=C'61'                                                   
         B     DFP10                                                            
*                                                                               
DFP09    DS    0H                                                               
         DC    H'0'                BAD VENDOR ID - CANNOT HAPPEN                
*                                                                               
DFP10    DS    0H                  COMMON CODE                                  
*                                  INVOICE (REFERENCE) NUMBER                   
         GOTO1 VSPFMTIN,DMCB,BDATE,(6,BINVNO),(QMED,B1PROF),B1XPROF             
*                                                                               
         L     RF,DMCB                                                          
         LA    RE,DFTREFN                                                       
         LHI   R0,10                                                            
*                                                                               
DFP12    DS    0H                                                               
         CLI   0(RF),C'-'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
*                                                                               
         LA    RF,1(RF)                                                         
         BCT   R0,DFP12                                                         
*                                  INVOICE (REFERENCE) DATE                     
         GOTO1 DATCON,DMCB,BQDATE,(20,WORK)   YYYYMMDD                          
         MVC   DFTREFD(4),WORK+4                                                
         MVC   DFTREFD+4(4),WORK                                                
*                                                                               
*     PAY DATE - BILL DUE DATE LESS 2 DAYS (NOT INCLUDING WEEKENDS)             
*        NOTE- THIS CODE COPIED FROM PPREP1002, KEEP IN SYNC!                   
*                                                                               
*              GET DUE DATE (WITH 'FUNNY' YEAR FOR GETDAY))                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLC   WORK+6(3),=3C' '                                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID DATE                                 
         ZIC   R6,0(R1)            DAY OF WEEK (1=MON...7=SUN)                  
         LHI   R5,-4               NUMBER OF DAYS TO SUBTRACT                   
         CHI   R6,1                MONDAY ?                                     
         BE    DFP13D              YES                                          
         CHI   R6,2                TUESDAY ?                                    
         BE    DFP13D              YES                                          
         LHI   R5,-3                                                            
         CHI   R6,7                SUNDAY ?                                     
         BE    DFP13D              YES                                          
         LHI   R5,-2               MUST BE WED - SAT                            
DFP13D   DS    0H                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+8,(R5)                                      
         GOTO1 DATCON,DMCB,(0,WORK+8),(20,WORK)   YYYYMMDD                      
         MVC   DFTPAYD(4),WORK+4                                                
         MVC   DFTPAYD+4(4),WORK                                                
*                                  GL ACCOUNT                                   
         CLI   NETPAKSW,C'Y'                                                    
         BNE   *+18                                                             
         MVC   DFTREM,SPACES       CLEAR COMMENT                                
         BAS   RE,DFPCOM           FOR NETPAK, COMMENT SPECIAL                  
         B     DFP13H                                                           
*                                                                               
         L     RF,ADEST                                                         
         LA    R4,EUSER2-ESTHDR(RF)                                             
         MVC   DFTGLACC(3),5(R4)   NNN-NNNN BUT DROP DASH                       
         MVC   DFTGLACC+3(4),9(R4) LAST 7 (8) OF EUSER2                         
*                                                                               
DFP13H   DS    0H                                                               
*                                  COST CENTER                                  
         L     RF,ADEST                                                         
         LA    R4,EUSER2-ESTHDR(RF)                                             
         MVC   DFTCOSTC,0(R4)      1ST 4 OF EUSER2                              
*                                  'ESTIMATE NAME'                              
         L     RF,ADEST                                                         
         LA    R4,EUSER1-ESTHDR(RF)                                             
         MVC   DFTESTN,0(R4)       1ST 12 OF EUSER1                             
*                                             CALENDAR YEAR                     
         GOTO1 DATCON,DMCB,TODAY,(20,WORK)   YYYYMMDD                           
         MVC   DFTCLYR,WORK                   YYYY                              
*                                  SET FIELD DELIMETERS                         
*                                  AMOUNT DUE                                   
         EDIT  BACTP,DFTAMT,2,FLOAT=-                                           
*                                  SUBSIDIARY CODE                              
         MVC   DFTSUBC,=C'07'      BASED ON COST CENTER CODE                    
         CLC   DFTCOSTC,=C'1010'                                                
         BE    DFP14                                                            
         MVC   DFTSUBC,=C'10'      BASED ON COST CENTER CODE                    
         CLC   DFTCOSTC,=C'0877'                                                
         BE    DFP14                                                            
         MVC   DFTSUBC,=C'01'                                                   
*                                  FISCAL YEAR                                  
DFP14    DS    0H                                                               
         GOTO1 DATCON,DMCB,TODAY,(20,WORK)                                      
         MVC   DFTFSYR,WORK                   YYYY                              
*                                  SET FIELD DELIMETERS                         
         MVI   DFTREFN-1,DFTFDEL                                                
         MVI   DFTADDR-1,DFTFDEL                                                
         MVI   DFTSAC-1,DFTFDEL                                                 
         MVI   DFTREFT-1,DFTFDEL                                                
         MVI   DFTREFD-1,DFTFDEL                                                
         MVI   DFTPAYD-1,DFTFDEL                                                
         MVI   DFTGLACC-1,DFTFDEL                                               
         MVI   DFTCOSTC-1,DFTFDEL                                               
         MVI   DFTESTN-1,DFTFDEL                                                
         MVI   DFTCLYR-1,DFTFDEL                                                
         MVI   DFTPROJN-1,DFTFDEL                                               
         MVI   DFTAMT-1,DFTFDEL                                                 
         MVI   DFTREM-1,DFTFDEL                                                 
         MVI   DFTSUBC-1,DFTFDEL                                                
         MVI   DFTFSYR-1,DFTFDEL                                                
         MVI   DFTRECX-1,DFTFDEL                                                
*                                                                               
DFPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
DFTVID   DS    CL1                 MY VENDOR ID                                 
         EJECT                                                                  
***********************************************************************         
*   DFPCOM - NETPAK DFP COMMENTS                                                
***********************************************************************         
*                                                                               
DFPCOM   NTR1                                                                   
         L     R5,=A(NETBLK)                                                    
         USING NETBLOCK,R5                                                      
*                                                                               
         BC    0,DFPC10            ONCE ONLY                                    
         OI    *-3,X'F0'                                                        
*                                                                               
         L     R4,ADBUY            USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
*                                                                               
DFPC10   DS    0H                                                               
         LA    RE,NETBLOCK                                                      
         LHI   RF,NBBLKEND-NETBLOCK                                             
         XCEF                                                                   
*                                                                               
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         LA    R5,WORK                                                          
         USING NCOMBLKD,R5                                                      
         XC    WORK,WORK                                                        
         MVC   NCBAIO,ADCOMREC     IOA AREA                                     
         MVC   NCBDMGR,DATAMGR     DATAMGR                                      
         MVC   NCBNETB,=A(NETBLK)  NETBLOCK                                     
         LA    RF,NCHOOK                                                        
         ST    RF,NCBAHOOK         HOOK                                         
         MVI   NCBKDFL,C'Y'        SET TO DEFAULT TO HIGHER KEY                 
         MVC   NCBID,=C'B*1'       BOTH TIME AND INTEG                          
*                                                                               
         MVC   NCBAM,BAGYMD        AGY/MED                                      
         MVC   NCBCLT,BCLT                                                      
*                                                                               
         L     RF,ADPRD                                                         
         MVC   NCBPRD,PCODE+1-PRDHDR(RF)                                        
*                                                                               
         L     RF,ADEST                                                         
         MVC   NCBEST,EKEYEST-ESTHDR(RF)                                        
*                                                                               
         GOTO1 =V(NETCOM),DMCB,NCOMBLKD                                         
         CLI   NCBERROR,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     DFPCX                                                            
         SPACE 3                                                                
NCHOOK   NTR1                  PROCESSING ROUTINE FOR NETCOM                    
         CLC   DFTREM,SPACES       IF ALREADY HAVE REMARK                       
         BNE   NCH12               DON'T LOOK FOR ANOTHER                       
         CLI   BLMED,C' '          DO ONLY SUB-MEDIA SPECIFICS                  
         BNH   NCH12                                                            
*                                                                               
         L     R2,NCBAIO           IO AREA                                      
         LA    R2,27(R2)           FIRST ELEM                                   
*                                                                               
NCH4     DS    0H                                                               
         CLI   0(R2),0             EOR                                          
         BE    NCH12                                                            
         CLI   0(R2),X'02'         COMMENT ELEM                                 
         BE    NCH6                                                             
*                                                                               
NCH5     DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     NCH4                                                             
*                                                                               
NCH6     DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   NCH7                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R2),P,3,=C'N'                                      
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+7(0),3(R2)                                                     
         BRAS  RE,PRNT                                                          
*                                                                               
NCH7     DS    0H                                                               
*                                                                               
         CLC   4(1,R2),BLMED       LINE MUST BE FOR RIGHT SUB-MED               
         BNE   NCH5                                                             
         CLI   3(R2),C'/'                                                       
         BNE   NCH5                                                             
         CLI   5(R2),C'/'                                                       
         BNE   NCH5                                                             
*                                                                               
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'7'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DFTREM(0),6(R2)                                                  
*                                                                               
NCH12    DS    0H                  DONE                                         
         B     DFPCX                                                            
*                                                                               
DFPCX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
NUFLIST  DC    CL8'UUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
         DROP  R7                                                               
         LTORG                                                                  
*                                                                               
NETBLK   DS    2000X                                                            
         EJECT                                                                  
*                                                                               
CLPROC   NMOD1 0,**CLPR**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         MVI   TPREC,C'A'                                                       
         MVI   TPREC+1,C','           DELIMITER                                 
         MVI   TPREC+2,C'I'                                                     
         CP    BACT2P,=P'0'                                                     
         BNL   *+8                                                              
         MVI   TPREC+2,C'C'           CREDIT INVOICE                            
         MVI   TPREC+3,C','           DELIMITER                                 
         MVC   TPREC+4(10),DINVFULL                                             
         MVI   TPREC+14,C','          DELIMITER                                 
*                                                                               
*        ORIGINAL INV # FOR CREDIT GOES HERE?                                   
*        SEARCH FOR FIRST BILL FOR THIS MED/CLT/PRD/EST                         
*                                                                               
         LA    R2,TPREC+15                                                      
*                                                                               
***      CP    BACT2P,=P'0'        SEE IF A CREDIT                              
***      BNL   OBPR15N                                                          
***      MVC   WORK(64),KEY        SAVE KEY FOR SEQ                             
***      XC    KEY,KEY                                                          
***      MVC   KEY(14),PBILLREC  MED/CLT/PRD/EST/MOS                            
***      GOTO1 HIGH                                                             
***      CLC   KEY(14),KEYSAVE                                                  
***      BNE   OBPR15N           DID I FIND ONE?                                
***      LA    R2,KEY                                                           
***      USING PBILLREC,R2                                                      
***      MVC   OBWORK+6(2),PBILKBMN    YEAR/MTH OF FOUND BILL                   
***      MVI   OBWORK+8,X'01'      SET DAY TO 1                                 
***      GOTO1 DATCON,DMCB,(3,OBWORK+6),(0,OBWORK)  YYMMDD                      
***                                                                             
***      GOTO1 =V(SPFMTINO),DMCB,OBWORK,(2,PBILKBNO),(PBILKMED,B1PROF)          
***            B1XPROF                                                          
***      DROP  R2                                                               
***                                                                             
***      L     RF,DMCB                                                          
***      MVC   TPREC+15(10),0(RF)   FULL INVOICE NUMBERR                        
***      LA    R2,TPREC+25                                                      
***      MVC   KEY(64),WORK                                                     
***      GOTO1 HIGH                                                             
***                                                                             
OBPR15N  MVI   0(R2),C','         DELIMITER                                     
         LA    R2,1(R2)                                                         
         GOTO1 DATCON,DMCB,(3,BQDATE),(20,WORK)        INV DATE                 
         MVC   0(2,R2),WORK+4       MM                                          
         MVC   2(2,R2),WORK+6       DD                                          
         MVC   4(4,R2),WORK         YYYY                                        
         LA    R2,8(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(3,R2),=C',,,'      3 EMPTY FIELDS                              
         LA    R2,3(R2)                                                         
*                                                                               
         MVC   0(10,R2),TPREC+240   SAVED EST USER 2                            
         MVI   10(R2),C','                                                      
         LA    R2,11(R2)                                                        
         MVC   0(2,R2),=C',,'    2 EMPTY FIELDS                                 
         LA    R2,2(R2)                                                         
         MVC   0(7,R2),TPREC+150   BILL TO CUSTOMER                             
*                                                                               
         MVI   7(R2),C','     DELIMITER                                         
         LA    R2,8(R2)                                                         
         MVC   0(27,R2),=C',,,,,,,,,,,,,,,,,,,,,,,,,,,'                         
         LA    R2,27(R2)                                                        
*                                                                               
*        LAST FIELD FOR A COMMENT?                                              
*                                                                               
         MVI   0(R2),C','     EMPTY FOR NOW                                     
*****************************************************                           
*****  END OF A RECORD                                                          
*****************************************************                           
         AP    OBCOUNT,=P'1'                                                    
         CLI   QOPT3,C'Y'                                                       
         BNE   OBPR20                                                           
         MVC   P(14),=C'***TAPE RECORD'                                         
         GOTO1 REPORT                                                           
         MVC   P(100),TPREC                                                     
         GOTO1 REPORT                                                           
         MVC   P(20),=C'***TAPE RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,TPREC,P,100,0                                        
         GOTO1 REPORT                                                           
*                                                                               
OBPR20   DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'         TEST RUN - NO TAPE                            
         BE    OBPR25                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
OBPR25   DS    0H                 INVOICE LINE DETAIL                           
*                                                                               
*                                                                               
OBPR29   MVI   TPREC,C' '                                                       
         MVC   TPREC+1(100),TPREC                                               
*                                                                               
         MVI   TPREC,C'B'                                                       
         MVI   TPREC+1,C','         DELIMITER                                   
***                                                                             
***      PO LINE NUMBER HERE FIRST EST USER?                                    
***      FOLLOWED BY PRODUCT CODE, PRODUCT DESCRIPTION,                         
***                                                                             
         MVC   TPREC+2(6),TPREC+200     SAVED EST USER 1                        
         LA    R2,TPREC+7                                                       
OBPR32   CLI   0(R2),C' '    SCAN BACKWARD FOR FIRST NON-SPACE                  
         BH    OBPR32X                                                          
         SH    R2,=H'1'                                                         
         B     OBPR32                                                           
*                                                                               
OBPR32X  DS    0H                                                               
         MVI   1(R2),C','     DELIMITER                                         
         LA    R2,1(R2)                                                         
         MVC   0(2,R2),=C',,'         2 EMPTY FIELDS                            
         LA    R2,2(R2)                                                         
         MVC   0(2,R2),=C'EA'   UNIT OF MEASURE - EACH?                         
         MVI   2(R2),C','        DELIMITER                                      
         LA    R2,3(R2)                                                         
         EDIT  BACT2P,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT              
         AR    R2,R0       LENGTH OF FIELD                                      
         MVI   0(R2),C','  DELIMITER                                            
         MVI   1(R2),C'1'        QUANTITY IS 1                                  
         MVI   2(R2),C','        DELIMITER                                      
         MVI   3(R2),C','        EMPTY TAX FIELD                                
         LA    R2,4(R2)                                                         
         EDIT  BACT2P,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT              
         AR    R2,R0       LENGTH OF FIELD                                      
         MVI   0(R2),C','  DELIMITER                                            
         MVC   1(2,R2),=C',,'      2 EMPTY FIELDS                               
*                                                                               
*         LAST COULD BE A COMMENT                                               
*                                                                               
*                                                                               
         AP    OBCOUNT,=P'1'                                                    
         CLI   QOPT3,C'Y'                                                       
         BNE   OBPR30                                                           
         MVC   P(14),=C'***TAPE RECORD'                                         
         GOTO1 REPORT                                                           
         MVC   P(100),TPREC                                                     
         GOTO1 REPORT                                                           
         MVC   P(20),=C'***TAPE RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,TPREC,P,100,0                                        
         GOTO1 REPORT                                                           
*                                                                               
OBPR30   DS    0H                                                               
*                                                                               
         CLI   QOPT3,C'Y'         TEST RUN - NO TAPE                            
         BE    OBPR35                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
OBPR35   DS    0H                                                               
*                                                                               
         CP    OBCOUNT,=P'0' ANY RECORDS CREATED?                               
         BE    OBPR50                                                           
*                                                                               
         MVI   TPREC,C' '                                                       
         MVC   TPREC+1(100),TPREC                                               
*                                                                               
         MVI   TPREC,C'C'                                                       
         MVI   TPREC+1,C','           DELIMITER                                 
         MVC   TPREC+2(7),=C',,,,,,,'     7 EMPTY FIELDS                        
*                                                                               
         LA    R2,TPREC+9                                                       
         EDIT  BACT2P,(13,0(R2)),2,ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT              
         AR    R2,R0       LENGTH OF FIELD                                      
         MVI   0(R2),C','  DELIMITER                                            
         MVI   1(R2),C','       1 EMPTY FIELD                                   
*                                                                               
*        RECORD COUNTS AND TOTAL $                                              
*                                                                               
*******  EDIT  OBCOUNT,(10,TPREC+2),0,FILL=0                                    
*******  MVI   TPREC+12,C','           DELIMITER                                
*******  EDIT  OBTOTAL,(13,TPREC+13),2,FILL=0,ZERO=NOBLANK,FLOAT=-              
*******                                                                         
         CLI   SVQOPT3,C'Y'                                                     
         BNE   OBPR45                                                           
         MVC   P(14),=C'***TAPE RECORD'                                         
         GOTO1 REPORT                                                           
         MVC   P(100),TPREC                                                     
         GOTO1 REPORT                                                           
         MVC   P(20),=C'***TAPE RECORD - HEX'                                   
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,TPREC,P,100,0                                        
         GOTO1 REPORT                                                           
*                                                                               
OBPR45   DS    0H                                                               
*                                                                               
         CLI   SVQOPT3,C'Y'         TEST RUN - NO TAPE                          
         BE    OBPR50                                                           
*                                                                               
         L     R1,=A(SBITAPE)                                                   
         LA    R0,TPREC                                                         
         PUT   (1),(0)                                                          
*                                                                               
OBPR50   DS   0H                                                                
         B     OBX                                                              
*                                                                               
OBX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
OBPRL    DS    0H                                                               
*                                                                               
*                                                                               
         CLI   OBERRSW,C'Y'          WERE ERRORS ECOUNTERED?                    
         BNE   OBPRL20                                                          
         MVC   P(34),=C'*** WARNING-ERRORS ENCOUNTERED ***'                     
         MVC   P2(54),=C'*** PRODUCTS WITH MISSING UCOMM DATA NOT PROCEX        
               SSED ***'                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CP    OBCOUNT,=P'0'         ANY RECORDS CREATED?                       
         BE    OBX                   NO - JUST EXIT                             
*                                                                               
OBPRL20  CLI   SVQOPT3,C'Y'          TEST RUN - NO TAPE                         
         BE    OBX                                                              
*                                                                               
         L     R2,=A(SBITAPE)                                                   
         CLOSE ((2),)                                                           
         B     OBX                                                              
*                                                                               
         XIT                                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
OBERRSW  DS    CL1                                                              
OBWORK   DS    CL20                                                             
*                                                                               
         DROP  R7                                                               
*                                                                               
***********************************************************************         
*   GETAOR                                                                      
***********************************************************************         
         SPACE 2                                                                
GETAOR   NMOD1 0,GETAOR                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   KEY2,KEY            PRESERVE THIS KEY                            
         LA    R5,KEY                                                           
         USING AORKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D45'     AOR RECORD CODE                              
         L     RF,ADCLT                                                         
         MVC   AORKAGMD(3),1(RF)   AGY/MED/CLT                                  
*                                                                               
         L     RE,ADBILL                                                        
         USING BILLREC,RE                                                       
         MVC   AORKPRD,BKEYPRD                                                  
         MVC   AORKEST+1(1),BKEYEST                                             
         DROP  RE                                                               
*                                                                               
         MVI   AORKDPT,X'FF'       DEFAULT DAYPART       (NOT USED IN           
         MVI   AORKSTYP,X'FF'      DEFAULT STATION TYPE   SPOT, ONLY            
         MVI   AORKEXT+2,X'FF'     3RD EXTRA NOT USED     IN NET)               
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      TEST THRU PRD                                
         BNE   GAKS21              NOTHING FOR PRODUCT                          
         CLC   KEY(10),KEYSAVE     TEST THRU EST                                
         BE    GAKS08                                                           
         CLC   KEY+8(2),=X'FFFF'   DID WE FIND DEFAULT                          
         BE    GAKS08              YES, USE IT                                  
*                                                                               
GAKS07   DS    0H                                                               
         MVC   KEYSAVE+8(2),=X'FFFF'  TRY DEFAULT EST                           
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GAKS21              NOTHING THERE EITHER                         
*                                                                               
GAKS08   DS    0H                                                               
         CLC   KEY+10(1),KEYSAVE+10   DAYPART                                   
         BE    GAKS10                                                           
         CLI   KEY+10,X'FF'        DID WE FIND DEFAULT                          
         BE    GAKS10                                                           
         CLI   KEYSAVE+10,X'FF'    DID WE TRY FOR IT                            
         BE    GAKS19              YES, TRY UNDER DEFAULT EST                   
*                                                                               
GAKS09   DS    0H                                                               
         MVI   KEYSAVE+10,X'FF'    TRY DEFAULT DAYPART                          
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   GAKS19              NOTHING, DONE                                
*                                                                               
GAKS10   DS    0H                                                               
         CLC   KEY+11(1),KEYSAVE+11    STATION TYPE                             
         BE    GAKS20                                                           
         CLI   KEY+11,X'FF'        DID WE FIND DEFAULLT                         
         BE    GAKS20                                                           
         CLI   KEYSAVE+11,X'FF'    DID WE TRY FOR IT                            
         BE    GAKS18                                                           
         MVI   KEYSAVE+11,X'FF'    TRY DEFAULT                                  
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    GAKS20                                                           
*                                                                               
GAKS18   DS    0H                  NOTHING FOUND                                
         CLI   KEYSAVE+10,X'FF'    WERE WE LOOKING UNDER ALL DPTS               
         BE    GAKS19              YES                                          
*                                  NO, DO IT NOW                                
         MVI   KEYSAVE+11,X'FF'    RESET STATION TYPE                           
         B     GAKS09                                                           
*                                                                               
GAKS19   DS    0H                                                               
         CLC   KEYSAVE+8(2),=X'FFFF'   WERE WE LOOKING UNDER ALL ESTS           
         BE    GAKS21              YES, NOTHING MORE TO DO                      
*                                  NO, DO IT NOW                                
         MVI   KEYSAVE+10,X'FF'    RESET DAYPART                                
         MVI   KEYSAVE+11,X'FF'    RESET STATION TYPE                           
         B     GAKS07                                                           
*                                                                               
GAKS20   DS    0H                  HAVE USABLE AOR KEY                          
         B     GAO4                                                             
*                                                                               
GAKS21   DS    0H                  NO USABLE AOR KEY                            
         B     GAO9                                                             
*                                                                               
GAO4     DS    0H                                                               
         L     R7,ADBUY            USE STATION BUCKET AREA                      
         ST    R7,AREC                                                          
         USING AORREC,R7                                                        
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         LA    R2,AORELS                                                        
*                                                                               
GAO6     DS    0H                                                               
         CLI   0(R2),X'02'         ADDRESS ELEM                                 
         BE    GAO7                                                             
         CLI   0(R2),X'03'         AOR INFO ELEM                                
         BE    GAO8                                                             
         CLI   0(R2),0             EOR                                          
         BE    GAO9                                                             
*                                                                               
GAO6D    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GAO6                                                             
*                                                                               
GAO7     DS    0H                  AOR ADDRESS ELEM                             
         USING AORADREL,R2                                                      
         MVC   WORK(30),AORLIN1    RETURN 'NAME' IN WORK                        
         B     GAO6D                                                            
*                                                                               
GAO8     DS    0H                  AOR INFO ELEM                                
         B     GAO6D               SKIP                                         
         USING AORELEM,R2                                                       
*                                                                               
GAO9     DS    0H                                                               
         MVC   KEY,KEY2            RESTORE SEQ                                  
         GOTO1 HIGH                                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   GETPGRP                                                                     
*                                                                               
*     NOTE: ON ENTRY, FIELD 'THREE' CONTAINS PRDGRP CODE                        
*           ON EXIT,  FIELD 'FULL' CONTAINS FORMATTED PRDGRP CODE               
*                     FIELD 'WORK' CONTAINS PRDGRP NAME                         
***********************************************************************         
         SPACE 2                                                                
GETPGRP  NMOD1 0,GETPGRP                                                        
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   FULL,SPACES         WILL CONTAIN PRDGRP CODE                     
         MVC   WORK,SPACES         WILL CONTAIN PRDGRP NAME                     
*                                                                               
         MVC   KEY2,KEY            PRESERVE THE CURRENT KEY                     
*                                                                               
         LA    R5,KEY              BUILD PRDGRP DEFINITION KEY                  
         USING PRGKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'     PRDGRP RECORD CODE                           
         L     RF,ADCLT                                                         
         MVC   PRGKAGMD,1(RF)      AGY/MED                                      
         MVC   PRGKCLT,2(RF)       CLIENT                                       
         MVC   PRGKID,THREE        PRDGRP ID (SCHEME)                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MUST BE THERE                                
         BNE   GPGRPX              PGRDEF KEY NOT FOUND                         
*                                                                               
         L     R7,ADBUY            USE STATION BUCKET AREA                      
         ST    R7,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVI   ELCODE,X'01'        LOOK FOR BREAK DESCRIPTION ELEMENT           
         BRAS  RE,GETEL                                                         
         BNE   GPGRPX                                                           
         USING PRGEL01,R7                                                       
         ZIC   R0,PRGBK1LN         BREAK 1 LENGTH                               
         ZIC   RE,PRGBK2LN         BREAK 2 LENGTH                               
         AR    RE,R0                                                            
         MVC   FULL(1),THREE       PRDGRP ID                                    
         UNPK  DUB,THREE+1(2)      PRDGRP NUMBER                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FULL+1(0),DUB+3     PRINTABLE PRDGRP NUMBER                      
         MVC   BYTE,PRGBK2LN       HANG ONTO BREAK2 LENGTH                      
         DROP  R7                                                               
*                                                                               
         MVC   PRGKGRP,THREE+1     PUT PRDGRP NUMBER INTO KEY                   
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DID WE FIND THE PRDGRP KEY?                  
         BNE   GPGRPX              NO                                           
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R7,ADBUY                                                         
         MVI   ELCODE,X'10'        LOOK FOR BREAK NAMES ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   GPGRPX                                                           
         USING PRGEL10,R7                                                       
         CLI   BYTE,0              IS 2ND BREAK NAME PRESENT?                   
         BE    *+14                NO, SO USE 1ST BREAK NAME                    
         MVC   WORK(L'PRGNAM2),PRGNAM2                                          
         B     GPGRPX                                                           
         MVC   WORK(L'PRGNAM1),PRGNAM1                                          
         DROP  R7                                                               
*                                                                               
GPGRPX   DS    0H                                                               
         MVC   KEY,KEY2            RESTORE SEQ                                  
         GOTO1 HIGH                                                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   PRNT - PRINTING ROUTINE                                                     
***********************************************************************         
PRNT     NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   CNTRY,C'C'          CANADA GETS DIFFERENT SPROG                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50         FOR GST                                      
*                                                                               
         CLC   P,SPACES                                                         
         BNE   PRNT1                                                            
         CLI   MODE,OFCLAST     IF P EMPTY AND MODE IS OFFICE LAST              
         BE    PRNTX            DON'T PRINT ANYTHING                            
*                                                                               
PRNT1    MVC   HEAD1+55(19),=C'NETPAK BILLING LIST'                             
         MVC   HEAD2+55(19),=C'-------------------'                             
         CLI   MEDIA,C'N'                                                       
         BE    *+14                                                             
         MVC   HEAD1+54(4),=C'SPOT'                                             
         MVI   HEAD2+54,C'-'                                                    
*                                                                               
         CLI   MODE,REQLAST      AT REQLAST DON'T SHOW AN OFFICE                
         BE    PRNT2                                                            
*                                                                               
         CLI   QCLT,C'$'           TEST OFFICE LIST                             
         BNE   PRNT2                                                            
         MVC   HEAD2(6),=C'OFFICE'                                              
         MVC   HEAD2+9(1),OFFICE                                                
         CLI   SV00APRF+2,C'Y'     TEST ALWAYS PRINT AS CHAR                    
         BE    PRNT2                                                            
*                                                                               
         MVC    HEAD2+9(2),SAVCOFF                                              
*                                                                               
******   GOTO1 =V(OFFOUT),DMCB,OFFICE,HEXOUT,HEAD2+9                            
*                                                                               
PRNT2    DS    0H                                                               
         CLI   QOPT5,C'A'         TEST AOR ONLY                                 
         BNE   *+10                                                             
         MVC   HEAD6(18),=C'**AOR BILLS ONLY**'                                 
*                                                                               
         CLI   QOPT5,C'B'         AOR AND AOR/CLIENT                            
         BNE   *+10                                                             
         MVC   HEAD6(24),=C'**AOR AND CLIENT BILLS**'                           
*                                                                               
         CLI   QOPT5,C'X'         NON=AOR BILLS ONLY                            
         BNE   *+10                                                             
         MVC   HEAD6(22),=C'**NON-AOR BILLS ONLY**'                             
*                                                                               
         CLI   QOPT5,C'C'         COMMISSION ONLY BILLS                         
         BNE   *+10                                                             
         MVC   HEAD6(25),=C'**COMMISSION ONLY BILLS**'                          
*                                                                               
         CLI   QOPT5,C'N'         EXCLUDE COMMISSION ONLY BILLS                 
         BNE   *+10                                                             
         MVC   HEAD6(34),=C'**COMMISSION ONLY BILLS EXCLUDED**'                 
*                                                                               
         CLI   QOPT5,C'S'         SOON BILLS ONLY?                              
         BNE   *+10                                                             
         MVC   HEAD6(19),=C'**SOON BILLS ONLY**'                                
*                                                                               
         CLI   QOPT4,C'M'         MOS DATES                                     
         BNE   *+10                                                             
         MVC   HEAD4+49(32),=C'**MONTH OF SERVICE DATE FILTER**'                
*                                                                               
         LA    R4,HEAD3+49                                                      
         MVC   0(23,R4),=C'PERIOD FROM          TO'                             
         GOTO1 DATCON,DMCB,SVQST,(8,12(R4))                                     
         GOTO1 DATCON,DMCB,SVQEND,(8,24(R4))                                    
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    XIT1                                                                   
         EJECT                                                                  
*                             HEADHOOK ROUTINE                                  
         SPACE 2                                                                
HHROUT   NTR1                                                                   
*                                                                               
         CLC   P,SPACES                                                         
         BNE   HHR1                                                             
         CLI   MODE,OFCLAST     IF P EMPTY AND MODE IS OFFICE LAST              
         BE    HHR4             DON'T PRINT ANYTHING                            
*                                                                               
HHR1     CLI   MODE,REQLAST        SKIP AT REQLAST                              
         BE    HHR4                                                             
*                                                                               
         CLC   =C'ALL',QCLT        FIX CLIENT HEADLINE                          
         BNE   HHR2                                                             
         CLI   PROGPROF+0,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2                                                             
         MVC   HEAD3+9(3),=C'ALL'                                               
         MVC   HEAD3+13(24),SPACES                                              
*                                                                               
HHR2     DS    0H                                                               
         MVC   HEAD4(07),=C'PRODUCT'                                            
         CLC   =C'ALL',QPRD        FIX PRODUCT HEADLINE                         
         BNE   HHR2B                                                            
         CLI   PROGPROF+1,C'Y'     UNLESS SEPARATE PAGE                         
         BE    HHR2B                                                            
         MVC   HEAD4+9(3),=C'ALL'                                               
         MVC   HEAD4+13(24),SPACES                                              
*                                                                               
HHR2B    DS    0H                                                               
         MVC   HEAD5(08),=C'ESTIMATE'                                           
         CLC   =C'ALL',QEST        FIX ESTIMATE HEADLINE                        
         BNE   HHR2C                                                            
         MVC   HEAD5+9(3),=C'ALL'                                               
         MVC   HEAD5+13(24),SPACES                                              
*                                                                               
HHR2C    CLI   MODE,OFCLAST        LAST FOR OFFICE                              
         BE    HHR4                                                             
         CLI   MODE,CLTLAST        UNLESS AFTER CLIENT LAST                     
         BH    HHR4                PUT CLIENT DATA IN MIDLINE                   
         CLC   SVMID+8(3),CLT      IF STILL WITH SAME CLIENT                    
         BNE   HHR4                                                             
         CLC   P,SVMID             DON'T REPEAT THE CLIENT LINE                 
         BE    HHR4                                                             
***      CLC   P,SPACES            EMPTY LINE?                                  
***      BE    HHR4                                                             
         MVC   MID1,SVMID                                                       
*                                                                               
HHR4     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PSTRT    DS    0X                                                               
         DC    X'00010203042A35380000000000000000'   00-0F                      
         DC    X'00050607000000000000000000000000'   10-1F                      
         DC    X'0008090A1F393A000000000000000000'   20-2F                      
         DC    X'000B0C21003B3C000000000000000000'   30-3F                      
         DC    X'000D0E0F101112132B00000000000000'   40-4F                      
         DC    X'0014151617182D2E3031373D3E3F4041'   50-5F                      
         DC    X'00222324252600000000000000000000'   60-6F                      
         DC    X'00203233340000000000000000000000'   70-7F                      
         DC    X'00000000000000000000000000000000'   80-8F                      
         DC    X'00000000000000000000000000000000'   90-9F                      
         DC    X'00000000000000000000000000000000'   A0-AF                      
         DC    X'001C0000000000000000000000000000'   B0-BF                      
         DC    X'1D002728292C2F000000000000000000'   C0-CF                      
         DC    X'00000000000000000000000000000000'   D0-DF                      
         DC    X'00000000000000000000000000000000'   E0-EF                      
         DC    X'191A1B00000000000000000000003600'   F0-FF                      
         PRINT OFF                                                              
         EJECT                                                                  
         MACRO                                                                  
         DSPEC &COL,&LEN,&DTYPE,&MEDIA_FILTER=,&ROUTINE=,              +        
               &FORMAT=,&LITERAL=,&SPECIAL=                                     
         LCLA  &EL,&DC,&FLV,&FL                                                 
         LCLC  &MF,&THREEB                                                      
.*                                                                              
&EL      SETA  8                                                                
&FLV     SETA  0                                                                
&FL      SETA  &LEN                                                             
.*                                                                              
&MF      SETC  ' '                                                              
         AIF   (T'&MEDIA_FILTER EQ 'O').P10                                     
&MF      SETC  '&MEDIA_FILTER'                                                  
.*                                                                              
.P10     AIF   (T'&ROUTINE EQ 'O').P20                                          
&DC      SETA  254                                                              
         AGO   .P60                                                             
.*                                                                              
.P20     AIF   (T'&FORMAT EQ 'O').P40                                           
.*                                                                              
         AIF   (T'&FORMAT NE 'N').P30                                           
&FLV     SETA  &FORMAT                                                          
         AGO   .P40                                                             
.*                                                                              
.P30     AIF   ('&FORMAT' EQ 'YYMMDD').P40                                      
         AIF   ('&FORMAT' EQ 'YYMM').P40                                        
&FLV     SETA  32                                                               
         AIF   ('&FORMAT' EQ 'MMDDYY').P40                                      
&FLV     SETA  33                                                               
         AIF   ('&FORMAT' EQ 'MMYY').P40                                        
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYYMMDD').P40                                     
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYYMM').P40                                       
&FLV     SETA  34                                                               
         AIF   ('&FORMAT' EQ 'CYY').P40                                         
&FLV     SETA  35                                                               
         AIF   ('&FORMAT' EQ 'CMMYY').P40                                       
&FLV     SETA  1                                                                
         AIF   ('&FORMAT' EQ 'PACKED').P40                                      
&FLV     SETA  2                                                                
         AIF   ('&FORMAT' EQ 'BINARY').P40                                      
&FLV     SETA  3                                                                
         AIF   ('&FORMAT' EQ 'UNPACKED').P40                                    
&FLV     SETA  4                                                                
         AIF   ('&FORMAT' EQ 'EBCDIC').P40                                      
&FLV     SETA  5                                                                
         AIF   ('&FORMAT' EQ 'LEADING_MINUS_SIGN').P40                          
&FLV     SETA  6                                                                
         AIF   ('&FORMAT' EQ 'TRAILING_MINUS_SIGN').P40                         
         MNOTE 8,'INVALID DATA FORMAT'                                          
         MEXIT                                                                  
.*                                                                              
.P40     AIF   (T'&SPECIAL EQ 'O').P50                                          
&DC      SETA  &SPECIAL                                                         
         AGO   .P60                                                             
.*                                                                              
.P50     AIF   (T'&LITERAL EQ 'O').P60                                          
&DC      SETA  241                                                              
&EL      SETA  &EL+K'&LITERAL-2                                                 
&FL      SETA  K'&LITERAL-2                                                     
.*                                                                              
.P60     ANOP                                                                   
&THREEB  SETC  'AL2(&COL),AL1(&FL)'                                             
         AIF   (T'&ROUTINE EQ 'O').P70                                          
&THREEB  SETC  'AL3(&ROUTINE)'                                                  
.*                                                                              
.P70     AIF   (T'&DTYPE EQ 'O').P80                                            
&DC      SETA  1                                                                
         AIF   ('&DTYPE' EQ 'AGENCY').P80                                       
&DC      SETA  2                                                                
         AIF   ('&DTYPE' EQ 'MEDIA').P80                                        
&DC      SETA  3                                                                
         AIF   ('&DTYPE' EQ 'OFFICE').P80                                       
&DC      SETA  4                                                                
         AIF   ('&DTYPE' EQ 'MKTGROUP').P80                                     
&DC      SETA  5                                                                
         AIF   ('&DTYPE' EQ 'RETAIL').P80                                       
&DC      SETA  6                                                                
         AIF   ('&DTYPE' EQ 'SUBMEDIA').P80                                     
&DC      SETA  7                                                                
         AIF   ('&DTYPE' EQ 'COSTTYPE').P80                                     
&DC      SETA  17                                                               
         AIF   ('&DTYPE' EQ 'CLTCODE').P80                                      
&DC      SETA  18                                                               
         AIF   ('&DTYPE' EQ 'CLTNAME').P80                                      
&DC      SETA  19                                                               
         AIF   ('&DTYPE' EQ 'CLTNUM').P80                                       
&DC      SETA  33                                                               
         AIF   ('&DTYPE' EQ 'PRDCODE').P80                                      
&DC      SETA  34                                                               
         AIF   ('&DTYPE' EQ 'PRDNAME').P80                                      
&DC      SETA  35                                                               
         AIF   ('&DTYPE' EQ 'PRDNUM').P80                                       
&DC      SETA  36                                                               
         AIF   ('&DTYPE' EQ 'DIVISION').P80                                     
&DC      SETA  37                                                               
         AIF   ('&DTYPE' EQ 'PUSER1').P80                                       
&DC      SETA  38                                                               
         AIF   ('&DTYPE' EQ 'PUSER2').P80                                       
&DC      SETA  49                                                               
         AIF   ('&DTYPE' EQ 'ESTCODE').P80                                      
&DC      SETA  50                                                               
         AIF   ('&DTYPE' EQ 'ESTNAME').P80                                      
&DC      SETA  51                                                               
         AIF   ('&DTYPE' EQ 'ESTFILTS').P80                                     
&DC      SETA  53                                                               
         AIF   ('&DTYPE' EQ 'EUSER1').P80                                       
&DC      SETA  54                                                               
         AIF   ('&DTYPE' EQ 'EUSER2').P80                                       
&DC      SETA  65                                                               
         AIF   ('&DTYPE' EQ 'INVNUM2').P80                                      
&DC      SETA  66                                                               
         AIF   ('&DTYPE' EQ 'INVNUM4').P80                                      
&DC      SETA  67                                                               
         AIF   ('&DTYPE' EQ 'INVNUM6').P80                                      
&DC      SETA  68                                                               
         AIF   ('&DTYPE' EQ 'RUNDATE').P80                                      
&DC      SETA  69                                                               
         AIF   ('&DTYPE' EQ 'INVDATE').P80                                      
&DC      SETA  70                                                               
         AIF   ('&DTYPE' EQ 'DUEDATE').P80                                      
&DC      SETA  71                                                               
         AIF   ('&DTYPE' EQ 'MOS').P80                                          
&DC      SETA  72                                                               
         AIF   ('&DTYPE' EQ 'TODAY').P80                                        
&DC      SETA  81                                                               
         AIF   ('&DTYPE' EQ 'GROSS').P80                                        
&DC      SETA  82                                                               
         AIF   ('&DTYPE' EQ 'NET').P80                                          
&DC      SETA  83                                                               
         AIF   ('&DTYPE' EQ 'ACTUAL').P80                                       
&DC      SETA  84                                                               
         AIF   ('&DTYPE' EQ 'GROSS_COMMISSION').P80                             
&DC      SETA  85                                                               
         AIF   ('&DTYPE' EQ 'ACTUAL_COMMISSION').P80                            
&DC      SETA  86                                                               
         AIF   ('&DTYPE' EQ 'TAX').P80                                          
&DC      SETA  87                                                               
         AIF   ('&DTYPE' EQ 'GROSS_MINUS_TAX').P80                              
&DC      SETA  88                                                               
         AIF   ('&DTYPE' EQ 'FEE').P80                                          
&DC      SETA  89                                                               
         AIF   ('&DTYPE' EQ 'MEDIA_ACTUAL').P80                                 
&DC      SETA  90                                                               
         AIF   ('&DTYPE' EQ 'GST').P80                                          
&DC      SETA  91                                                               
         AIF   ('&DTYPE' EQ 'PST').P80                                          
&DC      SETA  93                                                               
         AIF   ('&DTYPE' EQ 'MARKET').P80                                       
&DC      SETA  94                                                               
         AIF   ('&DTYPE' EQ 'PST_ALONE').P80                                    
&DC      SETA  95                                                               
         AIF   ('&DTYPE' EQ 'HST_ALONE').P80                                    
&DC      SETA  99                                                               
         AIF   ('&DTYPE' EQ 'ESTIMATE_ACTUAL').P80                              
&DC      SETA  102                                                              
         AIF   ('&DTYPE' EQ 'PGRNAME').P80                                      
&DC      SETA  113                                                              
         AIF   ('&DTYPE' EQ 'REVISION_STATUS').P80                              
&DC      SETA  114                                                              
         AIF   ('&DTYPE' EQ 'AOR_STATUS').P80                                   
&DC      SETA  115                                                              
         AIF   ('&DTYPE' EQ 'BILLTYPE').P80                                     
&DC      SETA  240                                                              
         AIF   ('&DTYPE' EQ 'ZEROES').P80                                       
&DC      SETA  242                                                              
         AIF   ('&DTYPE' EQ 'SPACES').P80                                       
         MNOTE 8,'INVALID DATATYPE'                                             
         MEXIT                                                                  
.*                                                                              
.P80     DC    X'05',AL1(&EL),AL1(&DC),&THREEB,C'&MF',AL1(&FLV)                 
         AIF   (T'&LITERAL EQ 'O').P90                                          
         DC    C&LITERAL                                                        
.P90     ANOP                                                                   
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
*        SPEC TABLES                                                            
         SPACE 2                                                                
DSELEM   DSECT                     DATA SPEC ELEM                               
         DS    XL1'05'             ELEM CODE                                    
         DS    AL1(0)              ELEM LENGTH                                  
DSECODE  DS    XL1                 DATA CODE                                    
DSEPROC  DS    AL3                 A(AGENCY ROUTINE)                            
         ORG   DSEPROC                                                          
DSEPOS   DS    XL2                 POSITION IN OUTPUT                           
DSELEN   DS    XL1                 DATA LENGTH                                  
DSEMEDF  DS    CL1                 MEDIA FILTER                                 
DSEFMT   DS    XL1                 FORMAT CONTROL                               
DSEDATA  DS    0X                  DATA (FOR LITERALS)                          
*                                                                               
         ORG   DSEPOS              REDEFINE FOR FILTERS                         
DSEFTYP  DS    CL1                                                              
DSEFILT  DS    0X                                                               
         ORG                                                                    
*                                                                               
*        DATA CODES-                                                            
*        X'01'=AGENCY                                                           
*        X'02'=MEDIA                                                            
*        X'03'=OFFICE                                                           
*        X'04'=MARKET GROUP                                                     
*        X'11'=CLT CODE                                                         
*        X'12'=CLT NAME                                                         
*        X'13'=CLT NUMBER                                                       
*        X'21'=PRD CODE                                                         
*        X'22'=PRD NAME                                                         
*        X'23'=PRD NUMBER                                                       
*        X'24'=DIVISION (FROM PRD HDR)                                          
*        X'31'=EST CODE                                                         
*        X'32'=EST NAME                                                         
*        X'33'=EST FILTERS                                                      
*        X'41'=INV NO(2)                                                        
*        X'42'=INV NO(4)                                                        
*        X'43'=INV NO(6)                                                        
*        X'44'=BILL RUN DATE                                                    
*        X'45'=INVOICE DATE                                                     
*        X'46'=DUE DATE                                                         
*        X'47'=MONTH OF SERVICE                                                 
*        X'48'=TODAY'S DATE                                                     
*        X'51'=GROSS                                                            
*        X'52'=NET                                                              
*        X'53'=ACTUAL                                                           
*        X'54'=AGYCOM (GROSS-NET)                                               
*        X'55'=AGYCOM (ACTUAL-NET)                                              
*        X'56'=ACTUAL-TAX                                                       
*        X'71'=REVISION STATUS                                                  
*        X'72'=AOR STATUS                                                       
*        X'73'=BILL TYPE                                                        
*        X'C0'=SPECIAL (JW)                                                     
*        X'C2'=SPECIAL (COMPTON NET/CABLE)                                      
*        X'C3'=SPECIAL (YNR MEDIA)                                              
*        X'C4'=SPECIAL (DDB AGENCY CODES)                                       
*        X'C5'=SPECIAL (DDB CLIENT ESTIMATES)                                   
*        X'B1'=FILTERS                                                          
*        X'F0'=ZEROS                                                            
*        X'F1'=LITERAL                                                          
*        X'F2'=SPACES                                                           
         SPACE 2                                                                
SPBT02   CSECT                                                                  
         EJECT                                                                  
*        MARSTELLER, FOOTE-CONE/LIEBER KATZ,OH (FCRJR), SCALI                   
         SPACE 2                                                                
STSPECS  DS    0X                                                               
         DSPEC 3,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 1,3,CLTCODE                                                      
         DSPEC 7,3,PRDCODE                                                      
         DSPEC 10,4,PRDNUM,FORMAT=EBCDIC                                        
         DSPEC 15,3,ESTCODE                                                     
         DSPEC 18,4,MOS,FORMAT=YYMM                                             
         DSPEC 22,6,INVNUM6                                                     
         DSPEC 28,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 34,6,GROSS,FORMAT=PACKED                                         
         DSPEC 40,6,NET,FORMAT=PACKED                                           
         DSPEC 46,6,ACTUAL,FORMAT=PACKED                                        
         DSPEC 52,6,ACTUAL_COMMISSION,FORMAT=PACKED                             
         DSPEC 58,6,ZEROES,FORMAT=PACKED                                        
         DSPEC 64,1,MEDIA                                                       
         DSPEC 74,LITERAL='O'                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*        SAATCHI/ZENITH TOYOTA SPECS - DF-T, TH-T                               
         SPACE 2                                                                
THTSPECS DS    0X                                                               
*        NOTE- EVERYTHING DONE IN SPECIAL PROC EXECPT DUMMY                     
*              EST NAME SPEC TO FORCE READING OF EST HDR.                       
*              WILL BE OVERWRITTEN.                                             
         DSPEC 1,20,ESTNAME                                                     
*                                                                               
         DSPEC ROUTINE=DFTPROC                              AGY SPCL            
         DC    X'0000'                                                          
         EJECT                                                                  
*          'STANDARD' TAPE SPECS - (LIKE PRODUCTION)                            
         SPACE 2                                                                
SPSPECS  DS    0X                                                               
         DSPEC 1,2,AGENCY                                                       
         DSPEC 3,LITERAL='S'                                S-SYSTEM            
         DSPEC 4,1,MEDIA                                                        
         DSPEC 5,3,CLTCODE                                                      
         DSPEC 8,3,PRDCODE                                                      
         DSPEC 11,3,ESTCODE                                                     
         DSPEC 17,1,OFFICE                                                      
         DSPEC 18,6,INVNUM6                                                     
         DSPEC 24,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 30,10,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 40,10,NET,FORMAT=UNPACKED                                        
         DSPEC 50,10,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 60,10,ZEROES,FORMAT=UNPACKED                                     
         DSPEC 70,4,MOS,FORMAT=YYMM                                             
         DSPEC 82,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 88,4,PRDNUM                                                      
         DC    X'0000'                                                          
         EJECT                                                                  
*        COMPTON (ALSO ROSS NY AND RUMRILL-HOYT)                                
         SPACE 2                                                                
COSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='02'                            MEDIA- R,X             
         DSPEC 1,LITERAL='01',MEDIA_FILTER=T             MEDIA-T                
         DSPEC 1,2,SPECIAL=194,MEDIA_FILTER=N                                   
         DSPEC 3,3,ESTCODE                                                      
         DSPEC 12,6,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 18,4,MOS,FORMAT=YYMM                                             
         DSPEC 25,3,CLTCODE                                                     
         DSPEC 28,3,PRDCODE                                                     
         DSPEC 31,3,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 79,6,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 103,9,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 112,7,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 119,9,NET,FORMAT=UNPACKED                                        
         DSPEC 128,9,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 137,5,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 147,4,INVNUM4                                                    
*                                                          148(3)               
         DSPEC 142,6,INVDATE,FORMAT=MMDDYY                                      
         DSPEC 151,11,ESTNAME                                                   
         DSPEC 162,16,CLTNAME                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
         SPACE 2                                                                
*                  OMDUSEC + PHDNY - STARBUCKS                                  
SBSPECS  DS    0X                                                               
         DSPEC 1,1,LITERAL='H'                                                  
         DSPEC 3,10,LITERAL='127326|100'                                        
         DSPEC 14,09,LITERAL='NEWYORK01'                                        
         DSPEC 33,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 151,4,MOS,FORMAT=YYMM      WILL BE USED IN PS070                 
         DSPEC 161,32,PUSER1              ALSO NEEDED IN PS070                  
         DSPEC 201,32,EUSER1              ALSO NEEDED IN PS070                  
         DSPEC ROUTINE=SBPROC                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*        BBDO - CHRYSLER                                                        
         SPACE 2                                                                
BDSPECSC DS    0X                                                               
         DSPEC 1,2,AGENCY                                                       
         DSPEC 3,LITERAL='B'                               MEDIA-B              
         DSPEC 4,1,MEDIA                                                        
         DSPEC 5,3,CLTCODE                                                      
         DSPEC 8,3,PRDCODE                                                      
         DSPEC 11,3,ESTCODE                                                     
         DSPEC 14,4,MOS,FORMAT=YYMM                                             
         DSPEC 18,6,RUNDATE,FORMAT=YYMMDD                                       
         DSPEC 24,17,SPACES                                                     
         DSPEC 41,6,INVDATE,FORMAT=YYMMDD                                       
*                                                          INV NO               
*                                                          (M-MM-NNNN)          
         DSPEC 47,1,MEDIA                                                       
         DSPEC 48,LITERAL='-'                                                   
         DSPEC 49,2,INVNUM2                                                     
         DSPEC 51,LITERAL='-'                                                   
         DSPEC 52,4,INVNUM4                                                     
*                                                                               
         DSPEC 56,20,ESTNAME                                                    
         DSPEC 76,20,PRDNAME                                                    
         DSPEC 96,12,GROSS,FORMAT=UNPACKED                                      
         DSPEC 108,12,NET,FORMAT=UNPACKED                                       
         DSPEC 120,12,ZEROES,FORMAT=UNPACKED                                    
         DSPEC 132,12,NET,FORMAT=UNPACKED                                       
         DSPEC ROUTINE=BDCPROC                             AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        PHDDE                                                                  
         SPACE 2                                                                
BNSPECSC DS    0X                                                               
         DSPEC 3,LITERAL='S'                               SYSTEM               
         DSPEC 3,LITERAL='N',MEDIA_FILTER=N                SYSTEM-NET           
         DSPEC 4,1,MEDIA                                                        
         DSPEC 4,1,SUBMEDIA,MEDIA_FILTER=N                                      
         DSPEC 5,3,CLTCODE                                                      
         DSPEC 8,20,CLTNAME                                                     
         DSPEC 28,3,PRDCODE                                                     
         DSPEC 31,20,PRDNAME                                                    
         DSPEC 51,3,ESTCODE                                                     
         DSPEC 54,20,ESTNAME                                                    
         DSPEC 74,20,SPACES                                                     
         DSPEC 94,LITERAL='S'                              SYSTEM               
         DSPEC 94,LITERAL='N',MEDIA_FILTER=N               SYSTEM-NET           
         DSPEC 95,1,MEDIA                                                       
         DSPEC 95,1,SUBMEDIA,MEDIA_FILTER=N                                     
         DSPEC 96,6,INVNUM6                                                     
**       DSPEC 102,8+8+6,BILLDATE+DUEDATE+MOS (FROM BNCPROC)                    
         DSPEC 124,6,ACTUAL,FORMAT=PACKED    $$ ARE 'FIXED' IN BNPROC           
         DSPEC 136,6,NET,FORMAT=PACKED                                          
         DSPEC 148,4,LITERAL=' .00'                                             
         DSPEC 152,6,ACTUAL_COMMISSION,FORMAT=PACKED                            
         DSPEC 164,6,GROSS,FORMAT=PACKED                                        
         DSPEC 176,32,PUSER1                                                    
         DSPEC 208,16,PUSER2                                                    
         DSPEC 224,32,EUSER1                                                    
         DSPEC 256,16,EUSER2                                                    
**       DSPEC 272,12,AORAMT                                                    
         DSPEC 284,12,RETAIL                                                    
         DSPEC 296,LITERAL='X'                                                  
         DSPEC ROUTINE=BNCPROC                             AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        HENDERSON                                                              
         SPACE 2                                                                
HESPECS  DS    0X                                                               
*                                               PNAME+18(2) AT REC+49           
         DSPEC 32,20,PRDNAME                                                    
         DSPEC 32,18,SPACES                                                     
         DSPEC 3,6,INVNUM6                                                      
         DSPEC 9,1,MEDIA                                                        
         DSPEC 9,LITERAL='TV',MEDIA_FILTER=T             MEDIA-T                
         DSPEC 11,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 17,10,GROSS,FORMAT=UNPACKED                                      
         DSPEC 46,4,PRDNUM,FORMAT=EBCDIC                                        
         DC    X'0000'                                                          
         EJECT                                                                  
*        INTERPUBLIC (MARSCHALK AND MCCANN)                                     
         SPACE 2                                                                
INSPECS  DS    0X                                                               
         DSPEC 1,2,AGENCY                                                       
         DSPEC 3,LITERAL='S'                             S=SPOT                 
         DSPEC 4,1,MEDIA                                                        
         DSPEC 5,3,CLTCODE                                                      
         DSPEC 8,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 12,1,OFFICE                                                      
         DSPEC 13,3,PRDCODE                                                     
         DSPEC 17,4,PRDNUM,FORMAT=EBCDIC                                        
         DSPEC 21,3,ESTCODE                                                     
         DSPEC 24,6,INVNUM6                                                     
         DSPEC 30,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 36,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 42,10,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 52,10,NET,FORMAT=UNPACKED                                        
         DSPEC 62,10,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 72,10,ZEROES,FORMAT=EBCDIC                                       
         DC    X'0000'                                                          
         EJECT                                                                  
*        MCCANN - ATT                                                           
         SPACE 2                                                                
MCASPCS  DS    0X                                                               
         DSPEC 1,LITERAL='S'                             S=SPOT                 
         DSPEC 2,1,MEDIA                                                        
         DSPEC 4,3,CLTCODE                                                      
         DSPEC 7,3,PRDCODE                                                      
         DSPEC 10,3,ESTCODE                                                     
         DSPEC 16,4,MOS,FORMAT=YYMM                                             
         DSPEC 20,6,ESTIMATE_ACTUAL,FORMAT=PACKED                               
         DSPEC 41,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 47,6,INVNUM6                                                     
         DSPEC 53,6,ACTUAL,FORMAT=PACKED                                        
         DSPEC 59,6,GROSS,FORMAT=PACKED                                         
         DSPEC 65,6,NET,FORMAT=PACKED                                           
         DSPEC 71,6,ZEROES,FORMAT=PACKED                                        
         DSPEC 77,1,REVISION_STATUS                                             
         DSPEC 78,1,ESTFILTS                                                    
         DSPEC 79,3,ESTNAME                                                     
         DSPEC 82,6,DUEDATE,FORMAT=YYMMDD                                       
         DC    X'0000'                                                          
         EJECT                                                                  
*        KENYON + ECKHARDT                                                      
         SPACE 2                                                                
KNSPECS  DS    0X                                                               
         DSPEC 2,4,PRDNUM,FORMAT=EBCDIC                    P NUM +3(2)          
         DSPEC 7,4,PRDNUM,FORMAT=EBCDIC                    P NUM +8(2)          
         DSPEC 1,3,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 6,3,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 11,LITERAL='S'                                                   
         DSPEC 12,1,MEDIA                                                       
         DSPEC 13,6,INVNUM6                                                     
         DSPEC 20,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 29,10,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 39,7,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 46,10,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 56,10,NET,FORMAT=UNPACKED                                        
         DSPEC 76,4,RUNDATE,FORMAT=MMYY                                         
         DSPEC 80,LITERAL='B'                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*        JWT                                                                    
*                                                                               
*        NOTE - THERE IS SPECIAL ADDITIONAL JWT CODE THAT MODIFIES              
*               THE TABLE-BUILD RECORD. SEE JWTPROC.                            
         SPACE 2                                                                
JWSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='1'                                                    
         DSPEC 5,4,PRDNUM,FORMAT=EBCDIC                    P NUM +5(3)          
         DSPEC 2,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 9,2,INVNUM2                                                      
         DSPEC 11,LITERAL='BS',MEDIA_FILTER=R              MEDIA - R            
         DSPEC 11,LITERAL='B ',MEDIA_FILTER=X              MEDIA - X            
         DSPEC 11,LITERAL='VS',MEDIA_FILTER=T              MEDIA - T            
         DSPEC 11,LITERAL='VB',MEDIA_FILTER=N              MEDIA - N            
         DSPEC 13,4,INVNUM4                                                     
         DSPEC 17,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 23,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 29,LITERAL='00'                                                  
         DSPEC 31,3,ESTCODE                                                     
         DSPEC 35,11,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 55,11,NET,FORMAT=UNPACKED                                        
         DSPEC 46,9,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 66,11,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 77,11,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 88,9,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 123,1,OFFICE                                                     
         DSPEC 124,3,CLTCODE                                                    
         DSPEC 127,3,PRDCODE                                                    
         DSPEC 131,5,SPECIAL=192                           MCA                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        MINDSHARE - BURGER KING                                                
*                                                                               
*        NOTE - 2 RECORDS WILL BE GENERATED FOR EACH INVOICE                    
*               A HEADER AND A DETAIL                                           
         SPACE 2                                                                
H7SPECS  DS    0X                                                               
         DSPEC 1,LITERAL='US60'                                                 
         DSPEC 17,LITERAL='64581'                                               
         DSPEC 27,LITERAL='H1'                                                  
         DSPEC 45,LITERAL='USD'                                                 
         DSPEC 70,11,ZEROES,FORMAT=EBCDIC  H7PROC WILL ADD DECIMAL PT           
         DSPEC ROUTINE=H7PROC                              AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        LORD GELLER  (VERY LIKE JWT)                                           
         SPACE 2                                                                
LGSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='1'                                                    
         DSPEC 5,4,PRDNUM,FORMAT=EBCDIC                    P NUM +3(3)          
         DSPEC 2,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 9,2,INVNUM2                                                      
         DSPEC 11,LITERAL='BS',MEDIA_FILTER=R              MEDIA-R              
         DSPEC 11,LITERAL='VS',MEDIA_FILTER=T              MEDIA-T              
         DSPEC 11,LITERAL='NE',MEDIA_FILTER=N              MEDIA-N              
         DSPEC 11,LITERAL='X ',MEDIA_FILTER=X              MEDIA-X              
         DSPEC 13,4,INVNUM4                                                     
         DSPEC 17,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 23,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 29,LITERAL='00'                                                  
         DSPEC 31,3,ESTCODE                                                     
         DSPEC 35,11,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 55,11,NET,FORMAT=UNPACKED                                        
         DSPEC 46,9,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 66,11,ACTUAL_COMMISSION,FORMAT=UNPACKED                          
         DSPEC 77,11,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 88,9,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 123,1,OFFICE                                                     
         DSPEC 124,3,CLTCODE                                                    
         DSPEC 127,3,PRDCODE                                                    
         DSPEC 131,5,SPECIAL=192                           MCA                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        NEEDHAM-HARPER-STEERS (NE)                                             
         SPACE 2                                                                
NHSPECS  DS    0X                                                               
         DSPEC 3,4,PRDNUM,FORMAT=EBCDIC                    P NUM +4(2)          
         DSPEC 1,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 1,LITERAL='2'                                                    
         DSPEC 1,LITERAL='N',MEDIA_FILTER=N                TYPE-NETWK           
         DSPEC 1,LITERAL='N',MEDIA_FILTER=X                TYPE-NETWK           
         DSPEC 8,LITERAL='14',MEDIA_FILTER=R               MEDIA-R              
         DSPEC 8,LITERAL='11',MEDIA_FILTER=X               MEDIA-X              
         DSPEC 8,LITERAL='24',MEDIA_FILTER=T               MEDIA-T              
         DSPEC 8,LITERAL='21',MEDIA_FILTER=N               MEDIA-N              
         DSPEC 10,3,ESTCODE                                                     
         DSPEC 13,LITERAL='0'                                                   
         DSPEC 21,4,MOS,FORMAT=MMYY                                             
         DSPEC 46,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 32,9,ACTUAL,FORMAT=UNPACKED                                      
         DSPEC 136,9,NET,FORMAT=UNPACKED                                        
         DSPEC 118,6,INVNUM6                                                    
         DSPEC 129,LITERAL='1'                                                  
         DSPEC 130,6,INVDATE,FORMAT=MMDDYY                                      
         DC    X'0000'                                                          
*                                                                               
*                                                                               
*        NOTE- FIRST THREE FIELDS ARE- TYPE(1),CLIENT(3),PRD(2)                 
         EJECT                                                                  
*        AMARATTI AND PURIS, LINTAS                                             
         SPACE 2                                                                
PUSPECS  DS    0X                                                               
         DSPEC 24,6,INVNUM6                                                     
         DSPEC 30,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 36,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 46,3,PRDCODE                                                     
         DSPEC 50,LITERAL='0'                                                   
         DSPEC 51,3,ESTCODE                                                     
         DSPEC 54,3,CLTCODE                                                     
         DSPEC 58,11,ACTUAL,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 69,11,NET,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 80,9,ZEROES,FORMAT=LEADING_MINUS_SIGN                            
         DSPEC 89,11,ACTUAL_COMMISSION,FORMAT=LEADING_MINUS_SIGN                
         DSPEC 100,11,GROSS,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 111,1,MEDIA                                                      
         DSPEC 113,3,CLTCODE                                                    
         DSPEC 116,3,PRDCODE                                                    
         DSPEC 141,20,PRDNAME                                                   
         DSPEC 161,11,TAX,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 172,1,OFFICE                                                     
         DSPEC 173,1,OFFICE                                                     
         DSPEC ROUTINE=PUPROC                              AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        GSDM (G7)                                                              
*        MULLEN (M$)                                                            
         SPACE 2                                                                
G7SPECS  DS    0X                                                               
*                                                                               
*        NOTE - $ FIELDS ARE SET TO PACKED HERE                                 
*        IN G7OUT THEY WILL BE CONVERTED TO THEIR PROPER FORMAT                 
*        PACKED FIELDS ARE NEEDED HERE FOR ACCUMULATING $                       
*        FOR AN INVOICE WITH MORE THAT ONE MOS                                  
*                                                                               
         DSPEC 24,6,INVNUM6                                                     
         DSPEC 30,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 36,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 46,3,PRDCODE                                                     
         DSPEC 50,LITERAL='0'                                                   
         DSPEC 51,3,ESTCODE                                                     
         DSPEC 54,3,CLTCODE                                                     
         DSPEC 58,8,ACTUAL,FORMAT=PACKED                                        
         DSPEC 69,8,NET,FORMAT=PACKED                                           
         DSPEC 80,8,ZEROES,FORMAT=PACKED                                        
         DSPEC 89,8,ACTUAL_COMMISSION,FORMAT=PACKED                             
         DSPEC 100,8,GROSS,FORMAT=PACKED                                        
         DSPEC 111,1,MEDIA                                                      
         DSPEC 112,1,SUBMEDIA                                                   
         DSPEC 113,3,CLTCODE                                                    
         DSPEC 116,3,PRDCODE                                                    
         DSPEC 141,20,PRDNAME                                                   
         DSPEC 161,8,TAX,FORMAT=PACKED                                          
         DSPEC 172,1,OFFICE                                                     
         DSPEC 173,1,OFFICE                                                     
         DSPEC ROUTINE=PUPROC                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*        SC JOHNSON INTERFACE AGY=H7                                            
*        NOW AGY=BN AND AGY=OO                                                  
*                                                                               
         SPACE 2                                                                
SCSPECS  DS    0X                                                               
*                                                                               
*        NOTE - $ FIELDS ARE SET TO PACKED HERE                                 
*        IN SCOUT THEY WILL BE CONVERTED TO THEIR PROPER FORMAT                 
*        PACKED FIELDS ARE NEEDED HERE FOR ACCUMULATING $                       
*        FOR AN INVOICE WITH MORE THAT ONE MOS                                  
*                                                                               
         DSPEC 1,LITERAL='DETAIL'                                               
*                                                                               
*        PHDNY USES ESTIMATE USER 1   FOR UNIQUE-REF-NUM                        
*        H7 AND SJ USE AN ESTIMATE UCOMM                                        
*                                                                               
         DSPEC 11,8,EUSER1                                                      
         DSPEC 19,3,PRDCODE                                                     
         DSPEC 38,3,ESTCODE                                                     
         DSPEC 65,4,MOS,FORMAT=YYMM                                             
*  NOTE SCIPROC WILL CONVERT TO MMMYYYY                                         
         DSPEC 92,8,ACTUAL,FORMAT=PACKED                                        
         DSPEC 108,8,GROSS,FORMAT=PACKED                                        
         DSPEC 124,8,NET,FORMAT=PACKED                                          
         DSPEC 140,8,TAX,FORMAT=PACKED                                          
         DSPEC ROUTINE=SCIPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
         SPACE 2                                                                
MZSPECS  DS    0X                                                               
*                                                                               
*        NOTE - $ FIELDS ARE SET TO PACKED HERE                                 
*        IN MZOUT THEY WILL BE CONVERTED TO THEIR PROPER FORMAT                 
*        PACKED FIELDS ARE NEEDED HERE FOR ACCUMULATING $                       
*        FOR AN INVOICE WITH MORE THAT ONE MOS                                  
*                                                                               
         DSPEC 1,LITERAL='L'                                                    
**OLD *  DSPEC 8,6,LITERAL='631000'   DEFAULT ACCOUNT                           
         DSPEC 29,8,ACTUAL,FORMAT=PACKED                                        
*                                                                               
*        FORMAT OF EST USER 1 IS CCCC-NNNNN                                     
*        CCCC=COST CENTER, NNNNN=ORDER #                                        
*                                                                               
* NOTE-  MZIPROC WILL BREAKOUT EUSER1 DATA                                      
*                                                                               
         DSPEC 160,11,EUSER1        COST CENTER-ORDER #                         
         DSPEC 8,6,EUSER2           G/L ACCOUNT                                 
         DSPEC 212,3,PRDCODE                                                    
         DSPEC 274,4,MOS,FORMAT=YYMM                                            
         DSPEC 305,6,PUSER1                                                     
*                                                                               
*        FIELDS BELOW NEEDED FOR INVOICE HEADER INFO                            
*        H AND FIRST L RECORDS                                                  
*                                                                               
*        20 CENTRY ASSUMED                                                      
*                                                                               
         DSPEC 340,6,DUEDATE,FORMAT=MMDDYY                                      
         DSPEC 348,2,LITERAL='20'                                               
         DSPEC 350,6,INVDATE,FORMAT=YYMMDD                                      
         DSPEC 356,2,LITERAL='20'                                               
         DSPEC 358,6,RUNDATE,FORMAT=YYMMDD                                      
         DSPEC ROUTINE=MZIPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        GSCA (GQ)                                                              
MG7SPECS  DS    0X                                                              
*                                                                               
         DSPEC 24,6,INVNUM6                                                     
         DSPEC 30,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 36,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 46,3,PRDCODE                                                     
         DSPEC 50,LITERAL='0'                                                   
         DSPEC 51,3,ESTCODE                                                     
         DSPEC 54,3,CLTCODE                                                     
         DSPEC 58,11,ACTUAL,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 69,11,NET,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 80,9,ZEROES,FORMAT=LEADING_MINUS_SIGN                            
         DSPEC 89,11,ACTUAL_COMMISSION,FORMAT=LEADING_MINUS_SIGN                
         DSPEC 100,11,GROSS,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 111,1,MEDIA                                                      
         DSPEC 112,1,SUBMEDIA                                                   
         DSPEC 113,3,CLTCODE                                                    
         DSPEC 116,3,PRDCODE                                                    
         DSPEC 141,20,PRDNAME                                                   
         DSPEC 161,11,TAX,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 172,1,OFFICE                                                     
         DSPEC 173,1,OFFICE                                                     
         DSPEC ROUTINE=PUPROC                                                   
         DC    X'0000'                                                          
*        TED BATES - GENERAL FOODS                                              
         SPACE 2                                                                
TBSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='S'                               SYSTEM               
         DSPEC 2,1,MEDIA                                                        
         DSPEC 4,3,CLTCODE                                                      
         DSPEC 7,3,PRDCODE                                                      
         DSPEC 10,3,ESTCODE                                                     
         DSPEC 13,LITERAL='B'                              B=BILLING            
         DSPEC 14,4,MOS,FORMAT=YYMM                                             
         DSPEC 18,6,ACTUAL,FORMAT=PACKED                                        
         DSPEC 24,6,GROSS,FORMAT=PACKED                                         
         DSPEC 30,6,NET,FORMAT=PACKED                                           
         DSPEC 36,6,ZEROES,FORMAT=PACKED                                        
         DC    X'0000'                                                          
         EJECT                                                                  
*        TRACY-LOCKE - NEW                                                      
         SPACE 2                                                                
TLSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='A'                               SYSTEM=A             
         DSPEC 2,6,CLTNUM                                                       
         DSPEC 8,LITERAL='1'       FOR MEDIA R AND X                            
         DSPEC 8,LITERAL='2',MEDIA_FILTER=T                                     
         DSPEC 8,LITERAL='2',MEDIA_FILTER=N                                     
         DSPEC 9,4,INVNUM4                                                      
         DSPEC 27,10,GROSS,FORMAT=UNPACKED                                      
         DSPEC 37,8,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 45,10,NET,FORMAT=UNPACKED                                        
         DSPEC 55,3,ESTCODE                                                     
         DSPEC 58,LITERAL='4'      SPOT TV                                      
         DSPEC 58,LITERAL='1',MEDIA_FILTER=X                                    
         DSPEC 58,LITERAL='2',MEDIA_FILTER=R                                    
         DSPEC 58,LITERAL='3',MEDIA_FILTER=N                                    
         DSPEC 59,LITERAL='1'      BUT COMM. ONLY GET '2' (VIA ROUTINE)         
         DSPEC ROUTINE=TLPROC                              AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        TRACY-LOCKE - OLD                                                      
         SPACE 2                                                                
TRSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='S'                             SYSTEM=S               
         DSPEC 2,1,MEDIA                                                        
         DSPEC 3,3,CLTCODE                                                      
         DSPEC 6,4,CLTNUM,FORMAT=EBCDIC                                         
         DSPEC 10,20,CLTNAME                                                    
         DSPEC 30,3,PRDCODE                                                     
         DSPEC 33,4,PRDNUM,FORMAT=EBCDIC                                        
         DSPEC 37,20,PRDNAME                                                    
         DSPEC 57,3,ESTCODE                                                     
         DSPEC 60,24,ESTNAME                                                    
         DSPEC 84,4,MKTGROUP                                                    
         DSPEC 88,6,INVNUM6                                                     
         DSPEC 94,6,RUNDATE,FORMAT=YYMMDD                                       
         DSPEC 100,6,INVDATE,FORMAT=YYMMDD                                      
         DSPEC 106,6,DUEDATE,FORMAT=YYMMDD                                      
         DSPEC 112,4,MOS,FORMAT=YYMM                                            
         DSPEC 116,12,GROSS,FORMAT=UNPACKED                                     
         DSPEC 128,12,NET,FORMAT=UNPACKED                                       
         DSPEC 140,12,ACTUAL,FORMAT=UNPACKED                                    
         DC    X'0000'                                                          
         EJECT                                                                  
*        Y&R                                                                    
         SPACE 2                                                                
YNSPECS  DS    0X                                                               
         DSPEC 2,6,INVNUM6                                                      
         DSPEC 8,LITERAL='0'                                                    
         DSPEC 9,4,PRDNUM,FORMAT=EBCDIC                     PRD NUM             
*                                                          (=CLTNUM)            
         DSPEC 13,3,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 16,1,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 17,2,PRDNAME                                                     
         DSPEC 19,LITERAL='R'                                                   
         DSPEC 20,4,MOS,FORMAT=MMYY                                             
         DSPEC 24,1,MEDIA                                                       
         DSPEC 24,LITERAL='W',MEDIA_FILTER=N               MEDIA N=W            
         DSPEC 25,3,CLTCODE                                                     
         DSPEC 28,3,PRDCODE                                                     
         DSPEC 31,3,ESTCODE                                                     
         DSPEC 34,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 40,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 46,9,ACTUAL,FORMAT=UNPACKED                                      
         DSPEC 55,9,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 64,9,NET,FORMAT=UNPACKED                                         
         DSPEC 73,9,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 85,5,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 89,2,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 91,3,SPECIAL=195                            MEDIA                
         DC    X'0000'                                                          
         EJECT                                                                  
*        MEDIACOM / GFMS                                                        
         SPACE 2                                                                
M2GSPECS DS    0X                                                               
*        DSPEC 3,6,INVNUM6                                                      
         DSPEC 9,3,ESTCODE                                                      
*                                                                               
*        +28(8)   PRINTED INVOICE DATE YYYYMMD                                  
*        +36(6    MONTH OF SERVICE YYYYMM                                       
*                                                                               
         DSPEC 43,LITERAL='22',MEDIA_FILTER=T          MEDIA-TV                 
         DSPEC 43,LITERAL='28',MEDIA_FILTER=R          MEDIA-RADIO              
         DSPEC 43,LITERAL='08',MEDIA_FILTER=X          MEDIA-NET RADIO          
*                                                                               
*        NETPAK DATA FOR THIS FIELD MUST BE SET IN M2GPROC                      
*                                                                               
         DSPEC 89,3,PRDCODE                                                     
         DSPEC ROUTINE=M2GPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        Y&R                                                                    
         SPACE 2                                                                
WWSPECS  DS    0X                                                               
         DSPEC 2,6,INVNUM6                                                      
         DSPEC 8,LITERAL='4'                                                    
         DSPEC 9,4,PRDNUM,FORMAT=EBCDIC                    PRD NUM              
*                                                          (=CLTNUM)            
         DSPEC 13,6,PRDNAME                                                     
         DSPEC 19,LITERAL='R'                                                   
         DSPEC 20,4,MOS,FORMAT=MMYY                                             
         DSPEC 24,3,ESTCODE                                                     
         DSPEC 34,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC 40,6,DUEDATE,FORMAT=MMDDYY                                       
         DSPEC 46,9,ACTUAL,FORMAT=UNPACKED                                      
         DSPEC 55,9,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 64,9,NET,FORMAT=UNPACKED                                         
         DSPEC 73,9,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 85,5,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 89,2,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 91,3,SPECIAL=195                            MEDIA                
         DC    X'0000'                                                          
         EJECT                                                                  
*        OGILVY & MATHER                                                        
         SPACE 2                                                                
OMSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='X'                                                    
         DSPEC 2,1,OFFICE                                                       
         DSPEC 3,LITERAL='1',MEDIA_FILTER=T                MEDIA-TV             
         DSPEC 3,LITERAL='2',MEDIA_FILTER=R                MEDIA-RADIO          
         DSPEC 3,LITERAL='8',MEDIA_FILTER=N                MEDIA-NET            
         DSPEC 3,LITERAL='9',MEDIA_FILTER=X                MEDIA-NTRAD          
         DSPEC 4,3,CLTCODE                                                      
         DSPEC 7,3,PRDCODE                                                      
         DSPEC 10,4,PRDNUM,FORMAT=EBCDIC                                        
         DSPEC 14,22,PRDNAME,FORMAT=EBCDIC                                      
         DSPEC 34,6,INVNUM6                                                     
         DSPEC 40,1,ZEROES,FORMAT=EBCDIC                                        
         DSPEC 41,3,ESTCODE                                                     
         DSPEC 44,3,ESTFILTS,FORMAT=EBCDIC                                      
         DSPEC 47,20,ESTNAME,FORMAT=EBCDIC                                      
         DSPEC 67,20,SPACES                                                     
         DSPEC 87,4,MOS,FORMAT=MMYY                                             
         DSPEC 91,12,MEDIA_ACTUAL,FORMAT=UNPACKED                               
         DSPEC 103,12,ZEROES,FORMAT=UNPACKED                                    
         DSPEC 115,12,ZEROES,FORMAT=UNPACKED                                    
         DSPEC 127,12,ACTUAL_COMMISSION,FORMAT=UNPACKED                         
         DSPEC 139,12,FEE,FORMAT=UNPACKED                                       
         DSPEC 151,12,NET,FORMAT=UNPACKED                                       
         DSPEC 163,6,INVDATE,FORMAT=MMDDYY                                      
         DSPEC 229,6,RETAIL                                                     
         DSPEC 241,6,TODAY,FORMAT=MMDDYY                                        
         DSPEC 253,1,AOR_STATUS                                                 
*                                                                               
         DC    X'0000'                                                          
*                                                                               
*  NOTE - FOR OM AOR BILLS THE BILL NUMBER IS TAKEN FROM THE                    
*         ASSOCIATED CLIENT BILL. SEE CODE BEFORE PB33D.                        
         EJECT                                                                  
*        DDB-NEEDHAM                                                            
         SPACE 2                                                                
DBSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='5'                                                    
         DSPEC 2,3,SPECIAL=196                             AGENCY               
         DSPEC 5,LITERAL='19',MEDIA_FILTER=X               MEDIA-X              
         DSPEC 5,LITERAL='06',MEDIA_FILTER=R               MEDIA-RADIO          
         DSPEC 5,LITERAL='07',MEDIA_FILTER=T               MEDIA-TV             
         DSPEC 5,2,SPECIAL=195,MEDIA_FILTER=N                                   
         DSPEC 7,4,PRDNUM,FORMAT=EBCDIC                                         
         DSPEC 11,2,SPACES                                                      
         DSPEC 13,6,INVNUM6                                                     
*        NB- DO MOS IN 2 PARTS TO GET MMY- DO BEFORE EST                        
*        SO EST WILL CLEAR UP POS 22                                            
         DSPEC 22,4,MOS,FORMAT=MMYY                                             
         DSPEC 23,2,MOS,FORMAT=MMYY                                             
         DSPEC 19,4,SPECIAL=197                            ESTIMATE             
         DSPEC 33,8,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 41,9,ACTUAL,FORMAT=UNPACKED                                      
         DSPEC 50,8,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 58,8,ACTUAL_COMMISSION,FORMAT=UNPACKED                           
         DSPEC 66,8,ZEROES,FORMAT=UNPACKED                                      
         DSPEC 74,9,NET,FORMAT=UNPACKED                                         
         DSPEC 88,6,INVDATE,FORMAT=MMDDYY                                       
         DSPEC ROUTINE=DBPROC                              AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        WESTERN                                                                
         SPACE 2                                                                
WTSPECS  DS    0X                                                               
         DSPEC 1,1,OFFICE                                                       
         DSPEC 2,LITERAL='S'                               SYSTEM               
         DSPEC 2,LITERAL='N',MEDIA_FILTER=N                SYSTEM-NET           
         DSPEC 3,1,MEDIA                                                        
         DSPEC 4,3,CLTCODE                                                      
         DSPEC 7,20,CLTNAME                                                     
         DSPEC 37,3,PRDCODE                                                     
         DSPEC 40,3,ESTCODE                                                     
         DSPEC 46,20,ESTNAME                                                    
         DSPEC 81,6,INVNUM6                                                     
         DSPEC 91,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 97,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 103,4,MOS,FORMAT=YYMM                                            
         DSPEC 107,9,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 116,9,ZEROES,FORMAT=UNPACKED                DISC (=0)            
         DSPEC 125,9,TAX,FORMAT=UNPACKED                                        
         DSPEC 134,9,GST,FORMAT=UNPACKED                                        
         DSPEC 143,9,PST,FORMAT=UNPACKED                                        
         DSPEC 152,2,AGENCY                                                     
         DSPEC 154,9,NET,FORMAT=UNPACKED                                        
         DSPEC ROUTINE=WTPROC                              AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        INITIATIVE MEDIA & TRUE NORTH                                          
         SPACE 2                                                                
IMTNSPCS DS    0X                                                               
         DSPEC 1,1,OFFICE                                                       
         DSPEC 2,LITERAL='S'                               SYSTEM               
         DSPEC 2,LITERAL='N',MEDIA_FILTER=N                SYSTEM-NET           
         DSPEC 3,1,MEDIA                                                        
         DSPEC 4,3,CLTCODE                                                      
         DSPEC 7,20,CLTNAME                                                     
         DSPEC 37,3,PRDCODE                                                     
         DSPEC 40,3,ESTCODE                                                     
         DSPEC 46,20,ESTNAME                                                    
         DSPEC 81,6,INVNUM6                                                     
         DSPEC 91,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 97,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 103,4,MOS,FORMAT=YYMM                                            
         DSPEC 107,11,ACTUAL,FORMAT=LEADING_MINUS_SIGN                          
         DSPEC 118,11,ZEROES,FORMAT=LEADING_MINUS_SIGN      DISC (=0)           
         DSPEC 129,11,TAX,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 140,11,GST,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 151,11,PST,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 162,2,AGENCY                                                     
         DSPEC 164,11,NET,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 268,32,PUSER1                                                    
         DSPEC 300,16,PUSER2                                                    
         DSPEC 316,32,EUSER1                                                    
         DSPEC 348,16,EUSER2                                                    
         DSPEC 364,4,BILLTYPE                                                   
         DSPEC 368,8,CLTNUM,FORMAT=EBCDIC                                       
         DSPEC 376,4,INVDATE,FORMAT=YYMM                                        
         DSPEC 380,11,GROSS,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 396,20,PRDNAME                                                   
         DSPEC 416,11,ACTUAL_COMMISSION,FORMAT=LEADING_MINUS_SIGN               
         DSPEC ROUTINE=IMTNPROC                            AGY SPCL             
         DC    X'0000'                                                          
         EJECT                                                                  
*        DONER-SCHUR                                                            
         SPACE 2                                                                
DASPECS  DS    0X                                                               
         DSPEC ROUTINE=DAPROC                              AGY SPCL             
         DSPEC 9,LITERAL='ST',MEDIA_FILTER=T               MEDIA                
         DSPEC 9,LITERAL='SR',MEDIA_FILTER=R               MEDIA                
         DSPEC 9,LITERAL='SN',MEDIA_FILTER=N               MEDIA                
         DSPEC 13,6,INVNUM6                                                     
         DSPEC 19,5,CLTNUM                                                      
         DSPEC 24,LITERAL='0'       PUT IN ZERO IN THE END OF CLT CODE          
         DSPEC 25,2,PRDNUM                                                      
         DSPEC 27,3,CLTCODE                                                     
         DSPEC 30,3,PRDCODE                                                     
         DSPEC 35,6,INVDATE,FORMAT=YYMMDD                                       
         DSPEC 41,6,DUEDATE,FORMAT=YYMMDD                                       
         DSPEC 53,11,GROSS,FORMAT=UNPACKED                                      
         DSPEC 64,11,ACTUAL,FORMAT=UNPACKED                                     
         DSPEC 75,11,NET,FORMAT=UNPACKED                                        
         DSPEC 86,4,MOS,FORMAT=YYMM                                             
         DSPEC 90,LITERAL='01'                             END OF MOS           
         DSPEC 92,11,GST,FORMAT=UNPACKED                                        
         DSPEC 103,11,HST_ALONE,FORMAT=UNPACKED                                 
         DSPEC 114,11,ZEROES,FORMAT=UNPACKED                                    
         DSPEC 125,11,PST_ALONE,FORMAT=UNPACKED                                 
         DC    X'0000'                                                          
         EJECT                                                                  
*        MCCANN                                                                 
         SPACE 2                                                                
MCSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='02'                                                   
         DSPEC 3,LITERAL='40'                                                   
         DSPEC 3,LITERAL='01',MEDIA_FILTER=N                                    
         DSPEC 5,1,ZEROES,FORMAT=EBCDIC                                         
         DSPEC 6,6,INVNUM6                                                      
         DSPEC 12,7,INVDATE,FORMAT=CYYMMDD                                      
         DSPEC 19,3,INVDATE,FORMAT=CYY                                          
         DSPEC 22,LITERAL='O'                                                   
         DSPEC 23,3,CLTCODE                                                     
         DSPEC 27,3,PRDCODE                                                     
         DSPEC 31,LITERAL='C'                                                   
         DSPEC 31,LITERAL='O',MEDIA_FILTER=N                                    
         DSPEC 32,1,MEDIA                                                       
         DSPEC 32,1,SUBMEDIA,MEDIA_FILTER=N                                     
         DSPEC 33,3,ESTCODE                                                     
         DSPEC 41,5,MOS,FORMAT=CYYMM                                            
         DSPEC 46,7,DUEDATE,FORMAT=CYYMMDD                                      
         DSPEC 53,7,RUNDATE,FORMAT=CYYMMDD                                      
         DSPEC 60,11,ACTUAL,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 71,11,NET,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 82,11,ACTUAL_COMMISSION,FORMAT=LEADING_MINUS_SIGN                
         DSPEC 93,11,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 104,11,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 115,9,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 124,LITERAL='P'                                                  
         DSPEC 125,11,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 136,3,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 139,11,GROSS,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 151,5,INVDATE,FORMAT=CYYMM                                       
         DSPEC 156,LITERAL='2'                                                  
         DSPEC 158,12,EUSER1                                                    
         DC    X'0000'                                                          
         EJECT                                                                  
*        MCCANN (ADWARE)                                                        
         SPACE 2                                                                
MCMSPECS DS    0X                                                               
         DSPEC 1,4,CLTNUM                                                       
         DSPEC 5,LITERAL='0'                                                    
         DSPEC 6,6,INVNUM6                                                      
         DSPEC 12,7,INVDATE,FORMAT=CYYMMDD                                      
         DSPEC 19,3,INVDATE,FORMAT=CYY                                          
         DSPEC 22,LITERAL='O'                                                   
         DSPEC 23,3,CLTCODE                                                     
         DSPEC 28,3,PRDCODE                                                     
         DSPEC 38,LITERAL='B'                 (OFFICE G GETS 'C')               
         DSPEC 38,LITERAL='K',MEDIA_FILTER=N  (OFFICES S,J GET 'O')             
         DSPEC 40,1,MEDIA                                                       
         DSPEC 40,1,SUBMEDIA,MEDIA_FILTER=N                                     
         DSPEC 41,3,ESTCODE                                                     
         DSPEC 53,20,ESTNAME                                                    
         DSPEC 83,5,MOS,FORMAT=CYYMM                                            
         DSPEC 88,7,DUEDATE,FORMAT=CYYMMDD                                      
         DSPEC 95,7,RUNDATE,FORMAT=CYYMMDD                                      
         DSPEC 102,12,ACTUAL,FORMAT=TRAILING_MINUS_SIGN                         
         DSPEC 114,12,NET,FORMAT=TRAILING_MINUS_SIGN                            
         DSPEC 126,12,ACTUAL_COMMISSION,FORMAT=TRAILING_MINUS_SIGN              
         DSPEC 138,12,GROSS,FORMAT=TRAILING_MINUS_SIGN                          
         DSPEC 150,12,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 162,9,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 172,12,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 187,12,GROSS,FORMAT=TRAILING_MINUS_SIGN                          
         DSPEC 200,5,INVDATE,FORMAT=CYYMM                                       
         DSPEC 205,LITERAL='2'                                                  
         DSPEC 206,32,PUSER1                                                    
         DSPEC 238,16,PUSER2                                                    
         DSPEC 254,32,EUSER1                                                    
         DSPEC 286,16,EUSER2                                                    
         DSPEC 341,LITERAL='0'                                                  
         DSPEC ROUTINE=MCMPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        MCCANN (CHRYLSER)   TYPE=C                                             
*        NOTE - $ FIELDS ARE SET TO PACKED HERE                                 
*        IN MCCOUT THEY WILL BE CONVERTED TO THEIR PROPER FORMAT                
*        PACKED FIELDS ARE NEEDED HERE FOR ACCUMULATING $                       
*        FOR AN INVOICE WITH MORE THAT ONE MOS                                  
         SPACE 2                                                                
MCCSPECS DS    0X                                                               
         DSPEC 1,4,CLTNUM                                                       
         DSPEC 5,LITERAL='0'                                                    
         DSPEC 6,6,INVNUM6                                                      
         DSPEC 12,7,INVDATE,FORMAT=CYYMMDD                                      
         DSPEC 19,3,INVDATE,FORMAT=CYY                                          
         DSPEC 22,LITERAL='O'                                                   
         DSPEC 23,3,CLTCODE                                                     
         DSPEC 28,3,PRDCODE                                                     
         DSPEC 38,LITERAL='B'                 (OFFICE G GETS 'C')               
         DSPEC 38,LITERAL='K',MEDIA_FILTER=N  (OFFICES S,J GET 'O')             
         DSPEC 40,1,MEDIA                                                       
         DSPEC 40,1,SUBMEDIA,MEDIA_FILTER=N                                     
         DSPEC 41,3,ESTCODE                                                     
         DSPEC 53,20,ESTNAME                                                    
         DSPEC 83,5,MOS,FORMAT=CYYMM                                            
         DSPEC 88,7,DUEDATE,FORMAT=CYYMMDD                                      
         DSPEC 95,7,RUNDATE,FORMAT=CYYMMDD                                      
****     DSPEC 102,12,ACTUAL,FORMAT=PACKED                                      
****     DSPEC 114,12,NET,FORMAT=PACKED                                         
****     DSPEC 126,12,ACTUAL_COMMISSION,FORMAT=PACKED                           
****     DSPEC 138,12,GROSS,FORMAT=PACKED                                       
         DSPEC 150,12,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 162,9,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 172,12,ZEROES,FORMAT=EBCDIC                                      
****     DSPEC 187,12,GROSS,FORMAT=PACKED                                       
         DSPEC 200,5,INVDATE,FORMAT=CYYMM                                       
         DSPEC 205,LITERAL='2'                                                  
         DSPEC 206,32,PUSER1                                                    
         DSPEC 238,16,PUSER2                                                    
         DSPEC 254,32,EUSER1                                                    
         DSPEC 286,16,EUSER2                                                    
         DSPEC 341,LITERAL='0'                                                  
         DSPEC ROUTINE=MCCPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        M2TO                                                                   
         SPACE 2                                                                
M2SPECS  DS    0X                                                               
         DSPEC 1,2,SPACES                                                       
         DSPEC 3,LITERAL='EA '                                                  
         DSPEC 3,LITERAL='DA ',MEDIA_FILTER=R                                   
         DSPEC 6,3,CLTCODE                                                      
         DSPEC 9,10,SPACES                                                      
         DSPEC 19,6,INVNUM6                                                     
*******  DSPEC 25,8,INVDATE,FORMAT=MMDDYYYY                                     
         DSPEC 34,6,ACTUAL,FORMAT=PACKED    $$ ARE 'FIXED' IN M2PROC            
         DSPEC 44,6,NET,FORMAT=PACKED                                           
         DSPEC 54,6,ACTUAL_COMMISSION,FORMAT=PACKED                             
         DSPEC 63,LITERAL='+       00'                                          
         DSPEC 73,12,SPACES                                                     
         DSPEC 122,LITERAL='01'                                                 
         DSPEC 126,3,PRDCODE                                                    
         DSPEC 129,LITERAL=' '                                                  
         DSPEC 130,12,RETAIL                                                    
         DSPEC ROUTINE=M2PROC                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
OOSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='00'                                                   
         DSPEC 3,2,SPACES                                                       
         DSPEC 5,6,INVNUM6                                                      
         DSPEC 15,2,SPACES                                                      
******   DSPEC 17,8,INVDATE,FORMAT=MMDDCCYY   DONE IN SPECIAL                   
         DSPEC 25,2,SPACES                                                      
******   DSPEC 27,8,DUEDATE,FORMAT=MMDDCCYY   DONE IN SPECIAL                   
         DSPEC 35,2,SPACES                                                      
         DSPEC 37,LITERAL='PROMS'                                               
         DSPEC 37,LITERAL='PROMN',MEDIA_FILTER=N     (NET)                      
*                                                                               
*        THE FIELD ABOVE WILL GET ALTERED IN OOPROC FOR OMDTO                   
*                                                                               
         DSPEC 47,16,SPACES                                                     
         DSPEC 63,15,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 78,15,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 93,15,ZEROES,FORMAT=EBCDIC                                       
         DSPEC 108,15,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 123,70,SPACES                                                    
         DSPEC 193,15,ZEROES,FORMAT=EBCDIC                                      
         DSPEC 208,100,SPACES                                                   
         DSPEC 308,41,SPACES                                                    
         DSPEC 350,LITERAL='0'                                                  
         DSPEC ROUTINE=OOPROC                                                   
         DC    X'0000'                                                          
         EJECT                                                                  
*        IPG                                                                    
         SPACE 2                                                                
IPGSPECS DS    0X                                                               
         DSPEC 1,LITERAL='S'                               SYSTEM               
         DSPEC 1,LITERAL='N',MEDIA_FILTER=N                SYSTEM-NET           
         DSPEC 2,2,AGENCY                                                       
         DSPEC 4,1,MEDIA                                                        
         DSPEC 5,LITERAL=' '                                                    
         DSPEC 5,1,SUBMEDIA,MEDIA_FILTER=N                                      
         DSPEC 6,3,CLTCODE                                                      
         DSPEC 9,20,CLTNAME                                                     
         DSPEC 30,1,OFFICE                                                      
         DSPEC 33,3,PRDCODE                                                     
         DSPEC 36,20,PRDNAME                                                    
         DSPEC 70,8,CLTNUM,FORMAT=EBCDIC                                        
         DSPEC 78,3,ESTCODE                                                     
         DSPEC 81,20,ESTNAME                                                    
         DSPEC 101,20,SPACES                                                    
         DSPEC 161,12,RETAIL                                                    
         DSPEC 271,1,COSTTYPE                                                   
         DSPEC 437,11,TAX,FORMAT=TRAILING_MINUS_SIGN                            
         DSPEC 448,11,GST,FORMAT=TRAILING_MINUS_SIGN                            
         DSPEC 459,11,PST_ALONE,FORMAT=TRAILING_MINUS_SIGN                      
         DSPEC 470,11,HST_ALONE,FORMAT=TRAILING_MINUS_SIGN                      
         DSPEC 494,5,MKTGROUP                                                   
         DSPEC 499,4,MARKET                                                     
         DSPEC 542,32,PUSER1                                                    
         DSPEC 574,16,PUSER2                                                    
         DSPEC 590,32,EUSER1                                                    
         DSPEC 622,16,EUSER2                                                    
         DSPEC ROUTINE=IPGPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        GM LMA                                                                 
         SPACE 2                                                                
GMSPECS  DS    0X                                                               
         DSPEC 1,12,EUSER1                                                      
         DSPEC 13,LITERAL='CRT2'                                                
         DSPEC 17,6,INVNUM6                                                     
         DSPEC 53,LITERAL='0'                                                   
         DSPEC 54,3,CLTCODE                                                     
         DSPEC 57,LITERAL='0'                                                   
         DSPEC 58,3,PRDCODE                                                     
         DSPEC 61,LITERAL='000'                                                 
         DSPEC 64,3,ESTCODE                                                     
         DSPEC 88,20,ESTNAME                                                    
         DSPEC 108,LITERAL='CRT'                                                
         DSPEC 111,LITERAL='00000009'                                           
         DSPEC ROUTINE=GMIPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
*        DIAGEO INTERFACE (CARAT- UB TYPE D)                                    
*                                                                               
*        NOTE - 2 RECORDS WILL BE GENERATED FOR EACH INVOICE                    
*               A HEADER AND A DETAIL                                           
*                                                                               
DGSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='0,'                                                   
         DSPEC 3,LITERAL='CARAT,'                                               
         DSPEC ROUTINE=DGIPROC                                                  
         DC    X'0000'                                                          
*                                                                               
SMSPECS  DS    0X                                                               
         DSPEC 201+(SMCNAME-SMUCKRSD),20,CLTNAME                                
         DSPEC 201+(SMEUDEF2-SMUCKRSD),16,EUSER2                                
         DSPEC 201+(SMEUDEF1-SMUCKRSD),32,EUSER1                                
         DSPEC ROUTINE=SMIPROC                                                  
         DC    X'0000'                                                          
         EJECT                                                                  
PHSPECS  DS    0X                                                               
         DSPEC 1,LITERAL='0,'                                                   
         DSPEC 3,LITERAL='HAVAS,'                                               
         DSPEC ROUTINE=PHIPROC                                                  
         DC    X'0000'                                                          
*                                                                               
CLSPECS  DS    0X                                                               
         DSPEC 150,LITERAL='1310276'                                            
         DSPEC 200,32,EUSER1                                                    
         DSPEC 240,16,EUSER2                                                    
         DSPEC ROUTINE=CLPROC                                                   
         DC    X'0000'                                                          
*                                                                               
AMSPECS  DS    0X                                                               
         DSPEC 150,LITERAL='1310276'    DIFFERENT FOR AMGEN?                    
         DSPEC 200,32,EUSER1                                                    
         DSPEC 240,16,EUSER2                                                    
         DSPEC ROUTINE=CLPROC       SAME AS COLGATE                             
         DC    X'0000'                                                          
*                                                                               
*        SJR - TEST                                                             
         SPACE 2                                                                
SJSPECS  DS    0X                                                               
         DSPEC 1,12,GROSS,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 13,1,SPACES                                                      
         DSPEC 14,12,NET,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 26,1,SPACES                                                      
         DSPEC 27,12,ACTUAL,FORMAT=LEADING_MINUS_SIGN                           
         DSPEC 39,1,SPACES                                                      
         DSPEC 40,12,GROSS_COMMISSION,FORMAT=LEADING_MINUS_SIGN                 
         DSPEC 52,1,SPACES                                                      
         DSPEC 53,12,ACTUAL_COMMISSION,FORMAT=LEADING_MINUS_SIGN                
         DSPEC 65,1,SPACES                                                      
         DSPEC 66,12,TAX,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 78,1,SPACES                                                      
         DSPEC 79,12,GROSS_MINUS_TAX,FORMAT=LEADING_MINUS_SIGN                  
         DSPEC 91,1,SPACES                                                      
         DSPEC 92,12,FEE,FORMAT=LEADING_MINUS_SIGN                              
         DSPEC 104,1,SPACES                                                     
         DSPEC 105,12,MEDIA_ACTUAL,FORMAT=LEADING_MINUS_SIGN                    
         DSPEC 117,1,SPACES                                                     
         DSPEC 118,12,GST,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 130,1,SPACES                                                     
         DSPEC 131,12,PST,FORMAT=LEADING_MINUS_SIGN                             
         DSPEC 143,1,SPACES                                                     
         DSPEC 144,12,PST_ALONE,FORMAT=LEADING_MINUS_SIGN                       
         DSPEC 156,1,SPACES                                                     
         DSPEC 157,12,HST_ALONE,FORMAT=LEADING_MINUS_SIGN                       
         DSPEC 169,1,SPACES                                                     
         DSPEC 170,12,ZEROES,FORMAT=EBCDIC                                      
         DC    X'0000'                                                          
         EJECT                                                                  
         PRINT OFF                                                              
         MACRO                                                                  
&NAME    AGSPC &AGENCY=,&MEDIA=ALL,&TAPE=,&BLKSIZE=,&LRECL=,           +        
               &SPECS=NONE,&ESTIMATE_AMOUNTS=NO,&RECFM=FB,&CLIENT=ALL           
         LCLC  &TC,&MED,&CTRL                                                   
         LCLA  &CTRL1,&CTRL2,&CTRL3,&CTRL4                                      
.*                                                                              
&CTRL1   SETA  0                                                                
&CTRL2   SETA  0                                                                
&CTRL3   SETA  0                                                                
&CTRL4   SETA  0                                                                
.*                                                                              
&TC      SETC  ' '                                                              
         AIF   (T'&TAPE EQ 'O').A10                                             
&TC      SETC  '&TAPE'                                                          
.*                                                                              
.A10     ANOP                                                                   
&MED     SETC  'Z'                                                              
         AIF   ('&MEDIA' EQ 'ALL').A15                                          
&MED     SETC  '&MEDIA'                                                         
.*                                                                              
.A15     ANOP                                                                   
&CLT     SETC  '   '                                                            
         AIF   ('&CLIENT' EQ 'ALL').A20                                         
&CLT     SETC  '&CLIENT'                                                        
.*                                                                              
.A20     AIF   ('&ESTIMATE_AMOUNTS' NE 'YES').A30                               
&CTRL1   SETA  &CTRL1+128                                                       
.*                                                                              
.A30     ANOP                                                                   
&RFM     SETC  'F'                                                              
         AIF   ('&RECFM' NE 'VB').A40                                           
&RFM     SETC  'V'                                                              
.*                                                                              
.A40     ANOP                                                                   
&NAME    DC    C'&AGENCY&MED&TC',AL2(&BLKSIZE,&LRECL)                           
         DC    C'&RFM'                                                          
         AIF   ('&SPECS' NE 'NONE').A50                                         
         DC    AL3(0)                                                           
         AGO   .A60                                                             
.A50     DC    AL3(&SPECS)                                                      
.A60     DC    AL1(&CTRL1,&CTRL2,&CTRL3,&CTRL4)                                 
         DC    CL3'&CLT'                                                        
         SPACE 1                                                                
         MEXIT                                                                  
         MEND                                                                   
         EJECT                                                                  
         PRINT ON                                                               
AGYTABD  DSECT                                                                  
AGYTAGY  DS    CL2                 AGENCY CODE                                  
AGYTMED  DS    C                   MEDIA (C'Z' = ALL)                           
AGYTAPE  DS    C                   TAPE CODE (C' ' = NONE)                      
AGYTBLKS DS    AL2                 BLKSIZE                                      
AGYTLRCL DS    AL2                 LRECL                                        
AGYRECFM DS    C                   'F' = FB, 'V' = VB                           
AGYTSPCS DS    AL3                 A(DATA SPECS)                                
AGYTCTRL DS    XL4                 SPECIAL CONTROLS                             
*                                    CNTRL1 X'80' = 'ESTIMATE AMOUNTS'          
*                                               (MCANN- ATT)                    
AGYTCLT  DS    CL3                 CLIENT (C'   ' = ALL)                        
AGYTABL  EQU   *-AGYTABD                                                        
*                                                                               
MQMSGD   DSECT                                                                  
MQHID    DS    CL6                 HUB RECORD ID                                
MQSYS    DS    CL3                 SYSTEM                                       
MQAGYID  DS    CL4                 AGENCY 1D 4-CHAR                             
MQQUAL   DS    CL16                QUALIFIER                                    
MQDATE   DS    CL6                 YYMMDD OF DSN                                
MQTIME   DS    CL6                 HHMMSS OF DSN                                
MQDATA1  DS    CL32                NOT USED                                     
MQDATA2  DS    CL32                NOT USED                                     
MQFILE   DS    CL64                DSN  (MINUS SFTPDISK.PROD.)                  
MQMSGLNQ EQU   *-MQMSGD                                                         
*                                                                               
*********************************************************************           
* DSECT TO COVER STARCOM CLIENT ACCOUNT CODES                                   
*********************************************************************           
                                                                                
H7ACCD   DSECT                                                                  
H7CLI    DS    CL3                 CLIENT                                       
H7FUND   DS    CL3                 FUND                                         
H7GLSR   DS    CL5                 G/L ACCOUNT RADIO                            
H7GLSX   DS    CL5                             NETWORK RADIO                    
H7GLST   DS    CL5                             TV                               
H7GLNN   DS    CL5                             NETWORK TV                       
H7GLPO   DS    CL5                             OUTDOOR                          
H7GLPM   DS    CL5                             MAGAZINE/NEWSPAPER               
H7GLCOM  DS    CL5                             COMMISSION                       
H7AGID   DS    CL5                 AGENCY ID                                    
H7AGNME  DS    AL3                 AGENCY NAME                                  
H7ACLNQ  EQU   *-H7ACCD                                                         
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER SMUCKERS CXML FOR DATA BUILT IN TPREC+200                      
*********************************************************************           
SMUCKRSD DSECT                     SMUCKERS DSECT                               
SMINVDT  DS    CL10                INVOICE DATE (YYYY-MM-DD)                    
SMINVMOS DS    CL10                INVOICE MOS (YYYY-MM-DD)                     
SMINVNUM DS    CL9                 INVOICE NUMBER (L-83-5149)                   
SMCNAME  DS    CL20                CLIENT NAME                                  
SMADD    DS    CL61                CLIENT ADDRESS (PADDR1/2)                    
SMCITY   DS    CL24                CLIENT CITY (PADDR3)                         
SMSTATE  DS    CL2                 CLIENT STATE (PADDR3)                        
SMZIP    DS    CL10                CLIENT ZIP CODE (PADDR3)                     
SMEUDEF2 DS    CL16                ESTIMATE UDEF 2                              
SMODATE  DS    CL10                ORDER DATE (YYYY-MM-DD)                      
SMEUDEF1 DS    CL32                ESTIMATE UDEF 1                              
SMBACT   DS    CL13                BILL ACTUAL                                  
SMEDESC  DS    CL20                EST DESCRIPTION (EDESC)                      
         EJECT                                                                  
SPBT02   CSECT                                                                  
AGYTAB   DS    0D                                                               
*                               BBDO-CHRYSLER                                   
         AGSPC AGENCY=BD,BLKSIZE=200,LRECL=200,SPECS=BDSPECSC,TAPE=C            
                                                                                
*                               PHDNY-STARBUCKS                                 
         AGSPC AGENCY=BN,BLKSIZE=130,LRECL=130,SPECS=SBSPECS,TAPE=B             
                                                                                
*                               BBDO-CHRYSLER                                   
         AGSPC AGENCY=BN,BLKSIZE=200,LRECL=200,SPECS=BDSPECSC,TAPE=C            
                                                                                
*                               BBDO                                            
         AGSPC AGENCY=BN,BLKSIZE=600,LRECL=150,SPECS=NHSPECS,TAPE=H             
                                                                                
*                               PHDDE                                           
         AGSPC AGENCY=BN,BLKSIZE=296,LRECL=296,SPECS=BNSPECSC,TAPE=P            
                                                                                
*                               PHDNY - SC JOHNSON                              
         AGSPC AGENCY=BN,BLKSIZE=160,LRECL=160,SPECS=SCSPECS,TAPE=S             
                                                                                
*                               B2MO SAME AS MCCANN (ADWARE)                    
         AGSPC AGENCY=B$,BLKSIZE=341,LRECL=341,SPECS=MCMSPECS                   
                                                                                
*                               MEDIA EDGE/Y&R SHARED FINANCIAL                 
         AGSPC AGENCY=CH,BLKSIZE=100,LRECL=100,SPECS=INSPECS                    
                                                                                
*                               DONER-SCHUR                                     
         AGSPC AGENCY=DA,BLKSIZE=135,LRECL=135,SPECS=DASPECS                    
                                                                                
*                               ZENITH TOYOTA - DF / SAATCHI                    
         AGSPC AGENCY=DF,BLKSIZE=150,LRECL=150,SPECS=THTSPECS,TAPE=T            
                                                                                
*                               INITIATIVE / NORTH AMERICA CANADA               
**INACT  AGSPC AGENCY=DH,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
**INACT  AGSPC AGENCY=DH,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               WPLA (PART OF WESTERN)                          
         AGSPC AGENCY=EB,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=EB,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               FORD MOTO / GROUPM                              
         AGSPC AGENCY=FR,BLKSIZE=146,LRECL=146,SPECS=M2GSPECS                   
                                                                                
*                               GSDM                                            
         AGSPC AGENCY=G7,BLKSIZE=200,LRECL=200,SPECS=G7SPECS                    
                                                                                
*                               GSCA                                            
**INACT  AGSPC AGENCY=GQ,BLKSIZE=200,LRECL=200,SPECS=MG7SPECS                   
                                                                                
*                               WINGATINO - SAME AS FOR M2 MEDIACOM             
         AGSPC AGENCY=G+,BLKSIZE=146,LRECL=146,SPECS=M2GSPECS                   
                                                                                
*                               HENDERSON                                       
         AGSPC AGENCY=HE,BLKSIZE=64,LRECL=64,SPECS=HESPECS                      
                                                                                
*                               MINDSHARE/GROUPM                                
         AGSPC AGENCY=H7,BLKSIZE=146,LRECL=146,SPECS=M2GSPECS                   
                                                                                
*                               MINDSHARE - BURGER KING                         
         AGSPC AGENCY=H7,BLKSIZE=132,LRECL=132,SPECS=H7SPECS,TAPE=B             
                                                                                
*                               MINDSHARE - SC JOHNSON                          
         AGSPC AGENCY=H7,BLKSIZE=160,LRECL=160,SPECS=SCSPECS,TAPE=S             
                                                                                
*                               MINDSHARE - MAZDA                               
         AGSPC AGENCY=H7,BLKSIZE=550,LRECL=550,SPECS=MZSPECS,TAPE=M             
                                                                                
*                               MINDSHARE - COLGATE                             
*ON HLD* AGSPC AGENCY=H7,BLKSIZE=100,LRECL=100,SPECS=CLSPECS,TAPE=C             
                                                                                
*                               MINDSHARE - AMGEN                               
*ON-HLD* AGSPC AGENCY=H7,BLKSIZE=100,LRECL=100,SPECS=AMSPECS,TAPE=A             
                                                                                
*                               SJR TEST  - SC JOHNSON                          
                                                                                
         AGSPC AGENCY=SJ,BLKSIZE=160,LRECL=160,SPECS=SCSPECS,TAPE=S             
                                                                                
*                               SJR TEST  - MAZDA                               
         AGSPC AGENCY=SJ,BLKSIZE=550,LRECL=550,SPECS=MZSPECS,TAPE=M             
                                                                                
*                               TCH1 TEST  - MEDIABRANDS TAX CODE               
         AGSPC AGENCY=T1,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                               STARCOM: PHILIP MORRIS                          
         AGSPC AGENCY=H9,BLKSIZE=4084,LRECL=408,RECFM=VB,TAPE=P                 
                                                                                
*                               JWT ACTION (JACTAK - JS)  MAZDA                 
         AGSPC AGENCY=JS,BLKSIZE=550,LRECL=550,SPECS=MZSPECS,TAPE=M             
                                                                                
*                               JWT                                             
         AGSPC AGENCY=JW,BLKSIZE=135,LRECL=135,SPECS=JWSPECS                    
                                                                                
*                               LORD GELLER                                     
**INACT  AGSPC AGENCY=LG,BLKSIZE=135,LRECL=135,SPECS=LGSPECS                    
                                                                                
*                               LINTAS & PARTNERS                               
         AGSPC AGENCY=LM,BLKSIZE=1000,LRECL=100,SPECS=STSPECS                   
                                                                                
*                               IPG - MCCANN                                    
         AGSPC AGENCY=MC,BLKSIZE=100,LRECL=100,SPECS=INSPECS                    
                                                                                
*                               MCANN (ATT)                                     
         AGSPC AGENCY=MC,BLKSIZE=1000,LRECL=100,SPECS=MCASPCS,TAPE=A,  +        
               ESTIMATE_AMOUNTS=YES                                             
                                                                                
*                               MCCANN (IPG - CHRYSLER)                         
         AGSPC AGENCY=MC,BLKSIZE=341,LRECL=341,SPECS=MCCSPECS,TAPE=C            
                                                                                
*                               MCCANN                                          
         AGSPC AGENCY=MC,BLKSIZE=169,LRECL=169,SPECS=MCSPECS,TAPE=B             
                                                                                
*                               MCCANN (ADWARE)                                 
*                               IPG                                             
         AGSPC AGENCY=MC,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                                                                               
         AGSPC AGENCY=MC,BLKSIZE=341,LRECL=341,SPECS=MCMSPECS,TAPE=M            
                                                                                
*                               MCCANN                                          
         AGSPC AGENCY=MC,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=S            
                                                                                
*                               MEDIA PARTNERSHIP (PART OF WESTERN)             
         AGSPC AGENCY=MD,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=MD,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               INITIATIVE / MEDIA FIRST                        
         AGSPC AGENCY=M1,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               MEDIACOM / GFMS                                 
         AGSPC AGENCY=M2,BLKSIZE=146,LRECL=146,SPECS=M2GSPECS                   
                                                                                
*                               MULLEN                                          
         AGSPC AGENCY=M$,BLKSIZE=200,LRECL=200,SPECS=G7SPECS                    
                                                                                
*                               DDB-NEEDHAM                                     
         AGSPC AGENCY=NE,BLKSIZE=930,LRECL=93,SPECS=DBSPECS                     
                                                                                
*                               NHS (NE)                                        
         AGSPC AGENCY=NE,BLKSIZE=600,LRECL=150,SPECS=NHSPECS,TAPE=H             
                                                                                
*                               NR=DDB-NEEDHAM                                  
         AGSPC AGENCY=NR,BLKSIZE=930,LRECL=93,SPECS=DBSPECS                     
                                                                                
*                               DWP/BATES TECHNOLOGY                            
         AGSPC AGENCY=OH,BLKSIZE=1000,LRECL=100,SPECS=STSPECS,TAPE=L            
                                                                                
*                               O&M                                             
         AGSPC AGENCY=OM,BLKSIZE=256,LRECL=256,SPECS=OMSPECS                    
                                                                                
*                               OMDUSEC- STARBUCKS                              
         AGSPC AGENCY=OO,BLKSIZE=130,LRECL=130,SPECS=SBSPECS,TAPE=B             
                                                                                
*                               OMDUSEC - FOR EDWARD JONES                      
         AGSPC AGENCY=OO,BLKSIZE=350,LRECL=350,SPECS=OOSPECS,TAPE=E             
                                                                                
*                               OPHDNA - MOVED FROM  PHDDE                      
         AGSPC AGENCY=OO,BLKSIZE=296,LRECL=296,SPECS=BNSPECSC,TAPE=P            
                                                                                
*                               OPHDNA- SC JOHNSON                              
         AGSPC AGENCY=OO,BLKSIZE=160,LRECL=160,SPECS=SCSPECS,TAPE=S             
                                                                                
*                               OMDTO - FOR EDWARD JONES  (CANADIAN)            
         AGSPC AGENCY=OU,BLKSIZE=350,LRECL=350,SPECS=OOSPECS,TAPE=E             
                                                                                
*                               A&P, LINTAS                                     
**INACT  AGSPC AGENCY=PU,BLKSIZE=200,LRECL=200,SPECS=PUSPECS                    
                                                                                
*                               WESTERN - ??                                    
APAGY    AGSPC AGENCY=QA,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
**INACT  AGSPC AGENCY=QA,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               MARTIN                                          
         AGSPC AGENCY=QM,BLKSIZE=341,LRECL=341,SPECS=MCMSPECS                   
                                                                                
*                               SAATCHI & SAATCHI                               
         AGSPC AGENCY=RH,BLKSIZE=200,LRECL=200,SPECS=COSPECS                    
                                                                                
*                               LOWE LINTAS & PARTNERS                          
         AGSPC AGENCY=SC,BLKSIZE=1000,LRECL=100,SPECS=STSPECS                   
                                                                                
*                               SJR - STANDARD                                  
         AGSPC AGENCY=SJ,BLKSIZE=93,LRECL=93,SPECS=YNSPECS                      
                                                                                
*                               SJR - TEST                                      
         AGSPC AGENCY=SJ,BLKSIZE=500,LRECL=500,SPECS=SJSPECS,TAPE=T             
                                                                                
*                               SAFFER GROUP                                    
         AGSPC AGENCY=ST,BLKSIZE=1000,LRECL=100,SPECS=STSPECS                   
                                                                                
*                               TED BATES (G.FOODS) BACKER SPIELVOGEL           
         AGSPC AGENCY=TB,BLKSIZE=600,LRECL=60,SPECS=TBSPECS,TAPE=G              
                                                                                
*                               ZENITH TOYOTA - TH                              
         AGSPC AGENCY=TH,BLKSIZE=150,LRECL=150,SPECS=THTSPECS,TAPE=T            
                                                                                
*                               SPOTPLUS                                        
         AGSPC AGENCY=TR,BLKSIZE=59,LRECL=59,SPECS=TLSPECS                      
                                                                                
*                               SPOTPLUS                                        
         AGSPC AGENCY=TR,BLKSIZE=2000,LRECL=200,SPECS=TRSPECS,TAPE=O            
                                                                                
*                               WESTERN                                         
WIAPAGY  AGSPC AGENCY=WI,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=WI,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               WESTERN - TEST                                  
         AGSPC AGENCY=WJ,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=WJ,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               WESTERN - WR                                    
         AGSPC AGENCY=WR,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
                                                                                
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=WR,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
**OLD                           WESTERN - CANADA                                
**OLD    AGSPC AGENCY=WT,BLKSIZE=250,LRECL=250,SPECS=WTSPECS                    
**OLD                                                                           
*                               INITIATIVE / TRUE NORTH                         
         AGSPC AGENCY=WT,BLKSIZE=500,LRECL=500,SPECS=IMTNSPCS,TAPE=A            
                                                                                
*                               WUNDERMAN                                       
         AGSPC AGENCY=WW,BLKSIZE=93,LRECL=93,SPECS=WWSPECS                      
                                                                                
*                               Y&R                                             
         AGSPC AGENCY=YN,BLKSIZE=93,LRECL=93,SPECS=YNSPECS                      
                                                                                
*                               CARAT - DIAGEO FILE                             
         AGSPC AGENCY=UB,BLKSIZE=050,LRECL=050,SPECS=DGSPECS,TAPE=D             
                                                                                
*                               CARAT - GM LMA                                  
         AGSPC AGENCY=UB,BLKSIZE=159,LRECL=159,SPECS=GMSPECS,TAPE=G             
                                                                                
*                               CARAT - SMUCKERS                                
         AGSPC AGENCY=UB,BLKSIZE=150,LRECL=150,SPECS=SMSPECS,TAPE=S             
                                                                                
*                               HAVAS - PHILIPS                                 
         AGSPC AGENCY=FM,BLKSIZE=PHTLEN,LRECL=PHTLEN,TAPE=P                     
                                                                                
*                               M2TO                                            
         AGSPC AGENCY=U#,BLKSIZE=141,LRECL=141,SPECS=M2SPECS                    
                                                                                
*                               IPG                                             
         AGSPC AGENCY=U#,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                               WITO                                            
         AGSPC AGENCY=WT,BLKSIZE=141,LRECL=141,SPECS=M2SPECS                    
                                                                                
*                               IPG                                             
         AGSPC AGENCY=WI,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                               IPG                                             
         AGSPC AGENCY=M1,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                               IPG                                             
         AGSPC AGENCY=D9,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                               IPG                                             
         AGSPC AGENCY=LM,BLKSIZE=1150,LRECL=1150,SPECS=IPGSPECS,TAPE=I          
                                                                                
*                                                                               
         DC    X'FFFF'             END-OF-TABLE                                 
         EJECT                                                                  
         GETEL R7,24,ELCODE                                                     
         SPACE 3                                                                
INVTAB   DS    0D                                                               
         ORG   *+(INVMAX*3)                                                     
         DC    X'00'                                                            
         EJECT                                                                  
BLINED   DSECT                                                                  
BLINE    DS    0CL132                                                           
         DS    CL3                                                              
BLPRD    DS    CL3                                                              
         DS    CL1                                                              
BLPNUM   DS    CL5                                                              
         DS    CL1                                                              
BLEST    DS    CL3                                                              
         DS    CL1                                                              
BLPER    DS    CL6                                                              
         DS    CL1                                                              
BLINVNO  DS    CL7                                                              
         DS    CL1                                                              
BLRUND   DS    CL8                                                              
         DS    CL1                                                              
BLINVD   DS    CL8                                                              
         DS    CL1                                                              
BLDUED   DS    CL8                                                              
         DS    CL1                                                              
BLTYPE   DS    CL3                                                              
BLGROSS  DS    CL14                                                             
BLACTUAL DS    CL14                                                             
BLNET    DS    CL14                                                             
BLAC     DS    CL14                                                             
BLGST    DS    CL14                                                             
         ORG   BLGST+132           PST ON 2ND LINE                              
BLPST    DS    CL14                                                             
*                                  FOR NON-CANADIAN AGENCIES, GST/PST           
*                                  ISN'T NEEDED, SO WIDEN $ COLUMNS             
         ORG   BLGROSS                                                          
         DS    C                                                                
BLGRSWID DS    CL15                GROSS                                        
         DS    C                                                                
BLACTWID DS    CL15                ACTUAL                                       
         DS    C                                                                
BLNETWID DS    CL15                NET                                          
         DS    C                                                                
BLACWIDE DS    CL15                AGENCY COMMISSION                            
         DS    CL6                 SPARE                                        
         ORG                                                                    
*                                                                               
DFTLIND  DSECT                     DSECT FOR DANCER - TOYOTA                    
DFTFDEL  EQU   C','                FIELD DELIMITER                              
*                                                                               
DFTVENN  DS    CL8                 VENDOR NUMBER                                
         DS    CL1                                                              
DFTREFN  DS    CL8                 REFERENCE (INVOICE) NUM                      
         DS    CL1                                                              
DFTADDR  DS    CL3                 ADDRESS                                      
         DS    CL1                                                              
DFTSAC   DS    CL6                 SAC                                          
         DS    CL1                                                              
DFTREFT  DS    CL2                 REFERENCE TYPE                               
         DS    CL1                                                              
DFTREFD  DS    CL8                 REFERENCE (INVOICE) DATE                     
         DS    CL1                                                              
DFTPAYD  DS    CL8                 PAY (DUE) DATE                               
         DS    CL1                                                              
DFTGLACC DS    CL7                 GL ACCOUNT                                   
         DS    CL1                                                              
DFTCOSTC DS    CL4                 COST CENTER                                  
         DS    CL1                                                              
DFTESTN  DS    CL12                ESTIMATE 'NAME'                              
         DS    CL1                                                              
DFTCLYR  DS    CL4                 CALENDAR YEAR                                
         DS    CL1                                                              
DFTPROJN DS    CL7                 PROJECT NUMBER (NOT USED)                    
         DS    CL1                                                              
DFTREM   DS    CL40                REMARKS                                      
         DS    CL1                                                              
DFTAMT   DS    CL12                $AMOUNT                                      
         DS    CL1                                                              
DFTSUBC  DS    CL2                 SUBSIDIARY CODE                              
         DS    CL1                                                              
DFTFSYR  DS    CL4                 FISCAL YEAR                                  
         DS    CL1                                                              
DFTRECX  EQU   *                   EOR                                          
         EJECT                                                                  
SPBTWRKD DSECT                                                                  
CARET    EQU   X'B0'                                                            
*                                                                               
VSPFMTIN DS    V                   V(SPFMTINO)                                  
MVOFFICE DS    V                   V(OFFICER)                                   
ASPECS   DS    A                                                                
ATOTS    DS    A                                                                
AG7MED   DS    A                                                                
ASCMED   DS    A                                                                
AMZKEY   DS    A                                                                
AMCMED   DS    A                                                                
RELO     DS    A                                                                
AMQRPT   DS    A                                                                
ATPFMT   DS    A                                                                
ARUNL    DS    A                                                                
ARUNF    DS    A                                                                
*                                                                               
*                                                                               
DSNLENQ  EQU   20                                                               
*                                                                               
EATOTGRS DS    PL6                                                              
EATOTAC  DS    PL6                                                              
EATOTACT DS    PL6                                                              
EATOTNET DS    PL6                                                              
*                                                                               
RECLEN   DS    H                                                                
ELCODE   DS    X                                                                
MYDUB    DS    PL8                                                              
BIGDUB   DS    PL12                                                             
PL16     DS    PL16                                                             
OPENSW   DS    C                                                                
RETAIL   DS    C                                                                
REVSW    DS    C                   REVISION SWITCH                              
BYTE2    DS    X                                                                
FISCAL   DS    CL4                                                              
LSTBLKY  DS    XL13                LAST BILL KEY                                
B1XPROF  DS    CL16                                                             
B1PROF   DS    CL16                                                             
SV00APRF DS    CL16                                                             
DINVMED  DS    CL2                 INVOICE MEDIA PART                           
DINVNO   DS    CL6                                                              
DINVFULL DS    CL10                FULL FORMAT INVOICE NUMBER                   
CONTROLS DS    0XL4                                                             
CONTROL1 DS    X                                                                
CONTROL2 DS    X                                                                
CONTROL3 DS    X                                                                
CONTROL4 DS    X                                                                
OFFICE   DS    CL1                                                              
SAVCOFF  DS    CL2                 OFFICER OUTPUT                               
SVQST    DS    CL6                                                              
SVQEND   DS    CL6                                                              
STARTMOS DS    XL2                 START MOS FILTER (BINARY YM)                 
ENDMOS   DS    XL2                 END MOS FILTER (BINARY YM)                   
CNTRY    DS    C                                                                
TESTMQ   DS    C                                                                
SCSFTP   DS    C                                                                
MZSFTP   DS    C                                                                
BKSFTP   DS    C                   Y=DOING BURGER KING SFTP                     
*                                                                               
BKFISCAL DS    CL4                 FISCAL YEAR                                  
BKBYR    DS    CL4                 BILLING MOS - YEAR                           
BKBMTH   DS    CL2                 BILLING MOS - MONTH                          
*                                  FOR MINDSHARE BURGER KING MQ MSG             
CTODAY   DS    CL8                 YYYYMMDD                                     
TIMEOFD  DS    CL8                 HH.MM.SS                                     
TODAYC   DS    XL2                 TODAY COMPRESSED                             
SVMID    DS    CL132                                                            
ELEM     DS    CL200                                                            
*                                                                               
MQMAPNM  DS    CL14                SFTPDISK.PROD.                               
*                                                                               
DSNAME   DS    CL35  DSN -  BIL.SYS.AGID.DYYYMMDD.THHMMSS                       
*                    FOR MINDSHARE BURGER KING AGID = H7BK                      
*                                                                               
NEWCLT   DS    C                                                                
NETPAKSW DS    C                                                                
SKIPBILL DS    X                                                                
JWOASW   DS    C                                                                
YEARDIG  DS    XL1                 YEAR DIGIT                                   
DECADE   DS    XL1                                                              
WIAPLSW  DS    C                   WESTERN/APL SWITCH                           
WIQASW   DS    C                   WESTERN/APL QA SWITCH                        
WAPLBTSW DS    C                   YET ANOTHER MODE                             
*                                                                               
BKCOM    DS    PL6                 COMMISSION AMOUNT                            
BKACCT   DS    CL10                BURGER KING ACCOUNT                          
BKCOMACC DS    CL10                BURGER KING COMMISSION ACCOUNT               
BKCOSTC  DS    CL10                BURGER KING COST CENTER                      
BKMAR#   DS    CL23                BURGER KING MAR NUMBER                       
*                                                                               
G7SW     DS    CL1                 SET TO 1 FOR GSTX REQS                       
MCCSW    DS    CL1                 SET TO 1 FOR MCCANN + TYPE C                 
SCSW     DS    CL1                 SET TO 1 FOR MSNY + TYPE S                   
DGSW     DS    CL1                 SET TO 1 FOR CARAT+ TYPE D                   
SMSW     DS    CL1                 SET TO 1 FOR CARAT+ TYPE S                   
SMHDSW   DS    CL1                 HEADER CREATED FOR SMUCKERS                  
SCLMOS   DS    XL2       YM        LOWEST MOS ON THE FILE                       
SCHMOS   DS    XL2       YM        HIGHEST MOS ON THE FILE                      
*                                                                               
MZSW     DS    CL1                 SET TO 1 FOR MSNY + TYPE S                   
SBSW     DS    CL1                 SET TO 1 FOR PHDNY+ TYPE S                   
SBCOUNT  DS    PL6                 STARBUCKS INVOICE COUNT                      
SBTOTAL  DS    PL6                 STARBUCKS $ COUNT                            
*                                                                               
OBCOUNT  DS    PL6                                                              
OBTOTAL  DS    PL6                                                              
OBISKIP  DS    CL1                                                              
*                                                                               
DGTYPE   DS    CL1                                                              
DGHDSW   DS    CL1                 Y= HEADER CREATED                            
DGERRSW  DS    CL1                                                              
DGISKIP  DS    CL1                                                              
DGCOUNT  DS    PL6                 DIAGEO RECORD COUNT                          
DGTOTAL  DS    PL6                 DIAGEO $ COUNT                               
*                                                                               
PHHDSW   DS    CL1                 Y= HEADER CREATED                            
PHTLEN   EQU   400                 PH TAPE LENGTH                               
*                                                                               
SVQOPT3  DS    CL1                 SAVED QOPT3 FOR GSTX                         
*                                                                               
CLFILT   DS    CL3                                                              
MEFILT   DS    CL1                                                              
*                                                                               
SAVER1   DS    F                                                                
SAVER4   DS    F                                                                
MYFULL   DS    F                                                                
*                                                                               
LOWINV   DS    H            LOW INVOICE NUMBER FILTER                           
HIINV    DS    H            HI INVOICE NUMBER FILTER                            
*                                                                               
INVPARS  DS    6F                  INVOICE BINSRCH PARS                         
*                                                                               
INVMAX   EQU   100000              MAX INVOICES PER CLT (WAS 50000)             
*                                                                               
EOT      EQU   X'FF'                                                            
*                                                                               
GBINVALS DS    0F                                                               
GBPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
GBPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
G7RECNT  DS    F                  NUMBER OF RECORDS ADDED                       
GBPAR4   DS    F                  LEN OF RECORD                                 
GBPAR5   DS    F                  KEY SIZE                                      
GBPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFG7T   DS    A                  ADDRESS OF G7TABLE                            
*                                                                               
G7KEY    DS    0XL12                                                            
G7MED    DS    CL1                                                              
G7CLI    DS    CL3                                                              
G7INUMB  DS    0XL3                                                             
G7INVMO  DS    XL1           INVOICE MONTH                                      
G7INVN   DS    XL2           INVOICE NUMBER                                     
G7PRO    DS    CL3                                                              
G7EST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
G7REC    DS    CL200                                                            
ENDG7R   DS    0C                                                               
*                                                                               
G7COUNT  DS    PL3                                                              
*                                                                               
*        SC JOHNSON INTERFACE (H7)                                              
*                                                                               
SCINVALS DS    0F                                                               
SCPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
SCPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
SCRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
SCPAR4   DS    F                  LEN OF RECORD                                 
SCPAR5   DS    F                  KEY SIZE                                      
SCPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFSCT   DS    A                  ADDRESS OF SCTABLE                            
*                                                                               
SCKEY    DS    0XL12                                                            
SCMED    DS    CL1                                                              
SCCLI    DS    CL3                                                              
SCINUMB  DS    0XL3                                                             
SCINVMO  DS    XL1           INVOICE MONTH                                      
SCINVN   DS    XL2           INVOICE NUMBER                                     
SCPRO    DS    CL3                                                              
SCEST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
SCREC    DS    CL200                                                            
ENDSCR   DS    0C                                                               
*                                                                               
SCCOUNT  DS    PL3                                                              
*                                                                               
MZINVALS DS    0F                                                               
MZPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
MZPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
MZRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
MZPAR4   DS    F                  LEN OF RECORD                                 
MZPAR5   DS    F                  KEY SIZE                                      
MZPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFMZT   DS    A                  ADDRESS OF MZTABLE                            
*                                                                               
MZKEY    DS    0XL15                                                            
MZMED    DS    CL1                                                              
MZCLI    DS    CL3                                                              
MZPRO    DS    CL3                                                              
MZEST    DS    XL2                                                              
MZINUMB  DS    0XL3                                                             
MZINVMO  DS    XL1           INVOICE MONTH                                      
MZINVN   DS    XL2           INVOICE NUMBER                                     
MZTYPE   DS    XL1           1=HEADER,2=DETAIL                                  
MZMOS    DS    XL2           ONLY PRESENT FOR TYPE 2                            
*            ______                                                             
*              15                                                               
MZREC    DS    CL375                                                            
ENDMZR   DS    0C                                                               
*                                                                               
*                                                                               
MBINVALS DS    0F                                                               
MCPAR1   DS    XL4                ADDRESS OF RECORD                             
*                                 HIGH ORDER BYTE IS ACTION X'01'=ADD           
MCPAR2   DS    A                  ADDRESS OF TABLE WHERE REC IS TO BE           
MCRECNT  DS    F                  NUMBER OF RECORDS ADDED                       
MCPAR4   DS    F                  LEN OF RECORD                                 
MCPAR5   DS    F                  KEY SIZE                                      
MCPAR6   DS    F                  MAX NUMBER OF RECORDS                         
*                                                                               
AOFMCT   DS    A                  ADDRESS OF MCTABLE                            
*                                                                               
MCKEY    DS    0XL12                                                            
MCMED    DS    CL1                                                              
MCCLI    DS    CL3                                                              
MCINUMB  DS    0XL3                                                             
MCINVMO  DS    XL1           INVOICE MONTH                                      
MCINVN   DS    XL2           INVOICE NUMBER                                     
MCPRO    DS    CL3                                                              
MCEST    DS    XL2                                                              
*            ______                                                             
*              12                                                               
MCREC    DS    CL341                                                            
ENDMCR   DS    0C                                                               
*                                                                               
MCCOUNT  DS    PL3                                                              
*                                                                               
CLTINVS  DS    F                   CLIENT INVOICES                              
OFFINVS  DS    F                   OFFICE INVOICES                              
REQINVS  DS    F                   REQUEST INVOICES                             
RUNINVS  DS    F                   RUN INVOICES                                 
*                                                                               
BAMTS    DS    (NAMTS)PL6          BILL TOTALS                                  
SVBAMTS  DS    (NAMTS)PL6          SAVED TOTALS                                 
PAMTS    DS    (NAMTS)PL6          PRODUCT TOTS                                 
CAMTS    DS    (NAMTS)PL6          CLIENT                                       
OAMTS    DS    (NAMTS)PL6          OFFICE                                       
RQAMTS   DS    (NAMTS)PL6          REQUEST                                      
RAMTS    DS    (NAMTS)PL6          RUN                                          
MYGST    DS    PL6                 SAVE CANADIAN TAXES                          
MYHST    DS    PL6                                                              
MYPST    DS    PL6                                                              
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
         EJECT                                                                  
       ++INCLUDE SPBVALD                                                        
         EJECT                                                                  
SVBILL   DS    XL256                                                            
*                                                                               
TPRECRDW DS    F                   RDW (FOR VARIABLE LENGTH RECORDS)            
TPREC    DS    XL1200                                                           
         ORG   TPREC+600                                                        
SVTPREC  DS    XL600                                                            
*                                                                               
******   VERY IMPORTANT NOTE  ******                                            
*        DO NOT USE SVTPREC WHEN YOUR LRECL IS OVER 600                         
*        DO NOT TRY TO EXPAND LENGTH OF SVTPREC                                 
*                                                                               
*        CURRENTLY ONLY IPGSPECS FOR SEVERAL AGENCIES                           
*        HAS A LRECL OVER 600 - 1150                                            
*        IT DOES NOT USE SVTPREC                                                
*                                                                               
         SPACE 3                                                                
AMOUNTSD DSECT                                                                  
AMTGRS   DS    PL6                 GROSS                                        
AMTAC    DS    PL6                 ACTUAL COMMISSION                            
AMTACT   DS    PL6                 ACTUAL                                       
AMTNET   DS    PL6                 NET                                          
AMTCNT   DS    PL6                 COUNT                                        
AMTGST   DS    PL6                 GST                                          
AMTPST   DS    PL6                 PST                                          
AMTHST   DS    PL6                 HST                                          
NAMTS    EQU   (*-AMOUNTSD)/6                                                   
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   QAREA+49                                                         
QMOS     DS    0CL8                REQUESTED MONTH-OF-SERVICE RANGE             
QMOSSTRT DS    CL4                 START MOS (YYMM)                             
QMOSEND  DS    CL4                 END MOS (YYMM)                               
         ORG   Q2USER                                                           
*                                  QAREA2 COL 21-COL 28                         
QINVNO1  DS    CL4                 START INVOICE #                              
QINVNO2  DS    CL4                 END INVOICE #                                
         ORG                                                                    
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAOR                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DDUCOMD                                                        
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDGETPROFD                                                     
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
NETBLKD  DSECT                                                                  
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NECOMBLOK                                                      
         PRINT ON                                                               
*                                                                               
G7TABLE  CSECT                                                                  
         DS    3000CL212                                                        
         SPACE 3                                                                
SCTABLE  CSECT                                                                  
         DS    2000CL212                                                        
         SPACE 3                                                                
MZTABLE  CSECT                                                                  
         DS    1000CL390                                                        
         SPACE 3                                                                
MCTABLE  CSECT                                                                  
         DS    2000CL353                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115SPREPBT02 09/23/19'                                      
         END                                                                    
