*          DATA SET TALNK10    AT LEVEL 004 AS OF 06/17/15                      
*PHASE T70410E                                                                  
TALNK10  TITLE '- TALENT - INITIAL AND COMMERCIAL DOWNLOADS'                    
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(CODE-SVRDEF)    SERVER ENTRY POINT                           
         DC    AL2(0)              NO FILE LIST                                 
         DC    AL2(0)              NO FACILITIES LIST                           
         DC    AL2(REQ-SVRDEF) REQUEST MAP                                      
         ORG   SVRDEF+(RSVRIND1-RSVRDEFD)                                       
         DC    AL1(RSVRILNK)                                                    
         ORG   SVRDEF+(RSVRSYS1-RSVRDEFD)                                       
         DC    AL1(TALSYSQ)                                                     
         ORG                                                                    
         EJECT                                                                  
CODE     DS    0D                                                               
******   PRINT NOGEN                                                            
         NMOD1 0,**TL10**,RR=RE                                                 
         USING LP_D,R1                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         BASR  R5,0                                                             
         AHI   R5,LITERALS-*                                                    
         USING LITERALS,R5         R5=A(LITERAL POOL)                           
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST 'FIRST FOR RUN' MODE                    
         BNE   PRCWRK                                                           
RUNSTR02 MVC   WVALUES(WVALUESL),LVALUES                                        
         LA    R1,OADCONS          RELOCATE ADCONS                              
         LA    R0,OADCONSN                                                      
         BASR  RE,0                                                             
         L     RF,0(R1)                                                         
         A     RF,SRVRRELO                                                      
         ST    RF,0(R1)                                                         
         AHI   R1,L'OADCONS                                                     
         BCTR  R0,RE                                                            
                                                                                
         L     R1,ALP                                                           
         LA    R0,SAVED                                                         
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
B#COM    EQU   3                                                                
         MVC   LP_BLKS+((B#COM-1)*L'LP_BLKS),AIO2                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALSL),REQVALS                                        
         XC    MYIOKEYS,MYIOKEYS                                                
         MVC   END,FFFS     SET TO FF'S                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
*                                                                               
         MVC   MAP#,LP_QMAPN        SET PROCESSING MAP NUMBER                   
                                                                                
         CLC   MAP#,=AL2(I#COMDLD)   COMMERCIAL DOWNLOAD?                       
         BE    RUNCD                                                            
*                                                                               
         CLC   MAP#,=AL2(I#INIDLD)   INITIAL DOWNLOAD?                          
         BE    *+6                                                              
         DC    H'0'              OTHERS NOT DEFINED                             
                                                                                
         LA    R2,COMTTABN                                                      
         STH   R2,COMTNUM        SAVE NUMBER OF COMMERCIAL TYPES                
         L     R2,AIO6       MOVE TABLE TO AIO6                                 
         MVC   0(CMTABL,R2),COMTTAB                                             
*                                                                               
*        USER CODE BYPASSED                                                     
*                                                                               
         B     RUNREQ04                                                         
*                                                                               
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         L     RF,ATWA                                                          
         MVC   CTIKNUM,TWAUSRID-TWAD(RF)                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO2'                            
         BNE   RUNREQ04                                                         
         L     R2,IOADDR                                                        
         LA    R2,CTIDATA                                                       
         USING CTORGD,R2                                                        
         SR    R0,R0                                                            
         XC    USIDNAME,USIDNAME                                                
RUNREQ02 CLI   CTORGEL,0                                                        
         BE    RUNREQ04                                                         
         CLI   CTORGEL,CTORGELQ                                                 
         BE    *+14                                                             
         IC    R0,CTORGLEN                                                      
         AR    R2,R0                                                            
         B     RUNREQ02                                                         
         MVC   USIDNAME,CTORGNAM   SET USER-ID NAME                             
         DROP  R2                                                               
                                                                                
RUNREQ04 DS    0H                  Validate Staff code                          
*                             SAVE ACCESS VALUES IN TWA                         
*                                                                               
         MVI   STAFFAND,0                                                       
         XC    STAFFAAD,STAFFAAD  CLEAR LIMIT ACCESS AGENCIES                   
         MVI   STAFFCND,0                                                       
         XC    STAFFCAD,STAFFCAD  CLEAR LIMIT ACCESS CLIENTS                    
*                                                                               
         OC    STAFFC,STAFFC      STAFF CODE                                    
         BZ    STAFFINV                                                         
         OC    STAFFPW,STAFFPW    PASSWORD                                      
         BZ    STFPWINV                                                         
         LA    R2,IOKEY                                                         
         USING TLSTD,R2                                                         
         XC    TLSTKEY,TLSTKEY                                                  
         MVI   TLSTCD,TLSTCDQ                                                   
         L     RF,ATWA                                                          
         MVC   TLSTUSER,TWAUSRID-TWAD(RF)   USER ID                             
         MVC   TLSTSTAF,STAFFC                                                  
         OC    TLSTSTAF,SPACES                                                  
*                                                                               
         USING TWAD,RF                                                          
         L     RF,ATWA                                                          
         XC    STAFFKEY,STAFFKEY                                                
         MVC   STAFFKEY(L'TLSTKEY),IOKEY                                        
         DROP  RF                                                               
*                                                                               
         L     R1,=A(IORD+IOTALDIR+IO8)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    RUNREQ06                                                         
         B     STAFFINV                                                         
                                                                                
RUNREQ06 DS    0H                                                               
         L     R1,=A(IOGET+IOTALFIL+IO8)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
                                                                                
         L     R2,IOADDR                                                        
         LA    R3,TLSTELEM         POINT TO FIRST ELEMENT                       
         USING TASTD,R3                                                         
RUNREQ08 DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BE    STFPWINV            INVALID/MISSING PASSWORD                     
         CLI   0(R3),TASTELQ                                                    
         BE    RUNREQ10                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNREQ08                                                         
*                                                                               
RUNREQ10 OC    STAFFPW,SPACES                                                   
         OC    TASTPWD,SPACES                                                   
         CLC   STAFFPW,TASTPWD                                                  
         BNE   STFPWINV                                                         
                                                                                
         L     RF,ATWA                                                          
         USING TWAD,RF                                                          
                                                                                
         MVC   STAFFN(L'TASTFST),TASTFST     FIRST NAME                         
         LA    R4,STAFFN+L'TASTFST                                              
RUNREQ12 CLI   0(R4),C' '                                                       
         BH    RUNREQ14                                                         
         SH    R4,=H'1'                                                         
         B     RUNREQ12                                                         
*                                                                               
RUNREQ14 LA    R4,2(R4)                                                         
         MVC   0(L'TASTLST,R4),TASTLST   FLOAT LAST NAME AFTER FIRST            
         DROP  RF                                                               
                                                                                
         XR    RE,RE                                                            
         LA    RF,STAFFAAD                                                      
*                                                                               
         XC    LASTAGY,LASTAGY                                                  
*                                                                               
         USING TAVAD,R3                                                         
         L     R2,IOADDR                                                        
         LA    R3,TLSTELEM                                                      
RUNREQ16 CLI   0(R3),0                                                          
         BE    RUNREQ20                                                         
         CLI   0(R3),TAVAELQ                                                    
         BNE   RUNREQ18                                                         
         OC    TAVAAGY,TAVAAGY                                                  
         BZ    RUNREQ20                                                         
         CLC   LASTAGY,TAVAAGY                                                  
         BE    RUNREQ18                                                         
         AHI   RE,1                                                             
         MVC   0(L'TAVAAGY,RF),TAVAAGY                                          
         MVC   LASTAGY,TAVAAGY                                                  
         CHI   RE,16                                                            
         BE    RUNREQ20                                                         
         LA    RF,L'TAVAAGY(RF)                                                 
RUNREQ18 ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNREQ16                                                         
         DROP  R3                                                               
                                                                                
RUNREQ20 STC   RE,STAFFAND                                                      
*                                                                               
         XR    RE,RE                                                            
*                                                                               
         USING TAVAD,R3                                                         
         L     R2,IOADDR                                                        
         LA    R3,TLSTELEM                                                      
RUNREQ21 CLI   0(R3),0                                                          
         BE    RUNREQ27                                                         
         CLI   0(R3),TAVAELQ                                                    
         BNE   RUNREQ26                                                         
         CLI   TAVALEN,TAVALNQ                                                  
         BNH   RUNREQ26                                                         
*                                                                               
         ZIC   R0,TAVALEN                                                       
         SHI   R0,TAVALNQ                                                       
         LA    R1,TAVACLI                                                       
*                                                                               
RUNREQ22 LA    RF,STAFFCAD                                                      
RUNREQ23 CLI   0(RF),0                                                          
         BE    RUNREQ24                                                         
         CLC   0(L'TAVACLI,R1),0(RF)                                            
         BE    RUNREQ25                                                         
         LA    RF,L'TAVACLI(RF)                                                 
         B     RUNREQ23                                                         
*                                                                               
RUNREQ24 AHI   RE,1                                                             
         MVC   0(L'TAVAAGY,RF),0(R1)                                            
         CHI   RE,16                                                            
         BE    RUNREQ27                                                         
*                                                                               
RUNREQ25 SHI   R0,L'TAVACLI                                                     
         LTR   R0,R0                                                            
         BZ    RUNREQ26                                                         
         LA    R1,L'TAVACLI(R1)                                                 
         B     RUNREQ22                                                         
*                                                                               
RUNREQ26 ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNREQ21                                                         
         DROP  R3                                                               
                                                                                
RUNREQ27 STC   RE,STAFFCND                                                      
         B     RUNREQX                                                          
         EJECT                                                                  
********************************************************************            
* RUN COMMERCIAL DOWNLOAD                                                       
********************************************************************            
RUNCD    EQU   *                                                                
*        SET USE VERSION FLAG                                                   
*                                                                               
         MVI   USEVFLAG,C'N' DON'T RETURN USES NOR CODES FOR HLD FEES           
         L     R1,ALP      just in case                                         
         CLC   LP_VRSN1,=AL1(1,1,0,4)   SEE IF CPC VERSION                      
         BL    *+8                      1.1.4 OR HIGHER                         
         MVI   USEVFLAG,C'Y'                                                    
*                                                                               
         MVC   AGYN,SPACES       CLEAR NAMES                                    
         MVC   CLIN,SPACES                                                      
         MVC   PRDN,SPACES                                                      
         MVC   CGPN,SPACES                                                      
                                                                                
         OC    AGYCD,AGYCD        MUST HAVE AN AGENCY                           
         BZ    MISSING                                                          
         OC    CLICD,CLICD        MUST HAVE CLIENT                              
         BZ    MISSING                                                          
                                                                                
*        CHECK STAFF'S AGENCY/CLIENT LIMIT ACCESS                               
*                                                                               
         USING TWAD,RF                                                          
         L     RF,ATWA                                                          
         MVC   IOKEY,STAFFKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR'                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  RF                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAVAD,R3                                                         
         L     R2,IOADDR                                                        
         LA    R3,TLSTELEM                                                      
RUNCD00  CLI   0(R3),0                                                          
         BE    RUNCD06                                                          
         CLI   0(R3),TAVAELQ                                                    
         BE    RUNCD04                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD00                                                          
*                                                                               
RUNCD01  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTALDIR'                                
*                                                                               
         USING TWAD,RF                                                          
         L     RF,ATWA                                                          
         CLC   IOKEY(TLSTSSEQ-TLSTD),STAFFKEY                                   
         BNE   CLIINV                                                           
         DROP  RF                                                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,IOADDR                                                        
         LA    R3,TLSTELEM                                                      
RUNCD02  CLI   0(R3),0                                                          
         BE    RUNCD01                                                          
         CLI   0(R3),TAVAELQ                                                    
         BE    RUNCD04                                                          
RUNCD03  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD02                                                          
*                                                                               
RUNCD04  OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    RUNCD06             STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
         CLC   AGYCD,TAVAAGY       IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   RUNCD03                                                          
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    RUNCD06             ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
RUNCD05  CLC   CLICD,0(RF)         IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    RUNCD06             ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   RUNCD05                                                          
         B     RUNCD03                                                          
         DROP  R3                                                               
*                                                                               
RUNCD06  LA    R2,IOKEY                                                         
         USING TLAYD,R2                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,AGYCD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO2'                            
         BNE   AGYINV                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
                                                                                
         L     RF,ATWA             MUST RESET RF                                
                                                                                
         L     R2,IOADDR                                                        
         LA    R3,TLAYELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
RUNCD08  DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BE    AGYINV            INVALID/MISSING NAME                           
         CLI   0(R3),TANAELQ                                                    
         BE    RUNCD10                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD08                                                          
*                                                                               
RUNCD10  ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BM    AGYINV                                                           
         EX    R4,MVANAME                                                       
         B     *+10                                                             
MVANAME  MVC   AGYN(0),TANANAME                                                 
                                                                                
         DROP  R2,R3                                                            
                                                                                
RUNCD15  OC    MEDCD,MEDCD        MEDIA REQUIRED?                               
         BZ    MISSING                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING TLCLD,R2                                                         
         XC    TLCLKEY,TLCLKEY                                                  
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,AGYCD                                                    
         MVC   TLCLCLI,CLICD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO2'                            
         BE    RUNCD22                                                          
         B     CLIINV                                                           
                                                                                
RUNCD22  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
         L     R2,IOADDR                                                        
         LA    R3,TLCLELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
RUNCD24  DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BE    CLIINV            INVALID/MISSING NAME                           
         CLI   0(R3),TANAELQ                                                    
         BE    RUNCD28                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD24                                                          
*                                                                               
RUNCD28  ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BM    CLIINV                                                           
         EX    R4,MVCNAME                                                       
         B     *+10                                                             
MVCNAME  MVC   CLIN(0),TANANAME                                                 
                                                                                
         DROP  R2,R3                                                            
                                                                                
RUNCD30  OC    PRDCD,PRDCD       SEE IF PRODUCT GIVEN                           
         BZ    RUNCD40           OPTIONAL                                       
         MVI   GLOBALP,0         INITIALIZE GLOBAL CHECK                        
         LA    R2,IOKEY                                                         
         USING TLPRD,R2                                                         
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,AGYCD                                                    
         MVC   TLPRCLI,CLICD                                                    
         MVC   TLPRPRD,PRDCD                                                    
RUNCD31  GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO2'                            
         BE    RUNCD32        FOUND RECORD I WAS SEACHING FOR                   
         CLI   GLOBALP,1      WAS I SEARCHING FOR THE GLOBAL?                   
         BE    PRDINV                                                           
         MVI   GLOBALP,1      NO - GO TRY FOR IT                                
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRPRD,PRDCD                                                    
         B     RUNCD31                                                          
                                                                                
RUNCD32  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
         L     R2,IOADDR                                                        
         LA    R3,TLPRELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
RUNCD34  DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BE    PRDINV            INVALID/MISSING NAME                           
         CLI   0(R3),TANAELQ                                                    
         BE    RUNCD38                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD34                                                          
*                                                                               
RUNCD38  ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BM    PRDINV                                                           
         EX    R4,MVPNAME                                                       
         B     *+10                                                             
MVPNAME  MVC   PRDN(0),TANANAME                                                 
                                                                                
         DROP  R2,R3                                                            
                                                                                
RUNCD40  DS    0H                                                               
         OC    CGPCD,CGPCD       SEE IF GROUP GIVEN                             
         BZ    RUNCD50           OPTIONAL                                       
         LA    R2,IOKEY                                                         
         USING TLOGD,R2                                                         
         XC    TLOGKEY,TLOGKEY                                                  
         MVI   TLOGCD,TLOGCDQ                                                   
         MVC   TLOGAGY,AGYCD                                                    
         MVC   TLOGCLI,CLICD                                                    
         MVC   TLOGPRD,PRDCD                                                    
         MVC   TLOGCOG,CGPCD                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO2'                            
         BE    RUNCD40B                                                         
         DC    H'0'                                                             
*                                                                               
RUNCD40A GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTALDIR+IO2'                            
*                                                                               
RUNCD40B CLC   IOKEY(TLOGPRD-TLOGKEY),IOKEYSAV   THRU CLIENT                    
         BNE   GRPINV                                                           
         OC    PRDCD,PRDCD    SEE IF PRODUCT CODE GIVEN                         
         BZ    RUNCD40D                                                         
         CLC   IOKEY(TLOGLEN-TLOGKEY),IOKEYSAV   THRU COMM. CODE                
         BE    RUNCD42                                                          
         B     GRPINV                                                           
*                                                                               
RUNCD40D CLC   TLOGCOG,CGPCD     MATCHES GROUP CODE?                            
         BE    RUNCD42                                                          
         B     RUNCD40A            KEEP LOOKING                                 
                                                                                
RUNCD42  DS    0H                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
         L     R2,IOADDR                                                        
         LA    R3,TLOGELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
RUNCD44  DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BE    GRPINV            INVALID/MISSING NAME                           
         CLI   0(R3),TANAELQ                                                    
         BE    RUNCD48                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RUNCD44                                                          
*                                                                               
RUNCD48  ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BM    GRPINV                                                           
         EX    R4,MVGNAME                                                       
         B     *+10                                                             
MVGNAME  MVC   CGPN(0),TANANAME                                                 
                                                                                
         DROP  R2,R3                                                            
**                                                                              
**       NOW READ COMMERCIALS                                                   
**                                                                              
RUNCD50  DS    0H                                                               
         OC    AGYN,SPACES                                                      
         OC    CLIN,SPACES                                                      
         OC    PRDN,SPACES                                                      
         OC    CGPN,SPACES                                                      
                                                                                
RUNREQX  L     R1,ALP              CALL DDLINK OUTPUT PROCESSOR                 
         GOTOR LP_APUTO                                                         
         J     EXITY                                                            
                                                                                
AGYINV   DS    0H                                                               
         LHI   R0,269                                                           
         B     INVTAL                                                           
CLIINV   DS    0H                                                               
         LHI   R0,394                                                           
         B     INVTAL                                                           
PRDINV   DS    0H                                                               
         LHI   R0,266                                                           
         B     INVTAL                                                           
GRPINV   DS    0H                                                               
         LHI   R0,289                                                           
         B     INVTAL                                                           
                                                                                
MISSING  LHI   R0,1            MISSING INPUT                                    
         B     INVGEN                                                           
                                                                                
STAFFINV LHI   R0,353          STAFF NOT FOUND                                  
         B     INVTAL                                                           
                                                                                
STFPWINV LHI   R0,141          PASSWORD MISSING/INVALID                         
         B     INVTAL                                                           
                                                                                
INVGEN   L     R1,ALP                                                           
         STCM  R0,3,LP_ERROR                                                    
         MVI   LP_EMSYS,10      GENSYSQ DIDN'T WORK                             
         J     EXITN                                                            
*                                                                               
INVTAL   L     R1,ALP                                                           
         STCM  R0,3,LP_ERROR                                                    
         MVI   LP_EMSYS,70      TALSYSQ DIDN'T WORK                             
         J     EXITN                                                            
         DROP  R1                                                               
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
         EJECT                                                                  
LITERALS DS    0D                                                               
         LTORG                                                                  
         DROP  R5                                                               
LVALUES  DS    0D                                                               
         DC    A(COMTAB)                                                        
         DC    A(PCOMTAB)                                                       
         DC    CL8'DMKEY'                                                       
         DC    CL8'TALDIR'                                                      
         DC    CL8'TALFIL'                                                      
         DC    XL4'FFFFFFFF'                                                    
         EJECT                                                                  
***********************************************************************         
*  GET NEXT COMMERCIAL                                                          
***********************************************************************         
                                                                                
NXTCOM   J     *+12                                                             
         DC    C'*NXTCOM*'                                                      
         LR    RB,RF                                                            
         USING NXTCOM,RB                                                        
NXTCOM2  EQU   *                                                                
         OC    PRDCD,PRDCD       ONE PRD REQ?                                   
         BNZ   NXTCOM4                                                          
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ACOMTAB),('B#COM',0),    X        
               (0,SAVED),0                                                      
         JNE   EXITY                                                            
         B     NXTCOMX                                                          
                                                                                
NXTCOM4  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',APCOMTAB),('B#COM',0),   X        
               (0,SAVED),0                                                      
         JNE   EXITY                                                            
NXTCOMX  EQU   *                                                                
         MVC   MYIOKEYS,IOKEY      SAVE KEY AND IOKEYSAV                        
         GOTOR FMTCOM                                                           
         BNE   NXTCOM2                                                          
         J     EXITY                                                            
                                                                                
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* FILTER AND FORMAT COMMERCIAL FOR DOWNLOADING                                  
***********************************************************************         
                                                                                
FMTCOM   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   SVIOADDR,IOADDR                                                  
                                                                                
         LA    R0,COMVALS1       CLEAR COMMERCIAL VALUES                        
         LHI   R1,COMVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R2,AIO5   CLEAR IO5/IO6/IO8                                      
*                        CLEAR 11250 BYTES (of 8192)  1024 X 8                  
         LA    R3,45                                                            
FMTCM2   XC    0(250,R2),0(R2)                                                  
         LA    R2,250(R2)                                                       
         BCT   R3,FMTCM2                                                        
                                                                                
******   XC    CCONTS,CCONTS       CLEAR CONTRACTS FIELD                        
         XC    CONTNUM,CONTNUM     CLEAR VALUE FOR ARRAY                        
                                                                                
         LA    R2,IOKEY                                                         
         USING TLCOPD,R2                                                        
         CLI   TLCOVVER,C'A'   ALWAYS BYPASS A VERSIONS                         
         JE    EXITN                                                            
         CLI   BEBCD,C'S'      SUMMARY ONLY?                                    
         BNE   FMTCM2A                                                          
         CLI   TLCOVVER,0                                                       
         JNE   EXITN          THEN NO VERSIONS                                  
                                                                                
         DROP  R2                                                               
                                                                                
FMTCM2A  L     R2,IOADDR                                                        
         USING TLCOD,R2                                                         
*                                                                               
         MVC   SAVCOMID,TLCOCOM    SAVE INTERNAL COMM ID                        
         MVC   CVCHAR,IOKEY+(TLCOVVER-TLCOPKEY) VER. LETTER IN IOKEY            
         MVC   CPRD,TLCOPRD        USE PRODUCT FROM KEY (AS DEFAULT)            
                                                                                
         CLI   CVCHAR,0       SEE IF I HAVE A LETTER (WON'T BE A)               
         BE    FMTCM2X                                                          
*                                                                               
*        I MUST FIND AND SAVE ISCII FROM COMMERCIAL                             
*        I WILL NEED IT IF I FIND A VERSION RECORD                              
*                                                                               
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TACOD,R3                                                         
FMTCM2B  CLI   0(R3),0             END OF REC?                                  
         BE    FMTCMNO                                                          
         CLI   0(R3),TACOELQ                                                    
         BE    FMTCM2C                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM2B                                                          
*                                                                               
FMTCM2C  DS    0H                                                               
         TM    TACOSTAT,X'A0'    SEE IF RELEASED OR LOCKED                      
         JNZ   EXITN             IF YES, SKIP THIS COMMERCIAL                   
*                                                                               
*        ALSO SKIP SOME COMMERCIAL TYPES                                        
*                                                                               
         CLI   TACOTYPE,CTYAUD     AUDITION                                     
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYCAN     CANCELLATION                                 
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYDEM     DEMO                                         
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYGRT     GUARANTEE                                    
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYIND     INDUSTRIAL                                   
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYMUS     MUSIC                                        
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYPRNT    PRINT                                        
         JE    EXITN                                                            
         CLI   TACOTYPE,CTYSOAP    SOAP                                         
         JE    EXITN                                                            
*                                                                               
         MVC   CVCODE,TACOCID                                                   
*                                                                               
*        NOW FIND AND SAVE THE ISCII FROM A VERSION ELEMENT                     
*                                                                               
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TAVRD,R3                                                         
FMTCM2C4 CLI   0(R3),0             END OF REC?                                  
         BNE   *+6                                                              
         DC    H'0'                 VERSION ELEMENT MISSING                     
         CLI   0(R3),TAVRELQ                                                    
         BE    FMTCM2C6                                                         
FMTCM2C5 ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM2C4                                                         
                                                                                
FMTCM2C6 EQU   *                                                                
         CLC   CVCHAR,TAVRVERS      VERSION CODES MUST MATCH                    
         BNE   FMTCM2C5                                                         
         MVC   CISCII,TAVRCID       SAVE VERSION'S ISCII                        
                                                                                
         DROP  R3                                                               
                                                                                
                                                                                
*     HERE I MUST CHECK FOR A VERSION RECORD                                    
*     AND PROCESS IT INSTEAD                                                    
*     THE VERSION RECORD IS IDENTICAL TO THE "NORMAL" COMMERCIAL                
                                                                                
         LA    R2,IOKEY                                                         
         USING TLVRD,R2                                                         
         XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,SAVCOMID    INTERNAL COMMERCIAL NUMBER                   
         MVC   TLVRVER,CVCHAR                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO4'                            
         BNE   FMTCM2F                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO4'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
         B     FMTCM2K                                                          
                                                                                
FMTCM2F  MVC   CVCODE,SPACES     CLEAR IF NO VERSION RECORD FOUND               
         MVC   IOADDR,SVIOADDR    RESTORE IOADDR TO PROPER RECORD               
                                                                                
         DROP  R2                                                               
                                                                                
FMTCM2K  L     R2,IOADDR           RESET R2                                     
         USING TLCOD,R2                                                         
                                                                                
FMTCM2X  LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TACOD,R3                                                         
FMTCM3   CLI   0(R3),0             END OF REC?                                  
         BE    FMTCMNO                                                          
         CLI   0(R3),TACOELQ                                                    
         BE    FMTCM3X                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM3                                                           
*                                                                               
FMTCM3X  TM    TACOSTAT,X'A0'    SEE IF RELEASED OR LOCKED                      
         JNZ   FMTCMNO           IF YES, SKIP THIS COMMERCIAL                   
*                                                                               
*        ALSO SKIP SOME COMMERCIAL TYPES                                        
*                                                                               
         CLI   TACOTYPE,CTYAUD     AUDITION                                     
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYCAN     CANCELLATION                                 
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYDEM     DEMO                                         
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYGRT     GUARANTEE                                    
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYIND     INDUSTRIAL                                   
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYMUS     MUSIC                                        
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYPRNT    PRINT                                        
         JE    FMTCMNO                                                          
         CLI   TACOTYPE,CTYSOAP    SOAP                                         
         JE    FMTCMNO                                                          
*                                                                               
                                                                                
         CLC   TACOMED,MEDCD       MUST MATCH MEDIA CODE                        
         BNE   FMTCMNO                                                          
*                                                                               
*    ACTIVE DATE NOW REQUIRED - SKIP IF MISSING   6/23/03                       
*                                                                               
         OC    TACOACT,TACOACT    DO I HAVE AN ACTIVE DATE?                     
         BZ    FMTCMNO            NO- SKIP                                      
*                                                                               
         OC    END,END            SEE IF END DATE GIVEN                         
         BZ    FMTCM5                                                           
         CLC   TACOACT,END        SEE IF ACTIVE DATE AFTER END                  
         BH    FMTCMNO            SKIP                                          
         B     FMTCM5A                                                          
*                                                                               
FMTCM5   CLC   TACOAIR,END        FIRST AIR DATE AFTER END                      
         BH    FMTCMNO            SKIP                                          
                                                                                
FMTCM5A  OC    START,START        SEE IF START GIVEN                            
         BZ    FMTCM8                                                           
FMTCM5C  OC    TACOINAC,TACOINAC  DO I HAVE AN INACTIVE DATE?                   
         BZ    FMTCM6             NO- USE EXPIRATION DATE                       
         CLC   TACOINAC,START     SEE IF INACTIVE DATE BEFORE START             
         BL    FMTCMNO            SKIP                                          
         B     FMTCM8                                                           
                                                                                
FMTCM6   OC    TACOEXP,TACOEXP    DO I HAVE AN EXPIRATION DATE?                 
         BZ    FMTCM8             N0 - PROCESS                                  
         CLC   TACOEXP,START      EXPIRATION DATE BEFORE START                  
         BL    FMTCMNO                                                          
                                                                                
FMTCM8   DS    0H                 PAST FILTERS - PROCESS                        
         OC    CTYCD,CTYCD        SEE IF TYPE FILTER ENTERED                    
         BZ    FMTCM9                                                           
         CLC   TACOTYPE,CTYCD     MUST MATCH                                    
         BNE   FMTCMNO                                                          
                                                                                
FMTCM9   DS    0H                                                               
         OC    CGPCD,CGPCD        SEE IF COMMERCIAL GROUP GIVEN                 
         BZ    FMTCM9C                                                          
         CLC   TACOCGRP,CGPCD     GROUP CODES MUST MATCH                        
         BNE   FMTCMNO                                                          
                                                                                
FMTCM9C  EQU   *                                                                
*                                                                               
         L     R2,IOADDR                                                        
         CLI   TLCOCD,TLVRCDQ     SEE IF A VERSION RECORD                       
         BE    *+10               CISCII WILL BE ALREADY SET                    
         MVC   CISCII,TACOCID     ISCII CODE                                    
         EDIT  (B1,TACOSEC),(3,CLEN),0,ALIGN=LEFT                               
         MVC   CEXDTE,TACOEXP     EXPIRATION DATE                               
*                                                                               
         MVC   CINVDTE,TACOINAC   INACTIVE DATE                                 
                                                                                
         DROP  R3                                                               
                                                                                
FMTCM9X  EQU   *                                                                
         CLI   CVCHAR,0            SEE IF VERSION COMMERCIAL                    
         BE    FMTCM12X            NO - SKIP                                    
         L     R2,IOADDR                                                        
         CLI   TLCOCD,TLVRCDQ      AM I PROCESSSING A VERSION RECORD?           
         BE    FMTCM12X            IF YES - I WON'T FIND VERSION ELEM           
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TAVRD,R3                                                         
FMTCM12  CLI   0(R3),0             END OF REC?                                  
         BNE   *+6                                                              
         DC    H'0'                 VERSION ELEMENT MISSING                     
         CLI   0(R3),TAVRELQ                                                    
         BE    FMTCM12C                                                         
FMTCM12B ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM12                                                          
                                                                                
FMTCM12C EQU   *                                                                
         CLC   CVCHAR,TAVRVERS      VERSION CODES MUST MATCH                    
         BNE   FMTCM12B                                                         
FMTCM12D EQU   *                                                                
         MVC   CVCODE,CISCII       MOVE BASIC TO CVCODE                         
         MVC   CISCII,TAVRCID      USE VERSION'S ISCII                          
         EDIT  (B1,TAVRSEC),(3,CLEN),0,ALIGN=LEFT                               
                                                                                
         DROP  R3                                                               
*                                                                               
FMTCM12X EQU   *                                                                
         L     R2,IOADDR                                                        
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
FMTCM16  CLI   0(R3),0             END OF REC?                                  
         BE    FMTCM20                                                          
         CLI   0(R3),TANAELQ                                                    
         BE    FMTCM16X                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM16                                                          
*                                                                               
FMTCM16X ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BM    FMTCM20                                                          
         EX    R4,MVTITLE                                                       
         B     *+10                                                             
                                                                                
MVTITLE  MVC   CTITLE(0),TANANAME       EXECUTED                                
                                                                                
         DROP  R3                                                               
                                                                                
FMTCM20  EQU   *                                                                
*                                                                               
         L     R2,IOADDR                                                        
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TAPRD,R3                                                         
FMTCM22  CLI   0(R3),0             END OF REC?                                  
         BE    FMTCM30                                                          
         CLI   0(R3),TAPRELQ                                                    
         BE    FMTCM20X                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM22                                                          
*                                                                               
FMTCM20X MVC   CPRD,TAPRPRD                                                     
                                                                                
         DROP  R3                                                               
                                                                                
*        CHECK FOR PRODUCT NAME IN FREE FORM NAME ELEMENT                       
*                                                                               
FMTCM30  L     R2,IOADDR                                                        
         LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
         USING TAFND,R3                                                         
FMTCM32  CLI   0(R3),0             END OF REC?                                  
         BE    FMTCM40                                                          
         CLI   0(R3),TAFNELQ                                                    
         BE    FMTCM30X                                                         
FMTCM34  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM32                                                          
*                                                                               
FMTCM30X CLI   TAFNTYPE,TAFNTPRD    TYPE MUST BE PRODUCT                        
         BNE   FMTCM34              KEEP LOOKING                                
         ZIC   R4,TAFNLEN                                                       
         SH    R4,=H'4'       4 DUE TO TYPE BYTE                                
         BM    FMTCM40                                                          
         EX    R4,MVPRDN                                                        
         B     *+10                                                             
                                                                                
MVPRDN   MVC   CPRDN(0),TAFNNAME       EXECUTED                                 
                                                                                
         DROP  R3                                                               
                                                                                
FMTCM40  DS    0H                                                               
         CLI   CPRDN,C' '         SEE IF NAME ENTERED                           
         BH    FMTCM40X                                                         
         CLI   CPRD,C' '          DO I HAVE A PRODUCT?                          
         BNH   FMTCM40X                                                         
                                                                                
*        MUST READ PRODUCT TO GET NAME                                          
                                                                                
         DROP  R2                                                               
                                                                                
         MVI   GLOBALP,0       INITIALIZE GLOBAL PRD CHECK                      
         LA    R2,IOKEY                                                         
         USING TLPRD,R2                                                         
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,AGYCD                                                    
         MVC   TLPRCLI,CLICD                                                    
FMTCM40A MVC   TLPRPRD,CPRD                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO4'                            
         BE    FMTCM40B                                                         
         CLI   GLOBALP,1                                                        
         BNE   *+6                                                              
         DC    H'0'               MUST FIND PRODUCT                             
         MVI   GLOBALP,1          GO TRY FOR A GOLBAL PRD                       
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ                                                   
         B     FMTCM40A                                                         
                                                                                
         DS    0H                                                               
FMTCM40B GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO4'                           
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
         L     R2,IOADDR                                                        
         LA    R3,TLPRELEM         POINT TO FIRST ELEMENT                       
         USING TANAD,R3                                                         
FMTCM40C DS    0H                                                               
         CLI   0(R3),0             END OF RECORD                                
         BNE   *+6               INVALID/MISSING NAME                           
         DC    H'0'              MUST HAVE A NAME                               
                                                                                
         CLI   0(R3),TANAELQ                                                    
         BE    FMTCM40D                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM40C                                                         
*                                                                               
FMTCM40D ZIC   R4,TANALEN                                                       
         SH    R4,=H'3'                                                         
         BNM   *+6                                                              
         DC    H'0'               NO NAME                                       
         MVC   CPRDN,SPACES                                                     
         EX    R4,MVCPNAM                                                       
         B     *+10                                                             
                                                                                
MVCPNAM  MVC   CPRDN(0),TANANAME        EXECUTED                                
                                                                                
         DROP  R2,R3                                                            
                                                                                
FMTCM40X EQU   *                                                                
         OC    CISCII,SPACES                                                    
         OC    CTITLE,SPACES                                                    
         OC    CVCODE,SPACES                                                    
         OC    CPRD,SPACES                                                      
         OC    CPRDN,SPACES                                                     
                                                                                
         L     R6,AIO5           WHERE TO STORE FIRST CONTRACT                  
         USING CONTABD,R6                                                       
         B     FMTCM60           SKIP CODE BELOW THAT CHECKED                   
*                                FOR CONTRACT ELEMENTS                          
*                                AS THOSE CONTRACTS DON'T EXIST                 
*                                                                               
*                                 SEARCH FOR CONTRACT ELEMENTS                  
*                                 AND BUILD ARRAY IN IO5                        
*        L     R2,IOADDR                                                        
*        USING TLCOD,R2                                                         
*        LA    R3,TLCOELEM         POINT TO FIRST ELEMENT                       
*        USING TACCD,R3                                                         
*MTCM42  CLI   0(R3),0             END OF REC?                                  
*        BE    FMTCM50                                                          
*        CLI   0(R3),TACCELQ                                                    
*        BE    FMTCM44                                                          
*        ZIC   R0,1(R3)                                                         
*        AR    R3,R0                                                            
*        B     FMTCM42                                                          
*                                                                               
*MTCM44  ZIC   R4,TACCLEN                                                       
*        SH    R4,=H'4'      SINCE THERE IS A SPARE BYTE                        
*        BM    FMTCM50                                                          
*        EX    R4,MVCONT                                                        
*        B     *+10                                                             
*                                                                               
*VCONT   MVC   CCONTS(0),TACCNCON       EXECUTED                                
*                                                                               
*        DROP  R3                                                               
*                                                                               
*MTCM50  EQU   *                                                                
*        L     R6,AIO5                                                          
*        USING CONTABD,R6                                                       
*                                                                               
*        CLI   CCONTS,0           SEE IF I HAVE ANY                             
*        BE    FMTCM60            NO - TRY AND READ PASSIVES                    
*                                                                               
**       READ EACH CONTRACT AND STORE ITS VALUES IN THE TABLE                   
*                                                                               
*        LA    R5,CCONTS+1      POINT TO FIRST CONTRACT                         
*MTCM50B CLI   0(R5),0                                                          
*        BE    FMTCMX       DONE PROCESSING CONTRACT ELEMENTS                   
*        LA    R2,IOKEY                                                         
*        USING TLCND,R2                                                         
*        XC    TLCNKEY,TLCNKEY                                                  
*        MVI   TLCNCD,TLCNCDQ                                                   
*        MVC   TLCNAGY,AGYCD                                                    
*        MVC   TLCNCNID,0(R5)                                                   
*        GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO4'                            
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        CLC   IOKEY(TLCNTRMS-TLCNKEY),IOKEYSAV      AGY AND NUMBER             
*        BNE   FMTCM50N  NOT FOUND -DON'T DIE - SKIP TO NEXT                    
*        GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO4'                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   CONSTART,TLCNTRMS     (IN KEY)                                   
*        MVC   CONEND,TLCNTRME       (ALSO IN KEY)                              
*                                                                               
*        L     R2,IOADDR                                                        
*        LA    R3,TLCNELEM        POINT TO FIRST ELEMENT                        
*        USING TARDD,R3                                                         
*MTCM50E CLI   0(R3),0             END OF RECORD                                
*        BE    FMTCM50K          MISSING DETAILS ELEMENT                        
*        CLI   0(R3),TARDELQ                                                    
*        BE    FMTCM50F                                                         
*        ZIC   R0,1(R3)                                                         
*        AR    R3,R0                                                            
*        B     FMTCM50E                                                         
*                                                                               
*MTCM50F MVC   CONTYPE(1),TARDTYP1   CONTRACT TYPE                              
*        MVC   CONAMT,TARDCFEE       AMOUNT                                     
*                                                                               
*        DROP  R3                                                               
*                                                                               
*MTCM50K DS    0H                                                               
*        LA    R6,CONTABL(R6)      BUMP TABLE ENTRY                             
*        LH    R0,CONTNUM          BUMP COUNTER                                 
*        AH    R0,=H'1'                                                         
*        STH   R0,CONTNUM                                                       
*                                                                               
*MTCM50N LA    R5,12(R5)       BUMP TO NEXT CONTRACT                            
*        B     FMTCM50B                                                         
                                                                                
FMTCM60  DS    0H                                                               
         LA    R2,IOKEY                                                         
         USING TLCNPD,R2                                                        
         XC    TLCNPKEY,TLCNPKEY                                                
         MVI   TLCNPCD,TLCNPCDQ                                                 
         MVC   TLCNPCOM,SAVCOMID    NOW EARLIER IN KEY                          
         MVC   TLCNPAGY,AGYCD                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO4'                            
         BE    FMTCM64                                                          
         DC    H'0'                                                             
                                                                                
FMTCM62  LH    R0,CONTNUM          BUMP COUNTER                                 
*        CHI   R0,2                                                             
*        BNL   FMTCM70                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTALDIR+IO4'                            
                                                                                
FMTCM64  CLC   IOKEY(TLCNPCND-TLCNPKEY),IOKEYSAV  THRU AGY                      
         BNE   FMTCM70     END OF COMMERCIAL/AGY                                
******** CLC   TLCNPCOM,SAVCOMID   COMMERCIAL IDS MUST MATCH                    
******** BNE   FMTCM62                                                          
*        SKIP IF CONTRACT DATES NOT IN MY PERIOD                                
                                                                                
         CLC   TLCNPTRS,END    SEE IF CONTRACT START AFTER MY END               
         BH    FMTCM62         SKIP                                             
         OC    TLCNPTRE,TLCNPTRE   BE SURE I HAVE AN END                        
         BZ    FMTCM64A                                                         
         CLC   TLCNPTRE,START  SEE IF CONTRACT END BEFORE MY START              
         BL    FMTCM62         SKIP                                             
                                                                                
FMTCM64A CLC   TLCNPVER,CVCHAR     VERSION CODES MUST ALSO MATCH                
         BE    FMTCM64B                                                         
         CLI   CVCHAR,X'00'  PROCESSING 'MAIN' COMMERCIAL?                      
         BNE   FMTCM62                                                          
         CLI   TLCNPVER,C'A'  ALSO PASS CONTRACTS FOR VERSION A                 
         BNE   FMTCM62                                                          
*                                                                               
FMTCM64B MVC   SVPKEY,IOKEY         SAVE KEYS                                   
********                                                                        
********        THIS PASSIVE POINTER IS TO THE CONTRACT                         
********        GO JUST READ IT                                                 
********                                                                        
********       SINCE THE KEY ABOVE POINTS TO THE COMMERCIAL                     
********       INSTEAD OF THE CONTRACT, I MUST READ FOR THE CONTRACT            
********                                                                        
******** DROP  R2                                                               
********                                                                        
******** LA    R2,IOKEY                                                         
******** USING TLCND,R2                                                         
******** XC    TLCNKEY,TLCNKEY                                                  
******** MVI   TLCNCD,TLCNCDQ                                                   
******** MVC   TLCNAGY,AGYCD                                                    
******** MVC   TLCNCNID,SVPKEY+(TLCNPCND-TLCNPKEY)                              
********                                                                        
******** GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO4'                            
******** BE    *+6                                                              
******** DC    H'0'                                                             
******** CLC   IOKEY(TLCNTRMS-TLCNKEY),IOKEYSAV      AGY AND NUMBER             
******** BE    *+6                                                              
******** DC    H'0'            CONTRACT MUST BE ON FILE                         
********                                                                        
******** SKIP IF CONTRACT DATES NOT IN MY PERIOD                                
********                                                                        
******** CLC   TLCNTRMS,END    SEE IF CONTRACT START AFTER MY END               
******** BH    FMTCM66X                                                         
******** OC    TLCNTRME,TLCNTRME   BE SURE I HAVE AN END                        
******** BZ    FMTCM64C                                                         
******** CLC   TLCNTRME,START  SEE IF CONTRACT END BEFORE MY START              
******** BL    FMTCM66X                                                         
                                                                                
FMTCM64C GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO4'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONSTART,TLCNPTRS   (IN KEY)                                     
         MVC   CONEND,TLCNPTRE     (ALSO IN KEY)                                
                                                                                
         DROP  R2                                                               
                                                                                
         L     R2,IOADDR                                                        
         USING TLCND,R2                                                         
         LA    R3,TLCNELEM        POINT TO FIRST ELEMENT                        
         USING TARDD,R3                                                         
FMTCM64E CLI   0(R3),0             END OF RECORD                                
         BE    FMTCM64K          MISSING DETAILS ELEMENT                        
         CLI   0(R3),TARDELQ                                                    
         BE    FMTCM64F                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM64E                                                         
*                                                                               
FMTCM64F MVC   CONTYPE(1),TARDTYP1   CONTRACT TYPE                              
         OC    CONTYPE,SPACES                                                   
*                                                                               
         MVC   CONAMT,TARDCFEE       AMOUNT                                     
*                                                                               
         CLI   TARDTYP1,TARDTYCG     SEE IF CLELBRITY/GUARANTEE                 
         BE    FMTCM65               SPECIAL CODE                               
*                                                                               
         LA    R1,CONDESC        SET R1 TO CONTRACT DESCRIPTION                 
         B     FMTCM66           FOR FMTCM66 ROUTINE                            
                                                                                
FMTCM64K DC    H'0'              MUST HAVE DETAILS ELEMENT                      
                                                                                
FMTCM65  EQU   *                  SPECIAL CODE FOR GUARANTEES                   
         MVC   CONTYPE(2),=C'GU'                                                
*                             INSTUCTION ABOVE MAY BE ONLY TEMPORARY            
*                                                                               
         OC    TARDSSN,TARDSSN    SEE IF I HAVE A SS NUMBER                     
         BZ    FMTCM66  JUST SEND COMMENT - I CAN'T READ FOR NAME               
**                                                                              
**       GET CAST MEMBERS NAME IN TAW4 RECORD                                   
**                                                                              
         LA    R2,IOKEY                                                         
         USING TLW4D,R2                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,TARDSSN       SS NUMBER                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO1'                            
         BE    FMTCM65C                                                         
         DC    H'0'                                                             
                                                                                
FMTCM65C EQU   *                                                                
         CLC   IOKEY(TLW4FID-TLW4KEY),IOKEYSAV   THRU SS NUMBER                 
         BE    *+6                                                              
         DC    H'0'        MUST FIND                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO1'                           
         BE    *+6                                                              
         DC    H'0'     DATAMGR ERROR                                           
                                                                                
         DROP  R2                                                               
         DROP  R3                                                               
                                                                                
         L     R2,IOADDR                                                        
         USING TLW4D,R2                                                         
         LA    R3,TLW4ELEM         POINT TO FIRST ELEMENT                       
         USING TAW4D,R3            CAST DETAILS                                 
FMTCM65E CLI   0(R3),0             END OF REC?                                  
         BNE   *+6                 NO DETAILS ELEMENT - DUMP                    
         DC    H'0'                BAD RECORD                                   
         CLI   0(R3),TAW4ELQ                                                    
         BE    FMTCM65F                                                         
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM65E                                                         
*                                                                               
FMTCM65F MVC   CSTNAM1,TAW4NAM1     FIRST NAME                                  
         MVC   CSTNAM2,TAW4NAM2     LAST NAME                                   
*        SET NAME AND DESCRIPTION IN THE DESCRIPTION FIELD                      
         MVC   CONDESC(L'CSTNAM1),CSTNAM1  FIRST NAME                           
         LA    R1,CONDESC+L'CSTNAM1                                             
FMTCM65G CLI   0(R1),C' '                                                       
         BH    FMTCM65H                                                         
         SH    R1,=H'1'                                                         
         B     FMTCM65G                                                         
*                                                                               
FMTCM65H LA    R1,2(R1)                                                         
         MVC   0(L'CSTNAM2,R1),CSTNAM2   LAST NAME                              
                                                                                
         LA    R1,CONDESC+(L'CSTNAM1+L'CSTNAM2+1)                               
FMTCM65I CLI   0(R1),C' '                                                       
         BH    FMTCM65J                                                         
         SH    R1,=H'1'                                                         
         B     FMTCM65I                                                         
*                                                                               
FMTCM65J MVC   1(3,R1),=C' / '                                                  
         LA    R1,4(R1)                                                         
*                                                                               
         DROP  R3                                                               
                                                                                
FMTCM66  EQU   *                                                                
         L     R2,AIO4            RESET TO CONTRACT                             
         USING TLCND,R2                                                         
         LA    R3,TLCNELEM        POINT TO FIRST ELEMENT                        
         USING TAFND,R3                                                         
FMTCM66E CLI   0(R3),0             END OF RECORD                                
         BE    FMTCM66K        NO COMMENT (DESCRIPTION) ELEM                    
         CLI   0(R3),TAFNELQ                                                    
         BE    FMTCM66G                                                         
FMTCM66F ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM66E                                                         
*                                                                               
FMTCM66G EQU   *                                                                
         CLI   TAFNTYPE,TAFNTCON     CONTRACT NAME?                             
         BNE   FMTCM66F                                                         
         ZIC   R4,TAFNLEN                                                       
         SH    R4,=H'3'                                                         
         BM    FMTCM66K                                                         
*                                                                               
FMTCM66H EX    R4,MVCOND                                                        
         B     *+10                                                             
MVCOND   MVC   0(0,R1),TAFNNAME     MAX IS 36 BYTES                             
                                                                                
         DROP  R2,R3                                                            
                                                                                
FMTCM66K EQU   *                                                                
                                                                                
         LA    R6,CONTABL(R6)      BUMP TABLE ENTRY                             
         LH    R0,CONTNUM          BUMP COUNTER                                 
         AH    R0,=H'1'                                                         
         STH   R0,CONTNUM                                                       
                                                                                
FMTCM66X LA    R2,IOKEY            RESET R2 TO IOKEY                            
         MVC   IOKEY,SVPKEY        RESTORE FOR SEQ READ                         
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO4'                            
         B     FMTCM62                                                          
         EJECT                                                                  
**                                                                              
**       NOW CHECK FOR GUARANTEES                                               
**                                                                              
FMTCM70  EQU   *                                                                
************************************************************                    
************************************************************                    
         B     FMTCM90     SKIP TO HOLDING FEE LOGIC                            
*                          GUARANTEE LOGIC NO-OPED (FOR NOW)                    
************************************************************                    
************************************************************                    
*        FIRST READ FOR CAST MEMBERS FOR THIS COMMERCIAL                        
*                                                                               
***      LA    R2,IOKEY                                                         
***      USING TLCAD,R2                                                         
***      XC    TLCAKEY,TLCAKEY                                                  
***      MVI   TLCACD,TLCACDQ                                                   
***      MVC   TLCACOM,SAVCOMID                                                 
***      GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO8'                            
***      BE    FMTCM74                                                          
***      DC    H'0'                                                             
***                                                                             
***CM72  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOTALDIR+IO8'                            
***                                                                             
***CM74  CLC   IOKEY(TLCASORT-TLCAKEY),IOKEYSAV  THRU COMM ID                   
***      BNE   FMTCM90          GO READ FOR HOLDING FEES                        
***      TM    TLCASORT,X'02'   IF ON - NOT A GUARANTEE - SKIP                  
***      BO    FMTCM72                                                          
***      GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO8'                           
***      BE    *+6                                                              
***      DC    H'0'                CAN'T READ                                   
***                                                                             
***      CLI   CVCHAR,0            SEE IF PROCESSING A VERSION                  
***      BE    FMTCM75X            SKIP VERSION CHECKING                        
***                                                                             
***      L     R2,IOADDR                                                        
***      USING TLCAD,R2                                                         
***      LA    R3,TLCAELEM         POINT TO FIRST ELEMENT                       
***      USING TAFND,R3                                                         
***CM75  CLI   0(R3),0             END OF REC?                                  
***      BE    FMTCM72             NO VERSION ELEMENT - SKIP                    
***      CLI   0(R3),TAFNELQ                                                    
***      BE    FMTCM75D                                                         
***CM75B ZIC   R0,1(R3)                                                         
***      AR    R3,R0                                                            
***      B     FMTCM75                                                          
***                                                                             
***CM75D CLI   TAFNTYPE,TAFNTVER    TYPE MUST BE VERSION                        
***      BNE   FMTCM75B                                                         
***      XC    CSTVL,CSTVL           CLEAR CAST VERSION LETTERS                 
***      ZIC   R4,TAFNLEN                                                       
***      SH    R4,=H'4'       4 DUE TO TYPE BYTE                                
***      BM    FMTCM72       NO CAST VERSION LETTERS                            
***                          SKIP THIS CAST MEMBER                              
***      EX    R4,MVCVER                                                        
***      B     *+10                                                             
***                                                                             
***VER   MVC   CSTVL(0),TAFNNAME       EXECUTED                                 
***                                                                             
***      LA    R1,CSTVL                                                         
***CM75G CLI   0(R1),0           END OF VERSION LIST                            
***      BE    FMTCM72           SKIP THIS CAST MEMBER                          
***      CLC   0(1,R1),CVCHAR                                                   
***      BE    FMTCM75X          PROCESS THE MEMBER                             
***      LA    R1,1(R1)                                                         
***      B     FMTCM75G                                                         
***                                                                             
***      DROP  R3,R2                                                            
***                                                                             
***CM75X EQU   *                 CAST MEMBER PROCESSING                         
***      MVC   SVPKEY,IOKEY    MUST SAVE KEYS                                   
***                                                                             
***      LA    R2,IOKEY                                                         
***      USING TLCAD,R2                                                         
***      MVC   CSTSSN,TLCASSN   SAVE CAST'S SS NUMBER                           
***                                                                             
***      DROP  R2                                                               
***                                                                             
***                                                                             
***      FIND AND SAVE GUARANTEE CODE                                           
***                                                                             
***      L     R2,IOADDR                                                        
***      USING TLCAD,R2                                                         
***      LA    R3,TLCAELEM         POINT TO FIRST ELEMENT                       
***      USING TACAD,R3            CAST DETAILS                                 
***CM77  CLI   0(R3),0             END OF REC?                                  
***      BNE   *+6                 NO DETAILS ELEMENT - DUMP                    
***      DC    H'0'                BAD RECORD                                   
***      CLI   0(R3),TACAELQ                                                    
***      BE    FMTCM77D                                                         
***      ZIC   R0,1(R3)                                                         
***      AR    R3,R0                                                            
***      B     FMTCM77                                                          
***                                                                             
***CM77D MVC   CSTGUA,TACAGUA       GUARANTEE CODE                              
***      OC    CSTGUA,CSTGUA        BE SURE I HAVE ONE                          
***      BZ    FMTCM72              IF NOT MUST SKIP                            
***                                                                             
***                                                                             
***      NOW READ GUARANTEE RECORD FOR CODE AND SS NUMBER                       
***                                                                             
***                                                                             
***      LA    R2,IOKEY                                                         
***      USING TLGUD,R2                                                         
***      XC    TLGUKEY,TLGUKEY                                                  
***      MVI   TLGUCD,TLGUCDQ                                                   
***      MVC   TLGUSSN,CSTSSN       SS NUMBER                                   
***      MVC   TLGUGUA,CSTGUA       GUA CODE                                    
***      XC    TLGUGUA,FFFS         COMPLEMENT IT                               
***      GOTOR (#IOEXEC,AIOEXEC),'IORD+IOTALDIR+IO1'                            
***      BE    FMTCM80                                                          
***      DC    H'0'        MISSING GUARANTEE RECORD                             
***                                                                             
***CM80  EQU   *                                                                
***      GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO1'                           
***      BE    *+6                                                              
***      DC    H'0'     DATAMGR ERROR                                           
***                                                                             
***      DROP  R2                                                               
***                                                                             
***      L     R2,IOADDR                                                        
***      USING TLGUD,R2                                                         
***      LA    R3,TLGUELEM      POINT TO FIRST ELEMENT                          
***      USING TAGUD,R3         GUARANTEE DETAILS                               
***CM81  CLI   0(R3),0             END OF REC?                                  
***      BNE   *+6                 NO DETAILS ELEMENT - DUMP                    
***      DC    H'0'                BAD RECORD                                   
***      CLI   0(R3),TAGUELQ                                                    
***      BE    FMTCM82                                                          
***      ZIC   R0,1(R3)                                                         
***      AR    R3,R0                                                            
***      B     FMTCM81                                                          
***                                                                             
***CM82  OC    TAGUCOM,TAGUCOM    SEE IF LARGE SCALE - TAGUCOM NOT SET          
***      BNZ   NXTCAST                                                          
***                                                                             
***      CHECK GUARNTEE START AND END VS. MY PERIOD                             
***                                                                             
***      CLC   TAGUSTRT,END    SEE IF START AFTER MY END                        
***      BH    NXTCAST                                                          
***      OC    TAGUEND,TAGUEND   BE SURE I HAVE AN END                          
***      BZ    FMTCM83                                                          
***      CLC   TAGUEND,START   SEE IF END BEFORE MY START                       
***      BL    NXTCAST                                                          
***      B     FMTCM83                                                          
***                                                                             
***CAST  XC    CSTVALS(CSTVALSL),CSTVALS CLEAR CAST VAUES                       
***      LA    R2,IOKEY            RESET R2 TO IOKEY                            
***      MVC   IOKEY,SVPKEY        RESTORE FOR SEQ READ                         
***      GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO8'                            
***      B     FMTCM72                                                          
***                                                                             
***CM83  MVC   CSTSDAT,TAGUSTRT                                                 
***      MVC   CSTEDAT,TAGUEND                                                  
***                                                                             
***      L     R2,IOADDR                                                        
***      USING TLGUD,R2                                                         
***      LA    R3,TLGUELEM      POINT TO FIRST ELEMENT                          
***      USING TACMD,R3         GUARANTEE DETAILS                               
***CM83C CLI   0(R3),0             END OF REC?                                  
***      BE    FMTCM83X            NO COMMENT ELEMENT                           
***      CLI   0(R3),TACMELQ                                                    
***      BE    FMTCM83F                                                         
***CM83D ZIC   R0,1(R3)                                                         
***      AR    R3,R0                                                            
***      B     FMTCM83C                                                         
***                                                                             
***CM83F CLI   TACMTYPE,TACMTYPG     GUARANTEE COMMENT                          
***      BNE   FMTCM83D                                                         
***                                                                             
***      ZIC   R4,TACMLEN                                                       
***      SH    R4,=H'4'                                                         
***      BM    FMTCM20                                                          
***      CH    R4,=H'145'                                                       
***      BNH   *+8                                                              
***      LH    R4,=H'145'    RESET TO MAX I CAN SAVE                            
***      EX    R4,MVGCOMM                                                       
***      B     *+10                                                             
***                                                                             
***COMM  MVC   CSTCOMM(0),TACMCOMM      EXECUTED                                
***                                                                             
***      DROP  R2,R3                                                            
***CM83X EQU   *                                                                
***      EJECT                                                                  
***                                                                             
***      FINALLY READ FOR CAST MEMBERS NAME IN TAW4 RECORD                      
***                                                                             
***      LA    R2,IOKEY                                                         
***      USING TLW4D,R2                                                         
***      XC    TLW4KEY,TLW4KEY                                                  
***      MVI   TLW4CD,TLW4CDQ                                                   
***      MVC   TLW4SSN,CSTSSN       SS NUMBER                                   
***      GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOTALDIR+IO1'                            
***      BE    FMTCM85                                                          
***      DC    H'0'                                                             
***                                                                             
***CM85  EQU   *                                                                
***      CLC   IOKEY(TLW4FID-TLW4KEY),IOKEYSAV   THRU SS NUMBER                 
***      BE    *+6                                                              
***      DC    H'0'        MUST FIND                                            
***      GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOTALFIL+IO1'                           
***      BE    *+6                                                              
***      DC    H'0'     DATAMGR ERROR                                           
***                                                                             
***      DROP  R2                                                               
***                                                                             
***      L     R2,IOADDR                                                        
***      USING TLW4D,R2                                                         
***      LA    R3,TLW4ELEM         POINT TO FIRST ELEMENT                       
***      USING TAW4D,R3            CAST DETAILS                                 
***CM87  CLI   0(R3),0             END OF REC?                                  
***      BNE   *+6                 NO DETAILS ELEMENT - DUMP                    
***      DC    H'0'                BAD RECORD                                   
***      CLI   0(R3),TAW4ELQ                                                    
***      BE    FMTCM87D                                                         
***      ZIC   R0,1(R3)                                                         
***      AR    R3,R0                                                            
***      B     FMTCM87                                                          
***                                                                             
***CM87D MVC   CSTNAM1,TAW4NAM1     FIRST NAME                                  
***      MVC   CSTNAM2,TAW4NAM2     LAST NAME                                   
***                                                                             
***CM89  EQU   *                                                                
***                                                                             
***      SET GUARANTEE VALUES LIKE A CONTRACT                                   
***                                                                             
***      MVC   CONTYPE(2),=C'GU'                                                
***      OC    CONTYPE,SPACES                                                   
***      MVC   CONSTART,CSTSDAT                                                 
***      MVC   CONEND,CSTEDAT                                                   
***                                                                             
***      SET NAME AND COMMENT IN THE DESCRIPTION FIELD                          
***      MVC   CONDESC(L'CSTNAM1),CSTNAM1  FIRST NAME                           
***      LA    R1,CONDESC+L'CSTNAM1                                             
***CM89C CLI   0(R1),C' '                                                       
***      BH    FMTCM89D                                                         
***      SH    R1,=H'1'                                                         
***      B     FMTCM89C                                                         
***                                                                             
***CM89D LA    R1,2(R1)                                                         
***      MVC   0(L'CSTNAM2,R1),CSTNAM2   LAST NAME                              
***                                                                             
***      LA    R1,CONDESC+(L'CSTNAM1+L'CSTNAM2+1)                               
***CM89E CLI   0(R1),C' '                                                       
***      BH    FMTCM89F                                                         
***      SH    R1,=H'1'                                                         
***      B     FMTCM89E                                                         
***                                                                             
***CM89F LA    R1,2(R1)                                                         
***      MVC   0(L'CSTCOMM,R1),CSTCOMM  COMMENT                                 
***                                                                             
***      LA    R6,CONTABL(R6)      BUMP TABLE ENTRY                             
***      LH    R0,CONTNUM          BUMP COUNTER                                 
***      AH    R0,=H'1'                                                         
***      STH   R0,CONTNUM                                                       
***                                                                             
***      B     NXTCAST                                                          
         EJECT                                                                  
**                                                                              
**      READ FOR HOLDING FEE RECORDS AND PROCESS LIKE CONTRACTS                 
**                                                                              
FMTCM90  EQU   *                                                                
*        FIRST READ FOR CAST MEMBERS FOR THIS COMMERCIAL                        
*                                                                               
         LA    R2,IOKEY                                                         
         USING TLINPD,R2                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,SAVCOMID                                                
         L     R1,=A(IOHI+IOTALDIR+IO8)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    FMTCM94                                                          
         DC    H'0'                                                             
                                                                                
FMTCM92  L     R1,=A(IOSQ+IOTALDIR+IO8)                                         
         GOTOR (#IOEXEC,AIOEXEC)                                                
                                                                                
FMTCM94  CLC   IOKEY(TLINHINV-TLINPKEY),IOKEYSAV THRU COMM ID                   
         BNE   FMTCMX           FINALLY DONE WITH COMMERCIAL                    
*                                                                               
         L     R1,=A(IOGET+IOTALFIL+IO8)                                        
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                CAN'T READ                                   
                                                                                
*                                                                               
         B     FMTCM95X            ALWAYS SKIP VERSION CHECKING                 
*                                  CHANGE REQUESTED 6/11/03                     
*                                                                               
**       CLI   CVCHAR,0            SEE IF PROCESSING A VERSION                  
**       BE    FMTCM95X            SKIP VERSION CHECKING                        
**                                                                              
**       L     R2,IOADDR                                                        
**       USING TLIND,R2                                                         
**       LA    R3,TLINELEM        POINT TO FIRST ELEMENT                        
**       USING TAVRD,R3                                                         
**TCM95  CLI   0(R3),0         END OF REC?                                      
**       BE    FMTCM92         NO MATCHING VERSION ELEMENT - SKIP               
**       CLI   0(R3),TAVRELQ                                                    
**       BE    FMTCM95D                                                         
**TCM95B ZIC   R0,1(R3)                                                         
**       AR    R3,R0                                                            
**       B     FMTCM95                                                          
**                                                                              
**TCM95D CLC   TAVRVERS,CVCHAR      VERSIONS MUST MATCH                         
**       BNE   FMTCM95B                                                         
**                                                                              
**       DROP  R2,R3                                                            
                                                                                
FMTCM95X L     R2,IOADDR                                                        
         USING TLIND,R2                                                         
         LA    R3,TLINELEM        POINT TO FIRST ELEMENT                        
         USING TAPDD,R3                                                         
FMTCM97  CLI   0(R3),0         END OF REC?                                      
         BE    FMTCM92         SKIP TO NEXT RECORD                              
         CLI   0(R3),TAPDELQ                                                    
         BE    FMTCM97D                                                         
FMTCM97B ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     FMTCM97                                                          
*                                                                               
FMTCM97D LA    RE,HLDTAB       SEE IF USE IS IN MY TABLE                        
         MVI   BYTE4,C'H'      SET USING HOLDING FEE TABLE                      
*                                                                               
FMTCM97E CLI   0(RE),0         END OF TABLE?                                    
         BE    FMTCM98         GO CHECK USE TABLE                               
         CLC   TAPDUSE(3),0(RE)   SEE IF HOLDING FEE                            
         BE    FMTCM97F         FOUND                                           
         LA    RE,25(RE)       SKIP TO NEXT TABLE ENTRY                         
         B     FMTCM97E                                                         
*                                                                               
*        CHECK START AND END VS. MY PERIOD                                      
*                                                                               
FMTCM97F DS    0H                                                               
         CLC   TAPDCYCS,END     SEE IF START AFTER MY END                       
         BH    FMTCM97B                                                         
         OC    TAPDCYCE,TAPDCYCE   BE SURE I HAVE AN END                        
         BZ    FMTCM97H                                                         
         CLC   TAPDCYCE,START   SEE IF END BEFORE MY START                      
         BL    FMTCM97B                                                         
                                                                                
FMTCM97H MVC   CONTYPE(3),=C'HLD' GET REPORTED AS HOLDING FEES                  
         MVC   CONSTART,TAPDCYCS                                                
         MVC   CONEND,TAPDCYCE                                                  
         MVC   CONAMT,TAPDGRS                                                   
         MVC   CONDESC(22),3(RE)  MOVE DESCRIPTION FROM HLDTAB                  
         CLI   USEVFLAG,C'Y'      CPC VERSION OVER 1.1.4?                       
         BNE   *+10                                                             
         MVC   CONDESC(25),0(RE)   IF SO - MOVE CODE + DESC                     
*                                                                               
         CLI   BYTE4,C'H'        FROM HOLDING FEE TABLE?                        
         BE    *+16                                                             
         MVC   CONTYPE(3),=C'USE'  MUST BE USE TABLE                            
         MVC   CONDESC(25),0(RE)   SEND CODE AND DESCRIPTION                    
*                                                                               
                                                                                
         LA    R6,CONTABL(R6)      BUMP TABLE ENTRY                             
         LH    R0,CONTNUM          BUMP COUNTER                                 
         AH    R0,=H'1'                                                         
         STH   R0,CONTNUM                                                       
         B     FMTCM92             CONTINUE LOOKING                             
*                                                                               
FMTCM98  CLI   BYTE4,C'U'          HAVE I CHECK USE TABLE?                      
         BE    FMTCM97B            YES - DONE WITH THIS ELEMENT                 
*                                                                               
         CLI   USEVFLAG,C'Y'       CPC VERSION OVER 1.1.4?                      
         BNE   FMTCM97B            NO - THEN DONE WITH THIS ELEMENT             
*                                                                               
         MVI   BYTE4,C'U'                                                       
         LA    RE,USETAB                                                        
         B     FMTCM97E                                                         
                                                                                
         DROP  R2,R3                                                            
                                                                                
FMTCMX   EQU   *                                                                
*        RESET IOKEY AND IOKEYSAV                                               
         MVC   IOADDR,SVIOADDR    RESTORE IOADDR                                
         MVC   IOKEY(L'MYIOKEYS),MYIOKEYS                                       
         J     EXITY              EXIT                                          
                                                                                
FMTCMNO  EQU   *                                                                
*        RESET IOKEY AND IOKEYSAV                                               
         MVC   IOADDR,SVIOADDR    RESTORE IOADDR                                
         MVC   IOKEY(L'MYIOKEYS),MYIOKEYS                                       
         J     EXITN              EXIT - SKIPPING THIS COMMERCIAL               
                                                                                
         LTORG                                                                  
GLOBALP  DS    XL1                                                              
SVPKEY   DS    CL38                                                             
SVIOADDR DS    A                                                                
SAVCOMID DS    XL4             INTERNAL COMMERCIAL ID                           
SAVVERSN DS    CL1             VERSION CODE                                     
*                                                                               
*   BELOW WAS CCONTS - NOT USED NOW                                             
*CONTS   DS    CL122           ROOM FOR 10 12 BYTE CONTRACTS                    
*                              FIRST BYTE IS THE NUMBER OF CONTRACTS            
*                              ONE EXTRA FOR END OF TABLE                       
         EJECT                                                                  
                                                                                
COMTAB   DS    0X             ** COMMERCIAL DRIVER TABLE **                     
*                                                                               
*        NOTE-USING TLCOPKEY LOOKS WRONG BUT IS NEEDED                          
*        ACTUAL KEYS BEING READ ARE THE VERSION LETTER                          
*        PASSIVE POINTERS (X'4D')                                               
*                                                                               
         DC    AL2(L'TLCOPKEY)                                                  
                                                                                
         DC    AL1(TLCOPCD-TLCOPKEY,L'TLCOCD-1)                                 
         DC    AL1(TLCOVRDQ),AL1(0)                                             
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(TLCOVAGY-TLCOPKEY,L'TLCOVAGY-1)                              
         DC    AL2(AGYCD-SAVED)                                                 
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(TLCOVCLI-TLCOPKEY,L'TLCOVCLI-1)                              
         DC    AL2(CLICD-SAVED)                                                 
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
*****    DC    AL1(TLCOVPRD-TLCOPKEY,L'TLCOVPRD-1)                              
*****    DC    AL2(PRDCD-SAVED)                                                 
*****    DC    AL1(LQ_TSINQ)                                                    
                                                                                
COMTABX  DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
PCOMTAB  DS    0X      ** COMMERCIAL DRIVER TABLE - ONE PRODUCT **              
*                                                                               
*        NOTE-USING TLCOPKEY LOOKS WRONG BUT IS NEEDED                          
*        ACTUAL KEYS BEING READ ARE THE VERSION LETTER                          
*        PASSIVE POINTERS (X'4D')                                               
*                                                                               
         DC    AL2(L'TLCOPKEY)                                                  
                                                                                
         DC    AL1(TLCOPCD-TLCOPKEY,L'TLCOCD-1)                                 
         DC    AL1(TLCOVRDQ),AL1(0)                                             
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(TLCOVAGY-TLCOPKEY,L'TLCOVAGY-1)                              
         DC    AL2(AGYCD-SAVED)                                                 
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(TLCOVCLI-TLCOPKEY,L'TLCOVCLI-1)                              
         DC    AL2(CLICD-SAVED)                                                 
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(TLCOVPRD-TLCOPKEY,L'TLCOVPRD-1)                              
         DC    AL2(PRDCD-SAVED)                                                 
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
PCOMTABX DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP                                                         *         
***********************************************************************         
                                                                                
REQ      DS    0X                                                               
                                                                                
REQINI   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQINIX+1-*)                                                 
         DC    AL2(I#INIDLD)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTINI-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
ST#STAFF EQU   1                                                                
         DC    AL2(D#STAFF)                                                     
         DC    CL5'STAFF'                                                       
         DC    AL1(ST#STAFF)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(STAFFC-SAVED)                                                
         DC    AL2(0)                                                           
         DC    AL1(L'STAFFC)                                                    
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#STAFF)                                       
         DC    XL4'00'                                                          
                                                                                
ST#STFPW EQU   2                                                                
         DC    AL2(D#STAFPW)                                                    
         DC    CL5'STFPW'                                                       
         DC    AL1(ST#STFPW)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(STAFFPW-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'STAFFPW)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#STFPW)                                       
         DC    XL4'00'                                                          
                                                                                
REQINIX  DC    AL1(LD_EOTQ)                                                     
                                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST FOR COMMERCIAL DOWNLOAD                                               
***********************************************************************         
REQCOM   DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQCOMX+1-*)                                                 
         DC    AL2(I#COMDLD)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTCOM-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
CM#AGY   EQU   1                                                                
         DC    AL2(D#AGY)                                                       
         DC    CL5'AGY  '                                                       
         DC    AL1(CM#AGY)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(AGYCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'AGYCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#AGYCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#MED   EQU   2                                                                
         DC    AL2(D#MED)                                                       
         DC    CL5'MEDIA'                                                       
         DC    AL1(CM#MED)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(MEDCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'MEDCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#MEDCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#CLI   EQU   3                                                                
         DC    AL2(D#CLI)                                                       
         DC    CL5'CLI  '                                                       
         DC    AL1(CM#CLI)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(CLICD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'CLICD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#CLICD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#CTY   EQU   4                                                                
         DC    AL2(D#COMTYP)                                                    
         DC    CL5'COMTY'                                                       
         DC    AL1(CM#CTY)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(CTYCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'CTYCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#CTYCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#PRD   EQU   5                                                                
         DC    AL2(D#PRD)                                                       
         DC    CL5'PRDCD'                                                       
         DC    AL1(CM#PRD)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PRDCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'PRDCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#PRDCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#CGP   EQU   6                                                                
         DC    AL2(D#COMMG)                                                     
         DC    CL5'CGPCD'                                                       
         DC    AL1(CM#CGP)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(CGPCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'CGPCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#CGPCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#BEB   EQU   7                                                                
         DC    AL2(D#BVEB)                                                      
         DC    CL5'BEB  '                                                       
         DC    AL1(CM#BEB)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(BEBCD-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'BEBCD)                                                     
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#BEBCD)                                       
         DC    XL4'00'                                                          
                                                                                
CM#SDT   EQU   8                                                                
         DC    AL2(D#START)                                                     
         DC    CL5'START'                                                       
         DC    AL1(CM#SDT)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(START-SAVED)                                                 
         DC    AL2(0)                                                           
         DC    AL1(L'START)                                                     
         DC    AL1(LD_PDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#START)                                       
         DC    XL4'00'                                                          
                                                                                
CM#EDT   EQU   9                                                                
         DC    AL2(D#END)                                                       
         DC    CL5'END  '                                                       
         DC    AL1(CM#EDT)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(END-SAVED)                                                   
         DC    AL2(0)                                                           
         DC    AL1(L'END)                                                       
         DC    AL1(LD_PDATQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#END)                                         
         DC    XL4'00'                                                          
                                                                                
CM#HIS   EQU   11                                                               
         DC    AL2(D#CHIST)                                                     
         DC    CL5'HIST '                                                       
         DC    AL1(CM#HIS)                                                      
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(HIST-SAVED)                                                  
         DC    AL2(0)                                                           
         DC    AL1(L'HIST)                                                      
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(TALSYSQ),AL2(TA#CHIS)                                        
         DC    XL4'00'                                                          
                                                                                
REQCOMX  DC    AL1(LD_EOTQ)                                                     
                                                                                
REQX     DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - INITIAL DOWNLOAD                                       *         
***********************************************************************         
                                                                                
OUTINI   DS    0X                                                               
                                                                                
         DC    AL2(OUTINIX-*)                                                   
         DC    AL2(1)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
*                                                                               
*        USER STUFF NO-OPED                                                     
*                                                                               
**ER     DC    AL2(USERX-*)                                                     
**       DC    AL2(1),C'UserV'                                                  
**       DC    AL1(0,0,0)                                                       
**       DC    XL4'00'                                                          
**                                                                              
**       DC    AL2(1)                                                           
**       DC    CL5'UsrID'                                                       
**       DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
**       DC    AL2(USIDNAME-SAVED),AL1(LD_CHARQ,L'USIDNAME)                     
**       DC    XL4'00'                                                          
**                                                                              
**ERX    DS    0X                                                               
                                                                                
STAFF    DC    AL2(STAFFX-*)                                                    
         DC    AL2(E#STAFF),C'STAFF'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#STAFFN)                                                    
         DC    CL5'Sname'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(STAFFN-SAVED),AL1(LD_CHARQ,L'STAFFN)                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#STAFAN)                                                    
         DC    CL5'Sagyn'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(STAFFAND-SAVED),AL1(LD_UBINQ,L'STAFFAND)                     
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#STAFFA)                                                    
         DC    CL5'Sagya'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(STAFFAAD-SAVED),AL1(LD_CHARQ,L'STAFFAAD)                     
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#STAFCN)                                                    
         DC    CL5'Sclin'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(STAFFCND-SAVED),AL1(LD_UBINQ,L'STAFFCND)                     
         DC    XL4'00'                                                          
                                                                                
                                                                                
         DC    AL2(D#STAFFC)                                                    
         DC    CL5'Sclia'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(STAFFCAD-SAVED),AL1(LD_CHARQ,L'STAFFCAD)                     
         DC    XL4'00'                                                          
                                                                                
STAFFX   DS    0X                                                               
                                                                                
COMT     DC    AL2(COMTX-*)                                                     
         DC    AL2(E#COMT),C'ARRAY'                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#COMT)                                                      
         DC    CL5'DLCTY'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYCOMM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
COMTX    DS    0X                                                               
OUTINIX  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMERCIAL DOWNLOAD                                    *         
***********************************************************************         
                                                                                
OUTCOM   DS    0X                                                               
                                                                                
         DC    AL2(OUTCOMX-*)                                                   
         DC    AL2(1)                                                           
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
COMM     DC    AL2(COMMX-*)               COMM                                  
         DC    AL2(E#ACPCG),C'COMM '                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#AGYN)                                                      
         DC    CL5'Aname'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AGYN-SAVED),AL1(LD_CHARQ,L'AGYN)                             
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CLIN)                                                      
         DC    CL5'Cname'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(CLIN-SAVED),AL1(LD_CHARQ,L'CLIN)                             
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PRDN)                                                      
         DC    CL5'Pname'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(PRDN-SAVED),AL1(LD_CHARQ,L'PRDN)                             
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#COMMGN)                                                    
         DC    CL5'Gname'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(CGPN-SAVED),AL1(LD_CHARQ,L'CGPN)                             
         DC    XL4'00'                                                          
                                                                                
COMMX    DS    0X                                                               
                                                                                
COMMD    DC    AL2(COMMDX-*)                                                    
         DC    AL2(E#COMM),C'Array'                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(E#COMM)                                                      
         DC    CL5'COMMD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYCOM-SVRDEF),AL1(0,0)                                      
         DC    XL4'00'                                                          
                                                                                
COMMDX   DS    0X                                                               
                                                                                
                                                                                
OUTCOMX  DS    0X                                                               
         EJECT                                                                  
*********************************************************************           
* ARRAY DEFINTITION FOR COMMERCIAL TYPE DOWNLOAD                                
*********************************************************************           
ARYCOMM  DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO6-WORKD)                                                  
         DC    AL2(COMTNUM-SAVED)                                               
         DC    AL2(COMTTABL,0)                                                  
         DC    AL1(COMTCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
COMTCOL  DS    0X                                                               
                                                                                
         DC    AL2(D#COMTYP),C'CMTcd'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(COMTCODE-COMTTABD),AL1(L'COMTCODE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#COMTYD),C'CMTds'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(COMTDESC-COMTTABD),AL1(L'COMTDESC)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
COMTCOLN EQU   (*-COMTCOL)/LX_COLSL                                             
*                                                                               
         EJECT                                                                  
********************************************************************            
*  ARRAY DEFINITION FOR COMMERCIAL DOWNLOAD                                     
********************************************************************            
                                                                                
ARYCOM   DS    0X                                                               
                                                                                
         DC    AL1(LX_IRTNQ+LX_IRBRQ,0,0)                                       
         DC    AL2(NXTCOM-SVRDEF)                                               
         DC    AL2(0)                                                           
         DC    AL2(0,0)                                                         
         DC    AL1(COMCOLN)                                                     
         DC    XL4'00'                                                          
                                                                                
COMCOL   DS    0X                                                               
         DC    AL2(E#COMM),C'Array'                                             
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYCOMV-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
COMCOLN  EQU   (*-COMCOL)/LX_COLSL                                              
         EJECT                                                                  
********************************************************************            
*  ARRAY DEFINITION FOR COMMERCIAL VALUES DOWNLOAD                              
********************************************************************            
                                                                                
ARYCOMV  DS    0X                                                               
                                                                                
         DC    AL1(B#SAVED+LX_INELQ,0,0)                                        
         DC    AL2(COMVALS-SAVED)                                               
         DC    AL2(1)                  SINGLE ROW                               
         DC    AL2(0,0)                                                         
         DC    AL1(COMVCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
COMVCOL  DS    0X                                                               
**       DC    AL2(D#AGYN),C'Aname'                                             
**       DC    AL1(B#SAVED,0,0)                                                 
**       DC    AL2(AGYN-SAVED),AL1(L'AGYN)                                      
**       DC    AL2(0)                                                           
**       DC    AL1(LD_CHARQ,0)                                                  
**       DC    XL4'00'                                                          
**                                                                              
**       DC    AL2(D#CLIN),C'Cname'                                             
**       DC    AL1(B#SAVED,0,0)                                                 
**       DC    AL2(CLIN-SAVED),AL1(L'CLIN)                                      
**       DC    AL2(0)                                                           
**       DC    AL1(LD_CHARQ,0)                                                  
**       DC    XL4'00'                                                          
**                                                                              
**       DC    AL2(D#PRDN),C'Pname'                                             
**       DC    AL1(B#SAVED,0,0)                                                 
**       DC    AL2(PRDN-SAVED),AL1(L'PRDN)                                      
**       DC    AL2(0)                                                           
**       DC    AL1(LD_CHARQ,0)                                                  
**       DC    XL4'00'                                                          
**                                                                              
**       DC    AL2(D#COMMGN),C'Gname'                                           
**       DC    AL1(B#SAVED,0,0)                                                 
**       DC    AL2(CGPN-SAVED),AL1(L'CGPN)                                      
**       DC    AL2(0)                                                           
**       DC    AL1(LD_CHARQ,0)                                                  
**       DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CISCII),C'ISCII'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CISCII-SAVED),AL1(L'CISCII)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CTITLE),C'TITLE'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CTITLE-SAVED),AL1(L'CTITLE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CLEN),C'LEN  '                                             
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CLEN-SAVED),AL1(L'CLEN)                                      
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CVCODE),C'VCODE'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CVCODE-SAVED),AL1(L'CVCODE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PRD),C'PRD  '                                              
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPRD-SAVED),AL1(L'CPRD)                                      
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#PRDN),C'P NAM'                                             
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CPRDN-SAVED),AL1(L'CPRDN)                                    
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CEXDTE),C'EXDTE'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CEXDTE-SAVED),AL1(L'CEXDTE)                                  
         DC    AL2(0)                                                           
         DC    AL1(LD_PDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CINDTE),C'INDTE'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(CINVDTE-SAVED),AL1(L'CINVDTE)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_PDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(E#CONT),C'ARRAY'                                             
         DC    AL1(0,LX_CIARR,0)                                                
         DC    AL2(0),AL1(0)                                                    
         DC    AL2(0)                                                           
         DC    AL2(ARYCON-SVRDEF)                                               
         DC    XL4'00'                                                          
                                                                                
COMVCOLN EQU   (*-COMVCOL)/LX_COLSL                                             
         EJECT                                                                  
*********************************************************************           
* ARRAY DEFINTITION FOR COMMERCIAL CONTRACTS                                    
*********************************************************************           
ARYCON   DS    0X                                                               
         DC    AL1(LX_IADDQ+B#WORKD+LX_INELQ,LX_IRADR,B#SAVED+LX_INERQ)         
         DC    AL2(AIO5-WORKD)                                                  
         DC    AL2(CONTNUM-SAVED)                                               
         DC    AL2(CONTABL,0)                                                   
         DC    AL1(CONTCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
CONTCOL  DS    0X                                                               
                                                                                
         DC    AL2(D#CNTYPE),C'CNtyp'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(CONTYPE-CONTABD),AL1(L'CONTYPE)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CNDESC),C'CNdes'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(CONDESC-CONTABD),AL1(L'CONDESC)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CNSDTE),C'CNsdt'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(CONSTART-CONTABD),AL1(L'CONSTART)                            
         DC    AL2(0)                                                           
         DC    AL1(LD_PDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CNEDTE),C'CNedt'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(CONEND-CONTABD),AL1(L'CONEND)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_PDATQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(D#CNAMT),C'CNamt'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(CONAMT-CONTABD),AL1(L'CONAMT)                                
         DC    AL2(0)                                                           
         DC    AL1(LD_CBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
CONTCOLN EQU   (*-CONTCOL)/LX_COLSL                                             
*                                                                               
         EJECT                                                                  
                                                                                
         EJECT                                                                  
COMTTAB  DS    0C    TABLE ON COMMERCIAL TYPES AND DESCRIPTIONS                 
         DC    C'A',CL12'ADDENDUM'                                              
         DC    C'B',CL12'PUBLIC SERVICE'                                        
*****    DC    C'C',CL12'CANCELLATION'                                          
*****    DC    C'D',CL12'DEMO'                                                  
         DC    C'E',CL12'SEASONAL'                                              
         DC    C'F',CL12'FOREIGN'                                               
*****    DC    C'G',CL12'GUARANTEE'                                             
*****    DC    C'I',CL12'INDUSTRIAL'                                            
*****    DC    C'M',CL12'MUSIC'                                                 
         DC    C'N',CL12'ANIMATICS'                                             
*****    DC    C'O',CL12'PRINT'                                                 
*****    DC    C'P',CL12'SOAP'                                                  
         DC    C'R',CL12'PROMO'                                                 
         DC    C'S',CL12'SPANISH'                                               
         DC    C'T',CL12'SHORT TERM'                                            
*****    DC    C'U',CL12'AUDITION'                                              
         DC    C'X',CL12'ASIAN'                                                 
         DC    C'7',CL12'COMM.TYPE NAME'                                        
*                                                                               
COMTTABX DC    XL13'00'             END OF TABLE                                
*                                                                               
CMTABL   EQU   *-COMTTAB  TABLE LENGHT (INCLUDES 13 X'00' AT END)               
*                                                                               
COMTTABN EQU   (COMTTABX-COMTTAB)/13    NUMBER OF TABLE ENTRIES                 
*                                                                               
         EJECT                                                                  
*        ALL THE BELOW WILL BE REPORTED AS HOLDING FEES                         
*        (HLD)  WITH THEIR DESCRIPTIONS                                         
*                                                                               
HLDTAB   DC    C'ADH',CL22'ADDENDUM HOLDING FEE'                                
         DC    C'ALF',CL22'ADDENDUM LIFT'  '                                    
         DC    C'BSC',CL22'ACTRA SESSION'                                       
         DC    C'BSS',CL22'SESSION'                                             
         DC    C'HLD',CL22'HOLDING FEE'                                         
         DC    C'LFT',CL22'LIFT'                                                
         DC    C'SCS',CL22'SPANISH CAB SESSION'                                 
         DC    C'SHL',CL22'SPANISH HOLDING FEE'                                 
         DC    C'SLF',CL22'SPANISH LIFT'                                        
         DC    C'SSS',CL22'SPANISH SESSION'                                     
         DC    X'000000'     END OF TABLE                                       
*                                                                               
*        TABLE OF NON-HOLDING FEE USES                                          
*                                                                               
USETAB   DC    C'GRT',CL22'GUARANTEE'                                           
         DC    C'BSU',CL22'UDA SESSION'                                         
         DC    C'CSS',CL22'CREW SESSION'                                        
         DC    C'PRT',CL22'PRINT'                                               
         DC    C'PRS',CL22'PRINT SESSION'                                       
         DC    C'DEM',CL22'DEMO/NON-AIR'                                        
         DC    C'ADD',CL22'ADDENDUM DEMO/NON-AIR'                               
         DC    C'SNA',CL22'SPAN DEM/NON-AIR'                                    
         DC    C'CDM',CL22'DEMO NON-AIR'                                        
         DC    C'CAU',CL22'AUDITION'                                            
         DC    C'RRS',CL22'RERECORD SESSION'                                    
         DC    C'ARS',CL22'AD REREC SESSION'                                    
         DC    C'SRS',CL22'SPANISH RERECORD'                                    
         DC    C'RRR',CL22'RADIO RERECORD'                                      
         DC    C'ARR',CL22'AD RADIO RERECORD'                                   
         DC    C'CLA',CL22'CLASS A'                                             
         DC    C'PAX',CL22'PAX'                                                 
         DC    C'ITN',CL22'ITN NETWORK'                                         
         DC    C'LNA',CL22'LATE NIGHT ABC'                                      
         DC    C'LNN',CL22'LATE NIGHT NBC'                                      
         DC    C'LNC',CL22'LATE NIGHT CBS'                                      
         DC    C'SNT',CL22'SPANISH NETWORK USE'                                 
         DC    C'NET',CL22'ACTRA/UDA TV NWK'                                    
         DC    C'LOC',CL22'LOCAL'                                               
         DC    C'DLR',CL22'DEALER'                                              
         DC    C'WSP',CL22'WILDSPOT'                                            
         DC    C'SWS',CL22'SPANISH WILDSPOT'                                    
         DC    C'IFB',CL22'INSERT FOR BOOKENDS'                                 
         DC    C'SWU',CL22'SPANISH WILDSPT UPG'                                 
         DC    C'SNW',CL22'SPANISH NWK/WSP'                                     
         DC    C'SNU',CL22'SPANISH NWK/WSP UPG'                                 
         DC    C'ADW',CL22'ADDENDUM WILDSPOT'                                   
         DC    C'ADC',CL22'ADDENDUM CMB SESS/WSP'                               
         DC    C'WSC',CL22'WILDSPOT'                                            
         DC    C'WSM',CL22'WILDSPOT COMBINED'                                   
         DC    C'CAB',CL22'CABLE'                                               
         DC    C'CBL',CL22'CABLE'                                               
         DC    C'SCB',CL22'SPANISH CABLE REUSE'                                 
         DC    C'LCB',CL22'LOCAL CABLE'                                         
         DC    C'ACB',CL22'ADDENDUM CABLE'                                      
         DC    C'RNT',CL22'RADIO NETWORK'                                       
         DC    C'BSM',CL22'MUSIC SESSION'                                       
         DC    C'IMS',CL22'INDUSTRIAL MUSIC SESS'                               
         DC    C'CMS',CL22'MUSIC SESSION'                                       
         DC    C'MUS',CL22'MUSIC'                                               
         DC    C'MRR',CL22'AFM MULT'                                            
         DC    C'NBM',CL22'NON-BRD MUS'                                         
         DC    C'FMU',CL22'FIRST MUSIC'                                         
         DC    C'SMU',CL22'SPAN AFM REU/DUB'                                    
         DC    C'CMR',CL22'AFM REUSE'                                           
         DC    C'FGN',CL22'FOREIGN'                                             
         DC    C'RLO',CL22'RADIO LOCAL'                                         
         DC    C'ULM',CL22'UDA LCL MONTREAL'                                    
         DC    C'OTH',CL22'OTHER'                                               
         DC    C'INA',CL22'INTERACTIVE USE'                                     
         DC    C'IRN',CL22'INTERNET USE'                                        
         DC    C'ISS',CL22'INTERACTIVE SESSION'                                 
         DC    C'PRM',CL22'PROMO SESSION'                                       
         DC    C'PRR',CL22'PROMO USE'                                           
         DC    C'INF',CL22'INFOMERCIAL USE'                                     
         DC    C'IFS',CL22'INFOMERCIAL SESSION'                                 
         DC    C'PNH',CL22'PENSION && HEALTH'                                   
         DC    C'SIR',CL22'SPANISH INTERNET'                                    
         DC    C'SOT',CL22'SPANISH OTHER'                                       
         DC    C'OTM',CL22'OTHER MUSIC'                                         
         DC    C'OTC',CL22'OTHER'                                               
         DC    C'DOR',CL22'ACTRA DRMNCY FEE'                                    
         DC    C'FGM',CL22'AFM FOREIGN'                                         
         DC    C'RGM',CL22'AFM REGIONAL USE'                                    
         DC    C'BSR',CL22'RADIO SESSION'                                       
         DC    C'GRR',CL22'RADIO GUARANTEE'                                     
         DC    C'RRN',CL22'RADIO REG NWK'                                       
         DC    C'PEN',CL22'PENALTY'                                             
         DC    C'SPN',CL22'SPANISH PENALTY'                                     
         DC    C'CPN',CL22'PENALTY'                                             
         DC    C'PEM',CL22'MUSIC PENALTY'                                       
         DC    C'TAG',CL22'TAG'                                                 
         DC    C'VNM',CL22'VARIATION N/W'                                       
         DC    C'VAR',CL22'VARIATION/INSERT'                                    
         DC    C'ADS',CL22'ADDENDUM SESSION'                                    
         DC    C'ADT',CL22'ADDENDUM SESSION'                                    
         DC    C'ADO',CL22'ADDENDUM SESSION'                                    
         DC    C'AUD',CL22'AUDITION'                                            
         DC    C'ADR',CL22'ADDENDUM REUSE'                                      
         DC    C'ARN',CL22'ADDENDUM REINSTATEMENT'     ADDENDUM                 
         DC    C'NBS',CL22'NON-BROADCAST SESSION'                               
         DC    C'REN',CL22'REINSTATEMENT'                                       
         DC    C'SRE',CL22'SPANISH REINSTATEMENT'                               
         DC    C'DWN',CL22'DOWNGRADE'                                           
         DC    C'CNL',CL22'CANCEL FEE'                                          
         DC    C'SCN',CL22'SPANISH CANCEL FEE'                                  
         DC    C'PPF',CL22'POSTPONEMENT FEE'                                    
         DC    C'INS',CL22'THEAT/INDST SESSION'                                 
         DC    C'FGS',CL22'FOREIGN SESSION'                                     
         DC    C'SFS',CL22'SPANISH FGN SESSION'                                 
         DC    C'PUB',CL22'PUBLIC SERV SESSION'                                 
         DC    C'PBS',CL22'PUBLIC SERV REUSE'                                   
         DC    C'INR',CL22'INDUSTRIAL REUSE'                                    
         DC    C'SIN',CL22'SPANISH INDUS REUSE'                                 
         DC    C'FGR',CL22'FOREIGN REUSE'                                       
         DC    C'SFR',CL22'SPANISH FOREIGN REUSE'                               
         DC    C'RET',CL22'TV RETAIL USE'                                       
         DC    C'SOP',CL22'SOAP'                                                
         DC    C'SOR',CL22'SOAP RES.'                                           
         DC    C'SRH',CL22'SOAP .5HR'                                           
         DC    C'SOC',CL22'SOAP CABLE RES.'                                     
         DC    C'SON',CL22'SOAP NEW MEDIA RES.'                                 
         DC    C'SDR',CL22'SOAP DIRECTOR'                                       
         DC    C'SDH',CL22'SOAP DIRECTOR .5HR'                                  
         DC    C'SDC',CL22'SOAP DIRECTR CBL'                                    
         DC    C'SDN',CL22'SOAP DIRECTOR NM'                                    
         DC    C'SWR',CL22'SOAP WRITER'                                         
         DC    C'SWH',CL22'SOAP WRITER .5HR'                                    
         DC    C'SWC',CL22'SOAP WRITER CBL'                                     
         DC    C'SWN',CL22'SOAP WRITERS NM'                                     
         DC    C'PNP',CL22'PRIME/NON PRIME'                                     
         DC    C'PRG',CL22'UDA PROGRAM USE'                                     
         DC    C'PRL',CL22'UDA PROLONG FEE'                                     
         DC    C'RAD',CL22'ACTRA RADIO REUSE'                                   
         DC    C'CNM',CL22'ACTRA NEW MEDIA'                                     
         DC    X'000000'     END OF USE TABLE                                   
*                                                                               
COMTTABD DSECT                                                                  
COMTCODE DS    CL1               COMMERCIAL TYPE CODE                           
COMTDESC DS    CL12              TYPE DESCRIPTION                               
COMTTABL EQU   *-COMTTABD                                                       
*                                                                               
CONTABD  DSECT                                                                  
CONTYPE  DS    CL3    CONTRACT TYPE  - TARDTYP1  (TARDD ELEMENT X'BE')          
CONDESC  DS    CL102  DESCRIPTION    - TAFNNAME  (TAFND ELEMENT X'26')          
*                                      MAY BEGIN WITH CAST NAME FROM            
*                                      TAW4NAM1 AND 2                           
*                                      CURRENT MAX LENGTH WOULD BE 100          
CONSTART DS    XL3    START DATE       IN KEY                                   
CONEND   DS    XL3    END DATE         IN KEY                                   
CONAMT   DS    XL4    AMOUNT           ?                                        
CONTABL  EQU   *-CONTABD                                                        
*                                                                               
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
STAFFC   DS    CL8                 STAFF CODE                                   
STAFFPW  DS    CL8                 STAFF PASSWORD                               
STAFFN   DS    CL25                LAST NAME FLOATED AFTER FIRST ?              
STAFFAND DS    XL1                 NUMBER OF LIMIT ACCESS AGENCIES              
STAFFAAD DS    CL100               AGENCY LIMIT ACCESS LIST                     
STAFFCND DS    XL1                 NUMBER OF LIMIT ACCESS CLIENTS               
STAFFCAD DS    CL100               CLIENT LIMIT ACCESS LIST                     
*        NOTE - STAFF LIMIT ACCESS VALUES ALSO SAVED IN TWA                     
*               SEE TALNKWRK                                                    
*                                                                               
WVALUES  DS    0D                                                               
OADCONS  DS    0A                                                               
ACOMTAB  DS    A                                                                
APCOMTAB DS    A                                                                
LASTAGY  DS    CL6                                                              
OADCONSN EQU   (*-OADCONS)/L'OADCONS                                            
                                                                                
DMKEY    DS    CL8                                                              
TALDIR   DS    CL8                                                              
TALFIL   DS    CL8                                                              
FFFS     DS    XL4                                                              
                                                                                
WVALUESL EQU   *-WVALUES                                                        
                                                                                
MAP#     DS    XL2                                                              
COMTNUM  DS    H                 NUMBER OF COMMERCIAL TYPES                     
CONTNUM  DS    H                 NUMBER OF COMMERCIAL CONTRACTS                 
USEVFLAG DS    CL1                                                              
USIDNAME DS    CL(L'CTORGNAM)      USER-ID NAME                                 
MYIOKEYS DS    CL76           38 X 2                                            
                                                                                
REQVALS  DS    0H                                                               
*                                                                               
AGYCD    DS    CL6                                                              
MEDCD    DS    CL1                                                              
CLICD    DS    CL6                                                              
CTYCD    DS    CL1                                                              
PRDCD    DS    CL6                                                              
CGPCD    DS    CL6                                                              
BEBCD    DS    CL1                                                              
START    DS    CL3                                                              
END      DS    CL3                                                              
HIST     DS    CL1                                                              
                                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
COMVALS  DS    0X                                                               
*                         NAMES RETURNED FOR REQUEST                            
AGYN     DS    CL36                                                             
CLIN     DS    CL36                                                             
PRDN     DS    CL36                                                             
CGPN     DS    CL36                                                             
*                                                                               
COMVALS1 DS    0X                                                               
*        DATA FROM THE COMMERCIAL RECORD                                        
*                                                                               
CISCII   DS    CL12       TACOCID (TACOD ELEMENT - X'72')                       
*                         NOTE - FIELD ABOVE WILL BE VERSION'S ISCII            
*                         WHEN PROCESSING A VERSION                             
CTITLE   DS    CL36                                                             
CLEN     DS    CL3        TACOSEC  (TACOD ELEMENT - X'72')                      
CVCHAR   DS    CL1        VERSION LETTER                                        
CVCODE   DS    CL12       BASIC ISCII - FOR VERSIONS                            
CPRD     DS    CL6        TAPRPRD  (TAPRD ELEMENT - X'38')                      
CPRDN    DS    CL36       TAFNNAME  (TAFND - X'26' - NAME TYPE P)               
CEXDTE   DS    XL3        TACOEXP   (TACOD ELEMENT - X'72')                     
CINVDTE  DS    XL3        TACOINAC  (TACOD ELEMENT - X'72')                     
*                                                                               
COMVALSL EQU   *-COMVALS1                                                       
                                                                                
CSTVALS  EQU   *          CAST VALUES                                           
CSTVL    DS    CL36       VERSION LETTERS                                       
CSTGUA   DS    CL4        GUARANTEE CODE                                        
CSTSSN   DS    CL9        SS NUMBER                                             
CSTSDAT  DS    XL3        START DATE                                            
CSTEDAT  DS    XL3        END DATE                                              
CSTNAM1  DS    CL16       FIRST NAME                                            
CSTNAM2  DS    CL16       LAST NAME                                             
CSTCOMM  DS    CL146      COMMENT (180-34(FOR NAMES))                           
CSTVALSL EQU   *-CSTVALS                                                        
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE CTGENFILE                                                      
                                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TALNK10   06/17/15'                                      
         END                                                                    
