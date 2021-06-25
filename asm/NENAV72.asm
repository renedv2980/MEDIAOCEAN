*          DATA SET NENAV72    AT LEVEL 003 AS OF 03/16/18                      
*PHASE T31872A                                                                  
NENAV72  TITLE '- Network Navigator - server support routines 2'                
NENAV72  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NN72**                                                       
                                                                                
         USING WORKD,R9            R9=A(global work area)                       
         L     R7,ALP                                                           
         USING LP_D,R7             R7=A(LP_D)                                   
         L     RA,LP_ATWA                                                       
         USING TWAD,RA             RA=A(TWA)                                    
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(Global literals)                        
         SR    RE,RE                                                            
         SLDL  RE,8                Branch index passed in hob of RF             
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          Ensure good index value                      
         JL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         JNZ   *+6                                                              
         DC    H'0'                Routine not defined                          
         AR    RF,RB               RF=A(routine)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=temporary w/s amount                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                Round amount to doublewords                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               Acquire storage from w/s pool                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               and clear it                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
ROUTAB   DS    0XL4                                                             
         DC    AL2(GETAGY-NENAV72),AL2(0)                                       
         DC    AL2(VALMED-NENAV72),AL2(0)                                       
         DC    AL2(EDTMED-NENAV72),AL2(0)                                       
         DC    AL2(VALCLT-NENAV72),AL2(0)                                       
         DC    AL2(GETCLT-NENAV72),AL2(GCWORKL)                                 
         DC    AL2(LIMCLT-NENAV72),AL2(LCWORKL)                                 
         DC    AL2(EDTCLT-NENAV72),AL2(0)                                       
         DC    AL2(VALDPT-NENAV72),AL2(0)                                       
         DC    AL2(VALPRD-NENAV72),AL2(0)                                       
         DC    AL2(SETCKS-NENAV72),AL2(0)                                       
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
***********************************************************************         
* Get agency record - R1=A(agency alpha id)                           *         
***********************************************************************         
                                                                                
GETAGY   J     *+12                                                             
         DC    C'*GETAGY*'                                                      
         USING AGYRECD,IOKEY                                                    
         USING AGYKEY,IOKEY                                                     
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,0(R1)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#AGYREC'                        
         JNE   EXIT                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#AGYREC'                       
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Validate media code                                                 *         
***********************************************************************         
                                                                                
VALMED   J     *+12                                                             
         DC    C'*VALMED*'                                                      
         LM    R2,R4,0(R1)                                                      
         MVC   QMEDA,0(R2)                                                      
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         LA    R5,AGYEL                                                         
         USING AGYMEDEL,R5                                                      
         SR    R0,R0                                                            
VALMED02 CLI   AGYMEDEL,0          Test end of agency record                    
         JE    EXITN                                                            
         CLI   AGYMEDEL,AGYMEDEQ   Test media element                           
         JNE   *+14                                                             
         CLC   AGYMEDCD,0(R2)      Match on media code                          
         JE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R5,R0                                                            
         J     VALMED02                                                         
         MVC   0(L'AGYMEDBT,R4),AGYMEDBT                                        
         MVC   QMEDX,AGYMEDBT      Set agency/media number                      
         J     EXITY                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* Edit media code                                                     *         
***********************************************************************         
                                                                                
EDTMED   J     *+12                                                             
         DC    C'*EDTMED*'                                                      
         LM    R2,R4,0(R1)                                                      
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         LA    R5,AGYEL                                                         
         USING AGYMEDEL,R5                                                      
         SR    R0,R0                                                            
EDTMED02 CLI   AGYMEDEL,0          Test end of agency record                    
         JE    EXITN                                                            
         CLI   AGYMEDEL,AGYMEDEQ   Test media element                           
         JNE   *+14                                                             
         CLC   AGYMEDBT,0(R2)      Match on agency/media number                 
         JE    *+14                                                             
         IC    R0,AGYMEDLN                                                      
         AR    R5,R0                                                            
         J     EDTMED02                                                         
         MVC   0(L'AGYMEDCD,R4),AGYMEDCD                                        
         J     EXITY                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* Validate client code                                                *         
***********************************************************************         
                                                                                
VALCLT   J     *+12                                                             
         DC    C'*VALCLT*'                                                      
         LM    R2,R4,0(R1)                                                      
         MVC   QCLTA,0(R2)                                                      
         CHI   R3,2                                                             
         JL    EXITN                                                            
         JH    *+8                                                              
         MVI   QCLTA+2,C' '                                                     
         GOTOR VCLPACK,DMCB,QCLTA,(R4)                                          
         CLI   0(R1),FF                                                         
         JE    EXITN                                                            
         MVC   QCLTX,0(R4)                                                      
         GOTOR (#GETCLT,AGETCLT)                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get client record QMEDX=agency/media, QCLTX=client                  *         
***********************************************************************         
                                                                                
GETCLT   J     *+12                                                             
         DC    C'*GETCLT*'                                                      
         USING GCWORKD,RC                                                       
         MVC   GCIOSAVE,IOVALS                                                  
         LA    R3,IOKEY                                                         
         USING CLTRECD,R3                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
         MVC   CKEYAM,QMEDX                                                     
         MVC   CKEYCLT,QCLTX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+#CLTREC'                        
         JNE   GETCLTX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+#CLTREC'                       
         JNE   GETCLTX                                                          
         GOTOR (#LIMCLT,ALIMCLT)   Apply client limit access                    
                                                                                
GETCLTX  MVC   IOVALS(IOVALL),GCIOSAVE                                          
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
GCWORKD  DSECT                     ** GETCLT s/r local w/s **                   
GCIOSAVE DS    XL(IOVALL)          Saved I/O values                             
GCWORKL  EQU   *-GCWORKD                                                        
NENAV72  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Apply client limit access                                           *         
***********************************************************************         
                                                                                
LIMCLT   J     *+12                                                             
         DC    C'*LIMCLT*'                                                      
         USING LCWORKD,RC                                                       
         USING OFFICED,LCOFFBLK    OFFICER control block                        
         L     R3,ACLTREC                                                       
         USING CLTRECD,R3          R3=A(client record)                          
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),QCLTA                             
         MVI   OFCSYS,NETLETQ                                                   
         MVC   OFCAGY,LP_AGY                                                    
         MVC   OFCAUTH,LP_ACCS                                                  
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLTA                                                     
         MVC   OFCSAGMD,CKEYAM                                                  
         MVC   OFCLMT,LP_ACCS                                                   
         MVC   OFCSECD,LP_ASECD                                                 
         MVC   OFCACCSC(L'CACCESS),CACCESS                                      
         MVI   OFCACCSM,FF                                                      
         MVI   OFCINDS,OFCI2CSC                                                 
         MVC   OFCCLT2,CKEYCLT                                                  
         GOTOR VOFFICER,DMCB,(C'N',OFFICED),ACOMFACS                            
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
LCWORKD  DSECT                     ** LIMCLT s/r local w/s **                   
LCOFFBLK DS    XL(OFCLENQ)         Officer control block                        
LCWORKL  EQU   *-LCWORKD                                                        
NENAV72  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Edit client code                                                    *         
***********************************************************************         
                                                                                
EDTCLT   J     *+12                                                             
         DC    C'*EDTCLT*'                                                      
         LM    R2,R4,0(R1)                                                      
         OC    0(L'CKEYCLT,R2),0(R2)                                            
         JNZ   *+14                                                             
         XC    4(4,R1),4(R1)       Clear the length if nothing to edit          
         J     EXITY                                                            
         LHI   R0,3                                                             
         STCM  R0,15,4(R1)                                                      
                                                                                
         L     R5,ACLTREC                                                       
         USING CLTHDR,R5                                                        
         CLC   CKEYAM,QMEDX        Test client record is around                 
         JNE   *+14                                                             
         CLC   CKEYCLT,0(R2)                                                    
         JE    EDTCLT02                                                         
         MVC   QCLTX,0(R2)                                                      
         GOTOR (#GETCLT,AGETCLT)   No - read it                                 
                                                                                
EDTCLT02 GOTOR VCLUNPK,DMCB,(CPROF+6,(R2)),(R4)                                 
         J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES DAYPART                                    *         
*        ON ENTRY ... P1 BYTE 0 = DAYPART EQUATE                      *         
*                     OR                                              *         
*                     P1 = A(DAYPART CODE)                            *         
***********************************************************************         
                                                                                
VALDPT   J     *+12                                                             
         DC    C'*VALDPT*'                                                      
         MVI   QDPT,0              INITIALIZE RETURN VALUES                     
         XC    QDPT2,QDPT2                                                      
                                                                                
***********************************************************************         
                                                                                
         CLI   0(R1),0             IF DAYPART CODE IS PROVIDED                  
         JNE   VDPT100                                                          
         ZICM  RE,1(R1),3                                                       
         MVC   HALF2,0(RE)         HALF2 = DAYPART CODE TO VALIDATE             
                                                                                
         USING DPTTABD,R4                                                       
         LARL  R4,DPTTAB                                                        
VDPT10   CLC   DPTCODE,HALF2       IF DAYPART CODE IS IN TABLE                  
         JNE   VDPT20                                                           
         MVC   QDPT,0(R4)                                                       
         MVC   QDPT2,0(R4)         SET RETURN VARIABLES                         
         J     EXITY               AND EXIT                                     
VDPT20   CLI   DPTTLNQ(R4),X'FF'                                                
         JE    VDPT30                                                           
         LA    R4,DPTTLNQ(R4)                                                   
         J     VDPT10                                                           
         DROP  R4                                                               
                                                                                
         USING NDPTHDR,R3                                                       
VDPT30   LA    R3,IOKEY            IF DAYPART CODE IS NOT IN TABLE              
         XC    NDPTKEY,NDPTKEY     LOOK FOR DAYPART RECORD AT MEDIA             
         MVC   NDPTKTYP,=XL2'0D07' LEVEL                                        
         MVC   NDPTAGM,QMEDX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOUNTDIR+IO3'                            
         J     VDPT50                                                           
VDPT40   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOUNTDIR+IO3'                            
VDPT50   CLC   IOKEY(NDPTDPTE-NDPTKEY),IOKEYSAV                                 
         JNE   VDPT60                                                           
         CLC   NDPTDPTA,HALF2                                                   
         JNE   VDPT40              IF FOUND                                     
         MVC   QDPT,NDPTDPTE       SET RETURN VARIABLES                         
         MVC   QDPT2,NDPTDPTA      AND EXIT                                     
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
VDPT60   OC    QCLTX,QCLTX         IF DAYPART CODE IS NOT FOUND AT              
         JZ    EXITN               MEDIA LEVEL AND CLIENT IS SET ...            
                                                                                
         USING NDPTHDR,R3                                                       
         XC    NDPTKEY,NDPTKEY     LOOK FOR DAYPART RECORD AT CLIENT            
         MVC   NDPTKTYP,=XL2'0D07' LEVEL                                        
         MVC   NDPTAGM,QMEDX                                                    
         MVC   NDPTCLT,QCLTX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOUNTDIR+IO3'                            
         J     VDPT80                                                           
VDPT70   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOUNTDIR+IO3'                            
VDPT80   CLC   IOKEY(NDPTDPTE-NDPTKEY),IOKEYSAV                                 
         JNE   EXITN                                                            
         CLC   NDPTDPTA,0(R2)                                                   
         JNE   VDPT70              IF FOUND                                     
         MVC   QDPT,NDPTDPTE       SET RETURN VARIABLES                         
         MVC   QDPT2,NDPTDPTA      AND EXIT                                     
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
VDPT100  MVC   BYTE4,0(R1)         BYTE4 = DAYPART EQUATE TO VALIDATE           
                                                                                
         USING DPTTABD,R4                                                       
         LARL  R4,DPTTAB                                                        
VDPT110  CLC   BYTE4,DPTCODE       IF DAYPART EQUATE IS IN TABLE                
         JNE   VDPT120                                                          
         MVC   QDPT,0(R4)                                                       
         MVC   QDPT2,0(R4)         SET RETURN VARIABLES                         
         J     EXITY               AND EXIT                                     
VDPT120  CLI   DPTTLNQ(R4),X'FF'                                                
         JE    VDPT130                                                          
         LA    R4,DPTTLNQ(R4)                                                   
         J     VDPT110                                                          
         DROP  R4                                                               
                                                                                
         USING NDPTHDR,R3                                                       
VDPT130  LA    R3,IOKEY                                                         
         XC    NDPTKEY,NDPTKEY     IF DAYPART EQUATE IS NOT IN TABLE            
         MVC   NDPTKTYP,=XL2'0D07' LOOK FOR DAYPART RECORD AT MEDIA/            
         MVC   NDPTAGM,QMEDX       CLIENT LEVEL                                 
         CLI   BYTE4,127                                                        
         JH    *+10                                                             
         MVC   NDPTCLT,QCLTX                                                    
         MVC   NDPTDPTE,BYTE4                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOUNTDIR+IO3'                            
         CLC   IOKEY(NDPTDES-NDPTKEY),IOKEYSAV                                  
         JNE   EXITN                                                            
         MVC   QDPT,NDPTDPTE       SET RETURN VARIABLES                         
         MVC   QDPT2,NDPTDPTA      AND EXIT                                     
         J     EXITY                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        VALID PAYPART TABLE                                          *         
***********************************************************************         
                                                                                
DPTTAB   DC    CL2'A '             ACCESS                                       
         DC    CL2'B '             CBLSPORT                                     
         DC    CL2'C '             CABLE                                        
         DC    CL2'D '             DAYTIME                                      
         DC    CL2'E '             EARLY                                        
         DC    CL2'F '             FRINGE                                       
         DC    CL2'H '             OTHER                                        
         DC    CL2'I '             SPECIAL                                      
         DC    CL2'J '             PROMO-ID                                     
         DC    CL2'K '             KIDS                                         
         DC    CL2'L '             LATE                                         
         DC    CL2'M '             WKNDAM                                       
         DC    CL2'N '             NEWS                                         
         DC    CL2'O '             OLYMPICS                                     
         DC    CL2'P '             PRIME                                        
         DC    CL2'Q '             INTRACTV                                     
         DC    CL2'R '             RADIO                                        
         DC    CL2'S '             SPORTS                                       
         DC    CL2'T '             TEENS                                        
         DC    CL2'U '             UNWIRED                                      
         DC    CL2'V '             OVERNITE                                     
         DC    CL2'W '             WKNDPM                                       
         DC    CL2'X '             SYND                                         
         DC    CL2'Y '             YOUTH                                        
         DC    XL1'FF'                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES PRODUCT AND LENGTH                         *         
*        ON ENTRY ... P1 = A(PRODUCT CODE)                            *         
*                     P2 = A(PRODUCT LENGTH)                          *         
*        ON EXIT ...  P1 BYTE 0 = 1 (PRODUCT CODE ERROR)              *         
*                                 2 (PRODUCT LENGTH ERROR)            *         
***********************************************************************         
                                                                                
VALPRD   J     *+12                                                             
         DC    C'*VALPRD*'                                                      
                                                                                
         ZICM  R2,1(R1),3          R2=A(PRODUCT CODE)                           
         ZICM  R4,5(R1),3          R4=A(PRODUCT LENGTH)                         
                                                                                
         LR    R5,R1                                                            
         MVI   0(R5),0             INITIALIZE ERROR FIELD                       
                                                                                
         USING PRDHDR,R3                                                        
         LA    R3,IOKEY                                                         
         XC    PKEY,PKEY           ENSURE PRODUCT EXISTS                        
         MVC   PKEYAM,QMEDX                                                     
         MVC   PKEYCLT,QCLTX                                                    
         MVC   PKEYPRD,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JE    VPRD10                                                           
         MVI   0(R5),1                                                          
         J     EXITN                                                            
         DROP  R3                                                               
                                                                                
VPRD10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO3'                           
                                                                                
         USING PRDHDR,R3                                                        
         L     R3,AIO3                                                          
         MVC   QPRODA,PKEYPRD     SET RETURN VARIABLES                          
         MVC   QPRODX,PCODE+1                                                   
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         OC    0(1,R4),0(R4)       IF PRODUCT LENGTH IS PROVIDED                
         JZ    EXITY                                                            
                                                                                
         CLI   0(R4),254           ENSURE IT IS 254 SECONDS OR LESS             
         JNH   EXITY                                                            
         MVI   0(R5),2                                                          
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS CHECK SUM INTO PROVIDED FIELD                   *         
*        ON ENTRY ... P1 = A(RECORD)                                  *         
*                     P2 = DISPLACEMENT OF RECORD LENGTH                        
*                     P3 = A(CHECK SUM OUTPUT FIELD)                  *         
***********************************************************************         
                                                                                
SETCKS   J     *+12                                                             
         DC    C'*SETCKS*'                                                      
                                                                                
         L     RE,4(R1)                                                         
         ZICM  RF,0(RE),2          RF=DISPLACEMENT OF RECORD LENGTH)            
         L     RE,0(R1)            RE=A(RECORD)                                 
         L     R2,8(R1)            R2=A(CHECK SUM OUTPUT FIELD)                 
                                                                                
         XR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *+4                                                              
         STCM  R0,15,0(R2)                                                      
         J     EXIT                                                             
EXITN    DS    0H                  Set CC not equal                             
EXITL    LHI   RE,0                Set CC low                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                Set CC high                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set CC equal                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
GLOBALS  DS    0D                  ** Global literals **                        
                                                                                
         LTORG                                                                  
                                                                                
* NENAVWORKD Etc.                                                               
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE NEGENDPT                                                       
STASTAL  EQU   L'STAPQSTA+L'STAPQNET                                            
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR DAYPART TABLE                                     *          
**********************************************************************          
                                                                                
DPTTABD  DSECT                                                                  
DPTCODE  DS    CL2                                                              
DPTTLNQ  EQU   *-DPTTABD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003NENAV72   03/16/18'                                      
         END                                                                    
