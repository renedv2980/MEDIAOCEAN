*          DATA SET SEACS24    AT LEVEL 009 AS OF 05/15/20                      
*PHASE TA0D24A                                                                  
*INCLUDE VEMAIL                                                                 
*                                                                               
         TITLE 'SEACS24 -SECURITY ACCESS - DOMAIN RECORDS '                     
*                                                                               
ACS24    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACS24*,RR=RE                                                  
         USING WORKD,R7                 R7=A(GLOBAL W/S)                        
         USING TWAD,R5                  R5=A(TWA)                               
         USING SAVAREA,R6               R6=A(GLOBAL SAVE AREA)                  
         LA    R2,IOKEY                                                         
         USING DOMRECD,R2               R2=A(RECORD KEY)                        
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC               RC=A(SYSTEM + LOCAL W/S)                
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                   01 - APMVALK                            
         B     VALREC                   02 - APMVALR                            
         B     DISKEY                   03 - APMDISK                            
         B     DISREC                   04 - APMDISR                            
         B     DELREC                   05 - APMDELR                            
         B     RESREC                   06 - APMRESR                            
         B     VALSEL                   07 - APMVALP                            
         B     GETSEL                   08 - APMGETS                            
         B     DISSEL                   09 - APMDISS                            
         B     XIT                      10 - APMVALS                            
         B     FSTLST                   11 - APMFLST                            
         B     XIT                      12 - APMPROC                            
         B     XIT                      13 - APMFSCR                            
         B     LSTSCR                   14 - APMLSCR                            
         B     XIT                      15 - APMVALQ                            
         B     XIT                      16 - APMREPP                            
         B     SETSCR                   17 - APMSETT                            
         B     XIT                      18 - APMPUTK                            
         B     XIT                      19 - APMNEWK                            
         B     XIT                      20 - APMFRP                             
         B     XIT                      21 - APMDISS2                           
*                                                                               
EXIT     OI    ACSSRVH+FHOID,FHOITR+FHOIMO AVOID NO DATA ENTERED                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY                                                       
***********************************************************************         
VALKEY   LA    R2,APRECKEY                                                      
*                                                                               
         XC    DOMKEY,DOMKEY                                                    
         MVI   DOMKMIN,DOMKMIQ          C'D' DOMAIN RECORD                      
         MVC   DOMKSAG,CUAALF           SECURITY AGENCY                         
                                                                                
*---------------------------------------                                        
* DOMAIN GROUP CODE                                                             
*---------------------------------------                                        
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,DOMGRPH                                                    
         BNE   SAEMIF                                                           
         CLI   FVILEN,L'DOMGRP                                                  
         BL    SAEIIF                                                           
         GOTO1 VALAN,APPARM,(FVILEN,FVIFLD)                                     
         BNE   SAEIIF                                                           
         MVC   DOMKGRP,FVIFLD                                                   
                                                                                
*---------------------------------------                                        
* READ FOR RECORD                                                               
*---------------------------------------                                        
VK030    LA    R2,IOKEY                                                         
         MVC   DOMKEY(DOMKEYL),APRECKEY SET KEY                                 
*                                                                               
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                  I/O ERROR EXIT                          
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK100                                                            
         TM    IOERR,IOEDEL             TEST RECORD IS DELETED                  
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VK100                                                            
         MVI   APINDS,APIOKADD                                                  
         B     VALKEYY                                                          
*                                                                               
VK100    CLI   APACTN,ACTADD                                                    
         BE    VALKEYX                                                          
         LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                  I/O ERROR EXIT                          
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A RECORD                                             
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD            ADD ACTION                              
         BNE   VR020                                                            
         XC    DOMKEY(DOMFIRST+1),DOMKEY                                        
         MVC   DOMKEY,APRECKEY                                                  
         LHI   R1,DOMFIRST+1                                                    
         STCM  R1,3,DOMFLEN                                                     
         MVI   SVDTOP,0                                                         
                                                                                
*---------------------------------------                                        
* DOMAIN GROUP NAME                                                             
*---------------------------------------                                        
VR020    XC    APELEM,APELEM            DELETE OLD ELEMENT                      
         LA    R3,APELEM                                                        
         USING SADSCD,R3                                                        
         MVI   SADSCEL,SADSCELQ                                                 
         MVI   SADSCLEN,0                                                       
         GOTO1 ADELELS,DOMRECD                                                  
*                                                                               
         LA    R4,DOMGNMH               ANY NAME ENETERED?                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R4)                                                       
         BNE   VR025                    NO                                      
*                                                                               
         IC    RF,FVXLEN                ADD NEW ELEMENT                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SADSC(0),FVIFLD                                                  
         AHI   RF,3                     FOR EX AND ELEMENT HEADER               
         STC   RF,SADSCLEN                                                      
         MVI   SADSCEL,SADSCELQ                                                 
         GOTO1 AADDELS,DOMRECD                                                  
         JNE   *+2                      CAN'T FIT ELEMENT                       
                                                                                
*---------------------------------------                                        
* DOMAIN ELEMENTS                                                               
*---------------------------------------                                        
VR025    LA    R4,DOMDOM1H              VALIDATE DOMAINS ON SCREEN              
         MVI   FVMINL,1                                                         
VR030    GOTO1 AFVAL,(R4)                                                       
         BNE   VR050                                                            
*                                                                               
         LLC   RF,FVILEN                                                        
         AHI   RF,L'R2D2                                                        
         MVC   TEMPEM(L'R2D2),R2D2                                              
         MVC   TEMPEM+L'R2D2(L'DOMDOM1),FVIFLD                                  
         GOTO1 =V(VEMAIL),APPARM,((RF),TEMPEM),(CULANG,0),0,0,         +        
               RR=APRELO                                                        
         CLI   APPARM,0                                                         
         BNE   SAEIIF                                                           
*                                                                               
VR050    AHI   R4,DOMDOM2H-DOMDOM1H     BUMP TO NEXT FIELD                      
         LA    RF,DOMDOMFH                                                      
         CR    R4,RF                    DID WE REACH THE BOTTOM?                
         BNH   VR030                    NO: GET NEXT                            
*                                                                               
         LA    R3,DOMFIRST(R2)          R3=CURRENT DOMAIN ELEMENT               
         LA    R4,DOMDOM1H              R4=CURRENT DOMAIN ON THE SCREEN         
         LLC   R8,SVDTOP                R8=FIRST DOMAIN ON THE SCREEN           
         XR    R9,R9                    R9=CURRENT DOMAIN ELEMENT #             
*                                                                               
         USING DOMELD,R3                                                        
VR052    CLI   DOMEL,0                  END OF RECORD?                          
         BE    VR056                    YES:                                    
         CLI   DOMEL,DOMELQ             DOMAIN ELEMENT?                         
         BNE   VR054                    NO: SKIP IT                             
         CR    R8,R9                    BUMP PAST ELEMS NOT ON SCREEN           
         BE    VR060                                                            
         AHI   R9,1                                                             
VR054    LLC   RF,DOMELL                                                        
         AR    R3,RF                                                            
         B     VR052                                                            
VR056    LR    R8,R9                                                            
*                                                                               
VR060    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R4)               CHECK DOMAIN ON SCREEN LINE             
         BNE   VR090                                                            
*                                                                               
VR080    CLI   DOMEL,0                  END OF RECORD?                          
         BE    VR085                    ADD THE ELEMENT HERE                    
         CLI   DOMEL,DOMELQ             POINTING TO THIS DOMAIN?                
         BE    VR082                    DELETE OLD AND ADD NEW                  
         LLC   RF,DOMELL                BUMP TO NEXT ELEMENT                    
         AR    R3,RF                                                            
         B     VR080                                                            
*                                                                               
VR082    MVI   0(R3),X'FF'              DELETE THIS EXACT ELEMENT               
         GOTO1 VHELLO,APPARM,(C'D',GENFIL),(X'FF',(R2)),0,0                     
         CLI   12(R1),0                 ERROR                                   
         BNE   SAEIIF                                                           
*                                                                               
VR085    XC    APELEM,APELEM            ADD ELEMENT AT CURRENT LOCATION         
         STCM  R3,15,TEMPWORD                                                   
         LA    R3,APELEM                                                        
         MVI   DOMEL,DOMELQ                                                     
         MVC   DOMEDOM(L'DOMDOM1),FVIFLD                                        
         LLC   R1,FVILEN                                                        
         AHI   R1,DOMELLQ                                                       
         STC   R1,DOMELL                                                        
         ICM   R3,15,TEMPWORD                                                   
         GOTO1 VHELLO,APPARM,(C'P',GENFIL),(R2),APELEM,ADDHERE,0,(R3)           
         CLI   12(R1),0                 ERROR                                   
         BNE   SAETMD                                                           
         LLC   RF,DOMELL                BUMP TO NEXT ELEMENT                    
         AR    R3,RF                                                            
         B     VR100                    NEXT SCREEN LINE                        
*                                                                               
VR090    CLI   DOMEL,0                  END OF RECORD?                          
         BE    VR100                    NEXT SCREEN LINE                        
         CLI   DOMEL,DOMELQ             POINTING TO THIS DOMAIN?                
         BE    VR095                    REMOVE THIS ELEMENT                     
         LLC   RF,DOMELL                BUMP TO NEXT ELEMENT                    
         AR    R3,RF                                                            
         B     VR090                                                            
*                                                                               
VR095    MVI   0(R3),X'FF'              DELETE THIS EXACT ELEMENT               
         GOTO1 VHELLO,APPARM,(C'D',GENFIL),(X'FF',(R2)),0,0                     
         CLI   12(R1),0                                                         
         BNE   SAEIIF                   ERROR                                   
         B     VR100                    NEXT SCREEN LINE                        
*                                                                               
VR100    AHI   R4,DOMDOM2H-DOMDOM1H     BUMP TO NEXT SCREEN FIELD               
         LA    RF,DOMDOMFH                                                      
         CR    R4,RF                    DID WE REACH THE BOTTOM?                
         BNH   VR060                    NO: GO TO NEXT                          
                                                                                
*---------------------------------------                                        
* GENFIL ACTIVITY ELEMENT X'  '                                                 
*---------------------------------------                                        
         GOTO1 ASETACT,DOMRECD          SET ACTIVITY ELEMENT                    
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY                                                        
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVI   SVDTOP,0                                                         
*                                                                               
         MVC   DOMGRP(L'DOMKGRP),DOMKGRP                                        
         OI    DOMGRPH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
DISKEYX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY                                                            
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         XR    R9,R9                                                            
         LA    R3,DOMFIRST(R2)                                                  
DR01     CLI   0(R3),0                  END OF RECORD?                          
         BE    DR04                                                             
         CLI   0(R3),SADSCELQ           DESCRIPTION ELEMENT?                    
         BE    DR03                     YES                                     
         CLI   0(R3),DOMELQ             DOMAIN ELEMENT?                         
         BNE   DR02                     NO: DON'T COUNT                         
         AHI   R9,1                                                             
DR02     LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DR01                                                             
*                                                                               
         USING SADSCD,R3                                                        
DR03     IC    RF,SADSCLEN                                                      
         AHI   RF,-3                    ELEM CODE, LENGTH, AND EXEC             
         BM    DR02                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DOMGNM(0),SADSC                                                  
         OI    DOMGNMH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DR02                                                             
         DROP  R3                                                               
*                                                                               
DR04     STC   R9,DOMCOUNT              SAVE THE DOMAIN COUNT                   
*                                                                               
         CLI   APPFKEY,0                ANY PFKEY PRESSED?                      
         BE    DR020                    NO                                      
         CLI   APPFKEY,PFK09                                                    
         BNE   DR002                                                            
         CLI   APACTN,ACTCHA                                                    
         BNE   DR020                                                            
         BAS   RE,SORTEL                SORT THE DOMAIN ELEMENTS                
*                                                                               
DR002    LLC   R8,SVDTOP                ADJUST SVDTOP FOR SCROLLING             
*                                                                               
         CLI   APPFKEY,PFK07            SCROLL UP?                              
         BNE   DR005                    NO                                      
         CHI   R8,SCROLLPG                                                      
         BH    *+8                                                              
         LHI   R8,SCROLLPG                                                      
         AHI   R8,-SCROLLPG                                                     
         B     DR010                                                            
*                                                                               
DR005    CLI   APPFKEY,PFK08            SCROLL DOWN?                            
         BNE   DR010                    NO                                      
*                                                                               
         XR    R9,R9                    COUNT THE # OF DOMAIN ELEMENTS          
         LA    R3,DOMFIRST(R2)                                                  
         USING DOMELD,R3                                                        
DR006    CLI   DOMEL,0                  END OF RECORD?                          
         BE    DR008                    YES: START HERE                         
         CLI   DOMEL,DOMELQ             DOMAIN ELEMENT?                         
         BNE   DR007                    NO: SKIP IT                             
         AHI   R9,1                                                             
DR007    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DR006                                                            
*                                                                               
DR008    CHI   R9,SCROLLPG                                                      
         BL    DR009                                                            
         AHI   R9,-1                                                            
         B     *+6                                                              
DR009    XR    R9,R9                                                            
*                                                                               
         AHI   R8,SCROLLPG                                                      
         CHI   R8,SCROLLMX                                                      
         BL    *+8                                                              
         LHI   R8,SCROLLMX                                                      
*                                                                               
         CR    R8,R9                                                            
         BL    *+6                                                              
         LR    R8,R9                                                            
*                                                                               
DR010    STC   R8,SVDTOP                 FIRST FOR NEXT SCREEN                  
*                                                                               
DR020    LA    R4,DOMDON1H                                                      
         LLC   R8,SVDTOP                                                        
         AHI   R8,1                                                             
DR022    MVC   L'FVIHDR(L'DOMDON1,R4),SPACES                                    
         CHI   R8,50                                                            
         BH    DR030                                                            
         EDIT  (R8),(2,8(R4))                                                   
DR030    OI    FVOIND-FVIHDR(R4),FVOXMT                                         
         AHI   R4,DOMDON2H-DOMDON1H     BUMP TO NEXT FIELD                      
         AHI   R8,1                                                             
         LA    RF,DOMDONFH                                                      
         CR    R4,RF                    DID WE REACH THE BOTTOM?                
         BNH   DR022                    NO: GO TO NEXT                          
*                                                                               
         LA    R3,DOMFIRST(R2)          R3=CURRENT DOMAIN ELEMENT               
         LA    R4,DOMDOM1H              R4=CURRENT DOMAIN ON THE SCREEN         
         LLC   R8,SVDTOP                R8=FIRST DOMAIN ON THE SCREEN           
         XR    R9,R9                    R9=CURRENT DOMAIN ELEMENT #             
*                                                                               
         USING DOMELD,R3                                                        
DR040    CLI   DOMEL,0                  END OF RECORD?                          
         BE    DR044                    YES: START HERE                         
         CLI   DOMEL,DOMELQ             DOMAIN ELEMENT?                         
         BNE   DR042                    NO: SKIP IT                             
         CR    R8,R9                    BUMP PAST ELEMS NOT ON SCREEN           
         BE    DR060                                                            
         AHI   R9,1                                                             
DR042    LLC   RF,DOMELL                                                        
         AR    R3,RF                                                            
         B     DR040                                                            
DR044    LR    R8,R9                    START FROM THIS ELEMENT                 
*                                                                               
DR060    LA    R4,DOMDOM1H                                                      
DR070    MVC   L'FVIHDR(L'DOMDOM1,R4),SPACES                                    
         OI    FVOIND-FVIHDR(R4),FVOXMT                                         
         AHI   R4,DOMDOM2H-DOMDOM1H     BUMP TO NEXT FIELD                      
         LA    RF,DOMDOMFH                                                      
         CR    R4,RF                    DID WE REACH THE BOTTOM?                
         BNH   DR070                    NO: GO TO NEXT                          
*                                                                               
         LA    R4,DOMDOM1H                                                      
DR080    CLI   0(R3),0                                                          
         BE    DR150                                                            
         CLI   0(R3),DOMELQ             DOMAIN ELEMENT                          
         BE    DR100                                                            
DR090    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR080                                                            
*                                                                               
         USING DOMELD,R3                                                        
DR100    LLC   R1,DOMELL                                                        
         AHI   R1,-DOMELLQ-1                                                    
         BM    DR090                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),DOMEDOM                                                  
         OI    FVOIND-FVIHDR(R4),FVOXMT                                         
         AHI   R4,DOMDOM2H-DOMDOM1H     BUMP TO NEXT FIELD                      
         LA    RF,DOMDOMFH                                                      
         CR    R4,RF                    DID WE REACH THE BOTTOM?                
         BNH   DR090                    NO: GO TO NEXT                          
*                                                                               
DR150    EDIT  (B1,DOMCOUNT),DOMDOMT,ZERO=NOBLANK                               
         OI    DOMDOMTH+(FVOIND-FVIHDR),FVOXMT                                  
*                                                                               
DISRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
***********************************************************************         
* DELETE A RECORD                                                               
***********************************************************************         
DELREC   LA    R2,IOKEY                                                         
         OI    DOMDSTAT,X'80'           SET DELETE FLAG IN DIRECTORY            
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,DOMRECD                                                  
         OI    DOMFSTAT,X'80'           SET DELETE FLAG IN RECORD               
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* RESTORE A DELETED MONEYFLOW COMPANY RECORD                                    
***********************************************************************         
RESREC   LA    R2,IOKEY                                                         
         NI    DOMDSTAT,X'FF'-X'80'     TURN DELETE BIT OFF IN KEY              
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         JNE   *+2                                                              
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,DOMRECD                                                  
         NI    DOMFSTAT,X'FF'-X'80'     TURN DELETE BIT OFF IN RECORD           
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         JNE   *+2                                                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)              
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
VALSEL   LA    R2,APRECKEY              BUILD FIRST RECORD KEY                  
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    DOMKEY(DOMKEYL),DOMKEY                                           
         MVI   DOMKMIN,DOMKMIQ          C'D' DOMAIN RECORDS                     
         MVC   DOMKSAG,CUAALF                                                   
*                                                                               
         MVI   FVMINL,1                 OFFICE FILTER                           
         GOTO1 AFVAL,LSTGRPH                                                    
         BNE   VS020                                                            
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOMKGRP,FVIFLD                                                   
*                                                                               
VS020    LA    R0,LSTACT1H              SET ADDRESS OF FIRST LIST LINE          
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H              SET LIST LINE LENGTH                    
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                                   
***********************************************************************         
GETSEL   LA    R2,IOKEY                 READ NEXT RECORD                        
         MVC   DOMKEY,APRECKEY          FROM LAST KEY                           
         TM    APINDS,APILNSEQ          FIRST LINE IN LIST SEQUENCE             
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOGENDIR+IOSQ+IO1     ELSE NEXT LIST LINE                     
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         MVC   APRECKEY(DOMKEYL),DOMKEY SAVE LAST RECORD KEY                    
         CLI   DOMKMIN,DOMKMIQ                                                  
         BNE   GSEND                    ELSE EXIT LIST                          
         CLC   DOMKSAG,CUAALF                                                   
         BNE   GSEND                                                            
         GOTO1 AIO,IOGET+IOGENFIL+IO1                                           
         BNE   GSEND                    I/O ERROR EXIT                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS           SET NO MORE RECORDS TO COME             
GETSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4                 R4=A(LIST/SELECT LINE)                  
         MVC   LISTGRP,DOMKGRP          OFFICE                                  
*                                                                               
         LA    R3,DOMFIRST(R2)          GET ELEMENT DATA                        
         LA    R8,LISTDOM                                                       
         LA    R9,L'LISTDOM-4                                                   
DS030    CLI   0(R3),0                                                          
         BE    DS070                    END OF RECORD                           
         CLI   0(R3),SADSCELQ           DESCRIPTION ELEMENT?                    
         BE    DS045                    YES                                     
         CLI   0(R3),DOMELQ                                                     
         BE    DS050                                                            
DS040    LLC   R0,1(R3)                 GET NEXT ELEMENT                        
         AR    R3,R0                                                            
         B     DS030                                                            
*                                                                               
         USING SADSCD,R3                                                        
DS045    IC    RF,SADSCLEN                                                      
         AHI   RF,-3                                                            
         BM    DS040                                                            
         CHI   RF,L'LISTDSC                                                     
         BL    DS046                                                            
         MVC   LISTDSC+L'LISTDSC-3(3),=C'...'                                   
         LHI   RF,L'LISTDSC-5                                                   
DS046    EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISTDSC(0),SADSC                                                 
         B     DS040                                                            
         DROP  R3                                                               
*                                                                               
         USING DOMELD,R3                                                        
DS050    LLC   R1,DOMELL                                                        
         AHI   R1,-DOMELLQ                                                      
         BNP   DS070                                                            
*                                                                               
         CR    R9,R1                                                            
         BH    *+6                                                              
         LR    R1,R9                                                            
*                                                                               
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),DOMEDOM                                                  
*                                                                               
         AHI   R1,3                                                             
         SR    R9,R1                                                            
         BNP   DS060                                                            
         AHI   R1,-2                                                            
         AR    R8,R1                                                            
         MVI   0(R8),C','                                                       
         LA    R8,2(,R8)                                                        
         B     DS040                                                            
DS060    LA    R8,LISTDOM+L'LISTDOM-3                                           
         MVC   0(3,R8),=C'...'                                                  
         B     DISSELX                                                          
*                                                                               
DS070    AHI   R8,-2                                                            
         CLI   0(R8),C','                                                       
         BNE   DISSELX                                                          
         MVI   0(R8),C' '                                                       
*                                                                               
DISSELX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* FIRST SCREEN INIT                                                             
***********************************************************************         
SETSCR   TM    TWASWPST,TWAFIRST                                                
         BZ    SETSCRX                                                          
         MVI   SVDTOP,0                                                         
         NI    TWASWPST,X'FF'-TWAFIRST    TURN OFF FIRST FOR RECORD             
SETSCRX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)              
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* SORT ELEMENTS AND WRITE BACK RECORD                                           
***********************************************************************         
SORTEL   NTR1                                                                   
*                                                                               
         MVI   SVDTOP,0                                                         
         L     R2,AIOAREA1                                                      
         L     R8,AIOAREA2                                                      
         LH    R3,DOMFLEN                                                       
         LR    R9,R3                                                            
         MVCL  R8,R2                                                            
*                                                                               
         L     R2,AIOAREA1                                                      
         L     R8,AIOAREA2                                                      
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING DOMELD,R3                                                        
         MVI   DOMEL,DOMELQ                                                     
         MVI   DOMELL,0                                                         
         GOTO1 ADELELS,DOMRECD                                                  
*                                                                               
         LA    R9,DOMFIRST(R8)                                                  
SE020    CLI   0(R9),0                  END OF RECORD?                          
         BE    SE050                    YES:                                    
         CLI   0(R9),DOMELQ             DOMAIN ELEMENT?                         
         BE    SE030                    NO: SKIP IT                             
SE025    LLC   RF,1(R9)                                                         
         AR    R9,RF                                                            
         B     SE020                                                            
*                                                                               
SE030    XC    APELEM,APELEM                                                    
         LLC   R1,1(R9)                                                         
         AHI   R1,-1                                                            
         BM    SE025                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R9)                                                  
         GOTO1 AADDELS,DOMRECD                                                  
         BE    SE025                                                            
         DC    H'0'                                                             
*                                                                               
SE050    GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC FIELD                                                   
*       ON ENTRY: 0(R1) = FIELD                                                 
*                 4(R1) = LENGTH                                                
*       ON EXIT:  CC=ZERO IF OK                                                 
***********************************************************************         
VALAN    NTR1                                                                   
         ICM   RE,15,0(R1)                                                      
         SR    RF,RF                                                            
         IC    RF,7(R1)                                                         
         LTR   RF,RF                                                            
         BZ    VAANOK                                                           
VAAN010  CLI   0(RE),C'A'                                                       
         BL    VAAN020                                                          
         CLI   0(RE),C'Z'                                                       
         BH    VAAN020                                                          
         B     VAAN030                                                          
VAAN020  CLI   0(RE),C'0'                                                       
         BL    VAANNO                                                           
         CLI   0(RE),C'9'                                                       
         BH    VAANNO                                                           
VAAN030  LA    RE,1(RE)                                                         
         BCT   RF,VAAN010                                                       
         B     VAANOK                                                           
*                                                                               
VAANNO   LTR   RB,RB                                                            
         B     EXIT                                                             
VAANOK   CR    RB,RB                                                            
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ERROR EXITS & CONSTANTS                                                       
***********************************************************************         
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                     INPUT FIELD ERROR                       
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                     MISSING FIELD                           
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     EXIT                     I/O ERROR                               
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                     RECORD NOT FOUND                        
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXIT                     ALREADY EXISTS                          
SAEDUP   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXIT                     DUPLICATED INPUT FIELD                  
SAETMD   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXIT                     INPUT LENGTH TOO LONG                   
SAESAF   MVC   FVMSGNO,=AL2(FVFTSAF)                                            
         B     EXIT                     STORAGE ALLOCATION FAILURE              
*                                                                               
FFILL    DC    32X'FF'                                                          
SPACES   DC    128C' '                                                          
R2D2     DC    C'R2D2@'                                                         
GENFIL   DC    C'GENFIL '                                                       
ADDHERE  DC    C'ADD=HERE'                                                      
         LTORG                                                                  
*                                                                               
SCROLLPG EQU   5                                                                
SCROLLMX EQU   35                                                               
                                                                                
***********************************************************************         
* SECURITY ACCESS WORK DSECT                                                    
***********************************************************************         
       ++INCLUDE SEACSWRK                                                       
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSBED                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSBFD                                                       
         ORG   SAVOVER                                                          
*----------------------------------------------------------------------         
* SAVED STORAGE                                                                 
*----------------------------------------------------------------------         
SVDTOP   DS    X                        FIRST DOMAIN DISPLAYED                  
         DS    X                        DOMAIN COUNT                            
*                                                                               
***********************************************************************         
* LIST/SELECT LINE LAYOUT                                                       
***********************************************************************         
LISTD    DSECT                                                                  
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                      ACTION FIELD                            
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         DS    CL2                                                              
LISTGRP  DS    CL3                      GROUP                                   
         DS    CL3                                                              
LISTDSC  DS    CL20                     DESCRIPTION                             
         DS    CL1                                                              
LISTDOM  DS    CL30                     DOMAINS                                 
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                                         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
WORK     DS    CL64                                                             
TEMPEMH  DS    CL(L'DOMDOM1H)                                                   
TEMPEM   DS    CL(L'DOMDOM1+L'R2D2)                                             
TEMPWORD DS    XL4                                                              
DOMCOUNT DS    XL1                                                              
LOCALX   EQU   *                                                                
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE GEGENDOM               DOMAIN RECORD                           
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SEACS24   05/15/20'                                      
         END                                                                    
