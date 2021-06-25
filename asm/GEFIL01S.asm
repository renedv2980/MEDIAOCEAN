*          DATA SET GEFIL01S   AT LEVEL 038 AS OF 08/29/00                      
*&&      SET   NOP=N                                                            
*PHASE T00AB1A                                                                  
*                                                                               
* TSMY 037 06AUG99 - SCRMOD CHANGED TO SET P3 PROPERLY                          
*                                                                               
         TITLE 'NEW FILE CONTROLLER SYSTEM LEVEL OBJECTS'                       
FILCONT  START                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
FILCNT   NMOD1 0,NFICNTRL,RR=R3                                                 
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         TM    0(R1),GCBOVER       CAN OVERRIDE CALL AT LOWER LEVEL?            
         BZ    FCNT02              NO                                           
*                                                                               
         L     RF,APRG                                                          
         BASR  RE,RF                                                            
         BNH   FCNTX               OVERRIDDEN AT LOWER LEVEL                    
*                                                                               
FCNT02   L     RE,0(R1)            GET REQUESTED OBJECT                         
         LA    R5,KNOWNOBJ         LIST OF KNOWN OBJECTS                        
         USING OBJTABD,R5                                                       
*                                                                               
FCNT04   CLI   OBJVERB,EOT         E.O.T.                                       
         BNE   *+6                                                              
         DC    H'0'                NOT KNOWN AT ANY LEVEL                       
*                                                                               
         CLM   RE,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    FCNT06              MATCHED                                      
         LA    R5,OBJTABL(R5)                                                   
         B     FCNT04              ITERATE VERB TABLE                           
*                                                                               
FCNT06   L     RF,=A(GEFIL01)      A(GENERAL ROUTINES 1)                        
         AR    RF,R3               RELOCATE IT                                  
         CLI   OBJIND2,2           OBJECT IN GENERAL ROUTINES 2?                
         BNE   *+8                 NO                                           
         L     RF,AGEN2                                                         
         CLI   OBJIND2,3           OBJECT IN LIST CONTROLLER?                   
         BNE   *+8                 NO                                           
         L     RF,AGENLST                                                       
         BASR  RE,RF                                                            
*                                                                               
FCNTX    XIT1  ,                                                                
         DROP  R5                                                               
*                                                                               
KNOWNOBJ DC    AL1(ORTYPE),AL1(0,002,0),AL4(0)                                  
         DC    AL1(OACT),AL1(0,002,0),AL4(0)                                    
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(OIO),AL1(0,0,0),AL4(IO)                                      
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(OLIST),AL1(0,003,0),AL4(0)                                   
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OPFK),AL1(0,002,0),AL4(0)                                    
         DC    AL1(OSES),AL1(0,0,0),AL4(SES)                                    
         DC    AL1(OFILT),AL1(0,0,0),AL4(FILTER)                                
         DC    AL1(OSUBACT),AL1(0,003,0),AL4(0)                                 
         DC    AL1(OPAGE),AL1(0,002,0),AL4(0)                                   
         DC    AL1(OHEIR),AL1(0,0,0),AL4(HEIR)                                  
         DC    AL1(OACTH),AL1(0,002,0),AL4(0)                                   
         DC    AL1(OSUBH),AL1(0,003,0),AL4(0)                                   
         DC    AL1(EOT)                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,RA,R9,R8                                                      
         EJECT                                                                  
***********************************************************************         
* GENERALISED OBJECT CONTROLLER PHASE 1                               *         
***********************************************************************         
         SPACE 1                                                                
GEFIL01  CSECT                                                                  
         NMOD1 RTWORKL,GEFIL1**,R7,RR=R3,CLEAR=YES                              
         USING RTWORKD,RC                                                       
         ST    R3,RTRELO                                                        
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    R1,RTPARMA          SAVE A(CALLERS R1)                           
         MVC   RTPARMS,0(R1)       SAVE CALLERS PARAMETERS                      
*                                                                               
         LA    R4,TABLEOO                                                       
         USING OBJTABD,R4                                                       
OBJ02    CLI   OBJTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   OBJVERB,RTPARMS1+3                                               
         BE    *+12                                                             
         LA    R4,OBJTABL(R4)                                                   
         B     OBJ02                                                            
*                                                                               
         ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  R4                                                               
*                                                                               
TABLEOO  DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(OIO),AL1(0,0,0),AL4(IO)                                      
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(SES)                                    
         DC    AL1(OFILT),AL1(0,0,0),AL4(FILTER)                                
         DC    AL1(OHEIR),AL1(0,0,0),AL4(HEIR)                                  
         DC    AL1(EOT)                                                         
*                                                                               
EXITND   MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         B     EXITL               FIELD NOT DEFINED AT ANY LEVEL ERROR         
*                                                                               
EXITL    MVI   GCDUB1,0            SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   GCDUB1,2            SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   GCDUB1,1            SET CC EQUAL                                 
         B     EXITCC                                                           
*                                                                               
EXITCC   CLI   GCDUB1,1                                                         
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
GSFRR    USING FRRELD,GSFRREL      CURRENT SESSION RECORD ELEMENT               
PSFRR    USING FRRELD,PSFRREL      PREVIOUS SESSION RECORD ELEMENT              
GSFRA    USING FRAELD,GSFRAEL      CURRENT SESSION ACTION ELEMENT               
PSFRA    USING FRAELD,PSFRAEL      PREVIOUS SESSION ACTION ELEMENT              
GSFRP    USING FRPELD,GSFRPEL      PFKEY ELEMENT (IF KEY PRESSED)               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE - EXPECTS VERB TO BE IN R1                  *         
*                         - EXPECTS A(TABLE TO BE IN RF               *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T - NOT KNOWN AT ANY LEVEL               
         BE    EXITH                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE VERB TABLE                           
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,RTRELO                                                        
         LA    R1,RTPARMS          GET BACK PARAMETER LIST                      
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
DMREAD   DC    C'DMREAD  '                                                      
DMWRITE  DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
CORETAB  DC    C'CORETAB '                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
FLTB     DC    CL3'*= ',AL1(1),AL1(FDRF1NOT+FDRF1EQ),AL4(DFV48)                 
         DC    CL3'<> ',AL1(1),AL1(FDRF1NOT+FDRF1EQ),AL4(DFV48)                 
         DC    CL3'<= ',AL1(1),AL1(FDRF1NOT+FDRF1GT),AL4(DFV48)                 
         DC    CL3'*> ',AL1(1),AL1(FDRF1NOT+FDRF1GT),AL4(DFV48)                 
         DC    CL3'>= ',AL1(1),AL1(FDRF1NOT+FDRF1LT),AL4(DFV48)                 
         DC    CL3'*< ',AL1(1),AL1(FDRF1NOT+FDRF1LT),AL4(DFV48)                 
         DC    CL3'=  ',AL1(0),AL1(FDRF1EQ),AL4(DFV46)                          
         DC    CL3'<  ',AL1(0),AL1(FDRF1LT),AL4(DFV46)                          
         DC    CL3'>  ',AL1(0),AL1(FDRF1GT),AL4(DFV46)                          
         DC    CL3'*<=',AL1(2),AL1(FDRF1GT),AL4(DFV50)                          
         DC    CL3'*>=',AL1(2),AL1(FDRF1LT),AL4(DFV50)                          
         DC    CL3'*  ',AL1(0),AL1(FDRF1NOT),AL4(DFV46)                         
         DC    AL1(EOT)                                                         
*                                                                               
FLTBD    DSECT                                                                  
FLTBCMP  DS    CL3                 FILTER CHARACTERS                            
FLTBLN   DS    AL1                 LENGTH-1 FOR EXECUTED COMPARE                
FLTBTRUE DS    XL1                 MASK FOR VALIDITY                            
FLTBADR  DS    AL4                 A(FIELD MODIFICATION ROUTINE)                
FLTBLEN  EQU   *-FLTBD                                                          
*                                                                               
GEFIL01  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* NTRY: P1 = OBJECT CODE                                              *         
* NTRY: P2 = EQUATED VERB                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
         USING *+4096,R6                                                        
SCREEN   LR    R7,RF               BAD ADDRESSIBILITY SET-UP...                 
         LA    R6,4095(R7)                                                      
         LA    R6,1(R6)                                                         
         LM    R0,R3,0(R1)                                                      
*                                                                               
         MVC   RTSOVSYS,GCOVSYS    SAVE SYS/PROG/REC FOR INTERNAL USE           
         MVC   RTSPRG,GCPRGNO                                                   
         MVC   RTSREC,CSREC                                                     
         MVC   RTCTRY,CUCTRY       SAVE COUNTRY FOR INTERNAL USE                
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE SCREEN TABLE                         
*                                                                               
SCRTABL  DC    AL1(SKEY),AL1(0,0,0),AL4(SCRKEY)         1                       
         DC    AL1(SPAGE),AL1(0,0,0),AL4(SCRPAGE)       2                       
         DC    AL1(SBACK),AL1(0,0,0),AL4(SCRBACK)       3                       
         DC    AL1(SFRWD),AL1(0,0,0),AL4(SCRFRWD)       4                       
         DC    AL1(SSCRN),AL1(0,0,0),AL4(SCRSCRN)       5                       
         DC    AL1(SLSTTOP),AL1(0,0,0),AL4(SCRLSTT)     7                       
         DC    AL1(SLSTBOT),AL1(0,0,0),AL4(SCRLSTB)     8                       
         DC    AL1(SSAVE),AL1(0,0,0),AL4(SCRSAVE)       9                       
         DC    AL1(SREST),AL1(0,0,0),AL4(SCRREST)      10                       
         DC    AL1(SDLOAD),AL1(0,0,0),AL4(SCRDOWN)     11                       
         DC    AL1(SSET),AL1(0,0,0),AL4(SCRSSET)       12                       
         DC    AL1(SREPORT),AL1(0,0,0),AL4(SCRREP)     13                       
         DC    AL1(SMOD),AL1(0,0,0),AL4(SCRMOD)        14                       
         DC    AL1(SKSET),AL1(0,0,0),AL4(SCRKCODE)     15                       
         DC    AL1(SKPRO),AL1(0,0,0),AL4(SCRKPRO)      16                       
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD MAINTENANCE KEY SCREEN                                   *  1 *         
*                                                                ******         
* EXIT: CC = EQUAL IF SCREEN ALREADY LOADED                           *         
***********************************************************************         
         SPACE 1                                                                
SCRKEY   GOTOX SCRHDR,FSRKPKEY     PULL OUT KEY SCREEN INTO AIO1                
         BL    EXITL                                                            
*                                                                               
         TM    GSINDSL1,GSISKCDE   TEST KEY CODE CHANGED FLAG                   
         BO    SKEY04              CHANGED - MUST LOAD IN A NEW SCREEN          
*                                                                               
         CLC   GSSMREC,RTSREC      TEST REC/PRG HAVE ALTERED                    
         BNE   SKEY04              IF SO - NEED TO LOAD ANOTHER SCREEN          
         CLC   GSSHPRG,RTSPRG                                                   
         BNE   SKEY04                                                           
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKPRO  RESET KEY SCREEN PROTECTION             
*                                                                               
         TM    GCINDS2,GCINTRS     JUST NTRSES'D?                               
         BZ    SKEY02                                                           
         LH    R0,GSDSPOVR         YES - REPLACE KEY SCREEN FDREL BLOCK         
         A     R0,ATWA                                                          
         XR    RF,RF                                                            
         GOTOX SCRSET,RTPARM,('RTSSIBLK',(RF)),(R0)                             
*                                                                               
SKEY02   GOTOX AGEN,RTPARM,OSCRN,SMOD,('SOLDQ',FSRKPKEY)                        
*                                  ALLOW PROG TO EDIT UNCHANGED SCREEN          
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KFLD                                            
*                                  PROCESS 'KEY' FIELD IF ON SCREEN             
*                                                                               
         B     EXITOK              FLAG SCREEN AS UNCHANGED                     
*                                                                               
SKEY04   LH    R0,GSDSPOVR         NEW KEY SCREEN MUST BE LOADED                
         A     R0,ATWA                                                          
         GOTOX SCRCLR,(R0)         CLEAR SCREEN IN TWA                          
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',0)                               
*                                  CLEAR FDREL SAVE AREA                        
         XR    RF,RF                                                            
         GOTOX SCRSET,RTPARM,('RTSSISCR+RTSSIBLK',(RF)),(R0)                    
*                                  BUILD KEY SCREEN AND FDREL BLOCK             
*                                                                               
         MVC   GSSMSEQ,GS#FDR      SAVE SEQ# FOR MAINT SCREENS                  
         L     R2,4(R1)            A(END OF SCREEN) RETURNED                    
         L     RF,8(R1)            A(1ST KEY FIELD)                             
         S     RF,ATWA             DISPLACEMENT TO 1ST KEY FIELD                
         STCM  RF,3,GS1STKEY                                                    
*                                                                               
         GOTOX SCRPFK,(R2)         BUILD PFKEY LINE                             
         GOTOX SCRXMIT             RETRANSMIT WHOLE SCREEN                      
*                                                                               
         XC    GSLSTTOP,GSLSTTOP                                                
         XC    GSLSTEND,GSLSTEND                                                
*                                                                               
         S     R2,ATWA             SAVE DISPLACEMENT TO SCREEN END              
         STCM  R2,3,GSSMNTD        FOR USE IN BUILDING MAINT SCREEN             
         MVC   GSSMREC,RTSREC      SET RECORD/PROGRAM USED FOR SCREEN           
         MVC   GSSHPRG,RTSPRG                                                   
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) BACK TO NATIVE SYSTEM                 
*                                                                               
         TM    GCINDS2,GCINTRS     TEST JUST NTRSESD                            
         BNO   SKEY06              NO                                           
         LA    RF,PSSAV                                                         
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         BZ    EXITH                                                            
         GOTOX AGEN,RTPARM,OSES,SCALLED                                         
         B     EXITH               CC=HIGH FOR NEW SCREEN BUILT                 
*                                                                               
SKEY06   GOTOX AGEN,RTPARM,OKEY,KFLD                                            
*                                  PROCESS 'KEY' FIELD IF PRESENT               
*                                                                               
         B     EXIT                KFLD RETURNS CC=HIGH IF ALL IS OK            
         SPACE 2                                                                
***********************************************************************         
* BUILD GIVEN DATA PAGE (ASSUMES KEY SCREEN BUILT ALREADY)       *  2 *         
*                                                                ******         
* NTRY: GSSMPAGE = PAGE NUMBER TO DISPLAY                             *         
* EXIT: CC = EQUAL IF PAGE DISPLAYED SUCCESSFULLY                     *         
*       CC = LOW IF PAGE 1 DISPLAYED INSTEAD                          *         
***********************************************************************         
         SPACE 1                                                                
SCRPAGE  OI    GCINDS1,GCISSCRL    SET SCREEN SCROLL FLAG                       
         XR    R1,R1               TRY REQUESTED PAGE                           
         ICM   R1,1,GSSMPAGE                                                    
         BNZ   *+12                                                             
         LA    R1,1                NO PAGE REQUESTED - USE FIRST PAGE           
         STCM  R1,1,GSSMPAGE                                                    
         STCM  R1,1,RTSPAG#                                                     
         GOTOX SCRMNT,(R1)                                                      
         BE    EXITOK                                                           
         CLI   GSSMPAGE,1          DID PROG WANT PAGE 1?                        
         BNE   *+18                                                             
         MVC   FVMSGNO,=AL2(GE$NOSCR) NO SCREEN RECORD - CONTACT DDS            
         NI    GCINDS1,FF-GCISSCRL UNSET SCREEN SCROLL FLAG                     
         B     EXITL                                                            
*                                                                               
         LA    R1,1                PAGE REQUESTED DOES NOT EXIST                
         STCM  R1,1,GSSMPAGE       TRY PAGE 1                                   
         STCM  R1,1,RTSPAG#                                                     
         GOTOX SCRMNT,1                                                         
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(GE$NOSCR) NO SCREEN RECORD - CONTACT DDS            
         NI    GCINDS1,FF-GCISSCRL UNSET SCREEN SCROLL FLAG                     
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* SCROLL MAINTENANCE SCREEN BACKWARD BY 1 PAGE                   *  3 *         
*                                                                ******         
* EXIT: CC = LOW IF NOT DONE                                          *         
***********************************************************************         
         SPACE 1                                                                
SCRBACK  XR    R1,R1               BUMP PAGE# 1 LINE                            
         IC    R1,GSSMPAGE                                                      
         BCT   R1,*+8                                                           
         B     EXITL                                                            
*                                                                               
         STCM  R1,1,RTSPAG#                                                     
         GOTOX SCRMNT,(R1)                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SCROLL MAINTENANCE SCREEN FORWARD BY 1 PAGE                    *  4 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRFRWD  XR    R1,R1               INCREMENT PAGE NUMBER                        
         IC    R1,GSSMPAGE                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,1,RTSPAG#                                                     
         GOTOX SCRMNT,(R1)                                                      
         BE    EXITOK                                                           
         MVI   RTSPAG#,1                                                        
         GOTOX SCRMNT,1            NO MORE - RE-DISPLAY PAGE 1                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SPECIAL SCREEN BUILD FOR SCREEN RECORD ONLY                    *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRSCRN  L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         R3 = LOAD POINT IN TWA                       
         GOTOX SCRCLR,(R3)         CLEAR SCREEN                                 
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',0)                               
*                                                                               
         L     R2,AIOREC                                                        
         USING FSRRECD,R2                                                       
         MVC   RTSOVSYS,FSRKSYS    GET SYSTEM/PROGRAM/RECORD                    
         MVC   RTSPRG,FSRKPRG                                                   
         MVC   RTSREC,FSRKREC                                                   
         MVC   RTCTRY,FSRKCTRY     GET COUNTRY CODE                             
         XI    RTCTRY,FF           GET 'REAL' COUNTRY CODE                      
*                                                                               
         CLI   FSRKPAGE,FSRKPKEY   TEST BUILDING KEY PAGE ONLY                  
         BE    SSCRN02                                                          
         CLI   FSRKPAGE,FSRKPLST   TEST BUILDING TOP OF LIST ONLY               
         BE    SSCRN02                                                          
         CLI   FSRKPAGE,FSRKPDWN   TEST BUILDING DOWNLOAD SCREEN                
         BE    SSCRN02                                                          
         CLI   FSRKPAGE,FSRKPREP   TEST BUILDING REPORT SCREEN                  
         BE    SSCRN02                                                          
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SKSET  SET KEY SCREEN CODE                     
*                                                                               
         LA    RF,FSRKPKEY         NO - BUILD 'KEY' PAGE FIRST THEN             
         GOTOX SCRSET,RTPARM,('RTSSISCR',(RF)),(R3)                             
         L     R3,4(R1)                                                         
*                                                                               
SSCRN02  GOTOX SCRSET,RTPARM,('RTSSISCR',AIOREC),(R3)                           
         L     R3,4(R1)                                                         
         MVC   RTCTRY,CUCTRY                                                    
         GOTOX SCRPFK,(R3)                                                      
         GOTOX SCRXMIT                                                          
*                                                                               
SCRSCRNX B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST HEADER KEY SCREEN                                   *  7 *         
*                                                                ******         
* EXIT: CC = EQUAL IF SCREEN ALREADY LOADED                           *         
***********************************************************************         
         SPACE 1                                                                
SCRLSTT  GOTOX SCRHDRL             PULL OUT LIST SCREEN INTO AIO1               
         BL    EXITL                                                            
*                                                                               
         LH    R0,GSDSPOVR         NEW KEY SCREEN MUST BE LOADED                
         A     R0,ATWA                                                          
         GOTOX SCRCLR,(R0)         CLEAR SCREEN IN TWA                          
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',0)                               
*                                  CLEAR FDREL SAVE AREA                        
         GOTOX SCRSET,RTPARM,('RTSSISCR+RTSSIBLK',FSRKPLST),(R0)                
*                                  BUILD LIST SCREEN AND FDREL BLOCK            
*                                                                               
         TM    GCINDS2,GCINTRS     TEST JUST NTRSESD                            
         BNO   EXITOK              NO                                           
         LA    RF,PSSAV                                                         
         TM    PSSAV+SNINDS1-SSAVD,SNIPARMS                                     
         BZ    EXITOK                                                           
         GOTOX AGEN,RTPARM,OSES,SCALLED                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LOAD IN BOTTOM OF LIST SCREEN                                  *  8 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLSTB  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SAVE TWA ON NTRSES                                             *  9 *         
*                                                                ******         
* NTRY: P3 = A(FESD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING FESD,R5                                                          
SCRSAVE  L     R5,RTPARMS3                                                      
         LH    RE,=Y(FESTWSV-FESD)                                              
         LA    R0,FESD(RE)                                                      
         LH    R1,=Y(FESTWSVL)                                                  
         LH    RE,GSDSPSAV                                                      
         A     RE,ATWA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
SCRSAVEX B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* RESTORE TWA ON XITSES                                          * 10 *         
*                                                                ******         
* NTRY: P3 = A(FESD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING FESD,R5                                                          
SCRREST  ICM   R5,15,RTPARMS3      R5=A(FESD)                                   
         BNZ   *+6                                                              
         DC    H'0'                NO FESD - SERIOUS PROBLEM HERE               
*                                                                               
         OC    PSHOLE,PSHOLE       TEST RESTORING A HOLE                        
         BNZ   SRES02                                                           
         LH    R0,GSDSPSAV         NO  - RESTORE ENTIRE SCREEN                  
         A     R0,ATWA                                                          
         LH    R1,=Y(FESTWSVL)                                                  
         LH    RE,=Y(FESTWSV-FESD)                                              
         LA    RE,FESD(RE)                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     SRES20                                                           
*                                                                               
SRES02   LH    R0,GSDSPSAV         COPY CURRENT SCREEN INTO XIO1/XIO2           
         A     R0,ATWA                                                          
         LH    R1,GSDSPMAX                                                      
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIO1                                                          
C        USING FHD,R2              R2=A(CURRENT SCREEN)                         
         LH    R3,=Y(FESTWSV-FESD)                                              
         LA    R3,FESD(R3)                                                      
R        USING FHD,R3              R3=A(RESTORED SCREEN)                        
         LH    R4,GSDSPSAV                                                      
         A     R4,ATWA                                                          
M        USING FHD,R4              R4=A(NEW MERGED SCREEN)                      
         XR    RF,RF                                                            
*                                                                               
SRES04   CLI   C.FHLN,0            TEST END OF CURRENT SCREEN                   
         BE    SRES06                                                           
         CLI   R.FHLN,0            NO  - TEST END OF RESTORED SCREEN            
         BE    SRES12              YES - ADD CURRENT FIELD                      
         CLC   C.FHAD,R.FHAD       NO  - TEST WHICH COMES FIRST                 
         BL    SRES12                                                           
         B     SRES08                                                           
SRES06   CLI   R.FHLN,0            YES - TEST END OF RESTORED SCREEN            
         BE    SRES18              YES - FINISHED                               
*                                                                               
SRES08   DS    0H                  ADD RESTORED FIELD TO NEW SCREEN             
         IC    RF,R.FHLN                                                        
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R1,3,R.FHAD                                                      
         D     R0,=AL4(COLS#Q)     R0/R1=COLUMN/ROW                             
         CLM   R1,1,PSHROW#1       TEST FIELD IN SAVED HOLE                     
         BL    SRES10                                                           
         BH    *+12                                                             
         CLM   R0,1,PSHCOL#1                                                    
         BL    SRES10                                                           
*                                                                               
         CLM   R1,1,PSHROW#X                                                    
         BH    SRES10                                                           
         BL    *+12                                                             
         CLM   R0,1,PSHCOL#X                                                    
         BH    SRES10                                                           
*                                                                               
         EX    RF,*+4              YES - ADD FIELD TO SCREEN                    
         MVC   M.FHD(0),R.FHD                                                   
         AR    R3,RF                                                            
         B     SRES16                                                           
*                                                                               
SRES10   BXH   R3,RF,SRES04        NO  - BUMP R3 TO NEXT FIELD                  
*                                                                               
SRES12   DS    0H                  ADD CURRENT FIELD TO NEW SCREEN              
         IC    RF,C.FHLN           RF=L'CURRENT SCREEN FIELD                    
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R1,3,C.FHAD                                                      
         D     R0,=AL4(COLS#Q)     R0/R1=COLUMN/ROW                             
         CLM   R1,1,PSHROW#1       TEST FIELD IN SAVED HOLE                     
         BL    SRES14                                                           
         BH    *+12                                                             
         CLM   R0,1,PSHCOL#1                                                    
         BL    SRES14                                                           
*                                                                               
         CLM   R1,1,PSHROW#X                                                    
         BH    SRES14                                                           
         BL    *+12                                                             
         CLM   R0,1,PSHCOL#X                                                    
         BH    SRES14                                                           
         BXH   R2,RF,SRES04        YES - BUMP R2 TO NEXT FIELD                  
*                                                                               
SRES14   EX    RF,*+4              NO - ADD FIELD TO SCREEN                     
         MVC   M.FHD(0),C.FHD                                                   
         AR    R2,RF                                                            
*                                                                               
SRES16   OI    M.FHOI,FHOITR       TRANSMIT MERGED FIELD                        
         NI    M.FHOI,FF-FHOICU    TURN OFF CURSOR                              
         BXH   R4,RF,SRES04        BUMP TO NEXT FIELD                           
SRES18   XC    PSHOLE,PSHOLE                                                    
*                                                                               
SRES20   LH    R1,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     R1,ATWA                                                          
         ST    R1,FVADDR                                                        
*                                                                               
         LH    R0,GSDSPOVR         R0=A(OVERLAY SCREEN START)                   
         A     R0,ATWA                                                          
         LA    R1,TWAMSGH          TRANSMIT SCREEN & TURN OFF CURSORS           
         USING FHD,R1                                                           
         LH    RF,GSDSPMAX                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                                                             
         XR    RE,RE                                                            
SRES22   CLI   FHLN,0                                                           
         BE    SRES26                                                           
         OI    FHOI,FHOITR                                                      
         OI    FHII,FHIIVA         SET BEEN VALIDATED                           
         NI    FHOI,FF-FHOICU                                                   
         CR    R1,R0               TEST INTO THE OVERLAY SCREEN AREA            
         BL    SRES24                                                           
         C     R0,FVADDR           TEST A(OVERLAY SCREEN FIELD) SET             
         BNH   SRES24                                                           
         TM    FHAT,FHATPR                                                      
         BNZ   SRES24                                                           
         ST    R1,FVADDR           A(FIRST UNPROT OVERLAY SCREEN FIELD)         
*                                                                               
SRES24   ICM   RE,1,FHLN                                                        
         BZ    *+8                                                              
         BXLE  R1,RE,SRES22                                                     
*                                                                               
SRES26   XC    FHD(8),FHD                                                       
         MVI   FHD+1,1             SET CLEAR BEFORE AND AFTER                   
         MVI   FHD+2,1                                                          
*                                                                               
SCRRESTX B     EXITOK                                                           
         DROP  R1,R5,C,R,M                                                      
         SPACE 2                                                                
***********************************************************************         
* BUILD DOWNLOAD SCREEN                                          * 11 *         
*                                                                ******         
* EXIT: CC = EQUAL IF SCREEN ALREADY LOADED                           *         
***********************************************************************         
         SPACE 1                                                                
SCRDOWN  GOTOX SCRHDR,FSRKPDWN                                                  
         BL    EXITL                                                            
*                                                                               
         CLC   GSSMREC,RTSREC      TEST ALREADY HAVE SCREEN                     
         BNE   SDOWN06                                                          
         CLC   GSSHPRG,RTSPRG                                                   
         BNE   SDOWN06                                                          
         CLI   GSSMPAGE,FSRKPDWN   MAKE SURE IT IS THE DOWNLOAD SCREEN          
         BNE   SDOWN06                                                          
         GOTOX AGEN,RTPARM,OSCRN,SMOD,('SOLDQ',FSRKPREP)                        
*                                  ALLOW PROGRAM TO MODIFY SCREEN               
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITOK              SET SCREEN ALREADY LOADED                    
*                                                                               
SDOWN06  LH    R0,GSDSPOVR         BUILD DOWNLOAD SCREEN                        
         A     R0,ATWA                                                          
         GOTOX SCRCLR,(R0)         CLEAR TWA                                    
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',0)                               
*                                  RESET FDREL SAVE AREA                        
         XR    RF,RF                                                            
         ICM   RF,1,=AL1(FSRKPDWN)                                              
         GOTOX SCRSET,RTPARM,('RTSSISCR+RTSSIBLK',(RF)),(R0)                    
*                                  BUILD SCREEN AND SET FDREL BLOCK             
         MVC   GSSMSEQ,GS#FDR      SAVE SEQ# FOR MAINT SCREENS                  
         L     R2,4(R1)            A(CURRENT END OF SCREEN)                     
         L     RF,8(R1)            SAVE DISPLACEMENT TO 1ST KEY FIELD           
         S     RF,ATWA                                                          
         STCM  RF,3,GS1STKEY                                                    
         GOTOX SCRPFK,(R2)         BUILD PFKEY LINE                             
         GOTOX SCRXMIT             RETRANSMIT WHOLE SCREEN                      
*                                                                               
         S     R2,ATWA             SAVE DISPLACEMENT INTO TWA FOR               
         STCM  R2,3,GSSMNTD        ANY FOLLOWING SCREEN                         
         MVC   GSSMREC,RTSREC      SET PRG/REC USED FOR SCREEN                  
         MVC   GSSHPRG,RTSPRG                                                   
         MVI   GSSMPAGE,FSRKPDWN                                                
         GOTOX AGEN,RTPARM,OSCRN,SMOD,('SNEWQ',FSRKPREP)                        
*                                  ALLOW PROGRAM TO MODIFY SCREEN               
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITH               CC=HIGH FOR NEW SCREEN BUILT                 
         SPACE 2                                                                
***********************************************************************         
* ALLOW PROGRAM TO SET A SCREEN CODE FOR A DATA SCREEN           * 12 *         
* NB: VERB INVOKED ONLY IF 'USE SCREEN CODE' SET IN RECORD REC.  ******         
*                                                                     *         
* GSSMCODE CONTAINS SCREEN CODE IF SET OR X'00'                       *         
***********************************************************************         
         SPACE 1                                                                
SCRSSET  MVC   RTBYTE1,GSSMCODE                                                 
         GOTOX APRG,RTPARM,('GCBOVER',OSCRN),SSET,GSRECKEY                      
         CLC   GSSMCODE,RTBYTE1    SCREEN CODE CHANGED?                         
         BE    EXITOK                                                           
*                                                                               
         XC    GSLSTTOP,GSLSTTOP   RESET MAINT LIST DISPLACEMENTS               
         XC    GSLSTEND,GSLSTEND                                                
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,3,GSDSPPAG       IS THERE A PAGE FIELD ON SCREEN?             
         BZ    EXITOK                                                           
*                                                                               
         A     R2,ATWA             ALLOW PAGE OBJECT TO FUNCTION                
         USING FHD,R2                                                           
         NI    FHII,FF-FHIIVA                                                   
         NI    FHAT,FF-FHATPR                                                   
*                                                                               
         GOTOX AGEN2,RTPARM,OPAGE,PGCOUNT                                       
         GOTOX AGEN2,RTPARM,OPAGE,PGDIS                                         
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD REPORT SCREEN                                            * 13 *         
*                                                                ******         
* EXIT: CC = EQUAL IF SCREEN ALREADY LOADED                           *         
***********************************************************************         
         SPACE 1                                                                
SCRREP   GOTOX SCRHDR,FSRKPREP                                                  
         BL    EXITL                                                            
*                                                                               
SRPO04   CLC   GSSMREC,RTSREC      TEST ALREADY HAVE SCREEN                     
         BNE   SRPO06                                                           
         CLC   GSSHPRG,RTSPRG                                                   
         BNE   SRPO06                                                           
         CLI   GSSMPAGE,FSRKPREP   MAKE SURE IT IS THE REPORT SCREEN            
         BNE   SRPO06                                                           
         GOTOX AGEN,RTPARM,OSCRN,SMOD,('SOLDQ',FSRKPREP)                        
*                                  ALLOW PROGRAM TO MODIFY SCREEN               
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITOK              SCREEN WAS LOADED PREVIOUSLY                 
*                                                                               
SRPO06   LH    R0,GSDSPOVR         NEED TO LOAD IN REP0RT SCREEN                
         A     R0,ATWA                                                          
         GOTOX SCRCLR,(R0)                                                      
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',0)                               
         XR    RF,RF                                                            
         ICM   RF,1,=AL1(FSRKPREP)                                              
         GOTOX SCRSET,RTPARM,('RTSSISCR+RTSSIBLK',(RF)),(R0)                    
         MVC   GSSMSEQ,GS#FDR      SAVE SEQ# FOR MAINT SCREENS                  
         L     R2,4(R1)            END OF SCREEN RETURNED HERE                  
         L     RF,8(R1)            A(1ST KEY FIELD)                             
         S     RF,ATWA                                                          
         STCM  RF,3,GS1STKEY       SAVE DISPLACEMENT TO FIRST KEY FIELD         
         GOTOX SCRPFK,(R2)         BUILD PFKEY LINE                             
         GOTOX SCRXMIT             TRANSMIT SCREEN                              
*                                                                               
         S     R2,ATWA                                                          
         STCM  R2,3,GSSMNTD        SAVE DISP. TO END OF KEY SCREEN              
         MVC   GSSMREC,RTSREC      SAVE RECORD USED                             
         MVC   GSSHPRG,RTSPRG      SAVE PROGRAM USED                            
         MVI   GSSMPAGE,FSRKPREP   SET REPORT SCREEN LOADED                     
*                                                                               
         GOTOX AGEN,RTPARM,OSCRN,SMOD,('SNEWQ',FSRKPREP)                        
*                                  ALLOW PROGRAM TO MODIFY SCREEN               
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITH               CC=HIGH FOR NEW SCREEN BUILT                 
         SPACE 2                                                                
***********************************************************************         
* ALLOW PROGRAM TO MODIFY SCREEN AFTER LOADING                   * 14 *         
*                                                                ******         
* P3 B0    'SOLDQ' IF SCREEN HAS NOT BEEN LOADED THIS TIME            *         
* P3 B0    'SNEWQ' IF SCREEN HAS BEEN LOADED THIS TIME                *         
* P3 B1-3  EQUATED SCREEN NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
SCRMOD   L     RF,RTPARMS3                                                      
         GOTOX APRG,RTPARM,('GCBOVER',OSCRN),SMOD,(RF)                          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ALLOW PROGRAM TO SET A KEY CODE IF REQUIRED                    * 15 *         
* NB: VERB INVOKED ONLY IF 'USES KEY CODES' SET IN RECORD RECORD ******         
*                                                                     *         
* GSSKCODE CONTAINS SCREEN CODE IF SET OR X'00'                       *         
***********************************************************************         
         SPACE 1                                                                
SCRKCODE GOTOX APRG,RTPARM,('GCBOVER',OSCRN),SKSET                              
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT ALL TEMPORARILY PROTECTED KEY FIELDS                 * 16 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRKPRO  TM    GCINDS2,GCINTRS     JUST NTRSES'D?                               
         BO    EXITOK              YES                                          
         TM    BCINDS1,BCINACT     NEW ACTION?                                  
         BZ    EXITOK              NO                                           
*                                                                               
         LH    RF,GSDSPOVR                                                      
         A     RF,ATWA                                                          
         XR    RE,RE                                                            
         USING FHD,RF                                                           
*                                                                               
SCKP02   ICM   RE,1,FHLN           ANY MORE FIELDS TO PROCESS?                  
         BZ    EXITOK              NO                                           
*                                                                               
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BZ    SCKP04              NO                                           
         LA    R1,FHD(RE)                                                       
         SH    R1,=Y(FHDAD)                                                     
         USING FHNU,R1                                                          
         TM    FHUS,FVX1PRO        PROTECTED TEMPORARILY?                       
         BZ    SCKP04              NO                                           
*                                                                               
         NI    FHUS,FF-(FVX1PRO)                                                
         NI    FHAT,FF-(FHATPR)                                                 
         OI    FHOI,FHOITR                                                      
*                                                                               
SCKP04   LA    RF,0(RE,RF)                                                      
         B     SCKP02                                                           
         DROP  RF,R1                                                            
         SPACE 2                                                                
***********************************************************************         
***********************************************************************         
** SCREEN OBJECT ROUTINES                                            **         
***********************************************************************         
***********************************************************************         
         SPACE 4                                                                
***********************************************************************         
* FIND HEADER SCREEN RECORD FOR TEST PHASE & COUNTRY                  *         
*                                                                     *         
* NTRY: R1 = CODE FOR SCREEN TO LOAD                                  *         
* EXIT: SCREEN RECORD IN AIO1                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRHDR   NTR1                                                                   
         STC   R1,RTSPAG#          SAVE PAGE NUMBER                             
*                                                                               
         NI    GSINDSL1,FF-GSISKCDE TURN OFF KEY CODE CHANGED FLAG              
         TM    GSFRR.FRRINDS1,FRR1UKC                                           
         BZ    SHDR02              APPLICATION ABLE TO SET KEY CODE?            
*                                                                               
         MVC   RTBYTE1,GSSKCODE    SAVE PREVIOUS KEY CODE                       
         GOTOX AGEN,RTPARM,OSCRN,SKSET  SET KEY SCREEN CODE                     
         CLC   RTBYTE1,GSSKCODE    HAS IT BEEN CHANGED BY APPLICATION?          
         BE    *+8                                                              
         OI    GSINDSL1,GSISKCDE   SET KEY CODE CHANGED FLAG                    
*                                                                               
SHDR02   GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON) SWITCH TO CONTROL SYSTEM          
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SHDR04              NO TRY LIVE THEN                             
*                                                                               
K        USING FSRRECD,IOKEY                                                    
         XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSKCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCTRY,CUCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDR08              TEST SCREEN FOR COUNTRY SET UP               
*                                                                               
SHDR04   XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSKCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCTRY,CUCTRY                                                
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDR08              SCREEN FOR THIS COUNTRY SET UP               
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SHDR06              NO TRY LIVE THEN                             
*                                                                               
         XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSKCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDR08                                                           
*                                                                               
SHDR06   XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSKCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDR08                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(GE$NOSCR)  NO SCREEN RECORD DEFINED                 
         B     SHDRL                                                            
*                                                                               
SHDR08   L     R1,=AL4(XOGENFIL+XOGET+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                ERROR ON GETREC FOR GENFILE                  
*                                                                               
         L     RF,AIO1             SEE IF 'SHADOW' ELEMENT                      
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   SHDROK              NO 'SHADOW' ELEMENT                          
*                                                                               
         L     RF,12(R1)           USE 'SHADOW' SCREEN PRG/REC INSTEAD          
         MVC   RTSPRG,SPRPRG-SPRELD(RF)                                         
         MVC   RTSREC,SPRREC-SPRELD(RF)                                         
         B     SHDROK                                                           
*                                                                               
SHDROK   GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITOK                                                           
*                                                                               
SHDRL    GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* FIND HEADER SCREEN RECORD FOR TEST PHASE & COUNTRY FOR LIST         *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: SCREEN RECORD IN AIO1                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRHDRL  NTR1                                                                   
         MVI   RTSPAG#,FSRKPLST     SET PAGE NUMBER                             
         TM    GSFRR.FRRINDS1,FRR1ULC                                           
         BZ    SHDRL02             APPLICATION ABLE TO SET LIST CODE?           
         GOTOX AOLY,RTPARM,OSCRN,SLSET                                          
*                                                                               
SHDRL02  GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON) SWITCH TO CONTROL SYSTEM          
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SHDRL04             NO TRY LIVE THEN                             
*                                                                               
K        USING FSRRECD,IOKEY                                                    
         XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSLCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCTRY,CUCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDRL08             TEST SCREEN FOR COUNTRY SET UP               
*                                                                               
SHDRL04  XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSLCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCTRY,CUCTRY                                                
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDRL08             SCREEN FOR THIS COUNTRY SET UP               
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SHDRL06             NO TRY LIVE THEN                             
*                                                                               
         XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSLCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDRL08                                                          
*                                                                               
SHDRL06  XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKCODE,GSSLCODE                                              
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SHDRL08                                                          
*                                                                               
         CLI   CSREC,O#MAX             CONTROLLER CAN HAVE NO SCREEN            
         BNH   SHDROK                                                           
         OI     GSINDSL3,GSI2LMSG   SET OWN MESSAGE?                            
         MVC   FVMSGNO,=AL2(GE$NOSCR)  NO SCREEN RECORD DEFINED                 
         B     SHDRLL                                                           
*                                                                               
SHDRL08  L     R1,=AL4(XOGENFIL+XOGET+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                ERROR ON GETREC FOR GENFILE                  
*                                                                               
         L     RF,AIO1             SEE IF 'SHADOW' ELEMENT                      
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   SHDRLOK             NO 'SHADOW' ELEMENT                          
*                                                                               
         L     RF,12(R1)           USE 'SHADOW' SCREEN PRG/REC INSTEAD          
         MVC   RTSPRG,SPRPRG-SPRELD(RF)                                         
         MVC   RTSREC,SPRREC-SPRELD(RF)                                         
         B     SHDRLOK                                                          
*                                                                               
SHDRLOK  GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITOK                                                           
*                                                                               
SHDRLL   GOTOX ('SWCHFC',AGROUTS),=AL1(0) SWITCH TO NATIVE SYSTEM               
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO LOAD IN MAINTENANCE DATA SCREEN                          *         
*                                                                     *         
* NTRY: R1 = PAGE NUMBER                                              *         
***********************************************************************         
         SPACE 1                                                                
SCRMNT   NTR1  ,                                                                
         LR    R0,R1               R0 = PAGE NUMBER                             
         TM    GSFRR.FRRINDS1,FRR1USC                                           
         BZ    SMNT02              APPLICATION WISHES TO SET TYPE CODE?         
         GOTOX AGEN,RTPARM,OSCRN,SSET                                           
*                                                                               
SMNT02   GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XR    R2,R2                                                            
         ICM   R2,3,GSSMNTD                                                     
         A     R2,ATWA                                                          
         GOTOX SCRCLR,(R2)         CLEAR REST OF SCREEN                         
         XR    RF,RF                                                            
         IC    RF,GSSMSEQ                                                       
         GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'C',(RF))                            
*                                                                               
         XC    GSLSTTOP,GSLSTTOP   RESET LIST ON SCREEN INDICATORS              
         XC    GSLSTEND,GSLSTEND                                                
*                                                                               
         GOTOX SCRSET,RTPARM,('RTSSISCR+RTSSIBLK',(R0)),(R2)                    
         BNE   SCRMNTH                                                          
*                                                                               
         ICM   RF,15,8(R1)         SAVE A(1ST RECORD FIELD)                     
         BZ    *+12                                                             
         S     RF,ATWA                                                          
         STCM  RF,3,GS1STREC                                                    
         L     R2,4(R1)                                                         
         GOTOX SCRPFK,(R2)                                                      
         STC   R0,GSSMPAGE                                                      
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
*                                                                               
         NI    GSINDSL1,FF-GSIMLST                                              
         OC    GSLSTTOP,GSLSTTOP   RESET LIST ON SCREEN INDICATORS              
         BNZ   SMNT04                                                           
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   SMNT04                                                           
         B     SMNT06                                                           
*                                                                               
SMNT04   OI    GSINDSL1,GSIMLST                                                 
*                                                                               
         GOTOX AGENLST,RTPARM,OLIST,LINIT                                       
         OC    GS1STREC,GS1STREC                                                
         BNZ   SMNT06                                                           
         MVC   GS1STREC,LS1STINP                                                
*                                                                               
SMNT06   GOTOX SCRXMIT                                                          
         GOTOX AGEN,RTPARM,OPAGE,PGDIS                                          
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
*                                                                               
SCRMNTH  GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITH                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SET UP SCREEN                                            *         
*                                                                     *         
* NTRY: P1 BYTE 0 = RTSSISCR ON IF BUILDING SCREEN                    *         
*                   RTSSIBLK ON IF SETTING UP BLOCK                   *         
*             1-3 = A(SCREEN RECORD)                                  *         
*           OR  3 = PAGE NUMBER                                                 
*       P2 BYTE 0 = NEXT SEQUENCE NUMBER                              *         
*             1-3 = A(POINT IN TWA)                                   *         
* EXIT: P2 IS UPDATED                                                 *         
*       P3 = A(FIRST INPUT FIELD)                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRSET   NTR1  ,                                                                
         ST    R1,RTSSAR1          SAVE CALLER'S R1                             
         MVC   RTSSPARM,0(R1)      SET INPUT PARAMETERS                         
         XC    RTSSAINP,RTSSAINP                                                
         MVC   GSSHPRG,RTSPRG      SET PROG/REC USED FOR SCREEN                 
         MVC   GSSHREC,RTSREC                                                   
         XC    GSPGNAM,GSPGNAM                                                  
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)  SWITCH TO CONTROL SYSTEM         
*                                                                               
         OC    RTSSAREC(2),RTSSAREC                                             
         BNZ   SCRS16              RECORD ALREADY SHOULD BE IN IO1              
*                                                                               
         MVC   RTSCODE,GSSMCODE    SET SCREEN CODE                              
         CLI   RTSPAG#,FSRKPKEY    KEY SCREEN?                                  
         BNE   *+10                                                             
         MVC   RTSCODE,GSSKCODE    KEY SCREENS HAVE A DIFFERENT CODE            
         CLI   RTSPAG#,FSRKPLST    LIST SCREEN?                                 
         BNE   *+10                                                             
         MVC   RTSCODE,GSSLCODE    LIST SCREENS HAVE A DIFFERENT CODE           
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         OC    ASTEST,ASTEST       CONNECTED TO TEST PHASE?                     
         BZ    SCRS02              NO - TRY LIVE THEN                           
*                                                                               
K        USING FSRRECD,IOKEY                                                    
         XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCODE,RTSCODE                                               
         MVC   K.FSRKCTRY,RTCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS06              TEST SCREEN FOR COUNTRY SET UP               
*                                                                               
SCRS02   XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCODE,RTSCODE                                               
         MVC   K.FSRKCTRY,RTCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS06              SCREEN FOR THIS COUNTRY SET UP               
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SCRS04              NO TRY LIVE THEN                             
*                                                                               
         XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCODE,RTSCODE                                               
         MVI   K.FSRKCTRY,X'FF'    READ FOR HOST PROCESSOR                      
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS06                                                           
*                                                                               
SCRS04   XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
         MVC   K.FSRKCODE,RTSCODE                                               
         MVI   K.FSRKCTRY,X'FF'    READ FOR HOST PROCESSOR                      
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS06                                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)  SWITCH BACK                          
         MVC   FVMSGNO,=AL2(GE$NOSCR)  NO SCREEN RECORD DEFINED                 
         B     EXITH                                                            
*                                                                               
SCRS06   L     R1,=AL4(XOGENFIL+XOGET+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                I/O ERROR TO GENFILE                         
*                                                                               
         L      RF,AIO1             SEE IF PAGE NAME                            
         GOTOX  VHELLO,RTPARM,(C'G',GCFILNAM),('PNMELQ',(RF)),0                 
         CLI    12(R1),0                                                        
         BNE    *+14                NO PAGE NAME                                
         L      RF,12(R1)                                                       
         MVC    GSPGNAM,PNMNAME-PNMELD(RF)                                      
*                                                                               
         L     RF,AIO1             SEE IF 'SHADOW' ELEMENT                      
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   SCRS14              NO 'SHADOW' ELEMENT                          
*                                                                               
         L     RF,12(R1)           SAVE 'SHADOW' SCREEN PROG/REC                
         MVC   RTSPRG,SPRPRG-SPRELD(RF)                                         
         MVC   RTSREC,SPRREC-SPRELD(RF)                                         
         MVC   GSSHPRG,RTSPRG                                                   
         MVC   GSSHREC,RTSREC      SAVE THESE FOR LATER                         
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SCRS08              NO TRY LIVE THEN                             
*                                                                               
K        USING FSRRECD,IOKEY                                                    
         XC    K.FSRKEY,K.FSRKEY   READ SHADOW FOR CONNECTED COUNTRY            
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
*                                                                               
         MVC   K.FSRKCODE,GSSMCODE SET SCREEN CODE                              
         CLI   K.FSRKPAGE,FSRKPKEY IS THIS A KEY SCREEN?                        
         BNE   *+10                                                             
         MVC   K.FSRKCODE,GSSKCODE KEY SCREENS HAVE A DIFFERENT CODE            
*                                                                               
         MVC   K.FSRKCTRY,RTCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS12              TEST SCREEN FOR COUNTRY SET UP               
*                                                                               
SCRS08   XC    K.FSRKEY,K.FSRKEY   READ SHADOW FOR CONNECTED COUNTRY            
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
*                                                                               
         MVC   K.FSRKCODE,GSSMCODE SET SCREEN CODE                              
         CLI   K.FSRKPAGE,FSRKPKEY IS THIS A KEY SCREEN?                        
         BNE   *+10                                                             
         MVC   K.FSRKCODE,GSSKCODE KEY SCREENS HAVE A DIFFERENT CODE            
*                                                                               
         MVC   K.FSRKCTRY,RTCTRY   TRY TO READ FOR THIS COUNTRY                 
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS12              SCREEN FOR THIS COUNTRY SET UP               
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SCRS10              NO TRY LIVE THEN                             
*                                                                               
         XC    K.FSRKEY,K.FSRKEY   READ SHADOW FOR HOST PROCESSOR               
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
*                                                                               
         MVC   K.FSRKCODE,GSSMCODE SET SCREEN CODE                              
         CLI   K.FSRKPAGE,FSRKPKEY IS THIS A KEY SCREEN?                        
         BNE   *+10                                                             
         MVC   K.FSRKCODE,GSSKCODE KEY SCREENS HAVE A DIFFERENT CODE            
*                                                                               
         MVI   K.FSRKCTRY,X'FF'    READ FOR HOST PROCESSOR                      
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS12                                                           
*                                                                               
SCRS10   XC    K.FSRKEY,K.FSRKEY   READ SHADOW FOR HOST PROCESSOR               
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,RTSOVSYS                                               
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVC   K.FSRKPAGE,RTSPAG#                                               
*                                                                               
         MVC   K.FSRKCODE,GSSMCODE SET SCREEN CODE                              
         CLI   K.FSRKPAGE,FSRKPKEY IS THIS A KEY SCREEN?                        
         BNE   *+10                                                             
         MVC   K.FSRKCODE,GSSKCODE KEY SCREENS HAVE A DIFFERENT CODE            
*                                                                               
         MVI   K.FSRKCTRY,X'FF'    READ FOR HOST PROCESSOR                      
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SCRS12                                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)  SWITCH BACK                          
         MVC   FVMSGNO,=AL2(GE$NOSCR)  NO SCREEN RECORD DEFINED                 
         B     EXITH                                                            
*                                                                               
SCRS12   L     R1,=AL4(XOGENFIL+XOGET+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1             SEE IF PAGE NAME                             
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('PNMELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   *+14                NO PAGE NAME                                 
         L     RF,12(R1)                                                        
         MVC   GSPGNAM,PNMNAME-PNMELD(RF)                                       
*                                                                               
SCRS14   MVC   RTSSAREC,AIO1+1     SAVE A(SCREEN RECORD)                        
         DROP  K                                                                
*                                                                               
SCRS16   L     R4,RTSSATWA                                                      
         USING FHD,R4              R4=TWA BUILD AREA                            
         XR    RF,RF                                                            
         ICM   RF,7,RTSSAREC       GET A(SCREEN RECORD)                         
         USING FSRRECD,RF                                                       
*                                                                               
         MVI   RTSSELQ,FDRELQ      SAVE ELEMENT CODE REQUIRED                   
         CLI   FSRKPAGE,FSRKPLST   LIST SCREENS NEED FLTRL'S                    
         BNE   *+8                                                              
         MVI   RTSSELQ,FLTRLQ                                                   
         LA    R3,FSRRECD+FSRFIRST                                              
         DROP  RF                                                               
         USING FSRELD,R3           R3=A(FIELD SCREEN ELEMENT)                   
*                                                                               
SSET10   CLI   FSREL,0             END OF RECORD                                
         BE    SSET52              YES                                          
         CLI   FSREL,FSRELQ        SCREEN ELEMENT?                              
         BNE   SSET50              NO                                           
*                                                                               
         TM    FSRIND1,FSRITAG     TEST TAG ONLY ELEMENT                        
         BZ    SSET12                                                           
*                                                                               
         XR    RE,RE               ADD TAG FIELD TO SCREEN                      
         IC    RE,FSRTLN           LENGTH OF TAG                                
         LA    R0,FHDAD(RE)                                                     
         STC   R0,FHLN             SET FIELD LENGTH                             
         MVC   FHAT,FSRTFHAT       SET ATTRIBUTES                               
         OI    FHAT,FHATPR         TAGS ARE ALWAYS PROTECTED                    
         MVC   FHAD,FSRPOSN        SET FIELD POSITION                           
         MVC   FHIL,FSRTFHXT       SET EXTENDED ATTRIBUTE                       
         CLI   FHIL,0                                                           
         BE    *+8                                                              
         MVI   FHII,FHIXAT                                                      
         MVC   FHDA(L'FSRTDICT),FSRTDICT                                        
         ICM   RF,15,=C'SL  '      TRANSLATE DICTIONARY REFERENCE               
         ICM   RF,2,RTSOVSYS                                                    
         GOTOX VDICTAT,RTPARM,(RF),FHDA                                         
         AR    R4,R0               INCREMENT TWA POINTER                        
         B     SSET50              NEXT SCREEN ELEMENT IN RECORD                
*                                                                               
SSET12   TM    FSRIND1,FSRSLST     TEST START OF LIST ELEMENT                   
         BZ    SSET26              NO                                           
*                                                                               
         TM    RTSSINDS,RTSSIBLK   TEST BUILDING SCREEN ONLY                    
         BZ    *+14                                                             
         MVC   GSLSTTOP,FSRPOSN    SET TOP OF LIST FOR LIST CONTROLLER          
         B     SSET50                                                           
*                                                                               
         XR    RE,RE               ADD 'SOL' FIELD FOR BUILD ACTION             
         LA    RE,3                LENGTH OF 'SOL'                              
         LA    R0,FHDAD(RE)                                                     
         STC   R0,FHLN                                                          
         MVC   FHAT,FSRTFHAT                                                    
         OI    FHAT,FHATPR                                                      
         MVC   FHAD,FSRPOSN        SET POSITION OF LIST START ON SCREEN         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FSRPOSN                                                     
         XR    RE,RE                                                            
         D     RE,=F'80'                                                        
         STH   RE,RTHALF1          RTHALF1=DISP INTO START LINE                 
         MH    RF,=H'80'                                                        
         STH   RF,RTHALF2          RTHALF2=DISP TO START OF START LINE          
         XR    RF,RF                                                            
         ICM   RF,7,RTSSAREC       GET A(SCREEN RECORD)                         
         LA    RF,FSRFIRST(RF)                                                  
EOL      USING FSRELD,RF           R3=A(FIELD SCREEN ELEMENT)                   
         XR    RE,RE                                                            
*                                                                               
SSET14   CLI   EOL.FSREL,0         LOOK FOR 'EOL' ELEMENT                       
         BE    SSET18              NOT FOUND                                    
         CLI   EOL.FSREL,FSRELQ                                                 
         BNE   SSET16                                                           
         TM    EOL.FSRIND1,FSRELST                                              
         BO    SSET20              FOUND ONE                                    
*                                                                               
SSET16   IC    RE,EOL.FSRLN        ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     SSET14                                                           
*                                                                               
SSET18   MVC   FHIL,3              END OF LIST NOT YET SET                      
         MVC   FHDA(3),=C'SOL'     MOVE IN MARKER FOR START OF LIST             
         AR    R4,R0                                                            
         B     SSET50              NEXT SCREEN ELEMENT                          
*                                                                               
SSET20   XR    R1,R1               FOUND END OF LIST ELEMENT                    
         ICM   R1,3,EOL.FSRPOSN                                                 
         XR    R0,R0                                                            
         D     R0,=F'80'                                                        
         AH    R0,=H'1'                                                         
         STH   R0,RTHALF3          RTHALF3=DISP INTO END LINE                   
         MH    R1,=H'80'                                                        
         STH   R1,RTHALF4          RTHALF4=DISP TO START OF END LINE            
         DROP  EOL                                                              
*                                                                               
         LH    RF,RTHALF1                                                       
         LH    RE,RTHALF3                                                       
         SR    RE,RF               RE HOLDS WIDTH OF LIST                       
         XR    RF,RF                                                            
         ICM   RF,3,FHAD           GET ADDRESS OF THIS FIELD                    
*                                                                               
SSET22   LA    R0,FHDAD(RE)        SET FIELD LENGTH                             
         STC   R0,FHLN                                                          
         MVI   FHAT,FHATPR         SET FIELD PROTECTED                          
         STCM  RF,3,FHAD           SAVE START OF FIELD                          
*                                                                               
         MVI   FHDA,C'*'           SET SOLID BLOCK FOR LIST WIDTH               
         SH    RE,=H'2'            1 THERE ALREADY, 1 FOR THE EX                
         BM    SSET24              WIDTH 1 LIST - NOT PERMITTED                 
         EX    RE,*+4                                                           
         MVC   FHDA+1(0),FHDA                                                   
         LA    RE,2(RE)            SET LENGTH IN RE CORRECT AGAIN               
         STC   RE,FHIL                                                          
*                                                                               
SSET24   AR    R4,R0               BUMP TWA                                     
         CLM   RF,3,RTHALF4        RTHALF4=DISP TO START OF END LINE            
         BH    SSET50              END OF LIST LINE DONE                        
*                                                                               
         LA    RF,80(RF)           SET TO NEXT LINE DOWN                        
         B     SSET22              CARRY ON DOING LINES                         
*                                                                               
SSET26   TM    FSRIND1,FSRELST     TEST END OF LIST ELEMENT                     
         BZ    SSET34                                                           
         TM    RTSSINDS,RTSSIBLK   TEST BUILDING SCREEN ONLY                    
         BZ    *+14                                                             
         MVC   GSLSTEND,FSRPOSN    SET END OF LIST FOR CONTROLLER               
         B     SSET50              NEXT ELEMENT                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,RTSSAREC       GET A(SCREEN RECORD)                         
         LA    RF,FSRFIRST(RF)                                                  
SOL      USING FSRELD,RF           R3=A(FIELD SCREEN ELEMENT)                   
         XR    RE,RE                                                            
*                                                                               
SSET28   CLI   SOL.FSREL,0         LOOK FOR 'SOL' ELEMENT                       
         BE    SSET32                                                           
         CLI   SOL.FSREL,FSRELQ                                                 
         BNE   SSET30                                                           
         TM    SOL.FSRIND1,FSRSLST                                              
         BO    SSET50              FOUND IT - PROCESSING HANDLED BY SOL         
*                                                                               
SSET30   IC    RE,SOL.FSRLN        ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     SSET28                                                           
         DROP  SOL                                                              
*                                                                               
SSET32   XR    RE,RE               ADD EOL FIELD                                
         LA    RE,3                LENGTH OF 'EOL'                              
         LA    R0,FHDAD(RE)                                                     
         STC   R0,FHLN                                                          
         MVC   FHAT,FSRTFHAT                                                    
         OI    FHAT,FHATPR                                                      
         XR    R1,R1                                                            
         ICM   R1,3,FSRPOSN                                                     
         SH    R1,=H'3'                                                         
         STCM  R1,3,FHAD                                                        
         MVC   FHIL,3                                                           
         MVC   FHDA(3),=C'EOL'                                                  
         AR    R4,R0                                                            
         B     SSET50              NEXT ELEMENT IN RECORD                       
*                                                                               
         PUSH  USING                                                            
         USING FDRRECD,IOKEY                                                    
SSET34   XC    FDRKEY,FDRKEY       READ FIELD RECORD FROM XA INTO AIO2          
         MVI   FDRKMIN,FDRKMINQ                                                 
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,RTSOVSYS                                                 
         MVC   FDRKPRG,RTSPRG                                                   
         MVC   FDRKREC,RTSREC                                                   
         MVC   FDRKNUM,FSRNUM                                                   
         MVC   FDRKCTRY,RTCTRY                                                  
         XI    FDRKCTRY,X'FF'                                                   
         MVI   FDRKSUB,X'FF'                                                    
         MVC   FDRKTEST,ASTEST    TEST PHASE                                    
         GOTOX ('GETFLD',AGROUTS),RTPARM,GFREAD,AIO2                            
         BE    *+6                                                              
         DC    H'0'                RECORD NOT IN GETFLD BUFFER                  
         POP   USING                                                            
*                                                                               
         L     R2,AIO2             FIELD RECORD IS IN AIO2                      
         LA    R2,FDRFIRST(R2)                                                  
         USING FDRELD,R2                                                        
         XR    RF,RF                                                            
SSET36   CLC   FDREL,RTSSELQ       LOOK FOR CORRECT FIELD ELEMENT               
         BE    SSET38                                                           
         CLI   FDREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEMENT IS NOT ON RECORD                     
         IC    RF,FDRLN                                                         
         BXH   R2,RF,SSET36                                                     
*                                                                               
SSET38   TM    FDRINDS1,FDR1DDS    TEST DDS ONLY FIELD                          
         BZ    *+12                NO                                           
         TM    CUSTAT,CUSDDS       DDS TERMINAL?                                
         BZ    SSET50              NO - IGNORE FIELD                            
*                                                                               
         TM    RTSSINDS,RTSSIBLK   TEST BUILDING BLOCK                          
         BZ    SSET42              NO                                           
         CLI   FDRSEC,0            TEST FIELD SECURITY SET                      
         BE    SSET40              NO                                           
         GOTOX ('FLDSEC',AGROUTS),FDRSEC                                        
         BL    SSET50              INVALID - IGNORE THIS FIELD                  
         BE    *+8                                                              
         OI    FDRINDS1,FDR1PRO    VALID FOR READ ONLY                          
*                                                                               
SSET40   GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'A',FDRELD)                          
*                                                                               
SSET42   TM    FDRINDS1,FDR1XTAG   TEST FOR SUPPRESS TAG ON FIELD               
         BO    SSET44              YES                                          
         XR    RE,RE               ADD FIELD TAG                                
         IC    RE,FDRTDEFL                                                      
         LA    R0,FHDAD(RE)                                                     
         STC   R0,FHLN                                                          
         MVC   FHAT,FDRTFHAT                                                    
         OI    FHAT,FHATPR                                                      
         MVC   FHAD,FSRPOSN                                                     
         MVC   FHIL,FDRTFHXT                                                    
         CLI   FHIL,0                                                           
         BE    *+8                                                              
         MVI   FHII,FHIXAT                                                      
         MVC   FHDA(L'FDRRTAG),FDRRTAG                                          
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,RTSOVSYS                                                    
         GOTOX VDICTAT,RTPARM,(RF),FHDA                                         
         AR    R4,R0               BUMP TWA POINTER                             
*                                                                               
SSET44   XR    RE,RE               ADD INPUT FIELD NEXT TO TAG                  
         IC    RE,FDRFDEFL                                                      
         LA    R0,FHDAD+FHDAD(RE)  INPUT FIELDS HAVE EXTENDED HEADERS           
         STC   R0,FHLN                                                          
         LA    R1,FHDA(RE)                                                      
X        USING FHNU,R1             R1=A(EXTENDED HEADER)                        
         MVC   FHAT,FDRFHAT                                                     
         OI    FHAT,FHATXH                                                      
         TM    FDRINDS1,FDR1PRO                                                 
         BZ    *+8                                                              
         OI    FHAT,FHATPR                                                      
*                                                                               
         ICM   RF,3,FSRPOSN        POSITION AFTER TAG                           
         TM    FDRINDS1,FDR1XTAG                                                
         BO    *+12                                                             
         IC    RE,FDRTDEFL                                                      
         LA    RF,1(RE,RF)                                                      
         STCM  RF,3,FHAD                                                        
*                                                                               
         TM    RTSSINDS,RTSSIBLK   BUILDING SCREEN ONLY?                        
         BO    SSET46                                                           
         MVI   FHDA,C'_'           YES - UNDERSCORE INPUT FIELDS                
         IC    RE,FDRFDEFL                                                      
         SH    RE,=H'2'            1 THERE ALREADY, 1 FOR THE EX                
         BM    SSET46                                                           
*                                                                               
         EX    RE,*+4                                                           
         MVC   FHDA+1(0),FHDA                                                   
*                                                                               
SSET46   MVC   X.FHNU,FDRHLP       SET HELP NUMBER                              
         MVC   X.FHSC,RTSREC       USE RECORD NUMBER FOR SCREEN CODE            
         MVC   X.FHXA,FDRFHXA                                                   
         XC    X.FHUS,X.FHUS                                                    
         MVC   X.FHUS+1(1),GS#FDR                                               
         DROP  X                                                                
*                                                                               
         TM    FHAT,FHATPR         TEST INPUT FIELD                             
         BO    SSET48              NO                                           
         OC    RTSSAINP,RTSSAINP   TEST HAVE A(1ST INPUT FIELD) SAVED           
         BNZ   SSET48                                                           
         ST    R4,RTSSAINP         NO - SAVE IT                                 
*                                                                               
SSET48   AR    R4,R0               INCREMENT TWA POINTER                        
         B     SSET50                                                           
*                                                                               
SSET50   XR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,FSRLN                                                         
         BXH   R3,RF,SSET10                                                     
         DROP  R3                                                               
*                                                                               
SSET52   ST    R4,RTSSATWA         SAVE DISPLACEMENT REACHED                    
         DROP  R4                                                               
*                                                                               
         USING FSRRECD,RF                                                       
         XR    RF,RF                                                            
         ICM   RF,7,RTSSAREC       GET A(SCREEN RECORD)                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,FSRKPAGE       PAGE NUMBER OF SCREEN LOADED                 
         ICM   R0,8,=AL1(SNEWQ)    SET NEW SCREEN LOADED                        
*                                                                               
         CLM   R0,1,=AL1(FSRKPKEY) KEY OR LIST SCREEN ONLY                      
         BE    *+12                                                             
         CLM   R0,1,=AL1(FSRKPLST)                                              
         BNE   SSET54                                                           
*                                                                               
         GOTOX ('PROSCR',AGROUTS)  PROTECT SCREEN IF NECESSARY                  
*                                                                               
SSET54   GOTOX ('SWCHFC',AGROUTS),=AL1(0)  SWITCH BACK                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,RTSSAREC       GET A(SCREEN RECORD)                         
         USING FSRRECD,RF                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,FSRKPAGE       PAGE NUMBER OF SCREEN LOADED                 
         ICM   R0,8,=AL1(SNEWQ)    SET NEW SCREEN LOADED                        
         GOTOX AGEN,RTPARM,OSCRN,SMOD,(R0)                                      
*                                  ALLOW PROGRAM TO MODIFY SCREEN               
*                                                                               
         L     R1,RTSSAR1          RESTORE CALLER'S PARAMETERS                  
         MVC   0(L'RTSSPARM,R1),RTSSPARM                                        
         B     EXITOK                                                           
         DROP  R2,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO CLEAR TWA                                                *         
*                                                                     *         
* NTRY: R1 = A(TWA POINT)                                             *         
***********************************************************************         
         SPACE 1                                                                
SCRCLR   NTR1  ,                                                                
         LR    RE,R1                                                            
         LH    RF,GSDSPMAX                                                      
         BCTR  RF,0                                                             
         A     RF,ATWA                                                          
         SR    RF,RE                                                            
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO BUILD PFK LINE                                           *         
*                                                                     *         
* NTRY: R1 = A(POINT IN TWA)                                          *         
***********************************************************************         
         SPACE 1                                                                
SCRPFK   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING PFKFLDD,R3                                                       
         USING FHD,PFKFLDH                                                      
         XC    PFKFLDD(PFKFLDL),PFKFLDD                                         
         MVI   FHLN,PFKFLDL                                                     
         MVI   FHAT,FHATLC+FHATPR+FHATXH+FHATHI                                 
         MVC   FHAD,=AL2(((ROWS#Q-1)*COLS#Q)+1)                                 
         MVC   PFKFLD,BCSPACES                                                  
         MVI   PFKFLDX,FD#PFK                                                   
         LA    R3,PFKFLDL(R3)                                                   
         MVC   0(L'EOS,R3),EOS                                                  
SCRPFKX  B     EXIT                                                             
*                                                                               
EOS      DC    X'0001010000008000' END-OF-SCREEN DATA                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* TRANSMIT ALL FIELDS ON SCREEN                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
SCRXMIT  NTR1  ,                                                                
         LA    R1,TWASCR           TRANSMIT SCREEN                              
         USING FHD,R1                                                           
         XR    RF,RF                                                            
         ICM   RF,1,FHLN                                                        
         BZ    *+12                                                             
         OI    FHOI,FHOITR                                                      
         BXH   R1,RF,*-12                                                       
         B     EXIT                                                             
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* NTRY: P1 = OBJECT CODE                                              *         
* NTRY: P2 = EQUATED VERB                                             *         
* NTRY: P3 = A(KEY BUFFER)                                            *         
* NTRY: P4 = SUB-ACTION (INTERNAL VERBS ONLY)                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
KEY      LR    R7,RF                                                            
         LM    R0,R3,0(R1)                                                      
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFST)       1                       
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLST)        2                       
         DC    AL1(KDIS),AL1(0,0,0),AL4(KEYDIS)         3                       
         DC    AL1(KVAL),AL1(0,0,0),AL4(KEYVAL)         4                       
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KEYFDIS)       5                       
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KEYFVAL)       6                       
*??USED  DC    AL1(KGET),AL1(0,0,0),AL4(EXITOK)         7                       
         DC    AL1(KSAVE),AL1(0,0,0),AL4(KEYSVE)        8                       
         DC    AL1(KRES),AL1(0,0,0),AL4(KEYRES)         9                       
         DC    AL1(KLBUILD),AL1(0,0,0),AL4(KEYBLD)     10                       
*??WHY   DC    AL1(KMBUILD),AL1(0,0,0),AL4(EXITOK)     11                       
         DC    AL1(KHEIR),AL1(0,0,0),AL4(KEYHEIR)      12                       
         DC    AL1(KUPD),AL1(0,0,0),AL4(EXITOK)        13                       
         DC    AL1(KMASK),AL1(0,0,0),AL4(EXITOK)       14                       
         DC    AL1(KFLD),AL1(0,0,0),AL4(KEYFLD)        15                       
         DC    AL1(KCHECK),AL1(0,0,0),AL4(EXITOK)      16                       
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY VERB 'VERB'                                 *  1 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYFST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY VERB 'VERB'                                  *  2 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY                                                    *  3 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYDIS   GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KFIRST,(R2),KDIS                    
         BL    EXITL               ERROR ON FIRST FOR DISPLAY                   
*                                                                               
         L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         START OF USER PORTION OF SCREEN              
         USING FHD,R3                                                           
         XR    R5,R5                                                            
KDIS02   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KDIS14              YES                                          
*                                                                               
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BO    *+12                YES                                          
         ST    R3,RTTAG            MUST BE A TAG FIELD THEN                     
         B     KDIS12                                                           
*                                                                               
         OC    GSLSTTOP,GSLSTTOP   MAINTENANCE LIST ON THIS SCREEN?             
         BNZ   KDIS04              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   KDIS04              YES                                          
         B     KDIS06              NO                                           
*                                                                               
KDIS04   CLC   FHAD,GSLSTTOP       HAVE WE REACHED TOP OF LIST YET?             
         BL    KDIS06              NO                                           
         CLC   FHAD,GSLSTEND       ARE WE PAST THE LIST YET?                    
         BNH   KDIS12              NO                                           
*                                                                               
KDIS06   ST    R3,FVADDR           SAVE A(CURRENT FIELD)                        
         LA    RF,FHD(R5)          EXTRACT EXTENDED FIELD HEADER                
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)                                                    
*                                                                               
         XR    R4,R4                                                            
         ICM   R4,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KDIS12              NOT A DATA FIELD IF THIS IS ZERO             
*                                                                               
         BCTR  R4,0                MAKE IT ZERO-BASED                           
         L     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         SLL   R4,2                                                             
         LA    RF,0(R4,RF)         INDEX INTO THE LIST OF FIELDS                
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         USING FDRELD,RF                                                        
*                                                                               
KDIS08   TM    FDRLVL,FDRIKEY      IS THIS FIELD A KEY FIELD?                   
         BZ    KDIS12              NO - IGNORE IT                               
*                                                                               
         TM    FDRINDS1,FDR1XTAG   DOES THIS FIELD HAVE A TAG?                  
         BO    KDIS10              NO                                           
         DROP  RF                                                               
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DMHED,(RF),RTTAG                               
         BL    EXITL               ERROR ON TAG AMEND                           
*                                                                               
KDIS10   GOTOX AGEN,RTPARM,(0,ODATA),DDIS,FHD,0                                 
         BNE   EXIT                ERROR ON DISPLAY                             
*                                                                               
KDIS12   LA    R3,0(R5,R3)         NEXT FIELD ON SCREEN                         
         B     KDIS02                                                           
*                                                                               
KDIS14   GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KLAST,(R2),KDIS                     
         BL    EXIT                ERROR ON LAST FOR DISPLAY                    
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY                                                   *  4 *         
*                                                                ******         
*                                                                     *         
* XIT: DIRECTORY RECORD LEFT ALIGNED IN GSRECKEY                      *         
* XIT: DIRECTORY STATUS LEFT ALIGNED IN GSRECSTA                      *         
* XIT: LINKED FILE D/A SET INTO GSRECDA (X'FFFFFFFF' IF NOT ON FILE)  *         
* XIT: VALID ACTION MASK AT DIRECTORY LEVEL SET INTO GSRECMSK         *         
***********************************************************************         
         SPACE 1                                                                
KEYVAL   XC    GSRECKEY,GSRECKEY   CLEAR CURRENT KEY BUFFER                     
         XC    GSRECSTA,GSRECSTA   CLEAR CURRENT STATUS BUFFER                  
         XC    GSRECDA,GSRECDA     SET CURRENT DIRECTORY RECORD UNREAD          
         MVC   GSRECMSK,BCEFFS     RESET CURRENT VALID ACTION MASK              
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KFIRST,(R2),KVAL                    
         BL    EXITL               ERROR ON FIRST FOR VALIDATE                  
*                                                                               
         LH    R3,GSDSPOVR                                                      
         A     R3,ATWA             START OF USER PROTION OF SCREEN              
         USING FHD,R3                                                           
         XR    R5,R5                                                            
KVAL02   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KVAL14              YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER ON FIELD?              
         BO    *+12                YES                                          
         ST    R3,RTTAG            MUST BE A TAG FIELD THEN                     
         B     KVAL12                                                           
*                                                                               
         OC    GSLSTTOP,GSLSTTOP   MAINTENANCE LIST ON THIS SCREEN?             
         BNZ   KVAL04              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   KVAL04              YES                                          
         B     KVAL06              NO                                           
*                                                                               
KVAL04   CLC   FHAD,GSLSTTOP       REACHED TOP OF LIST YET?                     
         BL    KVAL06              NO                                           
         CLC   FHAD,GSLSTEND       REACHED END OF LIST YET?                     
         BNH   KVAL12              NO                                           
*                                                                               
KVAL06   ST    R3,FVADDR           SAVE A(THIS FIELD)                           
         LA    RF,FHD(R5)          EXTRACT EXTENDED FIELD HEADER                
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)                                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KVAL12              NOT A DATA FIELD IF ZERO                     
*                                                                               
         BCTR  RF,0                MAKE ZERO-BASED                              
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FORMATTED FIELD ELEMENTS          
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                FIELD NOT IN LIST OF FIELDS                  
         USING FDRELD,RF                                                        
*                                                                               
         TM    FDRLVL,FDRIKEY      KEY FIELD?                                   
         BZ    KVAL12              NO                                           
*                                                                               
         TM    FDRINDS1,FDR1XTAG   TAG FIELD ASSOCIATED?                        
         BO    KVAL08              NO                                           
         DROP  RF                                                               
         GOTOX AGEN,RTPARM,ODATA,DMHED,(RF),RTTAG                               
         BL    EXITL               ERROR ON DISPLAY OF TAG FIELD                
*                                                                               
KVAL08   TM    GCINDS1,GCIPROT     RESET IF KEY CHANGES                         
         BO    KVAL09              NO                                           
         TM    FVIXUS1,FVX1PRO     IS THIS KEY UNPROTECTED NORMALLY?            
         BO    KVAL10              YES                                          
*                                                                               
KVAL09   TM    FHAT,FHATPR         FIELD PROTECTED?                             
         BZ    KVAL10              NO - CAN BE VALIDATED THEN                   
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DDIS,FHD,0                                     
         BL    EXIT                ERROR ON DISPLAY OF FIELD                    
         BH    EXITND                                                           
         B     KVAL12                                                           
*                                                                               
KVAL10   GOTOX AGEN,RTPARM,ODATA,DVAL,FHD,0                                     
         BL    EXIT                ERROR ON VALIDATE OF FIELD                   
         BH    EXITND                                                           
*                                                                               
KVAL12   LA    R3,0(R3,R5)         ITERATE FIELDS                               
         B     KVAL02                                                           
         DROP  R3                                                               
*                                                                               
KVAL14   GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KLAST,(R2),KVAL                     
         BL    EXITL               ERROR ON LAST FOR KEY                        
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IDIRGET  GET DIRECTORY RECORD                    
         BL    EXITL                                                            
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KCHECK,(R2)                         
         BL    EXITL               RECORD NOT VALID FOR THIS ACTION             
*                                                                               
         CLI   TWASESNL,1          ARE WE NTRSES'D?                             
         BNH   EXITOK              NO                                           
         TM    GCINDS2,GCINTRS+GCIXITS DID WE JUST CHANGE SESSION?              
         BNZ   EXITOK              YES                                          
         CLC   GSRECKEY,GCLASKEY   HAVE THE USERS CHANGED THE KEY?              
         BE    EXITOK              NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OSES,SRESLVL                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LEVEL FILTERS ONTO SCREEN                          *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYFDIS  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KFIRST,(R2),KFDIS                   
         BL    EXITL               ERROR ON FIRST FOR KEY FLT DISPLAY           
*                                                                               
         L     R3,ATWA             START OF USER SCREEN                         
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
KFDIS02  ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KFDIS06             YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BZ    KFDIS04             NO - IGNORE IT                               
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED HEADER                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KFDIS04             NOT A DATA FIELD IF ZERO                     
*                                                                               
         BCTR  RF,0                MAKE ZERO-BASED                              
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FORMATTED FIELD ELEMENTS          
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                FIELD NOT IN LIST OF FIELDS                  
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY  KEY FIELD?                            
         BZ    KFDIS04             NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DFDIS,FHD,(R2),0                               
         BL    EXITL               ERROR ON FILTER DISPLAY                      
*                                                                               
KFDIS04  LA    R3,0(R5,R3)         ITERATE FIELDS ON SCREEN                     
         LH    RF,LSHEDDSP         DISP TO START OF LIST HEADINGS               
         A     RF,ATWA             RF=A(START OF LIST)                          
         CR    R3,RF                                                            
         BL    KFDIS02             ONLY DISPLAY FOR FILTER SCREEN               
*                                                                               
KFDIS06  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KLAST,(R2),KFDIS                    
         BL    EXIT                ERROR ON LAST TIME CALL                      
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD FLTELS AND INITIAL KEY FOR LIST HEADER KEY FIELDS        *  6 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYFVAL  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KFIRST,(R2),KFVAL                   
         BL    EXITL               ERROR ON FIRST FOR KEY FLT VALIDATE          
*                                                                               
         L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         START OF USER SCREEN                         
         USING FHD,R3                                                           
         XR    R5,R5                                                            
         NI    RTFILTX,FF-(RTFILTST) NO DEFAULTS OVERRIDDEN YET                 
*                                                                               
KFVAL02  ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KFVAL12             YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BZ    KFVAL10             NO - IGNORE FIELD                            
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED FIELD HEADER                
*                                                                               
         XR    R4,R4                                                            
         ICM   R4,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KFVAL10             NOT AN INPUT FIELD IF ZERO                   
         BCTR  R4,0                MAKE IT ZERO-BASED                           
         SLL   R4,2                                                             
         A     R4,AFDRADDR         START OF FORMATTED ELEMENTS                  
         ICM   R4,15,0(R4)         A(THIS FIELD ENTRY)                          
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         USING FDRELD,R4                                                        
*                                                                               
         TM    GCINDS1,GCIPROT     RESET IF KEY CHANGES                         
         BO    KFVAL04             NO                                           
         TM    FVIXUS1,FVX1PRO     IS THIS KEY UNPROTECTED NORMALLY?            
         BO    KFVAL06             YES                                          
*                                                                               
KFVAL04  TM    FHAT,FHATPR         PROTECTED FIELD?                             
         BO    *+12                YES                                          
*                                                                               
KFVAL06  TM    FDRINDS2,FDR2FILT   FILTER FIELD?                                
         BO    KFVAL08             YES                                          
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DFDIS,FHD,(R2),0                               
         BL    EXITL               CAN`T VALIDATE A PROTECTED FIELD             
         B     KFVAL10                                                          
*                                                                               
KFVAL08  MVC   RTKEY,0(R2)         SAVE CURRENT STATE OF KEY                    
         GOTOX AGEN,RTPARM,(0,ODATA),DFVAL,FHD,(R2),0                           
         BL    EXITL               ERROR ON VALIDATE OF FILTER                  
*                                                                               
         CLI   GCFLAG,X'FF'        OVERRIDE FLAG PASSED BACK HERE               
         BNE   *+8                                                              
         OI    RTFILTX,RTFILTST    DEFAULTS HAVE BEEN OVERRIDDEN                
*                                                                               
         TM    RTFILTX,RTFILTST    ANY DEFAULTS OVERRIDEN IN THIS KEY?          
         BZ    *+10                YES - IGNORE KEY AFTER THIS POINT            
         MVC   0(L'RTKEY,R2),RTKEY KEY OBTAINED THUS FAR IS VALID               
*                                                                               
KFVAL10  LA    R3,0(R5,R3)         ITERATE FIELDS                               
         LH    RF,LSHEDDSP         DISPLACEMENT TO LIST HEADINGS                
         A     RF,ATWA                                                          
         CR    R3,RF               REACHED START OF LIST YET?                   
         BL    KFVAL02             NO                                           
*                                                                               
KFVAL12  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KLAST,(R2),KFVAL                    
         BL    EXIT                ERROR ON LAST TIME CALL                      
*                                                                               
         CLI   TWASESNL,1          ARE WE NTRSES'D?                             
         BNH   EXITOK              NO                                           
         TM    GCINDS2,GCINTRS+GCIXITS DID WE JUST CHANGE SESSION?              
         BNZ   EXITOK              YES                                          
         TM    LSSCIND1,LSSCIFLT   LIST KEY FILTERS CHANGED?                    
         BZ    EXITOK              NO                                           
         TM    GCINDS1,GCIPROT     RESET IF FILTERS CHANGE?                     
         BZ    EXITOK              NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OSES,SRESLVL                                         
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* SAVE KEY FIELD DATA                                            *  8 *         
*                                                                ******         
* NTRY: FVIFLD=FIELD                                                  *         
* NTRY: FVIHDR=EXTRACTED FIELD HEADER                                 *         
* NTRY: FVIXHDR=EXTRACTED EXTENDED HEADER                             *         
***********************************************************************         
         SPACE 1                                                                
KEYSVE   TM    FVIHDR+FHATD,FHATPR PROTECTED FIELD?                             
         BO    EXITOK              IGNORE IT THEN                               
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BNZ   *+6                                                              
         DC    H'0'                FIELD IS NOT VALID                           
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         PUSH  USING                                                            
         USING FDRELD,RF                                                        
TEMP     USING KEYELD,RTWORK                                                    
         XC    RTWORK,RTWORK       TEMP KEYEL BUILD AREA                        
         MVI   TEMP.KEYEL,KEYELQ                                                
         MVC   TEMP.KEYNUM,FDRNUM  SAVE UNIQUE FIELD NUMBER                     
         XR    RE,RE                                                            
         ICM   RE,1,FVILEN                                                      
         BZ    EXITOK              NO INPUT TO FIELD                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TEMP.KEYDATA(0),FVIFLD                                           
*                                                                               
         LA    RE,KEYLN1Q+1(RE)                                                 
         STC   RE,TEMP.KEYLN       SET LENGTH OF ELEMENT                        
*                                                                               
         L     R2,AKYFLD           KEY FIELD SAVE AREA                          
         USING KYFLDS,R2                                                        
*                                                                               
         CLI   KYFLDDT,KEYELQ      TABLE INITIALISED?                           
         BE    *+14                YES                                          
         MVC   KYFLDLN,=AL2(KYFLDDT-KYFLDLN+1)                                  
         B     KYSV04                                                           
*                                                                               
KYSV02   GOTOX VHELLO,BCPARM,(C'D',CORETAB),('KEYELQ',KYFLDS),         *        
               (L'KEYNUM,TEMP.KEYNUM)                                           
*                                                                               
KYSV04   LH    R0,=Y(L'KYFLDS)     LENGTH OF KEY FIELD CORE TABLE AREA          
         XR    RF,RF                                                            
         ICM   RF,3,KYFLDLN        CURRENT SPACE USED IN BUFFER                 
         XR    RE,RE                                                            
         ICM   RE,1,TEMP.KEYLN     LENGTH OF DATA TO BE INSERTED                
         AR    RF,RE               RF=CURRENT USED+LENGTH TO ADD                
         CR    R0,RF               DOES IT FIT WITHOUT OVERFLOW?                
         BNH   EXITOK              NO                                           
*                                                                               
         GOTOX VHELLO,BCPARM,(C'P',CORETAB),KYFLDS,TEMP.KEYELD                  
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                HELLO ERROR                                  
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* RESTORE KEY FIELD DATA TO SCREEN                               *  9 *         
*                                                                ******         
* NTRY: P3=A(FIELD HEADER)                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R4                                                           
KEYRES   ICM   R4,15,RTPARMS3      R4=A(FIELD HEADER)                           
         BNZ   *+6                                                              
         DC    H'0'                NO FIELD HEADER PASSED                       
*                                                                               
         MVC   FVIHDR,FHD          EXTRACT FIELD HEADER                         
         TM    FHAT,FHATXH                                                      
         BO    *+6                                                              
         DC    H'0'                NOT AN INPUT FIELD                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RF,FHD(RF)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED HEADER                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BNZ   *+6                                                              
         DC    H'0'                NOT A DATA FIELD                             
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         ICM   R3,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         USING FDRELD,R3                                                        
*                                                                               
         XR    RE,RE               CLEAR FIELD                                  
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         STC   RE,RTBYTE1          SAVE EX LENGTH IN RTBYTE1                    
         EX    RE,*+4                                                           
         XC    FHDA(0),FHDA                                                     
*                                                                               
         L     R2,AKYFLD           FIND KEY DATA ELEMENT                        
         GOTOX VHELLO,BCPARM,(C'G',CORETAB),('KEYELQ',(R2)),           *        
               (L'KEYNUM,FDRNUM)                                                
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO SAVED KEYEL FOR THIS FIELD                
*                                                                               
         USING KEYELD,R5                                                        
         L     R5,12(R1)           R5=A(THIS FIELD'S KEYEL)                     
         XR    RF,RF                                                            
         IC    RF,KEYLN                                                         
         SH    RF,=Y(KEYLN1Q+1)    COPY KEY DATA INTO FIELD                     
         CLM   RF,1,RTBYTE1        MAKE SURE IT WILL FIT OK                     
         BL    *+8                                                              
         ICM   RF,1,RTBYTE1                                                     
         EX    RF,*+4                                                           
         MVC   FHDA(0),KEYDATA                                                  
*                                                                               
         GOTOX VHELLO,BCPARM,(C'D',CORETAB),('KEYELQ',(R2)),           *        
               (L'KEYNUM,FDRNUM)   REMOVE OLD KEYEL                             
*                                                                               
         LA    R0,L'FVIFLD         REVALIDATE FIELD FOR CALLER ROUTINE          
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         B     EXITOK                                                           
         DROP  R3,R4,R5                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY FOR INITIAL LIST/SELECT ON NTRSES IF REQUIRED        * 10 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYBLD   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY TO INHERIT FROM ON ADD IF RECORD IS HEIRARCHICAL     * 12 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYHEIR  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY FROM KEY FIELD IF ONE EXISTS (CC HIGH IF NOT SET)  * 15 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
KEYFLD   XR    R2,R2                                                            
         ICM   R2,3,GSDSPKEY       'KEY' FIELD ON SCREEN?                       
         BZ    EXITH               NO                                           
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
*                                                                               
         OC    GSKEYSEP,GSKEYSEP   DIFFERENT SEPERATOR?                         
         BNZ   *+8                                                              
         MVI   GSKEYSEP,C','                                                    
         OC    GSKEYEQU,GSKEYEQU   DIFFERENT EQUALS SIGN?                       
         BNZ   *+8                                                              
         MVI   GSKEYEQU,C'='                                                    
*                                                                               
         CLI   FHIL,0              INPUT TO FIELD?                              
         BE    EXITH               NO                                           
         CLI   FHIL,1              SINGLE CHARACTER INPUT?                      
         BNE   KFLD10              NO                                           
         CLC   GSKEYSEP,FHDA       SPECIAL COMMAND?                             
         BNE   KFLD10              DO KRES CALLS FOR ALL FIELDS                 
         DROP  R2                                                               
*                                                                               
         L     R3,ATWA             START OF USER SCREEN                         
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
*                                                                               
KFLD02   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KFLD26              YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD?                              
         BZ    KFLD08              NO - IGNORE                                  
         TM    FHAT,FHATPR         PROTECTED FIELD?                             
         BO    KFLD08              YES - IGNORE                                 
*                                                                               
         OC    GSLSTTOP,GSLSTTOP   MAINTENANCE LIST ON THIS SCREEN?             
         BNZ   KFLD04              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   KFLD04              YES                                          
         B     KFLD06                                                           
*                                                                               
KFLD04   CLC   FHAD,GSLSTTOP       CHECK IF WITHIN BOUNDARIES OF LIST           
         BL    KFLD06              YES - IGNORE                                 
         CLC   FHAD,GSLSTEND                                                    
         BNH   KFLD08              YES - IGNORE                                 
*                                                                               
KFLD06   LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED FIELD HEADER                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KFLD08              IF ZERO - IGNORE FIELD                       
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FIELD ELEMENTS                    
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY  KEY FIELD?                            
         BZ    KFLD08              NO - IGNORE FIELD                            
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KRES,FHD REDISPLAY KEY                          
         OI    FHOI,FHOITR         REDISPLAY FIELD                              
*                                                                               
KFLD08   LA    R3,0(R3,R5)                                                      
         B     KFLD02                                                           
*                                                                               
KFLD10   ICM   RF,12,=C',='        SET USER CONTROLLED SEPERATORS               
         ICM   RF,2,GSKEYSEP                                                    
         ICM   RF,1,GSKEYEQU                                                    
*                                                                               
         GOTOX VSCANNER,RTPARM,(R2),RTSCAN,(RF),0                               
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EXITH               NO SCANNED FIELDS                            
*                                                                               
         LA    R4,RTSCAN           SCANNED FIELDS FORMATTED HERE                
         USING SCANBLKD,R4                                                      
         LH    R3,GSDSPOVR                                                      
         A     R3,ATWA             START OF USER SCREEN                         
         USING FHD,R3                                                           
         XR    R5,R5                                                            
*                                                                               
KFLD12   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    KFLD26              YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD?                              
         BZ    KFLD24              NO - IGNORE                                  
         TM    FHAT,FHATPR         PROTECTED FIELD?                             
         BO    KFLD24              YES - IGNORE                                 
*                                                                               
         OC    GSLSTTOP,GSLSTTOP   MAINTENANCE LIST ON THIS SCREEN?             
         BNZ   KFLD14              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   KFLD14              YES                                          
         B     KFLD16                                                           
*                                                                               
KFLD14   CLC   FHAD,GSLSTTOP       CHECK IF WITHIN BOUNDARIES OF LIST           
         BNL   KFLD24              YES - IGNORE                                 
         CLC   FHAD,GSLSTEND                                                    
         BNH   KFLD24              YES - IGNORE                                 
*                                                                               
KFLD16   LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED FIELD HEADER                
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    KFLD24              IF ZERO - IGNORE FIELD                       
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FIELD ELEMENTS                    
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY  KEY FIELD?                            
         BZ    KFLD24              NO - IGNORE FIELD                            
*                                                                               
         OI    FHOI,FHOITR         REDISPLAY FIELD                              
         CLI   SC1STLEN,0          IGNORE THIS FIELD?                           
         BNE   KFLD18              YES                                          
         GOTOX AGEN,RTPARM,OKEY,KRES,FHD REDISPLAY KEY                          
         B     KFLD22                                                           
*                                                                               
KFLD18   XR    RE,RE                                                            
         IC    RE,FHLN             GET LENGTH OF INPUT FIELD                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),BCSPACES    CLEAR INPUT FIELD                            
*                                                                               
         CLI   SC2NDLEN,0          IS THIS A DIVIDED FIELD?                     
         BE    KFLD20              NO                                           
*                                                                               
         XR    RE,RE               DIVIDED FIELD DEALT WITH HERE                
         IC    RE,FHLN             -----------------------------                
         SH    RE,=Y(FHDAD+FHDAD)  LENGTH OF INPUT FIELD                        
         CLM   RE,1,SC1STLEN       WILL ALL OF 1ST FIELD FIT IN                 
         BH    *+8                                                              
         ICM   RE,1,SC1STLEN       YES MOVE IT ALL IN                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),SC1STFLD                                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD)  LENGTH OF INPUT FIELD                        
         CLM   RF,1,SC1STLEN       SPACE TO MOVE IN SEPERATOR?                  
         BNH   KFLD22              NO                                           
                                                                                
         LA    R1,FHDA+1(RE)       NEXT FREE IN FIELD                           
         MVI   0(R1),C'='          DEFAULT SEPERATOR                            
         OC    GSKEYSEP,GSKEYSEP                                                
         BZ    *+10                                                             
         MVC   0(1,R1),GSKEYSEP    USER SEPERATOR                               
         LA    R1,1(R1)            NEXT FREE IN FIELD                           
*                                                                               
         ICM   RE,1,SC1STLEN       L' FIRST HALF OF FIELD                       
         SR    RF,RE               SUBTRACT FROM FIELD LENGTH                   
         SH    RF,=H'1'            -1 FOR SEPERATOR                             
         BNP   KFLD22              NO ROOM LEFT                                 
*                                                                               
         CLM   RF,1,SC2NDLEN       WILL ALL 2ND FIELD FIT?                      
         BNH   *+8                                                              
         ICM   RF,1,SC2NDLEN                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),SC2NDFLD    MOVE IN DATA FROM SECOND HALF                
         B     KFLD22                                                           
*                                                                               
KFLD20   XR    RE,RE               SINGLE FIELD DEALT WITH HERE                 
         IC    RE,FHLN             ----------------------------                 
         SH    RE,=Y(FHDAD+FHDAD)  LENGTH OF INPUT FIELD                        
         XR    RF,RF                                                            
         ICM   RF,1,SC1STLEN       LENGTH OF DATA FOR THIS FIELD                
         CR    RE,RF               WILL IT ALL FIT?                             
         BNH   *+8                                                              
         ICM   RE,1,SC1STLEN       YES - MOVE IT ALL IN                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),SCONEFLD                                                 
         LA    RE,1(RE)                                                         
         STC   RE,FHIL                                                          
         B     KFLD22              GO TO NEXT SCANNER BLOCK                     
*                                                                               
KFLD22   LA    R4,SCBLKLQ(R4)      NEXT SCANNER BLOCK                           
         BCT   R0,KFLD24           DO FOR ALL SCANNED FIELD ENTRIES             
         B     KFLD26                                                           
*                                                                               
KFLD24   LA    R3,0(R5,R3)         NEXT FIELD ON SCREEN                         
         B     KFLD12                                                           
         DROP  R3,R4                                                            
*                                                                               
KFLD26   LH    R2,GSDSPKEY         CLEAR KEY FIELD NOW DATA OUTPUT              
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         LA    RF,FHDAD+1                                                       
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         LA    RF,FHDAD+FHDAD+1                                                 
         SR    R1,RF                                                            
         EX    R1,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         MVI   FHIL,0                                                           
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT - RESIDES AT HIGHEST LEVEL                            *         
*                                                                     *         
* P1 HOLDS OBJECT NAME                                                *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
         USING *+4096,R6                                                        
RECORD   LR    R7,RF                                                            
         LR    R6,R7                                                            
         AH    R6,=H'4096'                                                      
         LM    R0,R3,0(R1)                                                      
         LA    RF,TABLREC                                                       
         B     ITER                ITERATE RECORD HANDLING TABLE                
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(EXITOK)                               
         DC    AL1(RLAST),AL1(0,0,0),AL4(EXITOK)                                
         DC    AL1(RDIS),AL1(0,0,0),AL4(RECDIS)                                 
         DC    AL1(RVAL),AL1(0,0,0),AL4(RECVAL)                                 
         DC    AL1(RADD),AL1(0,0,0),AL4(RECADD)                                 
         DC    AL1(RDEL),AL1(0,0,0),AL4(RECDEL)                                 
         DC    AL1(RRES),AL1(0,0,0),AL4(RECRES)                                 
         DC    AL1(RWRT),AL1(0,0,0),AL4(RECWRT)                                 
         DC    AL1(RGET),AL1(0,0,0),AL4(RECGET)                                 
         DC    AL1(RCPY),AL1(0,0,0),AL4(RECCPY)                                 
         DC    AL1(RREN),AL1(0,0,0),AL4(RECRNM)                                 
         DC    AL1(RMASK),AL1(0,0,0),AL4(EXITOK)                                
         DC    AL1(RFDIS),AL1(0,0,0),AL4(RECFDIS)                               
         DC    AL1(RFVAL),AL1(0,0,0),AL4(RECFVAL)                               
         DC    AL1(RRENO),AL1(0,0,0),AL4(RECRENO)                               
         DC    AL1(EOT)                                                         
*                                                                               
         LTORG                                                                  
         DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD                                                 *  3 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECDIS   GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DDIS),(R2),0           
         BL    EXIT                ERROR ON FIRST TIME FOR DATA DISPLAY         
*                                                                               
         L     R3,ATWA             START OF SCREEN                              
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
         MVI   RTBYTE1,0           RESET MAINT LIST PROCESSED                   
*                                                                               
RDIS02   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    RDIS14              YES                                          
         OC    GSLSTTOP,GSLSTTOP   MAINT LIST ON THIS SCREEN?                   
         BNZ   RDIS04              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   RDIS04              YES                                          
         B     RDIS08                                                           
*                                                                               
RDIS04   CLC   FHAD,GSLSTTOP       REACHED TOP OF MAINT LIST SCREEN?            
         BL    RDIS08              NOT YET                                      
         CLI   RTBYTE1,FF          PROCESSED MAINT SCREEN YET?                  
         BE    RDIS06              YES                                          
         MVI   RTBYTE1,FF                                                       
*                                                                               
         MVC   RTKEY,GSRECKEY      SAVE CURRENT KEY/STATUS/DA                   
         MVC   RTSTA,GSRECSTA      AS LIST CONTROLLER MESSES THEM UP            
         MVC   RTDA,GSRECDA                                                     
*                                                                               
         OI    LSSCIND2,LSSCIDIS   SET DISPLAY ONLY                             
         XR    RF,RF                                                            
         TM    GCINDS2,GCIXITS     DID WE JUST XITSES?                          
         BZ    *+8                 NO                                           
         LA    RF,ACTXIT                                                        
*                                                                               
         GOTOX AGENLST,RTPARM,OLIST,((RF),LPRC)                                 
*                                                                               
         MVC   GSRECKEY,RTKEY      RESTORE KEY/STATUS/DA                        
         MVC   GSRECSTA,RTSTA                                                   
         MVC   GSRECDA,RTDA                                                     
*                                  MAINT SCREEN BUILT HERE -                    
         ICM   R5,1,FHLN           LENGTH MAY HAVE CHANGED                      
         BZ    RDIS14              END OF SCREEN REACHED                        
*                                                                               
RDIS06   CLC   FHAD,GSLSTEND                                                    
         BNH   RDIS12              NEXT FIELD                                   
*                                                                               
RDIS08   TM    FHAT,FHATXH         EXTENDED HEADER?                             
         BO    *+12                YES                                          
         ST    R3,RTTAG            SAVE A(TAG FIELD)                            
         B     RDIS12                                                           
*                                                                               
         LA    RF,FHD(R5)          EXTRACT EXTENDED FIELD HEADER                
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)                                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    RDIS12              NOT AN INPUT FIELD                           
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         ICM   RF,15,0(RF)         A(THIS FIELD ELEMENT)                        
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY                                        
         BO    RDIS12              IGNORE ALL KEY FIELDS                        
         TM    FDRINDS1-FDRELD(RF),FDR1XTAG                                     
         BO    RDIS10              NO ASSOCIATED TAG FIELD                      
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DMHED,(RF),RTTAG                               
*                                                                               
RDIS10   GOTOX AGEN,RTPARM,ODATA,DDIS,FHD,0                                     
         BL    EXIT                ERROR ON DISPLAY OF FIELD                    
*                                                                               
RDIS12   LA    R3,0(R3,R5)         ITERATE FIELDS ON SCREEN                     
         B     RDIS02                                                           
*                                                                               
RDIS14   GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DDIS),(R2),0            
         BL    EXIT                                                             
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD                                                *  4 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECVAL   GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DVAL),(R2),0           
         BL    EXIT                                                             
*                                                                               
         L     R3,ATWA             START OF SCREEN                              
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
         NI    GCINDS2,FF-GCIANYCH RESET RECORD NOT CHANGED FLAG                
         MVI   RTBYTE1,0           RESET MAINTENANCE LIST PROCESSED             
*                                                                               
RVAL02   ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    RVAL16              YES                                          
         OC    GSLSTTOP,GSLSTTOP   MAINTENANCE LIST ON THIS SCREEN?             
         BNZ   RVAL04              YES                                          
         OC    GSLSTEND,GSLSTEND                                                
         BNZ   RVAL04              YES                                          
         B     RVAL08                                                           
*                                                                               
RVAL04   CLC   FHAD,GSLSTTOP       REACHED MAINTENANCE LIST YET?                
         BL    RVAL08              NO                                           
         CLI   RTBYTE1,FF          PROCESSED MAINTENANCE LIST YET               
         BE    RVAL06              YES                                          
         MVI   RTBYTE1,FF          SET MAINTENANCE LIST PROCESSED               
*                                                                               
         MVC   RTKEY,GSRECKEY      SAVE CURRENT KEY/STATUS/DA                   
         MVC   RTSTA,GSRECSTA      AS LIST CONTROLLER MESSES THEM UP            
         MVC   RTDA,GSRECDA                                                     
*                                                                               
         NI    LSSCIND2,FF-(LSSCIDIS)  SET TO ALLOW VALIDATION                  
         XR    RF,RF                                                            
         TM    GCINDS2,GCIXITS     DID WE JUST XITSES?                          
         BZ    *+8                 NO                                           
         LA    RF,ACTXIT                                                        
*                                                                               
         GOTOX AGENLST,RTPARM,OLIST,((RF),LPRC)                                 
*                                                                               
         MVC   GSRECKEY,RTKEY      RESTORE KEY/STATUS/DA                        
         MVC   GSRECSTA,RTSTA                                                   
         MVC   GSRECDA,RTDA                                                     
         BL    EXIT                                                             
*                                  MAINT SCREEN BUILT HERE -                    
         ICM   R5,1,FHLN           LENGTH WILL HAVE CHANGED                     
         BZ    RVAL16              END OF SCREEN REACHED                        
*                                                                               
RVAL06   CLC   FHAD,GSLSTEND       REACHED END OF MAINTENANCE LIST?             
         BNH   RVAL14              NO                                           
*                                                                               
RVAL08   TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BO    *+12                YES                                          
         ST    R3,RTTAG            SAVE ASSOCIATED TAG FIELD                    
         B     RVAL14                                                           
*                                                                               
         LA    RF,FHD(R5)          EXTRACT EXTENDED FIELD HEADER                
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)                                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    RVAL14              NOT AN INPUT FIELD                           
*                                                                               
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FORMATTED FIELD ELEMENTS          
         ICM   RF,15,0(RF)         A(THIS FIELD ELEMENT)                        
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY                                        
         BO    RVAL14              IGNORE ALL KEY FIELDS                        
*                                                                               
         TM    FDRINDS1-FDRELD(RF),FDR1XTAG                                     
         BO    RVAL10              NO ASSOCIATED TAG FIELD                      
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DMHED,(RF),RTTAG                               
*                                                                               
RVAL10   TM    FHAT,FHATPR         FIELD PROTECTED?                             
         BZ    RVAL12              NO                                           
         GOTOX AGEN,RTPARM,ODATA,DDIS,FHD,0                                     
         BL    EXIT                ERROR ON DISPLAY OF PROTECTED FIELD          
         B     RVAL14                                                           
*                                                                               
RVAL12   TM    FHII,FHIIVA         FIELD INPUT THIS TIME?                       
         BO    *+12                NO                                           
         OI    GCINDS2,GCIANYCH    SET FIELD INPUT FLAG                         
         OI    GSINDSL1,GSIDIRTY   SET RECORD CHANGED FLAG                      
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DVAL,FHD,0                                     
         BL    EXIT                ERROR ON VALIDATE OF INPUT FIELD             
*                                                                               
RVAL14   LA    R3,0(R5,R3)         ITERATE FIELDS                               
         B     RVAL02                                                           
*                                                                               
RVAL16   GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DVAL),(R2),0            
         BL    EXIT                                                             
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ADD RECORD TO FILE                                             *  5 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECADD   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RADD                   
         BL    EXIT                   ERROR ON FIRST FOR ADD                    
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECADD,(R2)                                     
         BL    EXIT                   ERROR WHILST ADDING RECORD                
*                                                                               
         OC    LSLINE#,LSLINE#        RECORD IS PART OF A LIST?                 
         BZ    RADD02                 NO                                        
         CLC   LSLINE#,LSPAG#1        SCOPE FOR RANGE                           
         BL    RADD02                 INVALID                                   
         CLC   LSLINE#,LSPAG#X                                                  
         BH    RADD02                 INVALID                                   
*                                                                               
         GOTOX (TSARIO,AGROUTS),TSAPUT                                          
*                                                                               
RADD02   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RADD                    
         BL    EXIT                   ERROR ON LAST FOR ADD                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DELETE RECORD FROM FILE                                        *  6 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECDEL   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RDEL                   
         BL    EXIT                ERROR ON FIRST FOR DELETE                    
*                                                                               
         OI    GSRECSTA,X'80'      SET DELETE FLAG ON                           
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1  SAVE RECORD INDICATORS                   
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)  ENSURE UPDATIVE FLAG OFF           
         GOTOX AGEN,RTPARM,OIO,IRECWRT                                          
         MVC   GSFRR.FRRINDS1,RTBYTE1  RESTORE RECORD INDICATORS                
         BL    EXIT                ERROR ON WRITE OF RECORD TO FILE             
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RDEL                    
         BL    EXIT                ERROR ON LAST FOR DELETE                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RESTORE RECORD TO FILE                                         *  7 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECRES   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RRES                   
         BL    EXIT                ERROR ON FIRST FOR RESTORE                   
*                                                                               
         NI    GSRECSTA,FF-X'80'   TURN OFF DELETE FLAG                         
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1  SAVE RECORD INDICATORS                   
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)  ENSURE UPDATIVE FLAG OFF           
         GOTOX AGEN,RTPARM,OIO,IRECWRT                                          
         MVC   GSFRR.FRRINDS1,RTBYTE1  RESTORE RECORD INDICATORS                
         BL    EXIT                ERROR ON WRITE OF RECORD TO FILE             
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RRES                    
         BL    EXIT                ERROR ON LAST FOR VALIDATE                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* WRITE RECORD TO FILE                                           *  8 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECWRT   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RWRT                   
         BL    EXIT                ERROR ON FIRST TIME FOR WRITE                
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECWRT          WRITE BACK RECORD               
         BL    EXIT                ERROR ON RECORD WRITE                        
*                                                                               
*                                                                               
         OC    LSLINE#,LSLINE#        RECORD IS PART OF A LIST?                 
         BZ    RWRT02                 NO                                        
         CLC   LSLINE#,LSPAG#1        SCOPE FOR RANGE                           
         BL    RWRT02                 INVALID                                   
         CLC   LSLINE#,LSPAG#X                                                  
         BH    RWRT02                 INVALID                                   
*                                                                               
         GOTOX (TSARIO,AGROUTS),TSAPUT                                          
*                                                                               
RWRT02   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RWRT                    
         BL    EXIT                   ERROR ON LAST FOR WRITE                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* COPY RECORD ON FILE                                            *  9 *         
*                                                                ******         
* NTRY: P3=A(DISK ADDRESS) OF RECORD TO COPY FROM                     *         
***********************************************************************         
         SPACE 1                                                                
RECCPY   MVC   RTWORK(L'GSRECSAV),GSRECSAV  SAVE CURRENT KEY                    
*                                                                               
         MVC   GSRECKEY,GSCPYKEY   FOR CONTROL FILE TYPES                       
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECRD,0,RTPARMS3,A(XIO11)                       
         BL    EXITL               ERROR ON READ OF COPY RECORD                 
*                                  (READ OF RECORD DESTROYS GSRECSAV)           
         MVC   GSRECSAV,RTWORK     RESTORE CURRENT KEY                          
*                                                                               
         LH    RF,GSDIRDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         USING NFITABD,RF                                                       
*                                                                               
         L     R2,AIOREC           A(RECORD TO COPY)                            
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECKEY    MOVE 'COPY TO' KEY INTO FILE RECORD          
*                                                                               
         XC    GSRECSTA,GSRECSTA   RESET RECORD STATUS AREA                     
         LA    R2,3(RE,R2)         RE POINTS TO STATUS AREA IN FILE             
         IC    RE,NFICTLL          LENGTH OF STATUS                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R2)   SAVE STATUS FROM 'COPY FROM' RECORD          
         NI    GSRECSTA,FF-X'80'   MAKE SURE NOT DELETED ON DIRETORY            
         NI    0(R2),FF-X'80'      MAKE SURE NOT DELETED ON FILE                
*                                                                               
         L     R2,AIOREC           RESTORE A(RECORD) TO R2                      
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RCPY                   
         BL    EXIT                ERROR ON FIRST TIME CALL FOR PROGRAM         
*                                                                               
         OC    GSRECDA,GSRECDA     'COPY TO' RECORD ON FILE?                    
         BNZ   RCPY02              YES - MUST WRITE RECORD BACK                 
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1                                           
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)                                     
         GOTOX AGEN,RTPARM,ORECH,RADD,(R2)                                      
         MVC   GSFRR.FRRINDS1,RTBYTE1                                           
         BL    EXITL               ERROR ON RECORD ADD                          
         B     RCPY04                                                           
*                                                                               
RCPY02   L     RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         L     R2,AIOREC                                                        
         LA    R2,2(RE,R2)         RF POINTS TO STATUS AREA                     
         IC    RE,NFICTLL          LENGTH OF STATUS AREA                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         XC    0(0,R2),0(R2)       FIDDLE FOR WRITE ROUTINE                     
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY RECORD INTO AIO1 FOR A BIT              
*                                                                               
         MVC   RTWORK(L'GSRECSAV),GSRECSAV                                      
         GOTOX AGEN,RTPARM,OIO,IRECRD,A(XOLOCK),GSRECDA,A(XIO11)                
         BL    EXITL               ERROR ON READ FOR UPDATE                     
         MVC   GSRECSAV,RTWORK                                                  
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               RESTORE RECORD FROM AIO1                     
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1                                           
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)                                     
         GOTOX AGEN,RTPARM,OIO,IRECWRT,0,0                                      
         MVC   GSFRR.FRRINDS1,RTBYTE1                                           
         BL    EXITL               ERROR ON WRITE BACK TO FILE                  
*                                                                               
RCPY04   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RCPY                    
         BL    EXIT                ERROR ON LAST FOR COPY CALL                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RENAME RECORD ON FILE                                          *  9 *         
*                                                                ******         
* NTRY: P3=A(DISK ADDRESS) OF RECORD TO RENAME                        *         
***********************************************************************         
         SPACE 1                                                                
RECRNM   MVC   RTWORK(L'GSRECSAV),GSRECSAV  SAVE CURRENT KEY                    
         GOTOX AGEN,RTPARM,OIO,IRECRD,0,RTPARMS3,A(XIO11)                       
         BL    EXITL               ERROR ON READ OF 'RENAME FROM' REC.          
         MVC   GSRECSAV,RTWORK     RESTORE CURRENT DIRECTORY                    
*                                                                               
         LH    RF,GSDIRDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         SAVE DIRECTORY DETAILS                       
         USING NFITABD,RF                                                       
         XR    RE,RE                                                            
         L     R2,AIOREC           WHERE RECORD IS                              
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECKEY    MOVE IN 'RENAME TO' KEY TO FILE              
*                                                                               
         XC    GSRECSTA,GSRECSTA                                                
         LA    R2,3(RE,R2)         R2 POINTS TO STATUS AREA                     
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R2)   COPY STATUS FROM 'RENAME FROM' REC.          
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RREN                   
         BL    EXIT                ERROR ON FIRST TIME FOR RENAME               
*                                                                               
         OC    GSRECDA,GSRECDA     RECORD TO RENAME TO EXISTS ALREADY?          
         BNZ   RNM02               YES - NEED TO WRITE BACK                     
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RADD,AIOREC                                    
         BL    EXITL               ERROR ON ADD OF RECORD TO FILE               
         B     RNM04                                                            
*                                                                               
RNM02    L     RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         L     R2,AIOREC                                                        
         LA    R2,2(RE,R2)         RF POINTS TO STATUS AREA                     
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         XC    0(0,R2),0(R2)       FIDDLE FOR WRITE ROUTINE                     
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE RECORD IN AIO1 FOR A BIT                
*                                                                               
         MVC   RTWORK(L'GSRECSAV),GSRECSAV                                      
         GOTOX AGEN,RTPARM,OIO,IRECRD,A(XOLOCK),GSRECDA,A(XIO11)                
         BL    EXITL               ERROR ON READ FOR UPDATE                     
         MVC   GSRECSAV,RTWORK                                                  
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE IN RENAMED RECORD                       
*                                                                               
         L     R2,AIOREC           A(RENAMED RECORD)                            
         L     RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         LA    R2,2(RE,R2)         RE POINTS TO STATUS AREA IN FILE             
         NI    GSRECSTA,FF-X'80'   MAKE SURE NOT DELETED ON DIRETORY            
         NI    0(R2),FF-X'80'      MAKE SURE NOT DELETED ON FILE                
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECWRT,0                                        
         BL    EXITL               ERROR ON WRITE BACK OF FILE                  
*                                                                               
RNM04    MVC   RTWORK(L'GSRECSAV),GSRECSAV                                      
         GOTOX AGEN,RTPARM,OIO,IRECRD,A(XOLOCK),RTPARMS3,A(XIO11)               
         BL    EXITL                                                            
         GOTOX AGEN,RTPARM,ORECH,RDEL,AIOREC                                    
         BL    EXITL                                                            
         MVC   GSRECSAV,RTWORK                                                  
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RREN                    
         BL    EXIT                                                             
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD LEVEL FILTERS ONTO SCREEN                       * ?? *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECFDIS  GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RFDIS                  
         BL    EXITL               ERROR ON FIRST FOR KEY FLT DISPLAY           
*                                                                               
         L     R3,ATWA             START OF USER SCREEN                         
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
RFDIS02  ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    RFDIS06             YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BZ    RFDIS04             NO - IGNORE IT                               
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED HEADER                      
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    RFDIS04             NOT A DATA FIELD IF ZERO                     
*                                                                               
         BCTR  RF,0                MAKE ZERO-BASED                              
         SLL   RF,2                                                             
         A     RF,AFDRADDR         INDEX INTO FIELD ELEMENT LIST                
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                FIELD NOT IN LIST OF FIELDS                  
*                                                                               
         TM    FDRLVL-FDRELD(RF),FDRIKEY  KEY FIELD?                            
         BO    RFDIS04             YES - IGNORE                                 
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DFDIS,FHD,(R2),0                               
         BL    EXITL               ERROR ON FILTER DISPLAY                      
*                                                                               
RFDIS04  LA    R3,0(R5,R3)         ITERATE FIELDS ON SCREEN                     
         LH    RF,LSHEDDSP         DISP TO START OF LIST HEADINGS               
         A     RF,ATWA             RF=A(START OF LIST)                          
         CR    R3,RF                                                            
         BL    RFDIS02             ONLY DISPLAY FOR FILTER SCREEN               
*                                                                               
RFDIS06  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),RLAST,(R2),RFDIS                    
         BL    EXIT                ERROR ON LAST TIME CALL                      
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD FLTELS AND INITIAL KEY FOR LIST HEADER KEY FIELDS        *  6 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECFVAL  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),RFIRST,(R2),RFVAL                   
         BL    EXITL               ERROR ON FIRST FOR KEY FLT VALIDATE          
*                                                                               
         L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         START OF USER SCREEN                         
         USING FHD,R3                                                           
         XR    R5,R5                                                            
         NI    RTFILTX,FF-(RTFILTST) NO DEFAULTS OVERRIDDEN YET                 
*                                                                               
RFVAL02  ICM   R5,1,FHLN           END OF SCREEN?                               
         BZ    RFVAL08             YES                                          
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER?                       
         BZ    RFVAL06             NO - IGNORE FIELD                            
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTRACT EXTENDED FIELD HEADER                
*                                                                               
         XR    R4,R4                                                            
         ICM   R4,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.) HERE           
         BZ    RFVAL06             NOT AN INPUT FIELD IF ZERO                   
         BCTR  R4,0                MAKE IT ZERO-BASED                           
         SLL   R4,2                                                             
         A     R4,AFDRADDR         START OF FORMATTED ELEMENTS                  
         ICM   R4,15,0(R4)         A(THIS FIELD ENTRY)                          
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         USING FDRELD,R4                                                        
*                                                                               
         TM    FDRLVL,FDRIKEY      KEY FIELD?                                   
         BO    RFVAL06             YES - IGNORE                                 
         TM    FHAT,FHATPR         PROTECTED FIELD?                             
         BZ    RFVAL04             NO                                           
         TM    FDRINDS2,FDR2FILT   FILTER FIELD?                                
         BO    RFVAL04             YES                                          
*                                                                               
         GOTOX AGEN,RTPARM,ODATA,DFDIS,FHD,(R2),0                               
         BL    EXITL               CAN`T VALIDATE A PROTECTED FIELD             
         B     RFVAL06                                                          
*                                                                               
RFVAL04  GOTOX AGEN,RTPARM,(0,ODATA),DFVAL,FHD,(R2),0                           
         BL    EXITL               ERROR ON VALIDATE OF FILTER                  
*                                                                               
RFVAL06  LA    R3,0(R5,R3)         ITERATE FIELDS                               
         LH    RF,LSHEDDSP         DISPLACEMENT TO LIST HEADINGS                
         A     RF,ATWA                                                          
         CR    R3,RF               REACHED START OF LIST YET?                   
         BL    RFVAL02             NO                                           
*                                                                               
RFVAL08  GOTOX AGEN,RTPARM,('GCBOVER',OKEY),RLAST,(R2),RFVAL                    
         BL    EXIT                ERROR ON LAST TIME CALL                      
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* GET RECORD FROM FILE                                           * 14 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECGET   GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RGET                   
         BL    EXIT                ERROR ON FIRST TIME FOR GET                  
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECGET          GET RECORD                      
         BL    EXIT                                                             
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RGET                    
         BL    EXIT                   ERROR ON LAST FOR WRITE                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RENAME RECORD OVER ANY RECORD WHETHER LIVE OR NOT              *  9 *         
*                                                                ******         
* NTRY: P3=A(DISK ADDRESS) OF RECORD TO RENAME FROM                   *         
***********************************************************************         
         SPACE 1                                                                
RECRENO  MVC   RTWORK(L'GSRECSAV),GSRECSAV  SAVE CURRENT KEY                    
         MVC   GSRECKEY,GSCPYKEY   FOR CONTROL FILE TYPES                       
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECRD,0,RTPARMS3,A(XIO11)                       
         BL    EXITL               ERROR ON READ OF COPY RECORD                 
*                                  (READ OF RECORD DESTROYS GSRECSAV)           
         MVC   GSRECSAV,RTWORK     RESTORE CURRENT KEY                          
*                                                                               
         LH    RF,GSDIRDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         USING NFITABD,RF                                                       
*                                                                               
         L     R2,AIOREC           A(RECORD TO COPY)                            
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECKEY    MOVE 'COPY TO' KEY INTO FILE RECORD          
*                                                                               
         XC    GSRECSTA,GSRECSTA   RESET RECORD STATUS AREA                     
         LA    R2,3(RE,R2)         RE POINTS TO STATUS AREA IN FILE             
         IC    RE,NFICTLL          LENGTH OF STATUS                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R2)   SAVE STATUS FROM 'COPY FROM' RECORD          
         NI    GSRECSTA,FF-X'80'   MAKE SURE NOT DELETED ON DIRETORY            
         NI    0(R2),FF-X'80'      MAKE SURE NOT DELETED ON FILE                
*                                                                               
         L     R2,AIOREC           RESTORE A(RECORD) TO R2                      
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RFIRST,(R2),RRENO                  
         BL    EXIT                                                             
*                                                                               
         OC    GSRECDA,GSRECDA     'COPY TO' RECORD ON FILE?                    
         BNZ   RRENO02             YES - MUST WRITE RECORD BACK                 
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1                                           
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)                                     
         GOTOX AGEN,RTPARM,ORECH,RADD,(R2)                                      
         MVC   GSFRR.FRRINDS1,RTBYTE1                                           
         BL    EXITL               ERROR ON RECORD ADD                          
         B     RRENO04                                                          
*                                                                               
RRENO02  L     RF,RTFILDIR         RECORD DIRECTORY DETAILS                     
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         L     R2,AIOREC                                                        
         LA    R2,2(RE,R2)         RF POINTS TO STATUS AREA                     
         IC    RE,NFICTLL          LENGTH OF STATUS AREA                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         XC    0(0,R2),0(R2)       FIDDLE FOR WRITE ROUTINE                     
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE               COPY RECORD INTO AIO1 FOR A BIT              
*                                                                               
         MVC   RTWORK(L'GSRECSAV),GSRECSAV                                      
         GOTOX AGEN,RTPARM,OIO,IRECRD,A(XOLOCK),GSRECDA,A(XIO11)                
         BL    EXITL               ERROR ON READ FOR UPDATE                     
         MVC   GSRECSAV,RTWORK                                                  
*                                                                               
         L     RE,AIOREC                                                        
         LA    RF,L'IOAREC-(L'IODA+L'IOWORK)                                    
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0               RESTORE RECORD FROM AIO1                     
*                                                                               
         MVC   RTBYTE1,GSFRR.FRRINDS1                                           
         NI    GSFRR.FRRINDS1,FF-(FRR1UPDT)                                     
         GOTOX AGEN,RTPARM,OIO,IRECWRT,0,0                                      
         MVC   GSFRR.FRRINDS1,RTBYTE1                                           
         BL    EXITL               ERROR ON WRITE BACK TO FILE                  
*                                                                               
RRENO04  MVC   RTWORK(L'GSRECSAV),GSRECSAV                                      
         GOTOX AGEN,RTPARM,OIO,IRECRD,A(XOLOCK),RTPARMS3,A(XIO11)               
         BL    EXITL               ERROR ON READ OF FROM RECORD                 
         GOTOX AGEN,RTPARM,ORECH,RDEL,AIOREC                                    
         BL    EXITL               ERROR ON DELETE                              
         MVC   GSRECSAV,RTWORK                                                  
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ORECH),RLAST,(R2),RRENO                   
         BL    EXIT                ERROR ON LAST FOR RENAME CALL                
         B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT - RESIDES AT HIGHEST LEVEL                              *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(FIELD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DATA     LR    R7,RF                                                            
         MVI   RTVFINDS,0          RESET INDICATORS                             
         L     RE,4(R1)            RE HOLDS EQUATED VERB OR ZERO                
         LTR   RE,RE                                                            
         BZ    EXITH               NO GLOBAL ACTIONS KNOWN FOR NOW              
*                                                                               
         LA    RF,VERBTAB          TABLE OF KNOWN OBJECTS                       
         USING VERBTABD,RF                                                      
*                                                                               
DATA02   CLI   VERBID,EOT          REACH END - NOT A KNOWN DATA TYPE            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLM   RE,1,VERBID         IS THIS THE VERB ?                           
         BE    DATA04              YES                                          
         LA    RF,VERBLQ(RF)                                                    
         B     DATA02              ITERATE THROUGH TABLE                        
*                                                                               
DATA04   ICM   RF,15,VERBADD       A(VERB HANDLER)                              
         A     RF,RTRELO                                                        
         BR    RF                  BRANCH TO VERB HANDLER                       
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN VERBS FOR THE DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
VERBTAB  DC    AL1(DDIS),AL4(DTADIS)   DISPLAY                                  
         DC    AL1(DVAL),AL4(DTAVAL)   VALIDATE                                 
         DC    AL1(DHED),AL4(DTAHED)   HEAD LINE                                
         DC    AL1(DNDIS),AL4(DTADNDS) DISPLAY FOR LIST                         
         DC    AL1(DLDIS),AL4(DTADDWN) DISPLAY FOR REPORT                       
         DC    AL1(DFDIS),AL4(DTAFDIS) DISPLAY FILTER FIELD                     
         DC    AL1(DMHED),AL4(DTAMHED) SET MAINT SCREEN TAG FIELD               
         DC    AL1(DFVAL),AL4(DTAFVAL) VALIDATE FILTER FIELD                    
         DC    AL1(DFDO),AL4(DTAFDO)   DO FILTERING                             
         DC    AL1(DNTR),AL4(DTADNTR)  CALL CALLER ON NTRSES                    
         DC    AL1(DDISTOP),AL4(DTATOP) DISPLAY TOP TOTAL                       
         DC    AL1(DSRCH),AL4(DTASRCH) SEARCH CALL TO OVERLAY                   
         DC    AL1(DDISBOT),AL4(DTABOT) DISPLAY BOTTOM TOTAL                    
         DC    AL1(DRDIS),AL4(DTARDIS) DISPLAY FOR REPORT                       
         DC    AL1(DRVAL),AL4(DTARVAL) VALIDATE FOR REPORT                      
         DC    AL1(EOT)                                                         
*                                                                               
VERBTABD DSECT                                                                  
VERBID   DS    XL1                 IDENTIFIER                                   
VERBADD  DS    AL4                 A(VERB HANDLER)                              
VERBLQ   EQU   *-VERBTABD                                                       
*                                                                               
GEFIL01  CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB                                                   *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTADIS   LR    R7,RF                                                            
         L     R4,8(R1)            R4=A(FIELD) OR 0                             
         USING FHD,R4                                                           
*                                                                               
         NI    GCINDS1,FF-GCIINFLD TURN OFF INPUT FIELD FLAG                    
*                                                                               
         ST    R4,FVADDR           SAVE A(FIELD)                                
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         TM    FHAT,FHATPR         TEST PROTECTED FIELD                         
         BO    *+8                                                              
         OI    GCINDS1,GCIINFLD    FLAG AS INPUT FIELD                          
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED FIELD HEADER                   
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DDIS                                                          
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
*                                                                               
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
*                                                                               
         LR    RE,R4               PROTECT AGAINST NTRSES DESTRUCTION           
         S     RE,ATWA                                                          
         CH    RE,GSDSPMAX         MAX SCREEN SIZE ALLOWED                      
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
*                                                                               
         LA    RE,1(RE)            VALIDATE INPUT LENGTH SET IN FVIHDR          
         STCM  RE,1,FHIL                                                        
         CLM   RE,1,FVIHDR+FHILD                                                
         BL    *+10                                                             
         MVC   FHIL,FVIHDR+FHILD                                                
*                                                                               
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         OI    FHII,FHIIVA                                                      
         MVC   FHOI,FVIHDR+FHOID                                                
         OI    FHOI,FHOITR                                                      
*                                                                               
         TM    FDRLVL,FDRIKEY                                                   
         BZ    EXITOK                                                           
         LTR   R4,R4               A(FIELD) PASSED?                             
         BZ    EXITOK              NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KSAVE,GSRECKEY                                  
         BL    EXIT                                                             
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATA VERB                                                  *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAVAL   LR    R7,RF                                                            
         L     R4,8(R1)            R4=A(FIELD) OR 0                             
         USING FHD,R4                                                           
         ST    R4,FVADDR                                                        
*                                                                               
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL,FDRIKEY      TEST KEY FIELD                               
         BZ    DVAL02                                                           
         CLI   FHDA,C'/'           TEST RESTORE                                 
         BNE   DVAL02                                                           
         GOTOX AGEN,RTPARM,OKEY,KRES,FHD REDISPLAY KEY                          
         BL    EXIT                                                             
*                                                                               
DVAL02   GOTOX AGEN,RTPARM,ODATA,DSRCH,FHD,FDRELD                               
*                                                                               
DVAL04   MVC   FVMAXL,FDRFMAX      CHECK LENGTHS ARE OK                         
         LA    R0,L'FVIFLD                                                      
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         BH    DVALN                                                            
         BE    DVAL06                                                           
         TM    FDRINDS1,FDR1REQ    INPUT REQUIRED?                              
         BZ    DVAL06                                                           
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DVALN                                                            
*                                                                               
DVAL06   CLI   FDRFMIN,0           MINIMUM INPUT LENGTH SET?                    
         BE    DVAL08              NO                                           
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    DVAL08              NO                                           
         CLC   FVILEN,FDRFMIN      LONG ENOUGH?                                 
         BNL   DVAL08              YES                                          
         TM    FDRINDS1,FDR1IDEF   DEFAULT VALUE?                               
         BO    DVAL08              YES                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         CLI   FVILEN,0            ANYTHING AT ALL?                             
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DVALN                                                            
*                                                                               
DVAL08   XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DVAL                                                          
*                                                                               
         TM    FDRINDS1,FDR1IDEF   DEFAULT CAN BE SET?                          
         BZ    *+16                                                             
         CLI   FVILEN,0            ANY INPUT TO VALIDATE?                       
         BNE   *+8                                                              
         LA    RE,DDFLT                                                         
*                                                                               
         ST    RE,8(R1)            P2=VERB - 'VALIDATE' OR 'SET DEFLT'          
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         NI    RTVFINDS,FF-(RTVFIVAL)                                           
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND                                                           
         BL    *+8                 ERROR ON VALIDATE                            
         OI    RTVFINDS,RTVFIVAL   FIELD IS VALID                               
*                                                                               
DVAL10   TM    FDRINDS1,FDR1RDIS   REDISPLAY THIS FIELD IF VALID?               
         BZ    DVAL12              NO                                           
         TM    RTVFINDS,RTVFIVAL   FIELD VALID?                                 
         BO    DVAL14              YES                                          
*                                                                               
DVAL12   TM    GCINDS1,GCIREDIS    TEST REDISPLAY REGARDLESS                    
         BZ    DVAL16              NO                                           
         NI    GCINDS1,FF-GCIREDIS                                              
*                                                                               
DVAL14   GOTOX AGEN,RTPARM,ODATA,DDIS,FHD,0                                     
         BNH   *+10                                                             
         MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         BNE   DVALN                                                            
*                                                                               
DVAL16   XR    RE,RE                                                            
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         MVC   FHIL,FVIHDR+FHILD                                                
         MVC   FHOI,FVIHDR+FHOID                                                
*                                                                               
         TM    RTVFINDS,RTVFIVAL                                                
         BZ    DVALN                                                            
         TM    FDRLVL,FDRIKEY      TEST RECORD KEY TYPE FIELD                   
         BZ    DVALOK                                                           
         LTR   R4,R4                                                            
         BZ    DVALOK                                                           
         GOTOX AGEN,RTPARM,OKEY,KSAVE,GSRECKEY                                  
         BL    DVALN                                                            
         B     DVALOK                                                           
*                                                                               
DVALOK   OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
DVALN    NI    FHII,FF-FHIIVA                                                   
         OI    FHOI,FHOITR                                                      
         B     EXITL                                                            
*                                                                               
         DROP  R4,R3                                                            
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DO A SEARCH CALL TO THE OVERLAY                          *         
* RETURNS CC EQUAL IF ALL OK                                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTASRCH  LR    R7,RF                                                            
*                                                                               
         TM    GSINDSG1,GSG1MSAV                                                
         BZ    DTSRC02                                                          
         NI    GSINDSG1,FF-GSG1MSAV                                             
         GOTOX ('SAVVAL',AGROUTS)                                               
*                                                                               
DTSRC02  L     R3,RTPARMS4                                                      
         USING FDRELD,R3                                                        
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         LA    RE,ODATA                                                         
         ST    RE,0(R1)            P1=DATA OBJECT                               
         MVI   0(R1),GCBOVER                                                    
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         MVI   0(R1),GCBOVER+GCBPS SET CALL PS OVERLAY                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P2=EQUATED FIELD NUMBER                      
         LA    RE,DSRCH                                                         
         ST    RE,8(R1)            P3=VERB - 'DISPLAY FILTER'                   
         L     RE,AIOREC           P4=A(RECORD)                                 
*                                                                               
         TM    FDRLVL,FDRIKEY                                                   
         BZ    *+8                                                              
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR                                                  
         BZ    *+8                                                              
         LA    RE,ATLST                                                         
*                                                                               
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                 IF YES -                                     
         LA    RE,PSRECKEY         ONLY EVER NEED KEY FOR FILTER DIS            
*                                                                               
         ST    RE,12(R1)           P4=A(RECORD AT CORRECT LEVEL)                
         MVC   16(4,R1),RTPARMS3   P5=A(FIELD)                                  
         ST    R3,20(R1)           P6=A(FDREL)                                  
*                                                                               
         GOTOX APRG                                                             
         BL    EXITL               ERROR ON SEARCH                              
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SET LIST HEAD-LINES                                                 *         
*                                                                     *         
* NTRY: P3 = SEQUENCE NUMBER                                          *         
*       P4 = HEADLINE 1 FIELD                                         *         
*       P5 = HEADLINE 2 FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAHED   LR    R7,RF                                                            
         LM    R2,R4,RTPARMS3                                                   
H1       USING FHD,R3                                                           
H2       USING FHD,R4                                                           
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         A     R2,AFDRADDR                                                      
         L     R2,0(R2)                                                         
         USING FDRELD,R2                                                        
*                                                                               
         MVC   RTWORK,BCSPACES     HEADLINE SETTING AREAS                       
         MVC   RTWORKA,BCSPACES                                                 
         OC    FDRLHED1,FDRLHED1                                                
         BZ    DTAHED02                                                         
         MVC   RTWORK(L'FDRLHED1),FDRLHED1                                      
         GOTOX VDICTAT,RTPARM,C'SL  ',RTWORK                                    
*                                                                               
DTAHED02 OC    FDRLHED2,FDRLHED2                                                
         BZ    DTAHED04                                                         
         MVC   RTWORKA(L'FDRLHED2),FDRLHED2                                     
         GOTOX VDICTAT,RTPARM,C'SL  ',RTWORKA                                   
*                                                                               
DTAHED04 L     RF,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT IN DIRECTORY?                          
         BZ    *+8                                                              
         LA    RF,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RF,ATLST                                                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,FDRNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(R0),DHED,(RF),RTWORK,    *        
               RTWORKA,H1.FHD,H2.FHD                                            
*                                                                               
         XR    RF,RF               MOVE IN THE HEADLINES                        
         ICM   RF,1,FDRLHLEN                                                    
         BZ    DTAHEDX                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   H1.FHDA(0),RTWORK                                                
         EX    RF,*+4                                                           
         MVC   H2.FHDA(0),RTWORKA                                               
*                                                                               
DTAHEDX  B     EXITOK                                                           
         DROP  H1,H2,R2                                                         
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SET MAINTENANCE SCREEN HEADING IF REQUIRED                          *         
*                                                                     *         
* NTRY: P3 = SEQUENCE NUMBER                                          *         
*       P4 = TAG FIELD                                                          
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAMHED  LR    R7,RF                                                            
         LM    R2,R3,RTPARMS3                                                   
H        USING FHD,R3                                                           
         USING FDRELD,R2                                                        
         MVC   RTWORK,BCSPACES     HEADLINE SETTING AREAS                       
*                                                                               
         L     RF,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT IN DIRECTORY?                          
         BZ    *+8                 NO                                           
         LA    RF,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RF,ATLST                                                         
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,FDRNUM                                                      
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(R0),DMHED,(RF),FDRELD,   *        
               RTWORK                                                           
*                                                                               
         CLC   RTWORK,BCSPACES     WANT TO SET HEADLINE?                        
         BH    DMHED02                                                          
*                                                                               
         MVC   RTWORK(L'FDRRTAG),FDRRTAG SET DEFAULT HEADLINE                   
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,RTPARM,(RF),RTWORK                                       
*                                                                               
DMHED02  XR    RF,RF               MOVE IN THE HEADLINES                        
         ICM   RF,1,FDRTDEFL                                                    
         BZ    EXITOK                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   H.FHDA(0),RTWORK                                                 
         OI    H.FHOI,FHOITR                                                    
         B     EXITOK                                                           
         DROP  H,R2                                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB GIVEN FLD#                                        *         
*                                                                     *         
* P3 HOLDS A(FLD NUMBER)                                              *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTADNDS  LR    R7,RF                                                            
         L     R1,8(R1)                                                         
         LH    RF,0(R1)            A(FIELD NUMBER) IN RF                        
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DDIS                                                          
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB FOR DOWNLOAD                                      *         
*                                                                     *         
* P3 HOLDS A(FLD NUMBER)                                              *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTADDWN  LR    R7,RF                                                            
         L     R1,8(R1)                                                         
         LH    RF,0(R1)            A(FIELD NUMBER) IN RF                        
*        BCTR  RF,0                MAKE IT ZERO-BASED                           
*        SLL   RF,2                MULTIPLY BY 4                                
*        L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
*        LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
*        L     R3,0(R3)            THIS FIELD ENTRY                             
         XR    R3,R3                                                            
         USING FDRELD,R3                                                        
*        LTR   R3,R3                                                            
*        BNZ   *+6                                                              
*        DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
*        XR    RE,RE                                                            
*        ICM   RE,3,FDRNUM                                                      
         ST    RF,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DDIS                                                          
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB FOR TOP TOTAL LINE                                *         
*                                                                     *         
* P3 HOLDS A(FLD NUMBER)                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTATOP   LR    R7,RF                                                            
         L     R1,8(R1)                                                         
         LH    RF,0(R1)            A(FIELD NUMBER) IN RF                        
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DDISTOP                                                       
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
         XR    RE,RE                                                            
         ST    RE,12(R1)                                                        
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITOK              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB FOR BOTTOM TOTAL LINE                             *         
*                                                                     *         
* P3 HOLDS A(FLD NUMBER)                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTABOT   LR    R7,RF                                                            
         L     R1,8(R1)                                                         
         LH    RF,0(R1)            A(FIELD NUMBER) IN RF                        
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DDISBOT                                                       
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
         XR    RE,RE                                                            
         ST    RE,12(R1)                                                        
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITOK              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA FOR FILTER OBJECT                                      *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(KEY)                                                     *         
* P5 HOLDS A(FDREL) OR 0                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAFDIS  LR    R7,RF                                                            
         XC    FLTIFLD,FLTIFLD     RESET FILTER DATA FIELD                      
         MVC   FVIFLD,BCSPACES                                                  
         L     R4,RTPARMS3                                                      
         ST    R4,FVADDR                                                        
         USING FHD,R4              R4=A(FIELD)                                  
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
*                                                                               
         ICM   R3,15,RTPARMS5                                                   
         BNZ   DFDS02              A(FDREL) PASSED?                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
DFDS02   XC    RTWORK3,RTWORK3     CLEAR FILTER SAVE AREA                       
         GOTOX VHELLO,RTPARM,(C'G',CORETAB),('FLTELQ',AFLTELSV),       *        
               (L'FDRNUM,FDRNUM)   GET OLD FILTER ELEMENT                       
         CLI   12(R1),0                                                         
         BE    DFDS04              ELEMENT TO DISPLAY                           
*                                                                               
         BAS   RE,DODIS            'FUNNY' DISPLAY FOR PROTECTED FLTS           
         XR    RE,RE                                                            
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         BM    EXITOK                                                           
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         MVC   FHIL,FVIHDR+FHILD                                                
         MVC   FHOI,FVIHDR+FHOID                                                
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
DFDS04   L     RF,12(R1)           A(FLTEL)                                     
SAVE     USING FLTELD,RTWORK3                                                   
         USING FLTELD,RF                                                        
         XR    RE,RE                                                            
         IC    RE,FLTLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4              SAVE FLTEL IN RTWORK3                        
         MVC   SAVE.FLTELD(0),FLTELD                                            
         DROP  RF                                                               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SAVE.FLTCNT      NUMBER OF ITEMS IN THIS LIST                 
         MVC   FVIFLD,BCSPACES     CLEAR INPUT FIELD                            
         MVC   FVILEN,0            RESET INPUT LENGTH                           
         LA    R6,SAVE.FLTINFO                                                  
         USING FLTINFO,R6                                                       
         LA    R5,RTSCAN                                                        
         USING UNBLK,R5                                                         
*                                                                               
UNBLK    DSECT                                                                  
UNBOTH   DS    0CL50               ** CHANGED BY PSHA 01/10/97                  
UN1      DS    CL40                ** CHANGED BY PSHA 01/10/97                  
UN2      DS    CL10                                                             
UNBLKLQ  EQU   *-UNBLK                                                          
*                                                                               
GEFIL01  CSECT                                                                  
*                                                                               
DFDS06   STH   RF,RTCOUNT                                                       
         MVC   RTFILT1,FLTIND1     SAVE FILTER INDICATORS                       
         MVC   RTFILT2,FLTIND2                                                  
         XC    FLTIFLD,FLTIFLD     RESET DATA FIELD                             
         MVC   UNBOTH,BCSPACES     CLEAR THIS UNSCAN FIELD SET                  
*                                                                               
         TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BZ    DFDS18              SINGLE VALUE ONLY TO DISPLAY                 
*                                                                               
DFDS08   TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BO    DFDS14              XXX-YYY BOTH SET                             
         TM    RTFILT2,FDRF2V1 FIRST VALUE SET ONLY SET                         
         BZ    DFDS12              NO                                           
*                                                                               
         XR    RF,RF               DISPLAY XXX-                                 
         IC    RF,SAVE.FLT1LEN                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FLTDATA  MOVE IN FILTER DATA                          
                                                                                
         BAS   RE,DODIS            DO THE DISPLAY ROUTINE CALL                  
         BH    EXIT                FILTER NOT KNOWN                             
*                                                                               
         LA    RF,UN1                                                           
         TM    RTFILT2,FDRF2NE     'NOT' THIS WANTED ?                          
         BZ    *+12                                                             
         MVI   0(RF),FLTNEG        MOVE IN NOT                                  
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         LA    R0,UN1                                                           
         SR    R0,RE               R0=L'MODIFIERS                               
         LA    RE,L'UN1                                                         
         SR    RE,R0               RE=LENGTH OF FIELD LEFT                      
         BCTR  RE,0                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      MOVE IN DATA INTO SPACE LEFT                 
         LA    R1,L'UN1                                                         
         LA    RF,UN1+L'UN1-1                                                   
         CLI   0(RF),C' '                                                       
         BH    DFDS10              DOESN`T FIT WITH SPACE TO SPARE              
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         LA    RF,1(RF)                                                         
DFDS10   MVI   0(RF),FLTRNG                                                     
         B     DFDS26                                                           
*                                                                               
DFDS12   XR    RF,RF               DISPLAY -YYY                                 
         IC    RF,SAVE.FLT1LEN                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FLTDATA  MOVE IN FILTER DATA                          
                                                                                
         BAS   RE,DODIS            DO THE DISPLAY ROUTINE CALL                  
         BH    EXIT                FILTER NOT KNOWN                             
*                                                                               
         LA    RF,UN1                                                           
         TM    RTFILT2,FDRF2NE     'NOT' THIS WANTED ?                          
         BZ    *+12                                                             
         MVI   0(RF),FLTNEG        MOVE IN NOT                                  
         LA    RF,1(RF)                                                         
         MVI   0(RF),FLTRNG        MOVE IN RANGE SYMBOL                         
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         LA    R0,UN1                                                           
         SR    R0,RE               R0=L'MODIFIERS                               
         LA    RE,L'UN1                                                         
         SR    RE,R0               RE=LENGTH OF FIELD LEFT                      
         BCTR  RE,0                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      MOVE IN DATA INTO SPACE LEFT                 
         B     DFDS26                                                           
*                                                                               
DFDS14   XR    RF,RF               DISPLAYING A RANGE OF VALUES                 
         IC    RF,SAVE.FLT1LEN                                                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FLTDATA  MOVE IN FILTER DATA FOR XXX-                 
                                                                                
         BAS   RE,DODIS            DO THE DISPLAY ROUTINE CALL                  
         BH    EXIT                FILTER NOT KNOWN                             
*                                                                               
         LA    RF,UN1                                                           
         TM    RTFILT2,FDRF2NE     'NOT' THIS WANTED ?                          
         BZ    *+12                                                             
         MVI   0(RF),FLTNEG        MOVE IN NOT                                  
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         LA    R0,UN1                                                           
         SR    R0,RE               R0=L'MODIFIERS                               
         LA    RE,L'UN1                                                         
         SR    RE,R0               RE=LENGTH OF FIELD LEFT                      
         BCTR  RE,0                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      MOVE IN DATA INTO SPACE LEFT                 
         LA    R1,L'UN1                                                         
         LA    RF,UN1+L'UN1-1                                                   
         CLI   0(RF),C' '                                                       
         BH    DFDS16              DOESN`T FIT WITH SPACE TO SPARE              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         LA    RF,1(RF)                                                         
*                                                                               
DFDS16   MVI   0(RF),FLTRNG        FIRST PART DONE                              
         LA    RF,1(RF)                                                         
         ST    RF,RTADR            SAVE NEXT FREE                               
*                                                                               
         XR    RF,RF               DISPLAY YYY                                  
         IC    RF,SAVE.FLT1LEN                                                  
         LA    RE,FLTDATA(RF)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),0(RE)    MOVE IN FILTER DATA FOR YYY                  
                                                                                
         BAS   RE,DODIS            DO THE DISPLAY ROUTINE CALL                  
         BH    EXIT                FILTER NOT KNOWN                             
*                                                                               
         L     RF,RTADR                                                         
         LR    RE,RF                                                            
         LA    R0,UN1                                                           
         SR    R0,RE               R0=L'MODIFIERS AND XXX-                      
         LA    RE,L'UN1                                                         
         SR    RE,R0               RE=LENGTH OF FIELD LEFT                      
         BCTR  RE,0                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      MOVE IN DATA INTO SPACE LEFT                 
         B     DFDS26                                                           
*                                                                               
DFDS18   XR    RF,RF               DISPLAY SINGLE VALUE                         
         IC    RF,SAVE.FLT1LEN     --------------------                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FLTDATA  MOVE IN THIS TIME FILTER DATA                
                                                                                
         BAS   RE,DODIS            DO THE DISPLAY ROUTINE CALL                  
         BH    EXIT                FILTER NOT KNOWN                             
*                                                                               
         TM    RTFILT1,FF-(FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ)                    
         BZ    DFDS24              NO OVERRIDE TO CONSIDER                      
         LA    RF,FLTB                                                          
         USING FLTBD,RF                                                         
         XR    RE,RE                                                            
*                                                                               
DFDS20   CLI   FLTBCMP,EOT         TRY TO MATCH ON FILTER CODES                 
         BE    DFDS24                                                           
         IC    RE,FLTBTRUE         SEE IF THIS WAS THE FILTER                   
         EX    RE,*+8                                                           
         BO    DFDS22              VALIDATE FILTER COMBINATION                  
         TM    RTFILT1,0                                                        
         LA    RF,FLTBLEN(RF)      ITERATE THROUGH TABLE                        
         B     DFDS20                                                           
*                                                                               
DFDS22   XR    RE,RE                                                            
         IC    RE,FLTBLN           LENGTH-1 OF FILTER FOR EX                    
         EX    RE,*+4                                                           
         MVC   UN1(0),FLTBCMP      MOVE IN MODIFIER                             
         LA    RE,1(RE)            RE=ACTUAL LENGTH OF MODIFIER                 
         LA    RF,L'UN1                                                         
         SR    RF,RE               AMOUNT LEFT IN UNSCAN FIELD                  
*                                                                               
         LA    RE,UN1(RE)          WHERE DATA GOES AFTER MODIFIER               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD      MOVE IN DATA FROM FVIFLD                     
         B     DFDS26                                                           
*                                                                               
DFDS24   MVC   UN1,FVIFLD                                                       
*                                                                               
DFDS26   XR    RF,RF                                                            
         IC    RF,SAVE.FLT1LEN                                                  
         TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BNO   *+8                                                              
         SLL   RF,1                2X AS LONG IF RANGE XXX-YYY                  
*                                                                               
         LA    R6,2(RF,R6)         NEXT DATA ON ELEMENT                         
         LA    R5,UNBLKLQ(R5)      NEXT UNSCAN BLOCK                            
         LH    RF,RTCOUNT                                                       
         BCT   RF,DFDS06                                                        
*                                                                               
         LA    R0,RTSCAN           A(SCAN BLOCK)                                
         ICM   R0,8,SAVE.FLTCNT    NUMBER OF FIELDS TO OUTPUT                   
         CLI   SAVE.FLTCNT,1                                                    
         BNE   DFDS28                                                           
         LR    RE,R0               A(SCAN BLOCK)                                
         LA    RF,L'UN1+L'UN2-1                                                 
         EX    RF,*+8                                                           
         BH    DFDS28                                                           
         CLC   0(0,RE),BCSPACES    PACIFY UNSCAN                                
         LTR   R4,R4               A(FIELD) PASSED?                             
         BNZ   DFDS32              YES                                          
         B     DFDS34                                                           
*                                                                               
DFDS28   LTR   R4,R4               A(FIELD) PASSED?                             
         BNZ   DFDS30              YES                                          
*                                                                               
         ICM   RF,7,=CL3'$LT'                                                   
         ICM   RF,8,=AL1(L'UN1)                                                 
         GOTOX VUNSCAN,RTPARM,(R0),(C'C',FVIFLD),0,(RF)                         
         LA    R1,L'FVIFLD                                                      
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,FVILEN                                                        
         MVI   FVXLEN,0                                                         
         LTR   R1,R1                                                            
         BZ    DFDS34                                                           
         BCTR  R1,0                                                             
         STC   R1,FVXLEN                                                        
         B     DFDS34                                                           
*                                                                               
DFDS30   ICM   R4,8,=AL1(0)                                                     
         ICM   RF,7,=CL3'$LT'                                                   
         ICM   RF,8,=AL1(L'UN1)                                                 
         GOTOX VUNSCAN,RTPARM,(R0),((R4)),0,(RF)                                
*                                                                               
DFDS32   LA    R0,L'FVIFLD                                                      
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         CLC   FVIXHDR,BCSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
DFDS34   LR    RE,R4               PROTECT AGAINST NTRSES DESTRUCTION           
         S     RE,ATWA                                                          
         CH    RE,GSDSPMAX         MAX SCREEN SIZE ALLOWED                      
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FDRLVL,FDRIKEY      IS THIS A KEY FIELD?                         
         BZ    EXITOK              YES                                          
         LTR   R4,R4                                                            
         BZ    EXITOK                                                           
         GOTOX AGEN,RTPARM,OKEY,KSAVE,RTPARMS4,0,0                              
         BL    EXIT                                                             
         B     EXITOK                                                           
         DROP  SAVE                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DO A DISPLAY CALL TO THE OVERLAY                         *         
* RETURNS CC EQUAL IF ALL OK                                          *         
***********************************************************************         
         SPACE 1                                                                
DODIS    NTR1                                                                   
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         LA    RE,ODATA                                                         
         ST    RE,0(R1)            P1=DATA OBJECT                               
         MVI   0(R1),GCBOVER                                                    
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         MVI   0(R1),GCBOVER+GCBPS SET CALL PS OVERLAY                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P2=EQUATED FIELD NUMBER                      
         LA    RE,DFDIS                                                         
         ST    RE,8(R1)            P3=VERB - 'DISPLAY FILTER'                   
         L     RE,RTPARMS4         P4=A(RECORD)                                 
*        TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
*        BZ    *+8                 IF YES -                                     
*        LA    RE,PSRECKEY         ONLY EVER NEED KEY FOR FILTER DIS            
*                                                                               
         ST    RE,12(R1)           P4=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P5=A(FDREL)                                  
*                                                                               
         GOTOX APRG                                                             
         BH    EXITND              NOT KNOWN                                    
         BL    EXITL               ERROR ON VALIDATE OF FILTER                  
         B     EXITOK                                                           
         DROP  R3,R4,R5,R6                                                      
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATA FILTER VALUE VERB                                     *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(KEY) OR 0                                                *         
* P5 HOLDS A(FLTRL) OR 0                                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAFVAL  LR    R7,RF                                                            
         L     R4,RTPARMS3                                                      
         USING FHD,R4              R4=A(FIELD)                                  
         ST    R4,FVADDR                                                        
*                                                                               
         ICM   R3,15,RTPARMS5                                                   
         BNZ   DFV02               A(FDREL) PASSED?                             
*                                                                               
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
DFV02    TM    FDRLVL,FDRIKEY      TEST KEY FIELD                               
         BZ    DFV04                                                            
         CLI   FHDA,C'/'           TEST RESTORE                                 
         BNE   DFV04                                                            
         GOTOX AGEN,RTPARM,OKEY,KRES,FHD REDISPLAY KEY                          
         BL    EXIT                                                             
*                                                                               
OLD      USING FLTELD,RTWORK3      PREVIOUS FILTER ELEMENT SAVED HERE           
NEW      USING FLTELD,RTWORK4      THIS TIME FILTER ELEMENT BUILT HERE          
         PUSH  USING                                                            
*                                                                               
DFV04    LA    R1,RTPARM           NEW FILTER SEARCH CALL                       
         LA    RE,ODATA                                                         
         ST    RE,0(R1)                                                         
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         MVI   0(R1),GCBPS         SET CALL PS OVERLAY                          
         LA    RE,DSRCH                                                         
         ST    RE,4(R1)                                                         
         LA    RE,FHD                                                           
         ST    RE,8(R1)                                                         
         LA    RE,FDRELD                                                        
         ST    RE,12(R1)                                                        
         L     RF,AGEN                                                          
         BASR  RE,RF                                                            
*                                                                               
         XC    RTWORK3,RTWORK3     CLEAR FILTER BUILD AREA                      
         XC    FLTIFLD,FLTIFLD     RESET FILTER DATA FIELD                      
         L     RF,AFLTELSV                                                      
         USING FLTELSV,RF                                                       
         CLI   FLTELSDT,0          INITIALIZE CORE FILTER RECORD?               
         BNE   DFV06               ALREADY DONE                                 
*                                                                               
         LA    R0,FLTELSDT-FLTELSV+1                                            
         STCM  R0,3,FLTELSV                                                     
         B     DFV08               SET INITIAL LENGTH                           
         DROP  RF                                                               
*                                                                               
DFV06    GOTOX VHELLO,RTPARM,(C'G',CORETAB),('FLTELQ',AFLTELSV),       *        
               (L'FDRNUM,FDRNUM)   GET OLD FILTER ELEMENT                       
         CLI   12(R1),0                                                         
         BNE   DFV08               NO OLD ELEMENT                               
*                                                                               
         L     RF,12(R1)           A(FLTEL)                                     
         USING FLTELD,RF                                                        
         XR    RE,RE                                                            
         IC    RE,FLTLN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   OLD.FLTELD(0),FLTELD   SAVE OLD ELEMENT IN RTWORK3               
         DROP  RF                                                               
*                                                                               
         GOTOX VHELLO,RTPARM,(C'D',CORETAB),('FLTELQ',AFLTELSV),       *        
               (L'FDRNUM,FDRNUM)   DELETE OLD FILTER ELEMENT                    
*                                                                               
DFV08    XC    RTWORK4,RTWORK4     BUILD A NEW FILTER ELEMENT                   
         MVI   NEW.FLTEL,FLTELQ      FILTER ELEMENT                             
         MVC   NEW.FLTNUM,FDRNUM     FIELD NUMBER                               
         MVC   NEW.FLTLVL,FDRLVL     LEVEL OF DATA TO FILTER ON                 
         MVI   NEW.FLTLN,FLTLNQ      EMPTY LENGTH                               
         MVC   NEW.FLT1LEN,FDRFSAVL  LENGTH OF SINGLE SAVE FILTER               
*                                                                               
         TM    FDRINDS1,FDR1IDEF   DEFAULT VALUE POSSIBLE FOR FIELD             
         BZ    RRR                 NO                                           
*                                                                               
         XR    RF,RF               SEE IF THIS FIELD HAS ANY WORTHWHILE         
         IC    RF,FHLN             INPUT TO VALIDATE                            
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+8                                                           
         BH    RRR                                                              
         CLC   FHDA(0),BCSPACES                                                 
*                                                                               
         MVC   FVIFLD,BCSPACES                                                  
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         MVI   3(R1),ODATA                                                      
         MVI   0(R1),GCBOVER                                                    
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         MVI   0(R1),GCBOVER+GCBPS SET CALL PS OVERLAY                          
*                                                                               
         MVC   6(L'FDRNUM,R1),FDRNUM P2=FIELD NUMBER                            
         LA    RE,DDFLTF                                                        
         ST    RE,8(R1)            P3=VERB                                      
         MVC   12(4,R1),RTPARMS4   P4=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P5=A(FDREL)                                  
         GOTOX APRG                                                             
         BL    EXITL               ERROR ON SETTING DEFAULT                     
*                                                                               
         XR    RF,RF               PUT OUTPUT INTO FIELD TO FOOL FVAL           
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
*                                                                               
RRR      MVC   FVMAXL,FDRFMAX      CHECK LENGTHS ARE OK                         
         MVI   FVMINL,0                                                         
         LA    R0,L'FVIFLD                                                      
         GOTOX ('FLDVAL',AGROUTS),FHD VALIDATE INPUT FIELD                      
         BH    DFV72               NOT VALID                                    
         BE    DFV10                                                            
         TM    FDRINDS1,FDR1REQ    INPUT REQUIRED?                              
         BZ    DFV10                                                            
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DFV72                                                            
*                                                                               
DFV10    CLI   FDRFMIN,0           MINIMUM INPUT LENGTH SET?                    
         BE    DFV12               NO                                           
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    DFV12               NO                                           
         CLC   FVILEN,FDRFMIN      LONG ENOUGH?                                 
         BNL   DFV12               YES                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         CLI   FVILEN,0            ANYTHING AT ALL?                             
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DFV72                                                            
*                                                                               
DFV12    CLI   FVILEN,0            ANYTHING TO FILTER ON?                       
         BNE   DFV14               YES                                          
         CLI   OLD.FLTEL,FLTELQ    ANYTHING THERE BEFORE?                       
         BNE   *+8                                                              
         OI    LSSCIND1,LSSCIFLT   SET REBUILD LIST FLAG                        
         TM    FDRFVAL2,FDRF2OWN   OWNER HANDLES DATA?                          
         BZ    DFV74               NO                                           
*                                                                               
         BAS   RE,DOVAL            VALIDATE CALL FOR A OWNER FIELD              
         BH    EXITH                                                            
         BNE   *+12                                                             
         OI    RTVFINDS,RTVFIVAL   VALIDATED OK                                 
         B     DFV64                                                            
         OI    FHOI,FHOITR                                                      
         B     EXITL               SOME ERROR                                   
*                                                                               
DFV14    MVC   RTWORK5,FVIFLD      SAVE THE INPUT FIELD                         
         XC    FLTIFLD,FLTIFLD     CLEAR STORAGE FIELDS                         
         XC    RTWORK1,RTWORK1                                                  
         XC    RTWORK2,RTWORK2                                                  
*                                                                               
         ICM   RF,15,=XL4'6B7E6BFF'  CHANGE C',=' TO C',',X'FF'                 
         GOTOX VSCANNER,RTPARM,(C'C',FVIFLD),(X'86',RTSCAN),(RF)                
         CLI   4(R1),RTSCAN#                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$TMINF)                                           
         B     DFV72               TOO MANY INPUT FIELDS                        
*                                                                               
         TM    FDRFVAL2,FDRF2OWN   OWNER HANDLES DATA                           
         BNO   DFV15               NO - PITY                                    
         BAS   RE,DOVAL            VALIDATE CALL FOR A SINGLE FIELD             
         BH    EXITH                                                            
         BNE   *+12                                                             
         OI    RTVFINDS,RTVFIVAL   VALIDATED OK                                 
         B     DFV64                                                            
*                                                                               
         OI    FHOI,FHOITR                                                      
         B     EXITL               SOME ERROR                                   
*                                                                               
DFV15    TM    FDRFVAL2,FDRF2LST   ALLOWED TO LIST?                             
         BO    DFV16               YES                                          
         CLI   4(R1),1                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$LNVFF)                                           
         B     DFV72               LIST NOT VALID FOR THIS FILTER               
*                                                                               
DFV16    LA    R6,RTSCAN           A(SCANNED INPUT FIELDS)                      
         USING SCANBLKD,R6                                                      
*                                                                               
         MVC   RTFILT1,FDRFVAL1    SET INDICATOR DEFAULTS HERE                  
         NI    RTFILT1,FF-(FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ)                    
         MVC   RTFILT2,FDRFVAL2                                                 
         NI    RTFILT2,FF-(FDRF2V1+FDRF2V2+FDRF2NE)                             
*                                                                               
         XR    RF,RF                                                            
         MVI   NEW.FLTCNT,0                                                     
         MVC   RTCOUNT,=H'1'                                                    
         ICM   RF,1,4(R1)          NO INPUT IN FIELDS                           
         BZ    DFV52                                                            
*                                                                               
         STC   RF,NEW.FLTCNT       COUNT OF ITEMS IN LIST                       
DFV18    STH   RF,RTCOUNT          SAVE COUNT OF LIST FIELDS REMAINING          
*                                                                               
         MVC   RTFILT1,FDRFVAL1    SET INDICATOR DEFAULTS HERE                  
         NI    RTFILT1,FF-(FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ)                    
         MVC   RTFILT2,FDRFVAL2                                                 
         NI    RTFILT2,FF-(FDRF2V1+FDRF2V2+FDRF2NE)                             
*                                                                               
         CLI   SC2NDLEN,0          NO SECOND OPTIONS VALID                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$IARGO)                                           
         B     DFV62                                                            
*                                                                               
         TM    FDRFVAL2,FDRF2NOV   NO OVERRIDES?                                
         BO    DFV36                                                            
*                                                                               
         TM    FDRFVAL2,FDRF2RNG   DO WE SUPPORT A RANGE?                       
         BNO   DFV36                                                            
         LA    RE,1                                                             
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         LA    RF,SC1STFLD(RF)                                                  
         LA    R1,SC1STFLD                                                      
         CLI   0(R1),FLTRNG                                                     
         BE    DFV20                                                            
         BXLE  R1,RE,*-8                                                        
         B     DFV36               NO RANGE INPUT                               
*                                                                               
DFV20    TM    FDRFVAL2,FDRF2RNG   DO WE SUPPORT A RANGE?                       
         BO    DFV22               YES                                          
         MVC   FVERRNDX,SC1STNUM   CURSOR POSITION ON INVALID FIELD             
         MVC   FVMSGNO,=AL2(GE$IARGO)                                           
         B     DFV62                                                            
*                                                                               
X        USING SCANBLKD,RTSCANS                                                 
DFV22    MVC   X.SCANBLKD(SCBLKLQ),SCANBLKD                                     
         MVC   X.SCONEFLD,BCSPACES                                              
         MVI   X.SC1STLEN,0        RANGE FORMAT IS XXX-YYY                      
         MVI   X.SC2NDLEN,0                                                     
*                                                                               
         LA    RF,SC1STFLD                                                      
         CLI   0(RF),FLTNEG        'NOT' RANGE WANTED                           
         BNE   *+12                                                             
         OI    RTFILT2,FDRF2NE     FLAG 'NOT' RANGE                             
         LA    RF,1(RF)                                                         
         ST    RF,GCFULL2          IN CASE WE WANT -YYY ONLY                    
*                                                                               
         CLI   0(RF),FLTRNG        -YYY ONLY WANTED                             
         BE    DFV24               YES                                          
         ST    RF,GCFULL1          A(START OF XXX-)                             
*                                                                               
         CLI   0(RF),FLTRNG        SCAN FOR THE '-'                             
         BE    *+12                WE KNOW IT'S THERE SOMEWHERE                 
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         ST    RF,GCFULL2          SAVE A('-')                                  
         S     RF,GCFULL1                                                       
         STC   RF,X.SC1STLEN       RF=LENGTH OF FIRST PORTION                   
         BCTR  RF,0                                                             
         L     RE,GCFULL1                                                       
         EX    RF,*+4              MOVE IN THE XXX PART                         
         MVC   X.SC1STFLD(0),0(RE)                                              
*                                                                               
DFV24    L     RF,GCFULL2          A('-')                                       
         LA    RF,1(RF)                                                         
         ST    RF,GCFULL1          A(YYY PORTION START)                         
         LA    RE,SCONEFLD+L'SCONEFLD-1                                         
         S     RE,GCFULL1                                                       
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8              SEE IF REST OF FIELD HAS INPUT               
         BE    DFV26               NO INPUT XXX- ONLY                           
         CLC   0(0,RF),BCSPACES                                                 
*                                                                               
         OI    RTFILT2,FDRF2V2 SET SECOND VALUE FLAG                            
         LA    RE,SCONEFLD+L'SCONEFLD-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8              LOOK FOR LAST NON-SPACE                      
         LA    RE,1(RE)                                                         
         S     RE,GCFULL1                                                       
         STC   RE,X.SC2NDLEN       RE=LENGTH OF YYY PORTION                     
         BCTR  RE,0                                                             
         EX    RE,*+4              MOVE IN YYY PORTION                          
         MVC   X.SC2NDFLD(0),0(RF)                                              
*                                                                               
DFV26    MVC   SCANBLKD(SCBLKLQ),X.SCANBLKD                                     
         DROP  X                                                                
*                                                                               
         CLI   SC1STLEN,0          DOES USER WANT -YYY ONLY?                    
         BE    DFV30               YES                                          
         OI    RTFILT2,FDRF2V1 FLAG RANGE 'FROM' VALUE INPUT                    
*                                                                               
         LA    RF,FIXSCRN          DUMMY UP SCREEN WITH 1ST VALUE               
         LA    R5,SC1STFLD         ------------------------------               
         XR    R0,R0                                                            
         IC    R0,SC1STLEN                                                      
         CLI   0(R5),FLTNEG        NEGATIVE ON FRONT?                           
         BNE   *+12                NO                                           
         LA    R5,1(R5)            MOVE PAST NEGATIVE                           
         SH    R0,=H'1'            REDUCE LENGTH                                
*                                                                               
         GOTOX (RF),RTPARM,(R5),(R0)                                            
*                                                                               
         BAS   RE,DOVAL            CALL OVERLAY TO VALIDATE DATA                
         BH    EXIT                VALIDATION NOT KNOWN                         
         BE    DFV28               CARRY ON FOR SECOND HALF OF RANGE            
*                                                                               
         MVC   FVERRNDX,SC1STNUM   CURSOR POSITION ON INVALID FIELD             
         MVC   FVMSGNO,=AL2(GE$IARGO)                                           
         B     DFV62                                                            
*                                                                               
DFV28    MVC   RTWORK1,FLTIFLD     SAVE FIRST PIECE OF FILTER DATA              
         XC    FLTIFLD,FLTIFLD     RESET FIELD                                  
*                                                                               
DFV30    CLI   SC2NDLEN,0          VALIDATE SECOND HALF OF A RANGE              
         BE    DFV32               -------------------------------              
         OI    RTFILT2,FDRF2V2 FLAG RANGE 'TO' VALUE INPUT                      
*                                                                               
         LA    RF,FIXSCRN          DUMMY UP SCREEN WITH 2ND FIELD               
         LA    R5,SC2NDFLD                                                      
         XR    R0,R0                                                            
         IC    R0,SC2NDLEN                                                      
         GOTOX (RF),RTPARM,(R5),(R0)                                            
*                                                                               
         BAS   RE,DOVAL            CALL OVERLAY TO VALIDATE DATA                
         BE    *+14                                                             
         MVC   FVERRNDX,SC2NDNUM                                                
         B     DFV62               FIELD NOT VALID                              
*                                                                               
         MVC   RTWORK2,FLTIFLD     SAVE SECOND FILTER DATA                      
*                                                                               
DFV32    XR    RF,RF               DUMMY UP FLTIFLD FOR ELEMENT BUILD           
         IC    RF,FDRFSAVL         LENGTH OF SAVE DATA                          
         BCTR  RF,0                                                             
         LA    RE,FLTIFLD          START OF DATA                                
*                                                                               
         TM    RTFILT2,FDRF2V1 'FROM' VALUE INPUT ?                             
         BZ    DFV34               NO                                           
         EX    RF,*+4                                                           
         MVC   0(0,RE),RTWORK1                                                  
         LA    RE,1(RF,RE)         NEXT FREE IN FLTIFLD                         
*                                                                               
DFV34    TM    RTFILT2,FDRF2V2 'TO' VALUE INPUT ?                               
         BZ    DFV54               NO                                           
         EX    RF,*+4                                                           
         MVC   0(0,RE),RTWORK2                                                  
         B     DFV54               NOW CHANGE ELEMENT                           
*                                                                               
DFV36    TM    FDRFVAL2,FDRF2NOV   NO OVERRIDES?                                
         BO    DFV44                                                            
*                                                                               
         LA    RF,FLTB             SINGLE VALUE - CHECK MODIFIERS               
         USING FLTBD,RF            ------------------------------               
*                                                                               
DFV38    CLI   FLTBCMP,EOT         END OF TABLE                                 
         BE    DFV44               NO MODIFIER                                  
         XR    RE,RE                                                            
         IC    RE,FLTBLN                                                        
         EX    RE,*+8              MODIFIER ON FRONT OF DATA?                   
         BE    DFV40                                                            
         CLC   FLTBCMP(0),SC1STFLD                                              
         LA    RF,FLTBLEN(RF)                                                   
         B     DFV38               ALL POSSIBLE MODIFIERS IN FLTB               
*                                                                               
DFV40    IC    RE,FLTBTRUE                                                      
         EX    RE,*+8                                                           
         BO    DFV42               VALID FILTER COMBINATION?                    
         TM    FDRFVAL1,0                                                       
         MVC   FVMSGNO,=AL2(GE$INFLT)                                           
         MVC   FVERRNDX,SC1STNUM                                                
         B     DFV62               FILTER NOT VALID                             
*                                                                               
DFV42    OC    RTFILT1,FLTBTRUE    SAVE FILTER MODIFICATION BITS                
         ICM   RF,15,FLTBADR                                                    
         A     RF,RTRELO           TAKE FILTER MODIFIERS OFF FRONT              
         BR    RF                                                               
         DROP  RF                                                               
*                                                                               
DFV44    LA    R0,SC1STFLD         DUMMY UP INPUT FIELD, BUT DON'T              
         XR    RE,RE               REMOVE ANY CHARACTERS FROM FRONT             
         IC    RE,SC1STLEN                                                      
         LA    RF,FIXSCRN                                                       
         GOTOX (RF),RTPARM,(R0),(RE)                                            
         B     DFV52                                                            
*                                                                               
DFV46    LA    R0,SC1STFLD+1       REMOVE 1 CHARACTER FROM FIELD FRONT          
         XR    RE,RE               -----------------------------------          
         IC    RE,SC1STLEN         ACTUAL INPUT LENGTH                          
         SH    RE,=H'1'            NUMBER TO REMOVE FROM FRONT                  
         LA    RF,FIXSCRN          ROUTINE TO DO IT                             
         GOTOX (RF),RTPARM,(R0),(RE)                                            
         B     DFV52                                                            
*                                                                               
DFV48    LA    R0,SC1STFLD+2       REMOVE 2 CHARACTERS FROM FIELD FRONT         
         XR    RE,RE               ------------------------------------         
         IC    RE,SC1STLEN                                                      
         SH    RE,=H'2'                                                         
         LA    RF,FIXSCRN                                                       
         GOTOX (RF),RTPARM,(R0),(RE)                                            
         B     DFV52                                                            
*                                                                               
DFV50    LA    R0,SC1STFLD+3       REMOVE 3 CHARACTERS FROM FIELD FRONT         
         XR    RE,RE               ------------------------------------         
         IC    RE,SC1STLEN                                                      
         SH    RE,=H'3'                                                         
         LA    RF,FIXSCRN                                                       
         GOTOX (RF),RTPARM,(R0),(RE)                                            
         B     DFV52                                                            
*                                                                               
DFV52    BAS   RE,DOVAL            VALIDATE A SINGLE FIELD                      
         BE    *+14                                                             
         MVC   FVERRNDX,SC1STNUM                                                
         B     DFV62               ERROR ON VALIDATE                            
*                                                                               
DFV54    LA    R1,NEW.FLTINFO      FIRST SET OF FLAGS/DATA                      
         XR    RE,RE                                                            
         IC    RE,NEW.FLTLN                                                     
         CLI   NEW.FLTLN,FLTLNQ    FIRST TIME IN?                               
         BE    *+8                 YES                                          
         LA    R1,NEW.FLTELD(RE)   BUMP TO NEXT FREE                            
         USING FLTINFO,R1                                                       
                                                                                
         MVC   FLTIND1,RTFILT1     SAVE FILTER INDICATORS 1                     
         MVC   FLTIND2,RTFILT2     SAVE FILTER INDICATORS 2                     
         XR    RF,RF                                                            
         ICM   RF,1,FDRFSAVL       LENGTH OF FILTER DATA TO SAVE                
         BZ    EXITOK              NOTHING IS TO BE SAVED - END HERE            
*                                                                               
         TM    FLTIND2,FDRF2V1+FDRF2V2                                          
         BNO   *+8                                                              
         SLL   RF,1                DOUBLE LENGTH FOR A RANGE                    
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FLTDATA(0),FLTIFLD  MOVE IN STORED FILTER DATA                   
*                                                                               
         CLI   NEW.FLTLN,FLTLNQ    FIRST TIME IN?                               
         BE    *+8                 NO                                           
         LA    RE,2(RE)            FILTER INDICATORS LENGTH ADJUSTMENT          
*                                                                               
         LA    RF,1(RE,RF)         ADJUST LENGTH OF ELEMENT                     
         STC   RF,NEW.FLTLN        AND SAVE IT                                  
*                                                                               
         TM    RTFILT1,FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ                         
         BZ    *+8                                                              
         OI    GCFLAG,FF           DEFAULT FILTER VALUES OVERRIDE FLAG          
*                                                                               
         TM    RTFILT2,FDRF2NE+FDRF2V1+FDRF2V2                                  
         BZ    *+8                                                              
         OI    GCFLAG,FF           RANGE HAS BEEN SET                           
         DROP  R1                                                               
*                                                                               
         LH    RF,RTCOUNT          NUMBER OF FIELDS LEFT                        
         LA    R6,SCBLKLQ(R6)      NEXT SCANNER BLOCK                           
         BCT   RF,DFV18            CARRY ON FILTER VALIDATION FOR LIST          
*                                                                               
         MVC   FVIFLD,RTWORK5      PUT BACK WHAT USER INPUT IN FIELD            
         OI    RTVFINDS,RTVFIVAL   FIELD IS VALID                               
*                                                                               
         CLI   OLD.FLTEL,FLTELQ    OLD FLTEL TO VALIDATE ?                      
         BE    DFV56               YES                                          
         CLI   NEW.FLTLN,FLTLNQ    ANYTHING TO FILTER ON THIS TIME ?            
         BE    DFV74               NO - EXIT                                    
*                                                                               
DFV56    CLC   OLD.FLTLN,NEW.FLTLN                                              
         BNE   DFV58               FILTER LENGTH CHANGED                        
*                                                                               
         XR    RE,RE               COMPARE OLD FLTEL WITH NEW FLTEL             
         IC    RE,OLD.FLTLN        IF LENGTHS ARE THE SAME                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         BE    DFV60               NOTHING CHANGED IN THIS FILTER               
         CLC   OLD.FLTELD(0),NEW.FLTELD                                         
*                                                                               
DFV58    OI    LSSCIND1,LSSCIFLT   FLAG LIST FILTER CHANGED                     
         NI    LSSTAT2,FF-LSTSFULL                                              
         CLI   NEW.FLTLN,FLTLNQ    ANY FILTER DATA TO SAVE?                     
         BE    DFV74                                                            
*                                                                               
DFV60    CLI   NEW.FLTCNT,1        IF LIST MUST START FILTERING AT TOP          
         BNH   *+8                                                              
         OI    GCFLAG,FF                                                        
         CLI   NEW.FLTCNT,0        FOR IF SETTING DEFAULT                       
         BNE   *+8                                                              
         MVI   NEW.FLTCNT,1                                                     
*                                                                               
         GOTOX VHELLO,RTPARM,(C'P',CORETAB),AFLTELSV,NEW.FLTELD                 
         CLI   12(R1),0                                                         
         BE    DFV64                                                            
         DC    H'0'                ERROR ON PUT FOR HELLO                       
         POP   USING                                                            
*                                                                               
DFV62    MVC   FVIFLD,RTWORK5      PUT BACK WHAT USER INPUT                     
         NI    RTVFINDS,FF-(RTVFIVAL)                                           
*                                                                               
DFV64    TM    FDRINDS1,FDR1RDIS   REDISPLAY THIS FIELD IF VALID?               
         BZ    DFV66               NO                                           
         TM    RTVFINDS,RTVFIVAL   FIELD VALID?                                 
         BO    DFV68               YES                                          
*                                                                               
DFV66    TM    GCINDS1,GCIREDIS    TEST REDISPLAY REGARDLESS                    
         BZ    DFV70               NO                                           
         NI    GCINDS1,FF-GCIREDIS                                              
*                                                                               
DFV68    XR    R0,R0                                                            
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         LA    R0,GCBPS            SET CALL PS OVERLAY                          
*                                                                               
         GOTOX AGEN,RTPARM,((R0),ODATA),DFDIS,FHD,RTPARMS4,0                    
         BNH   *+10                                                             
         MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         BNE   DFV72                                                            
*                                                                               
DFV70    XR    RE,RE                                                            
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         MVC   FHIL,FVIHDR+FHILD                                                
         MVC   FHOI,FVIHDR+FHOID                                                
*                                                                               
         TM    RTVFINDS,RTVFIVAL   FIELD VALID?                                 
         BZ    DFV72               NO - EXIT ON ERROR                           
*                                                                               
         TM    FDRLVL,FDRIKEY      TEST FILTER KEY TYPE FIELD                   
         BZ    DFV74                                                            
         LTR   R4,R4               A(FIELD) PASSED                              
         BZ    DFV74                                                            
         GOTOX AGEN,RTPARM,OKEY,KSAVE,RTPARMS4                                  
         BNH   *+10                                                             
         MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         BNE   DFV72                                                            
         B     DFV74                                                            
*                                                                               
DFV72    OI    FHOI,FHOITR         ERROR EXIT                                   
         NI    FHII,FF-FHIIVA                                                   
         NI    LSSTAT2,FF-LSTSFULL                                              
         GOTOX VHELLO,RTPARM,(C'P',CORETAB),AFLTELSV,NEW.FLTELD                 
         CLI   12(R1),0                                                         
         BE    EXITL                                                            
         DC    H'0'                ERROR ON PUT FOR HELLO                       
         SPACE 2                                                                
DFV74    OI    FHOI,FHOITR         OK EXIT                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DO A VALIDATION CALL TO THE OVERLAY                      *         
* RETURNS CC EQUAL & SETS RTVFIVAL IF ALL OK                          *         
* R3 HOLDS A(FDREL)                                                   *         
***********************************************************************         
         SPACE 1                                                                
DOVAL    NTR1                                                                   
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         LA    RE,ODATA                                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),GCBOVER                                                    
         TM    RTPARMS,GCBPS       RECORD FROM PREVIOUS SESSION?                
         BZ    *+8                                                              
         MVI   0(R1),GCBOVER+GCBPS SET CALL PS OVERLAY                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DFVAL                                                         
*                                                                               
         ST    RE,8(R1)            P2=VERB                                      
         L     RE,RTPARMS4         P4=A(RECORD)                                 
*                                                                               
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         NI    RTVFINDS,FF-(RTVFIVAL)                                           
         GOTOX APRG                                                             
         BH    EXITND                                                           
         BL    EXITL               ERROR ON VALIDATE OF FILTER                  
*                                                                               
         OI    RTVFINDS,RTVFIVAL   FIELD IS VALID - FLAG OK                     
         B     EXITOK                                                           
         DROP  OLD,NEW                                                          
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DUMMY UP A SCREEN FIELD WITH SOME DATA & VALIDATE IT     *         
* P1 A(DATA)                                                          *         
* P2 L'DATA                                                           *         
* SCREEN FIELD IS IN R4                                               *         
***********************************************************************         
         SPACE 1                                                                
FIXSCRN  NTR1  ,                   ROUTINE TO DUMMY UP A SCREEN FIELD           
         L     RF,0(R1)            RF=A(DATA)                                   
         L     RE,4(R1)            RE=L'DATA TO MOVE IN                         
         BCTR  RE,0                                                             
         XR    R1,R1                                                            
         IC    R1,FHLN                                                          
*                                                                               
         SH    R1,=Y(FHDAD+FHDAD+1)                                             
         EX    R1,*+4                                                           
         XC    FHDA(0),FHDA        CLEAR THE INPUT FIELD                        
         CR    RE,R1                                                            
         BNH   *+6                                                              
         LR    RE,R1               TOO MUCH STUFF FOR FIELD                     
         EX    RE,*+4                                                           
         MVC   FHDA(0),0(RF)       MOVE IN THE DATA TO THE INPUT FIELD          
*                                                                               
         LA    R0,L'FVIFLD         REVALIDATE INPUT FIELD                       
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         B     EXITOK              AND GO BACK TO CALLER                        
*                                                                               
         DROP  R3,R4                                                            
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DO FILTER VERB                                                      *         
*                                                                     *         
* P3 HOLDS A(FLTEL)                                                   *         
* P4 HOLDS A(KEY OR RECORD AT CORRECT LEVEL)                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTAFDO   LR    R7,RF                                                            
         L     R3,RTPARMS3                                                      
         USING FLTELD,R3                                                        
*                                                                               
TEMP     USING FLTELD,RTWORK3                                                   
DUMMY    USING FLTELD,RTWORK4                                                   
         XC    RTWORK3,RTWORK3                                                  
         XR    RF,RF                                                            
         IC    RF,FLTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TEMP.FLTELD(0),FLTELD SAVE FLTEL                                 
         DROP  R3                                                               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TEMP.FLTCNT                                                   
         LA    R4,TEMP.FLTINFO     FIRST INDICATOR                              
         USING FLTINFO,R4                                                       
*                                                                               
DFDO02   STH   RF,RTCOUNT          SAVE NUMBER OF ITEMS IN LIST                 
*                                                                               
         MVC   RTFILT1,FLTIND1 GET INDICATORS FOR THIS FILTER                   
         MVC   RTFILT2,FLTIND2                                                  
*                                                                               
         XC    FLTIFLD,FLTIFLD                                                  
         TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BNZ   DFDO04              TRYING TO DO A RANGE                         
*                                                                               
         BAS   RE,SINGLE           FILTER ON SINGLE VALUE                       
         BE    EXITOK              PASSED ON THIS SINGLE VALUE                  
         B     DFDO14              FAILED ON THIS SINGLE VALUE                  
*                                                                               
DFDO04   TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BO    DFDO08              XXX-YYY BOTH SET                             
*                                                                               
         TM    RTFILT2,FDRF2V1 FIRST VALUE SET ONLY SET                         
         BZ    DFDO06              NO                                           
*                                                                               
         OI    RTFILT1,FDRF1NOT+FDRF1LT :- XXX- MEANS >=XXX                     
         TM    RTFILT2,FDRF2NE             'NOT' THIS WANTED ?                  
         BZ    *+8                                                              
         NI    RTFILT1,FF-(FDRF1NOT)                                            
         BAS   RE,SINGLE           FILTER ON A SINGLE VALUE                     
         BE    EXITOK              PASSED FILTER ON THIS VALUE                  
         B     DFDO14              KEEP GOING DOWN LIST                         
*                                                                               
DFDO06   OI    RTFILT1,FDRF1NOT+FDRF1GT :-  -YYY MEANS <=                       
         TM    RTFILT2,FDRF2NE              'NOT' THIS WANTED ?                 
         BZ    *+8                                                              
         NI    RTFILT1,FF-(FDRF1NOT)                                            
         BAS   RE,SINGLE           FILTER ON A SINGLE VALUE                     
         BE    EXITOK              PASSED FILTER ON THIS VALUE                  
         B     DFDO14                                                           
*                                                                               
DFDO08   XC    RTWORK4,RTWORK4     FILTERING ON A RANGE OF VALUES               
         LA    RF,FLTLNQ           ------------------------------               
         SH    RF,=H'3'                                                         
         EX    RF,*+4              MOVE IN FIXED PORTION OF DATA                
         MVC   DUMMY.FLTELD(0),TEMP.FLTELD                                      
*                                                                               
         OI    RTFILT1,FDRF1NOT+FDRF1LT :- XXX- MEANS >=XXX                     
         TM    RTFILT2,FDRF2NE             'NOT' WANTED ?                       
         BZ    *+8                                                              
         NI    RTFILT1,FF-(FDRF1NOT)                                            
*                                                                               
         MVC   DUMMY.FLTIND1,RTFILT1                                            
         MVC   DUMMY.FLTIND2,RTFILT2                                            
         IC    RF,TEMP.FLT1LEN     LENGTH OF SINGLE FILTER DATA                 
         LA    RE,FLTLNQ(RF)                                                    
         STC   RE,DUMMY.FLTLN      SAVE DUMMY LENGTH                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   DUMMY.FLTDATA(0),FLTDATA                                         
*                                                                               
         BAS   RE,DFDNTR           FILTER ON FIRST PART OF RANGE                
         BH    EXITH               FILTER NOT DEFINED                           
         BL    DFDO10              FAILED THIS FILTER                           
*                                                                               
*                                  PASSED FIRST PART OF RANGE TEST              
*                                  -------------------------------              
         TM    RTFILT2,FDRF2NE     'NOT' MEANS ALL RESULTS IN RANGE             
         BO    EXITOK              OF FROM - TO ARGUMENT, SO NO NEED            
         B     DFDO12              TO PASS SECOND TEST ALSO                     
*                                                                               
*                                  FAILED FIRST PART OF RANGE TEST              
*                                  -------------------------------              
DFDO10   TM    RTFILT2,FDRF2NE     'NOT' MEANS ALL RESULTS NOT IN RANGE         
         BO    DFDO12              OF FROM - TO ARGUMENT, SO MUST TRY           
         B     DFDO14              SECOND VALUE ALSO                            
*                                                                               
DFDO12   XR    RF,RF               TEST SECOND HALF OF RANGE HERE               
         IC    RF,TEMP.FLT1LEN     ------------------------------               
         LA    RE,FLTDATA(RF)      SECOND PORTION OF RANGE FILTER               
         BCTR  RF,0                                                             
         EX    RF,*+4              MOVE IT IN                                   
         MVC   DUMMY.FLTDATA(0),0(RE)                                           
*                                                                               
         OI    RTFILT1,FDRF1NOT+FDRF1GT :- -YYY MEANS <=                        
         TM    RTFILT2,FDRF2NE                                                  
         BZ    *+8                                                              
         NI    RTFILT1,FF-(FDRF1NOT)                                            
         BAS   RE,DFDNTR           FILTER SECOND PORTION                        
         BE    EXITOK              PASSED RANGE FILTERING                       
*                                                                               
DFDO14   XR    RF,RF                                                            
         IC    RF,TEMP.FLT1LEN                                                  
         TM    RTFILT2,FDRF2V1+FDRF2V2                                          
         BNO   *+8                                                              
         SLL   RF,1                2X AS LONG IF RANGE XXX-YYY                  
*                                                                               
         LA    R4,2(RF,R4)         NEXT DATA ON ELEMENT                         
         LH    RF,RTCOUNT                                                       
         BCT   RF,DFDO02                                                        
         B     EXITL               FAILURE ON ALL PARTS OF THE LIST             
         SPACE 2                                                                
***********************************************************************         
* BUILD AND FILTER ON A SINGLE DUMMY FILTER ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
SINGLE   NTR1  ,                   BUILD A SINGLE DUMMY FILTER ELEMENT          
         CLI   TEMP.FLTCNT,1       PART OF A LIST ?                             
         BE    SGL02               NO                                           
*                                                                               
         TM    RTFILT1,FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ                         
         BNZ   *+8                 OVERRIDES HAVE BEEN SET BY CALLER            
*                                                                               
         OI    RTFILT1,FDRF1EQ     IF PART OF A LIST WITH NO OVERRIDES,         
*                                  FILTER VALUE IS EQUAL                        
*                                                                               
SGL02    XC    RTWORK4,RTWORK4     BUILD A SINGLE DUMMY FILTER ELEMENT          
         LA    RF,FLTLNQ                                                        
         SH    RF,=H'3'                                                         
         EX    RF,*+4                                                           
         MVC   DUMMY.FLTELD(0),TEMP.FLTELD                                      
         MVC   DUMMY.FLTIND1,RTFILT1                                            
         MVC   DUMMY.FLTIND2,RTFILT2                                            
         IC    RF,TEMP.FLT1LEN                                                  
         LA    RE,FLTLNQ(RF)                                                    
         STC   RE,DUMMY.FLTLN                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   DUMMY.FLTDATA(0),FLTDATA                                         
*                                                                               
         BAS   RE,DFDNTR                                                        
         BNE   EXITL               FAILED TEST                                  
         B     EXITOK              PASSED TEST                                  
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A FLTEL BUILT IN ADDRESS COVERED BY DUMMY.FLTELD    *         
***********************************************************************         
         SPACE 1                                                                
DFDNTR   NTR1  ,                                                                
         XR    RF,RF                                                            
         IC    RF,DUMMY.FLTLN                                                   
         SH    RF,=Y(FLTLNQ+1)                                                  
         BNM   *+6                                                              
         DC    H'0'                INVALID FILTER ELEMENT                       
*                                                                               
         EX    RF,*+4              MOVE DATA INTO FLTIFLD                       
         MVC   FLTIFLD(0),DUMMY.FLTDATA                                         
*                                                                               
         TM    RTFILT1,FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ                         
         BZ    DFD04               NO OVERRIDES SET BY CALLER                   
*                                                                               
         TM    RTFILT1,FDRF1GT+FDRF1LT+FDRF1EQ                                  
         BNZ   DFD02               TEST IF JUST 'NOT' OVERRIDE SET              
*                                                                               
         NI    RTFILT1,FF-(FDRF1NOT+FDRF1GT+FDRF1LT+FDRF1EQ)                    
         OI    RTFILT1,FDRF1NOT+FDRF1EQ  'NOT' MEANS 'NOT EQUAL'                
*                                                                               
DFD02    XR    RF,RF               OVERRIDES SET BY CALLER                      
         IC    RF,RTFILT1                                                       
         SRL   RF,4                                                             
         STC   RF,RTFILT1          OVERRIDE DEFAULTS SET BY RECORD              
*                                                                               
DFD04    XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,DUMMY.FLTNUM                                                
         ST    RE,4(R1)            P2=EQUATED FIELD NUMBER                      
         LA    RE,DFDO                                                          
         ST    RE,8(R1)            P3=VERB - 'DO FILTER'                        
         L     RE,RTPARMS4         A(KEY OR RECORD)                             
         MVC   12(4,R1),RTPARMS4   P4=A(RECORD AT CORRECT LEVEL)                
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND              THIS DATA IS NOT KNOWN                       
*                                                                               
         TM    0(R1),GCBOVER                                                    
         BO    EXITOK              NO RETURN CODE DEFINED - DEFAULT IS          
*                                  TO PASS THIS FILTER AS OK                    
*                                                                               
         CLI   0(R1),DFLTX         DEFINATELY NOT WANTED FOR THE LIST           
         BE    EXITL                                                            
*                                                                               
         CLI   0(R1),DFLTE         RETURN CODE FROM APP. FILTER                 
         BL    DFLOW               RETURN CODE OF 0 = LOW                       
         BE    DFEQ                               1 = EQUAL                     
         BH    DFHI                               2 = HIGH                      
*                                                                               
DFLOW    LA    RF,LOTAB            RETURN CODE WAS LOW                          
         B     DFD06                                                            
*                                                                               
DFEQ     LA    RF,EQTAB            RETURN CODE WAS EQUAL                        
         B     DFD06                                                            
*                                                                               
DFHI     LA    RF,HITAB            RETURN CODE WAS HIGH                         
         B     DFD06                                                            
*                                                                               
         USING FOVD,RF                                                          
DFD06    CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                RTFILT1 IS FUCKED UP                         
*                                                                               
         CLC   FOVVAL,RTFILT1      TRY TO MATCH CONDITIONS                      
         BE    DFD08                                                            
         LA    RF,FOVLQ(RF)        ITERATE TABLE OF KNOWN COMPARATORS           
         B     DFD06                                                            
*                                                                               
DFD08    ICM   RF,15,FOVADD        TELLS WHETHER GOOD OR BAD RETURN             
         A     RF,RTRELO           FOR THIS FILTER                              
         BR    RF                                                               
         DROP  RF                                                               
*                                                                               
LOTAB    DC    AL1(FDRD1LT),AL4(EXITOK)          <                              
         DC    AL1(FDRD1EQ),AL4(EXITL)           =                              
         DC    AL1(FDRD1GT),AL4(EXITL)           >                              
         DC    AL1(FDRD1NOT+FDRD1LT),AL4(EXITL)  *<                             
         DC    AL1(FDRD1NOT+FDRD1GT),AL4(EXITOK) *>                             
         DC    AL1(FDRD1NOT+FDRD1EQ),AL4(EXITOK) *=                             
         DC    AL1(EOT)                                                         
*                                                                               
EQTAB    DC    AL1(FDRD1LT),AL4(EXITL)           <                              
         DC    AL1(FDRD1EQ),AL4(EXITOK)          =                              
         DC    AL1(FDRD1GT),AL4(EXITL)           >                              
         DC    AL1(FDRD1NOT+FDRD1LT),AL4(EXITOK) *<                             
         DC    AL1(FDRD1NOT+FDRD1EQ),AL4(EXITL)  *=                             
         DC    AL1(FDRD1NOT+FDRD1GT),AL4(EXITOK) *>                             
         DC    AL1(EOT)                                                         
*                                                                               
HITAB    DC    AL1(FDRD1LT),AL4(EXITL)           <                              
         DC    AL1(FDRD1EQ),AL4(EXITL)           =                              
         DC    AL1(FDRD1GT),AL4(EXITOK)          >                              
         DC    AL1(FDRD1NOT+FDRD1LT),AL4(EXITOK) *<                             
         DC    AL1(FDRD1NOT+FDRD1EQ),AL4(EXITOK) *=                             
         DC    AL1(FDRD1NOT+FDRD1GT),AL4(EXITL)  *>                             
         DC    AL1(EOT)                                                         
*                                                                               
FOVD     DSECT                                                                  
FOVVAL   DS    AL1                                                              
FOVADD   DS    AL4                                                              
FOVLQ    EQU   *-FOVD                                                           
*                                                                               
GEFIL01  CSECT                                                                  
         LTORG                                                                  
         DROP  R4,TEMP,DUMMY                                                    
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB ON NTRSES GIVEN FDREL                             *         
*                                                                     *         
* P3 HOLDS A(FDREL)                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTADNTR  LR    R7,RF                                                            
         ICM   R3,15,RTPARMS3                                                   
         USING FDRELD,R3                                                        
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DNTR                                                          
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BL    EXITL                                                            
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA VERB FOR RECORD                                        *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTARDIS  LR    R7,RF                                                            
         L     R4,8(R1)            R4=A(FIELD) OR 0                             
         USING FHD,R4                                                           
*                                                                               
         NI    GCINDS1,FF-GCIINFLD TURN OFF INPUT FIELD FLAG                    
*                                                                               
         ST    R4,FVADDR           SAVE A(FIELD)                                
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         TM    FHAT,FHATPR         TEST PROTECTED FIELD                         
         BO    *+8                                                              
         OI    GCINDS1,GCIINFLD    FLAG AS INPUT FIELD                          
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED FIELD HEADER                   
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         MVC   FVIFLD,BCSPACES     SPACE FILL INPUT FIELD                       
         XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DRDIS                                                         
         ST    RE,8(R1)            P2=VERB - 'DISPLAY'                          
*                                                                               
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND              THIS DATA IS NOT KNOWN                       
*                                                                               
         LA    R0,L'FVIFLD         FIND FIRST NON-SPACE                         
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,FVILEN           SET INPUT LENGTH                             
         BCTR  R0,0                                                             
         STC   R0,FVXLEN           SET INPUT LENGTH-1                           
*                                                                               
         LR    RE,R4               PROTECT AGAINST NTRSES DESTRUCTION           
         S     RE,ATWA                                                          
         CH    RE,GSDSPMAX         MAX SCREEN SIZE ALLOWED                      
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         OI    FHII,FHIIVA                                                      
         MVC   FHIL,FVIHDR+FHILD                                                
         MVC   FHOI,FVIHDR+FHOID                                                
         OI    FHOI,FHOITR                                                      
*                                                                               
         TM    FDRLVL,FDRIKEY                                                   
         BZ    EXITOK                                                           
         LTR   R4,R4               A(FIELD) PASSED?                             
         BZ    EXITOK              NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OKEY,KSAVE,GSRECKEY                                  
         BL    EXIT                                                             
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATA VERB                                                  *         
*                                                                     *         
* P3 HOLDS A(FIELD)                                                   *         
* P4 HOLDS A(RECORD IF NOT AIOREC) - ONLY VALID FOR RECORD LEVEL      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DTARVAL  LR    R7,RF                                                            
         L     R4,8(R1)            R4=A(FIELD) OR 0                             
         USING FHD,R4                                                           
         ST    R4,FVADDR                                                        
*                                                                               
         MVC   FVIHDR,FHD          COPY FIELD HEADER                            
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    RE,FHD(RE)                                                       
         SH    RE,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BCTR  RF,0                MAKE IT ZERO-BASED                           
         SLL   RF,2                MULTIPLY BY 4                                
         L     R3,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R3,0(RF,R3)         A(THIS FIELD ENTRY)                          
         L     R3,0(R3)            THIS FIELD ENTRY                             
         USING FDRELD,R3                                                        
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
*                                                                               
         TM    FDRLVL,FDRIKEY      TEST KEY FIELD                               
         BZ    DRVAL02                                                          
         CLI   FHDA,C'/'           TEST RESTORE                                 
         BNE   DRVAL02                                                          
         GOTOX AGEN,RTPARM,OKEY,KRES,FHD REDISPLAY KEY                          
         BL    EXIT                                                             
*                                                                               
DRVAL02  GOTOX AGEN,RTPARM,ODATA,DSRCH,FHD,FDRELD                               
*                                                                               
DRVAL04  MVC   FVMAXL,FDRFMAX      CHECK LENGTHS ARE OK                         
         LA    R0,L'FVIFLD                                                      
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         BH    DRVALN                                                           
         BE    DRVAL06                                                          
         TM    FDRINDS1,FDR1REQ    INPUT REQUIRED?                              
         BZ    DRVAL06                                                          
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DRVALN                                                           
*                                                                               
DRVAL06  CLI   FDRFMIN,0           MINIMUM INPUT LENGTH SET?                    
         BE    DRVAL08             NO                                           
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    DRVAL08             NO                                           
         CLC   FVILEN,FDRFMIN      LONG ENOUGH?                                 
         BNL   DRVAL08             YES                                          
         TM    FDRINDS1,FDR1IDEF   DEFAULT VALUE?                               
         BO    DRVAL08             YES                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         CLI   FVILEN,0            ANYTHING AT ALL?                             
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     DRVALN                                                           
*                                                                               
DRVAL08  XC    RTPARM,RTPARM       CLEAR PARAMETER LIST                         
         LA    R1,RTPARM           BUILD PARAMETER LIST                         
         XR    RE,RE                                                            
         ICM   RE,3,FDRNUM                                                      
         ST    RE,4(R1)            P1=EQUATED FIELD NUMBER                      
         LA    RE,DRVAL                                                         
*                                                                               
         TM    FDRINDS1,FDR1IDEF   DEFAULT CAN BE SET?                          
         BZ    *+16                                                             
         CLI   FVILEN,0            ANY INPUT TO VALIDATE?                       
         BNE   *+8                                                              
         LA    RE,DDFLTR                                                        
*                                                                               
         ST    RE,8(R1)            P2=VERB - 'VALIDATE' OR 'SET DEFLT'          
*                                                                               
         ST    RE,8(R1)            P2=VERB - 'VALIDATE' OR 'SET DEFLT'          
         ICM   RE,15,RTPARMS4                                                   
         BNZ   *+8                                                              
         L     RE,AIOREC                                                        
         TM    FDRLVL,FDRIKEY      IS IT A KEY FIELD?                           
         BZ    *+8                 NO                                           
         LA    RE,GSRECKEY                                                      
         TM    FDRLVL,FDRITSAR     IS IT ON TSAR RECORD?                        
         BZ    *+8                 NO                                           
         L     RE,ATLST                                                         
         ST    RE,12(R1)           P3=A(RECORD AT CORRECT LEVEL)                
         ST    R3,16(R1)           P4=A(FDREL)                                  
*                                                                               
         NI    RTVFINDS,FF-(RTVFIVAL)                                           
         GOTOX APRG,RTPARM,('GCBOVER',ODATA)                                    
         BH    EXITND                                                           
         BL    *+8                 ERROR ON VALIDATE                            
         OI    RTVFINDS,RTVFIVAL   FIELD IS VALID                               
*                                                                               
DRVAL10  TM    FDRINDS1,FDR1RDIS   REDISPLAY THIS FIELD IF VALID?               
         BZ    DRVAL12             NO                                           
         TM    RTVFINDS,RTVFIVAL   FIELD VALID?                                 
         BO    DRVAL14             YES                                          
*                                                                               
DRVAL12  TM    GCINDS1,GCIREDIS    TEST REDISPLAY REGARDLESS                    
         BZ    DRVAL16             NO                                           
         NI    GCINDS1,FF-GCIREDIS                                              
*                                                                               
DRVAL14  GOTOX AGEN,RTPARM,ODATA,DRDIS,FHD,0                                    
         BNH   *+10                                                             
         MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         BNE   DRVALN                                                           
*                                                                               
DRVAL16  XR    RE,RE                                                            
         IC    RE,FHLN             COPY HEADER AND DATA BACK                    
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         EX    RE,*+4                                                           
         MVC   FHDA(0),FVIFLD                                                   
         MVC   FHAT,FVIHDR+FHATD                                                
         MVC   FHII,FVIHDR+FHIID                                                
         MVC   FHIL,FVIHDR+FHILD                                                
         MVC   FHOI,FVIHDR+FHOID                                                
*                                                                               
         TM    RTVFINDS,RTVFIVAL                                                
         BZ    DRVALN                                                           
         TM    FDRLVL,FDRIKEY      TEST RECORD KEY TYPE FIELD                   
         BZ    DRVALOK                                                          
         LTR   R4,R4                                                            
         BZ    DRVALOK                                                          
         GOTOX AGEN,RTPARM,OKEY,KSAVE,GSRECKEY                                  
         BL    DRVALN                                                           
         B     DRVALOK                                                          
*                                                                               
DRVALOK  OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
DRVALN   NI    FHII,FF-FHIIVA                                                   
         OI    FHOI,FHOITR                                                      
         B     EXITL                                                            
*                                                                               
         DROP  R4,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO OBJECT - PROVIDES INTERFACE TO IO ROUTINE                        *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
IO       LR    R7,RF                                                            
         TM    GSINDSL1,GSINOIO   OVERRIDING IO OBJECT?                         
         BZ    IO02                                                             
         MVC   RTPARM,RTPARMS                                                   
         GOTOX APRG,RTPARM                                                      
         B     EXIT                                                             
*                                                                               
IO02     LM    R0,R2,RTPARMS                                                    
         LA    RF,TABLEIO                                                       
         B     ITER                                                             
*                                                                               
TABLEIO  DC    AL1(IDIRGET),AL1(0,0,0),AL4(GETDIR)                              
         DC    AL1(IRECGET),AL1(0,0,0),AL4(GETREC)                              
         DC    AL1(IRECRD),AL1(0,0,0),AL4(RDREC)                                
         DC    AL1(IRECADD),AL1(0,0,0),AL4(ADDREC)                              
         DC    AL1(IRECWRT),AL1(0,0,0),AL4(WRTREC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* GET DIRECTORY RECORD AND FORMAT IT INTO GSRECKEY/STA/DA             *         
* ASSUMES KEY IS IN GSRECKEY & ONLY WORKS FOR CURRENT SYSTEM          *         
***********************************************************************         
         SPACE 1                                                                
GETDIR   ICM   R1,15,=AL4(XIO11+XORDD) READ DELETED FILES                       
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         RTFILDIR=A(DIRECTORY TABLE ENTRY)            
         USING NFITABD,RF                                                       
*                                                                               
         XC    IOKEY,IOKEY                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF DIRECTORY KEY                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   IOKEY(0),GSRECKEY                                                
*                                                                               
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                MULTIPLY IT BY 256 FOR EQUATE                
         LA    R1,0(RE,R1)                                                      
         GOTOX ('XIO',AGROUTS)     DO THE I/O                                   
*                                                                               
         CLI   IOERR,0             TEST RECORD READ SUCCESSFULLY                
         BE    GDI02                                                            
         CLI   IOERR,IOEDEL        TEST RECORD DELETED                          
         BE    GDI02                                                            
         CLI   IOERR,IOERNF        TEST RECORD NOT ON FILE (YET)                
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    GSRECSTA,GSRECSTA   SAVE THE DIRECTORY STATUS DETAILS            
         XC    GSRECDA,GSRECDA     CLEAR D/A FOR NOT ON FILE                    
         B     GDI04                                                            
*                                                                               
GDI02    L     RF,RTFILDIR                                                      
         XC    GSRECSTA,GSRECSTA   SAVE THE DIRECTORY STATUS DETAILS            
         XR    RE,RE                                                            
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         XR    R1,R1                                                            
         IC    R1,NFIKEYL          LENGTH OF THE KEY                            
*                                                                               
         LA    R3,IOKEY            NORMAL DIRECTORY READ HERE                   
         TM    NFIINDS2,NFIIID                                                  
         BO    *+12                                                             
         L     R3,AIOREC           CONTROL FILE PASSES FULL RECORD              
         LA    R3,2(R3)            ALLOW 2 FOR RECORD LENGTH                    
*                                                                               
         LA    R1,0(R3,R1)         START OF THE CONTROL INFORMATION             
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R1)                                                
*                                                                               
         MVC   GSRECDA,BCEFFS      SET SPECIAL I/S READ                         
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BZ    *+10                NO                                           
         MVC   GSRECDA,IODA        SAVE THE DISK ADDRESS                        
*                                                                               
GDI04    MVC   GSRECMSK,BCEFFS     SET MASK FROM DIRECTORY                      
         GOTOX APRG,RTPARM,('GCBOVER',OKEY),KMASK,GSRECKEY,GSRECMSK             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* GET FILE RECORD FROM GSRECKEY/STA/DA                                *         
*                                                                     *         
* NTRY: R2=XOLOCK TO READ FOR UPDATE, OR 0                            *         
* EXIT: CC=HIGH IF RECORD HAS CHANGED SINCE LAST READ                 *         
***********************************************************************         
         SPACE 1                                                                
GETREC   LH    RF,GSDIRDSP         DIRECTORY ENTRY                              
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR                                                      
         LH    RF,GSFILDSP         FILE ENTRY                                   
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILREC                                                      
         USING NFITABD,RF                                                       
*                                                                               
         NI    GCINDS1,FF-GCIRCSLR TURN OFF CHANGED FLAG FOR USER               
         L     RF,RTFILDIR         A(DIRECTORY ENTRY)                           
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
         L     R3,AIOREC           A(RECORD AREA)                               
         USING GRECD,R3                                                         
         TM    GCINDS1,GCIFILE     TEST ALREADY READ A RECORD                   
         BZ    GRE02               NO                                           
*                                                                               
* REMOVED 4/8/97 BY TSMY                                                        
******** EX    RE,*+8              IS RECORD THE ONE REQUIRED?                  
******** BE    GRE10               YES                                          
******** CLC   GSRECKEY(0),0(R3)                                                
*                                                                               
*                                                                               
GRE02    OI    GCINDS1,GCIFILE     SET READ A RECORD                            
         CLC   GSRECDA,BCEFFS      RECORD IS CONTROL FILE SPECIAL?              
         BE    GRE04               YES                                          
         OC    GSRECDA,GSRECDA     RECORD IS ON FILE?                           
         BNZ   GRE04               YES                                          
*                                                                               
         LR    R0,R3               CLEAR IO AREA COMPLETELY                     
         LH    R1,=Y(IOAREALN)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         SH    R1,=Y(L'IODA+L'IOWORK)                                           
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,RTFILREC         CREATE A VIRGIN RECORD                       
         IC    RE,NFIKEYL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),GSRECKEY                                                 
*                                                                               
         LA    RE,1(RE,R3)         LENGTH OF RECORD HERE                        
         XR    R0,R0                                                            
         IC    R0,NFIDISP          DISP TO FIRST RECORD                         
         AH    R0,=H'1'                                                         
         STCM  R0,3,0(RE)                                                       
*                                                                               
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BO    GRE08               YES                                          
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         IC    R0,NFIKEYL          V/L I/S RECORDS ARE DIFFERENT                
         LR    RF,R0                                                            
         IC    R0,NFICTLL          LENGTH=KEY+STATUS+LENGTH(=2)+1               
         AR    RF,R0                                                            
         LA    RF,3(RF)                                                         
         STCM  RF,3,0(RE)                                                       
         B     GRE08                                                            
*                                                                               
GRE04    CLC   GSRECDA,BCEFFS      IS THIS A CONTROL FILE SPECIAL?              
         BNE   GRE06               NO                                           
*                                                                               
*                              *** CONFIRM THIS IS V/L I/S ***                  
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BZ    *+6                 NO                                           
         DC    H'0'                                                             
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         MVC   IOKEY(L'GSRECKEY),GSRECKEY                                       
         L     RF,RTFILDIR         I/S FILE TABLE ENTRY                         
         XR    R1,R1                                                            
         IC    R1,NFINUM                                                        
         SLL   R1,8                MULTIPLY IT BY 256 FOR EQUATE                
         A     R1,=A(XORDEL+XORD+XIO11)                                         
         LA    R1,0(R2,R1)         SEE IF LOCK OR NOT                           
*                                                                               
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDSK                                                     
         BZ    GRE08                                                            
         DC    H'0'                                                             
*                                                                               
GRE06    L     RF,RTFILREC         RECORD TABLE ENTRY                           
         XR    R1,R1                                                            
         IC    R1,NFINUM                                                        
         SLL   R1,8                MULTIPLY IT BY 256 FOR EQUATE                
         A     R1,=A(XIO11+XORDEL+XOGET)                                        
         LA    R1,0(R2,R1)         SEE IF LOCK OR NOT                           
*                                                                               
         L     RE,AIOREC           IS D/A IN AIOREC ALREADY?                    
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         CLC   GSRECDA,0(RE)                                                    
         BE    *+10                                                             
         MVC   IODAOVER,GSRECDA                                                 
*                                                                               
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDSK                                                     
         BZ    GRE08                                                            
         DC    H'0'                                                             
*                                                                               
GRE08    MVC   RTRECACT,GSRECACT                                                
         GOTOX ('GETRAC',AGROUTS),RTPARM,AIOREC,GCRACADD,GCRACCHA               
         MVC   GSRECACT,GCRACCHA+(RACDATE-RACELD)                               
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ORECH),RMASK,AIOREC,GSRECMSK              
*                                                                               
         CLC   GCLASKEY,GSRECKEY   FIRST TIME?                                  
         BNE   GRE10               NO                                           
         CLC   GSRECACT,RTRECACT   RECORD CHANGED?                              
         BE    GRE10               NO                                           
*                                                                               
         GOTOX ('GETPID',AGROUTS),GCRACCHA+(RACPERS-RACELD)                     
         MVC   FVXTRA(8),BCWORK     - SOMEONE ELSE CHANGED IT                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$RCRED)                                           
         B     GETRECH                                                          
*                                                                               
GRE10    TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    EXITOK              RECORD WRITTEN BACK TO FILE ONLY             
         CLC   GSRECKEY,GCLASKEY                                                
         BNE   EXITOK              KEY HAS CHANGED                              
         OC    GSRECDA,GSRECDA     RECORD IS ON FILE?                           
         BZ    GRE12               NO                                           
         CLC   GSRECDA,BCEFFS      RECORD IS CONTROL FILE SPECIAL?              
         BE    GRE12               NO                                           
*                                                                               
         MVC   IOKEY,GSRECKEY      SET RECORD KEY FOR READ                      
         L     RF,RTFILDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM           GET FILE NUMBER/256                          
         SLL   R1,8                REAL FILE NUMBER                             
         A     R1,=A(XOREAD+XIO11)                                              
         A     R1,RTPARMS3         ADD LOCKED STATUS IF REQUIRED                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               SETS UP THE D/A IN THE IO AREA?              
*                                                                               
GRE12    L     R2,AIO1                                                          
         USING TLSTD,R2                                                         
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         OI    TLKSES,TLKSUPDT                                                  
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSARDH',TLSTD)                       
         BNE   EXITOK                                                           
*                                                                               
         LA    R4,TLUSER                                                        
         L     RF,RTFILDIR                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
         LA    R1,0(RE,R4)         R1=A(LENGTH OF RECORD ON TSAR REC)           
*                                                                               
         BCTR  RE,0                                                             
         L     RF,AIOREC                                                        
         EX    RE,*+8                                                           
         BNE   EXITOK              KEY IS DIFFERENT                             
         CLC   0(0,RF),0(R4)       COMPARE FILE KEY WITH TSAR RECORD            
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,0(R1)          LENGTH OF RECORD ON TSAR RECORD              
*                                                                               
         L     R0,AIOREC           MOVE INTO AIOREC SAVED RECORD                
         LR    R1,R5                                                            
         MVCL  R0,R4                                                            
         B     EXITOK                                                           
*                                                                               
GETRECH  OI    GCINDS1,GCIRCSLR   SET FLAG FOR RECORD CHANGED                   
         B     EXITH                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* GET FILE RECORD FROM DISK ADDRESS                                   *         
*                                                                     *         
* NTRY: P3=XOLOCK TO READ FOR UPDATE, OR 0                            *         
* NTRY: P4=DISK ADDRESS                                               *         
* NTRY: P5=EQUATE FOR IOAREA, OR 0 FOR AIOREC                         *         
* EXIT: CC=HIGH IF RECORD HAS CHANGED SINCE LAST READ                 *         
***********************************************************************         
         SPACE 1                                                                
RDREC    L     R2,ATLST            CURRENT TSAR RECORD                          
         USING TLSTD,R2                                                         
*                                                                               
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         SAVE RECORD I/S FILE                         
         LH    RF,GSFILDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILREC         SAVE RECORD D/A FILE                         
         USING NFITABD,RF                                                       
*                                                                               
         L     RE,RTPARMS4         A( A(RECORD) WHEREVER IT IS )                
         ICM   RE,15,0(RE)         RECORD EXISTS?                               
         BNZ   RDRC02              YES                                          
*                                                                               
         ICM   R3,15,RTPARMS5      EQUATE FOR RECORD AREA REMEMBER              
         BNZ   *+8                                                              
         ICM   R3,15,=AL4(XIO11)   AIOREC IS DEFAULT                            
         SRL   R3,4-2                                                           
         L     R3,AIO1-L'AIO1(R3)  R3=A(RECORD STORAGE AREA)                    
         LR    R0,R3               CLEAR IO AREA COMPLETELY                     
         LH    R1,=Y(IOAREALN)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,RTFILREC         CREATE A VIRGIN RECORD                       
         IC    RE,NFIKEYL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R3),GSRECKEY                                                 
*                                                                               
         LA    RE,1(RE,R3)         LENGTH OF RECORD HERE                        
         XR    R0,R0                                                            
         IC    R0,NFIDISP          DISP TO FIRST RECORD                         
         AH    R0,=H'1'                                                         
         STCM  R0,3,0(RE)                                                       
         B     RDRC12                                                           
*                                                                               
RDRC02   L     RE,RTPARMS4                                                      
         CLC   0(4,RE),BCEFFS      IS THIS A CONTROL FILE SPECIAL?              
         BNE   RDRC08              NO                                           
*                                                                               
*                              *** CONFIRM THIS IS V/L I/S TYPE ***             
         L     RF,RTFILDIR                                                      
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BZ    *+6                 NO                                           
         DC    H'0'                                                             
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         MVC   GCDUB1,NFINAME      SAVE NAME OF FILE                            
         GOTOX VDMGR,RTPARM,DTFADD,GCDUB1                                       
         L     RF,ACOM                                                          
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         BASR  RE,RF                                                            
*                                                                               
         L     R1,RTPARM+12                                                     
         ICM   R1,8,=AL1(0)        ENSURE 24-BIT                                
         ST    R1,RTADR            SAVE A(DTF)                                  
         USING ISDTF,R1                                                         
         LA    RF,*+10             SWITCH INTO 31-BIT MODE FOR MVC              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         LH    RF,ISKEYLN1                                                      
         L     RE,ISPDKEY                                                       
         EX    RF,*+4                                                           
         MVC   RTKEY(0),0(RE)      SAVE CURRENT KEY                             
*                                                                               
         LA    RF,*+10             SWITCH BACK INTO 24-BIT MODE                 
         N     RF,=X'7FFFFFFF'                                                  
         BSM   0,RF                                                             
         DROP  R1                                                               
*                                                                               
         L     RF,ACOM                                                          
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   IOKEY,GSRECKEY      SET RECORD KEY FOR READ                      
         L     RF,RTFILDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM           GET FILE NUMBER/256                          
         SLL   R1,8                REAL FILE NUMBER                             
         LA    R1,XOREAD(R1)       SET TO READ THIS KEY                         
         A     R1,RTPARMS3         ADD IN LOCK STATUS IF REQUESTED              
         A     R1,RTPARMS5         TELL IT I/O AREA TO READ INTO                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
*                                                                               
         L     RF,ACOM                                                          
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         BASR  RE,RF                                                            
*                                                                               
         L     R1,RTADR            A(DTF)                                       
         USING ISDTF,R1            CHECK TO SEE IF KEY CHANGED                  
         LA    RF,*+10             SWITCH INTO 31-BIT MODE FOR CLC              
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         LH    RF,ISKEYLN1                                                      
         L     RE,ISPDKEY                                                       
         EX    RF,SAMEKEY          READING SAME RECORD?                         
         BE    RDRC04              READ SEQUENCED UNDISTURBED                   
         EX    RF,NULLKEY          PREVIOUS KEY BEEN READ?                      
         BZ    RDRC04              NO PREVIOUS KEY READ                         
         B     RDRC06                                                           
         DROP  R1                                                               
*                                                                               
RDRC04   L     RF,ACOM                                                          
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         BASR  RE,RF                                                            
         LA    RF,RDRC12           SWITCH BACK INTO 24-BIT MODE                 
         N     RF,=X'7FFFFFFF'                                                  
         BSM   0,RF                                                             
         DC    H'0'                CHECK THIS WORKS                             
*                                                                               
SAMEKEY  CLC   RTKEY(0),0(RE)                                                   
NULLKEY  OC    RTKEY(0),RTKEY                                                   
*                                                                               
RDRC06   L     RF,ACOM                                                          
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         BASR  RE,RF                                                            
         LA    RF,*+10             SWITCH BACK INTO 24-BIT MODE                 
         N     RF,=X'7FFFFFFF'                                                  
         BSM   0,RF                                                             
*                                                                               
         L     RF,RTFILDIR         DISPLACEMENT TO DIRECTORY FILE               
         MVC   IOKEY,RTKEY         RESTORE READ SEQUENCE?                       
         XR    R1,R1                                                            
         IC    R1,NFINUM           GET FILE NUMBER/256                          
         SLL   R1,8                REAL FILE NUMBER                             
         A     R1,=A(XOHI+XIO1)    YOU CRAP ON IO1 DOING THIS - BUT IT          
         GOTOX ('XIO',AGROUTS)     DOES RESTORE THE READ SEQUENCE               
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG - LOOK AT IOERR              
*                                                                               
         L     R0,AIO1             CLEAR IO1 JUST TO MAKE IT ALL NEAT           
         LH    R1,=Y(IOAREALN)                                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         MVCL  R0,RE                                                            
         B     RDRC12                                                           
*                                                                               
RDRC08   L     RE,RTPARMS4         GETFLD REQUIRED?                             
         CLI   GSSYS,QSCON                                                      
         BNE   RDRC10                                                           
         OC    0(2,RE),0(RE)       CHECK D/A IS 16BIT OR LESS                   
         BNZ   RDRC10              NO - MUST BE ON FILE THEN                    
         ICM   R3,15,RTPARMS5      EQUATE FOR RECORD AREA REMEMBER              
         BNZ   *+8                                                              
         ICM   R3,15,=AL4(XIO11)   NEED AIOREC                                  
         SRL   R3,4-2                                                           
         L     R3,AIO1-L'AIO1(R3)  R3=A(RECORD STORAGE AREA)                    
         L     RF,RTPARMS4                                                      
         ICM   RF,15,0(RF)         GET INDEX INTO TABLE                         
         GOTOX ('GETFLD',AGROUTS),RTPARM,GFGREC,(R3),(RF)                       
         BL    EXITL                                                            
         B     RDRC12                                                           
*                                                                               
RDRC10   L     RF,RTFILDIR         A(DIRECTORY ENTRY)                           
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
*                                                                               
         OI    GCINDS1,GCIFILE                                                  
         L     RF,RTFILREC         RECORD TABLE ENTRY                           
         XR    R1,R1                                                            
         IC    R1,NFINUM                                                        
         SLL   R1,8                MULTIPLY IT BY 256 FOR EQUATE                
         ICM   RF,15,RTPARMS5      LIST NOT BUILT IN AIOREC IF N.Z.             
         BNZ   *+8                                                              
         ICM   RF,15,=AL4(XIO11)                                                
         A     RF,=A(XORDEL)                                                    
         LA    R1,XOGET(RF,R1)                                                  
         A     R1,RTPARMS3         SEE IF LOCK OR NOT                           
*                                                                               
         L     RF,RTPARMS4                                                      
         MVC   IODAOVER,0(RF)                                                   
*                                                                               
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDSK        HARDWARE ERROR?                              
         BZ    *+6                                                              
         DC    H'0'                CHECK IOERR <> 'FF' (SWITCH ERROR)           
*                                                                               
RDRC12   CLC   =AL4(XIO11),RTPARMS5  LISTING REAL RECORDS?                      
         BNE   RDRCX               NO                                           
*                                                                               
         GOTOX ('BLDDIR',AGROUTS),RTPARM,0                                      
*                                                                               
         GOTOX ('GETRAC',AGROUTS),RTPARM,AIOREC,GCRACADD,GCRACCHA               
         MVC   GSRECACT,GCRACCHA+(RACDATE-RACELD)                               
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ORECH),RMASK,AIOREC,GSRECMSK              
*                                                                               
RDRCX    B     EXITOK                                                           
*                                                                               
DTFADD   DC    CL8'DTFADD'                                                      
         SPACE 2                                                                
***********************************************************************         
* ADD RECORD TO FILE                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDREC   TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    *+12                RECORD WRITTEN BACK ALWAYS                   
         CLI   GSFRP.FRPTYPE,FRPTUPDT                                           
         BNE   ARE02               RECORD WRITTEN WITH UPDATE PFKEY             
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),RTPARM,('RACTADD+RACTCHA',AIOREC)             
         MVC   GSRECACT,GCRACCHA+(RACDATE-RACELD)                               
*                                                                               
ARE02    GOTOX AGENLST,RTPARM,OLIST,LMNTUPD                                     
         BL    EXITL                                                            
*                                                                               
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         DIRECTORY                                    
         LH    RF,GSFILDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILREC         FILE                                         
         USING NFITABD,RF                                                       
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    *+12                RECORD WRITTEN BACK ALWAYS                   
         CLI   GSFRP.FRPTYPE,FRPTUPDT                                           
         BNE   ARE06               RECORD WRITTEN WITH UPDATE PFKEY             
*                                                                               
         L     RF,RTFILREC         RECORD FILE DETAILS                          
         XR    RE,RE                                                            
         L     R3,AIOREC           WHERE RECORD IS                              
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         LA    R3,2(RE,R3)         2 IS FOR RECORD LENGTH                       
*                                                                               
         L     RF,RTFILDIR         STATUS LENGTH IN HERE                        
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4              MOVE IN STATUS                               
         MVC   0(0,R3),GSRECSTA                                                 
*                                                                               
         L     RF,RTFILREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                                                             
         ICM   R1,15,=AL4(XIO11+XOADDREC)                                       
*                                                                               
         L     RF,RTFILDIR                                                      
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BO    ARE04               YES                                          
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                WHAT IS IT THEN?                             
         ICM   R1,15,=AL4(XIO11+XOADD)                                          
*                                                                               
ARE04    LA    R1,0(RE,R1)                                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,RTFILDIR         A(DIRECTORY FOR STATUS LENGTH)               
         L     R1,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
         BCTR  RE,0                                                             
         XC    GSRECKEY,GSRECKEY                                                
         EX    RE,*+4                                                           
         MVC   GSRECKEY(0),0(R1)   MOVE IN RECORD KEY                           
*                                                                               
         LA    RE,3(RE)            +1 FOR EX; +2 FOR LENGTH;                    
         LA    R1,0(RE,R1)         A(RECORD STATUS BYTE(S))                     
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                RE=L'RECORD STATUS BYTES-1                   
         XC    GSRECSTA,GSRECSTA                                                
         EX    RE,*+4                                                           
         MVC   GSRECSTA(0),0(R1)   MOVE IN STATUS                               
         MVC   GSRECDA,IODA        MOVE IN D/A                                  
*                                                                               
         L     RF,RTFILDIR                                                      
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BO    ARE06               YES                                          
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         MVC   GSRECDA,BCEFFS      SET SPECIAL CONTROL FILE D/A                 
*                                                                               
ARE06    L     R2,AIO1                                                          
         USING TLSTD,R2                                                         
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         OI    TLKSES,TLKSUPDT                                                  
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSARDH',TLSTD)                       
         BNE   ARE08                                                            
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSADEL',TLSTD)                       
*                                                                               
ARE08    XC    TLKEY,TLKEY         RESET MY KEY                                 
         MVC   TLKSES,TWASESNL                                                  
         OI    TLKSES,TLKSUPDT                                                  
         L     R4,AIOREC                                                        
         L     RF,RTFILDIR                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
         LA    R1,0(RE,R4)         R1=A(LENGTH OF RECORD ON FILE REC)           
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,0(R1)          LENGTH OF RECORD ON FILE RECORD              
         LA    RF,TLMINLNQ(R5)                                                  
         STCM  RF,3,TLRLEN         SET TSAR RECORD LENGTH                       
*                                                                               
         LA    R0,TLUSER           MOVE INTO TSAR RECORD FROM AIOREC            
         LR    R1,R5                                                            
         MVCL  R0,R4                                                            
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSAADD',TLSTD)                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* WRITE RECORD ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
WRTREC   TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    *+12                RECORD WRITTEN BACK ALWAYS                   
         CLI   GSFRP.FRPTYPE,FRPTUPDT                                           
         BNE   WRE02               RECORD WRITTEN WITH UPDATE PFKEY             
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),RTPARM,('RACTCHA',AIOREC)                     
         MVC   GSRECACT,GCRACCHA+(RACDATE-RACELD)                               
*                                                                               
WRE02    GOTOX AGENLST,RTPARM,OLIST,LMNTUPD                                     
         BL    EXITL                                                            
*                                                                               
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         DIRECTORY ENTRY                              
         LH    RF,GSFILDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILREC         FILE ENTRY                                   
         USING NFITABD,RF                                                       
*                                                                               
         TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    *+12                RECORD WRITTEN BACK ALWAYS                   
         CLI   GSFRP.FRPTYPE,FRPTUPDT                                           
         BNE   WRE08               RECORD WRITTEN WITH UPDATE PFKEY             
*                                                                               
         L     RF,RTFILDIR                                                      
         TM    NFIINDS2,NFIIID     HAS THIS A D/A FILE LINKED?                  
         BO    WRE04               YES                                          
*                                                                               
         TM    NFIINDS,NFIIVL      IS THIS A VARIABLE LENGTH I/S THEN?          
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOREC           A(I/S RECORD)                                
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         LA    R2,2(RE,R2)         A(STATUS AREA)                               
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECSTA    MOVE IN STATUS                               
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOWRITE)                                        
         XR    RE,RE                                                            
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                                                             
         LA    R1,0(RE,R1)                                                      
         GOTOX ('XIO',AGROUTS)     WRITE FILE BACK                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) FLAG CLEAN RECORD                         
         TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    EXITOK              RECORD DOES NOT NEED SAVING IN TSAR          
         B     WRE08                                                            
*                                                                               
WRE04    L     RF,RTFILDIR         RECORD FILE DETAILS                          
         XR    RE,RE                                                            
         L     R2,AIOREC           WHERE RECORD IS                              
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         LA    R2,2(RE,R2)         2 IS FOR RECORD LENGTH                       
*                                                                               
         IC    RE,NFICTLL          LENGTH OF DIRECTORY STATUS                   
         BCTR  RE,0                                                             
         EX    RE,*+8              COMPARE STATUS AGAINST DIR STATUS            
         BE    WRE06               UNCHANGED                                    
         CLC   GSRECSTA(0),0(R2)                                                
*                                                                               
         XC    IOKEY,IOKEY         GET DIRECTORY RECORD                         
         MVC   IOKEY(L'GSRECKEY),GSRECKEY                                       
*                                                                               
         L     R1,=AL4(XORDD+XOLOCK)                                            
         L     RF,RTFILDIR                                                      
         IC    RF,NFINUM           GET DIRECTORY NUMBER                         
         SLL   RF,8                MULTIPLY IT BY 256 FOR EQUATE                
         LA    R1,0(RF,R1)                                                      
         GOTOX ('XIO',AGROUTS)     READ FOR UPDATE                              
*                                                                               
         L     RF,RTFILDIR                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF DIRECTORY KEY                      
         LA    R2,IOKEY(RE)                                                     
         IC    RE,NFICTLL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECSTA    MOVE IN THE CHANGED STATUS                   
*                                                                               
         LA    R1,XOWRITE          WRITE BACK THE DIRECTORY                     
         L     RF,RTFILDIR                                                      
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                MULTIPLY IT BY 256 FOR EQUATE                
         LA    R1,0(RE,R1)                                                      
         GOTOX ('XIO',AGROUTS)     DO THE I/O                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WRE06    L     RF,RTFILDIR         PUT BACK RECORD                              
         L     R2,AIOREC                                                        
         XR    RE,RE                                                            
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         LA    R2,2(RE,R2)         2 IS FOR THE RECORD LENGTH                   
         IC    RE,NFICTLL          LENGTH OF STATUS                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),GSRECSTA    MOVE THE CHANGED STATUS INTO RECORD          
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOPUT)                                          
*                                                                               
         L     RF,RTFILREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFINUM           GET FILE NUMBER                              
         SLL   RE,8                                                             
         LA    R1,0(RE,R1)                                                      
         GOTOX ('XIO',AGROUTS)     WRITE FILE BACK                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) FLAG CLEAN RECORD                         
         TM    GSFRR.FRRINDS1,FRR1UPDT                                          
         BZ    EXITOK              RECORD DOES NOT NEED SAVING IN TSAR          
*                                                                               
WRE08    L     R2,AIO1             GET THIS SESSION UPDATIVE RECORD             
         USING TLSTD,R2                                                         
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         OI    TLKSES,TLKSUPDT                                                  
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSARDH',TLSTD)                       
         BNE   WRE10                                                            
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSADEL',TLSTD)                       
*                                                                               
WRE10    LA    R0,TLSTD            RESET IO AREA                                
         LH    R1,=Y(IOAREALN-L'IODA-L'IOWORK)                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TLKSES,TWASESNL     SET THIS IS AN UPDATIVE TSAR RECORD          
         OI    TLKSES,TLKSUPDT                                                  
         L     R4,AIOREC           GET LATEST COPY OF RECORD                    
         L     RF,RTFILDIR                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL                                                       
         LA    R1,0(RE,R4)         R1=A(LENGTH OF RECORD ON FILE REC)           
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,3,0(R1)          R5=LENGTH OF RECORD ON FILE RECORD           
         LA    R1,TLMINLNQ(R5)                                                  
         STCM  R1,3,TLRLEN         R1=TSAR RECORD LENGTH                        
*                                                                               
         LA    R0,TLUSER           MOVE INTO TSAR RECORD FROM AIOREC            
         LR    R1,R5                                                            
         MVCL  R0,R4                                                            
*                                                                               
         GOTOX ('TSARIO',AGROUTS),RTPARM,('TSAADD',TLSTD)                       
         B     EXITOK                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SESSION OBJECT                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SES      LR    R7,RF                                                            
         LM    R0,R2,0(R1)                                                      
         LA    RF,TABLESES                                                      
         B     ITER                                                             
*                                                                               
TABLESES DC    AL1(SRESLVL),AL1(0,0,0),AL4(SESRES)                              
         DC    AL1(SNTR),AL1(0,0,0),AL4(SESNTR)                                 
         DC    AL1(SXIT),AL1(0,0,0),AL4(SESXIT)                                 
         DC    AL1(SNTROUT),AL1(0,0,0),AL4(SESNOUT)                             
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(SESNIN)                               
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(SESXOUT)                             
         DC    AL1(SXITIN),AL1(0,0,0),AL4(SESXIN)                               
         DC    AL1(SCALLER),AL1(0,0,0),AL4(SESS)                                
         DC    AL1(SCALLED),AL1(0,0,0),AL4(CESS)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* RESET SESSION LEVEL                                            *  1 *         
*                                                                ******         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
SESRES   NI    GCINDS2,FF-(GCINTRS+GCIXITS)                                     
         GOTOX ('UPROSCR',AGROUTS)                                              
*                                                                               
         LA    RE,1                RESET NEST LEVEL TO 1                        
         STC   RE,TWASESNL         SAVE OFF NEW NEST LEVEL                      
         BCTR  RE,0                                                             
         SLL   RE,1                SAVE REC/ACT FOR RECURSION PREVENT           
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   0(L'TWASESRA,RE),CSRECACT                                        
*                                                                               
         XC    LSSTAT1,LSSTAT1     CLEAR LIST INDICATORS - REBUILD LIST         
         XC    LSSTAT2,LSSTAT2     AT NEST LEVEL 1 SO ALL IS OK                 
         XC    LSLTIND1,LSLTIND1                                                
         XC    LSLTIND2,LSLTIND2                                                
         XC    LSSCIND1,LSSCIND1                                                
         XC    LSSCIND2,LSSCIND2                                                
         XC    LSLST#1,LSLST#1                                                  
         XC    LSLST#X,LSLST#X                                                  
         XC    LSPAG#1,LSPAG#1                                                  
         XC    LSPAG#X,LSPAG#X                                                  
         XC    LSLINE#,LSLINE#                                                  
*                                                                               
*              MIGHT HAVE TO RESET ALL THE PS... STUFF                          
*                                                                               
         B     EXITOK              THAT SHOULD BE ENOUGH                        
         SPACE 2                                                                
***********************************************************************         
* ENTER NEW SESSION                                              *  2 *         
*                                                                ******         
* NTRY: TWASESNL  = CURRENT LEVEL OF NESTING                          *         
* NTRY: NSSAV     = NEXT SESSION DATA (SEE SSAVD)                     *         
* EXIT: TWASESNL  = NEXT LEVEL                                        *         
***********************************************************************         
         SPACE 1                                                                
N        USING SSAVD,NSSAV         NEXT SESSION                                 
P        USING SSAVD,PSSAV         PREV SESSION                                 
*                                                                               
SESNTR   OI    GCINDS2,GCIDONTR    SET DOING AN NTRSES                          
         CLI   N.SREC,R#ALL        SPECIFY RECORD/ACTION                        
         BNE   *+10                                                             
         MVC   N.SREC,CSREC                                                     
*                                                                               
         CLI   N.SACT,A#ALL                                                     
         BNE   *+10                                                             
         MVC   N.SACT,CSACT                                                     
*                                                                               
         OC    CSREC,CSREC                                                      
         BZ    SESN02                                                           
*                                  ALLOW CALLER TO SET PARAMETERS               
         GOTOX AGEN,RTPARM,OSES,SNTROUT,NSSAV                                   
*                                                                               
         CLI   N.SREC,O#MAX        CONTROLLER PHASE REQUESTED?                  
         BH    *+8                 NO                                           
         NI    N.SNINDS1,FF-(SNIPARMS) CONTROLLER HAS NO PARAMETERS             
*                                                                               
         CLI   N.SREC,O#ACV        RECORD ACTIVITY DETAILS REQUESTED?           
         BNE   *+8                 NO                                           
         OI    N.SNINDS1,SNIUSECR  YES - USE CURRENT RECORD DETAILS             
*                                                                               
         TM    N.SNINDS1,SNIPARMS  CALLER PASSING PARAMETERS?                   
         BZ    SESN02              NO                                           
         GOTOX AGEN,RTPARM,OSES,SCALLER                                         
*                                                                               
SESN02   ICM   RF,12,=C'L='        SAVE TIA                                     
         ICM   RF,3,=Y(TWAMAX)                                                  
         GOTOX VDMGR,RTPARM,DMWRITE,TEMPSTR,(4,0),ATIA,,(RF)                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,TWASESNL         CURRENT NESTING LEVEL                        
         LA    RE,1(RE)            INCREMENT TO NEXT LEVEL                      
         CLM   RE,1,=AL1(TWASESMX)                                              
         BNH   *+6                                                              
         DC    H'0'                MAXIMUM NEST LEVEL EXCEEDED                  
*                                                                               
         STC   RE,TWASESNL         SAVE OFF NEW NEST LEVEL                      
         SRDL  RE,1                /2 FOR ABSOLUTE TEMPSTR PAGE NUMBER          
         STC   RE,BCBYTE1          SAVE TEMPSTR PAGE NUMBER                     
*                                                                               
         IC    RE,TWASESNL         SAVE RECORD/ACTION IN 'USED' LIST            
         BCTR  RE,0                (PREVENTS PROGRAM RECURSION)                 
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         MVC   0(L'TWASESRA,RE),CSRECACT                                        
*                                                                               
         XC    RTHALF1,RTHALF1     DISPLACEMENT INTO TEMPSTR PAGE               
         LTR   RF,RF               USING SECOND HALF OF PAGE?                   
         BNZ   SESN04              YES                                          
         L     R0,ATIA             CLEAR TEMPSTR PAGE (TIA IS BUFFER)           
         LHI   R1,TWAMAX                                                        
         CLI   BCBYTE1,4                                                        
         BNE   *+8                                                              
         LHI   R1,TWAMAX/2         PAGE 4 SECOND HALF HOLDS FDRELS              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     SESN06                                                           
*                                                                               
SESN04   MVC   RTHALF1,=Y(TWAMAX/2) USING SECOND HALF OF A PAGE -               
         ICM   RF,12,=C'L='        SO MUST READ THAT PAGE FROM TEMPSTR          
         ICM   RF,3,=Y(TWAMAX)                                                  
         GOTOX VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                TEMPSTR READ NOT HAPPY                       
*                                                                               
SESN06   L     R2,ATIA             SAVE START ADDRESS INTO TIA                  
         AH    R2,RTHALF1                                                       
         USING FESD,R2             R2=A(SESSION SAVE AREA)                      
*                                                                               
         MVC   FESSCRN,TWASCRN     SAVE GLOBAL VALUES                           
         MVC   FESSCRF,TWASCRF                                                  
*                                                                               
         LA    R0,FESSAVE          SAVE CURRENT OVERLAY SAVE VALUES             
         LA    R1,FESSAVEL                                                      
         LA    RE,TWAD                                                          
         AH    RE,=Y(TWSAVE-TWAD)                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,FESD                                                          
         AH    R0,=Y(FESFLSV-FESD) SAVE CURRENT FILTER VALUES                   
         LA    R1,FESFLSVL                                                      
         LA    RE,TWAD                                                          
         AH    RE,=Y(FLTELSV-TWAD)                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,FESCSSV          SAVE CURRENT SESSION VALUES                  
         LA    R1,FESCSSVL                                                      
         LA    RE,CSVALS                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,FESGSSV          SAVE CURRENT SESSION VALUES                  
         LA    R1,FESGSSVL                                                      
         LA    RE,GSSAVE                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  SAVE CURRENT SCREEN                          
         GOTOX AGEN,RTPARM,OSCRN,SSAVE,FESD                                     
*                                                                               
*                                  WRITE TEMPSTR PAGE TO BUFFER                 
         GOTOX VDMGR,BCPARM,DMWRITE,TEMPSTR,(BCBYTE1,0),ATIA                    
*                                                                               
         LH    R0,=Y(TWSAVE-TWAD)  CLEAR SAVED BLOCK FOR NEW SESSION            
         A     R0,ATWA                                                          
         LA    R1,L'TWSAVE                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,TWAD             CLEAR FILTERS FOR NEW SESSION                
         AH    R0,=Y(FLTELSV-TWAD)                                              
         LA    R1,FLTELSVL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AOVERWRK         CLEAR OVERWRK FOR NEW SESSION                
         LH    R1,=Y(OVERWRKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,GSDSPOPT       CLEAR OPTIONS FIELD                          
         BZ    SESN08                                                           
         A     RE,ATWA                                                          
         USING FHD,RE                                                           
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),BCSPACES                                                 
         OI    FHOI,FHOITR                                                      
         DROP  RE                                                               
*                                                                               
SESN08   MVC   CSPSRECN,CSHIRECN   SET NEXT SESSION LOW TSAR RECORD#            
*                                                                               
         MVC   P2SYS,PSSYS         SAVE ALL PREVIOUS SESSION VALUES             
         MVC   P2PRG,PSPRG                                                      
         MVC   P2REC,PSREC                                                      
         MVC   PSRECACT,CSRECACT                                                
         MVC   PSRECKEY,GSRECKEY                                                
         MVC   PSRECSTA,GSRECSTA                                                
         MVC   PSRECDA,GSRECDA                                                  
         MVC   PSRECSTA,GSRECSTA                                                
         MVC   PSSYS,GSSYS                                                      
         MVC   PSPRG,GCPRGNO                                                    
         MVC   PSLSTAT1,LSSTAT1                                                 
         MVC   PSSMPAGE,GSSMPAGE                                                
         MVC   PSLST#1,LSLST#1                                                  
         MVC   PSLST#X,LSLST#X                                                  
*??      MVC   LSLST#1,LSLST#X                                                  
         MVC   PSFRREL,GSFRREL                                                  
         MVC   PSFRAEL,GSFRAEL                                                  
         MVC   PSSHSYS,GSSHSYS                                                  
         MVC   PSSHPRG,GSSHPRG                                                  
         MVC   PSSHREC,GSSHREC                                                  
         MVC   PSSUBLEN,LSSUBLEN                                                
         MVC   PSINDSL,GSINDSL                                                  
         TM    N.SNINDS1,SNIUSECR  USE CURRENT RECORD?                          
         BZ    SESN10              NO                                           
         OC    GSRECDA,GSRECDA     GOT A CURRENT FILE RECORD?                   
         BZ    SESN10              NO                                           
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECGET,0                                        
         B     SESN12                                                           
*                                                                               
SESN10   XC    GSRECKEY,GSRECKEY   RESET RECORD DETAILS                         
         XC    GSRECSTA,GSRECSTA                                                
         XC    GSRECDA,GSRECDA                                                  
         XC    GSRECMSK,GSRECMSK                                                
         XC    CSRECACT,CSRECACT   RESET RECORD/ACTION                          
*                                                                               
SESN12   XC    CSINDSG,CSINDSG                                                  
         XC    CSINDSL,CSINDSL     RESET CURRENT SESSION INDICATORS             
         MVI   CSINDSL1,CSIUSELC                                                
         XC    GSINDSL,GSINDSL     RESET CURRENT SESSION INDICATORS             
*                                                                               
         MVC   PSSAV,NSSAV         CLEAR NS & SET PS SAVE BLOCKS                
         XC    NSSAV,NSSAV                                                      
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTSET,(P.SREC,0)                              
         BNL   *+6                                                              
         DC    H'0'                SERIOUS ERROR                                
         GOTOX (RF),(R1),OACT,ASET,(P.SACT,0),0                                 
         BNL   *+6                                                              
         DC    H'0'                SERIOUS ERROR                                
*                                  ALLOW CALLED TO USE PARAMETERS               
         GOTOX AGEN,RTPARM,OSES,SNTRIN,PSSAV,FESD                               
*                                                                               
         GOTOX AGEN,RTPARM,ORTYPE,RTDIS                                         
         GOTOX (RF),(R1),OACT,ADIS                                              
*                                                                               
         OI    GCINDS2,GCINTRS     SET NTRSES ISSUED FLAG                       
         NI    GCINDS2,FF-(GCIXITS+GCIDONTR)                                    
         NI    GCINDS1,FF-(GCIMLPRC) RESET MAINT LIST PROCESS STATUS            
*                                                                               
         TM    PSINDSL2,GSI2PLST   PRIOR NEST HAS LIST                          
         BZ    *+8                                                              
         OI    GSINDSL2,GSI2PLST   KEEP STATUS FOR SCREEN PROTECT               
*                                                                               
         TM    PSINDSL1,GSIMLST    LAST SESSION HAS MAINT LIST                  
         BZ    *+8                 NO                                           
         OI    GSINDSL2,GSI2PLST   SET STATUS FOR SCREEN PROTECT                
*                                                                               
         CLI   PSFRA.FRAVERB,ACTLST                                             
         BNE   *+8                 NO                                           
         OI    GSINDSL2,GSI2PLST   SET STATUS FOR SCREEN PROTECT                
*                                                                               
         XC    GS#FDR,GS#FDR       RESET CURRENT COUNT OF FDRELS                
         LA    RE,GSFDRLST         CLEAR LIST OF FDRELS                         
         LA    RF,GS#FDRMX*L'GSFDRLST                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0) GET INTO NATIVE SYSTEM                
*                                                                               
SESNX    OI    GSINDSG1,GSG1MSAV                                                
         L     RD,BCSVRD           RETURN TO ROOT & CONTINUE PROCESSING         
         LM    RF,RC,16(RD)                                                     
         L     RE,GCANTR                                                        
         BR    RE                                                               
         DROP  R2,N,P                                                           
         SPACE 2                                                                
***********************************************************************         
* RESTORE PREVIOUS SESSION                                       *  3 *         
*                                                                ******         
* NTRY: TWASESNL = CURRENT NESTING LEVEL                              *         
*       PSSAV    = PREVIOUS SESSION DATA (SEE SSAVD)                  *         
* EXIT: TWASESNL = PREVIOUS NESTING LEVEL                             *         
***********************************************************************         
         SPACE 1                                                                
N        USING SSAVD,NSSAV                                                      
P        USING SSAVD,PSSAV                                                      
*                                                                               
SESXIT   MVC   XSNXRECN,CSHIRECN   SAVE THIS SESSION INFORMATION                
         MVC   XSRECACT,CSRECACT                                                
         MVC   XSLST#1,LSLST#1                                                  
         MVC   XSLST#X,LSLST#X                                                  
         MVC   XSHOLE,PSHOLE                                                    
         MVC   XSFRPEL,GSFRP.FRPELD                                             
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,TWASESNL         GET CURRENT SESSION NESTING LEVEL            
         SH    RE,=H'1'            DECREMENT TO PREVIOUS LEVEL                  
         BP    *+6                                                              
         DC    H'0'                MINIMUM NEST LEVEL EXCEEDED                  
*                                                                               
         STC   RE,TWASESNL         SAVE NEW NESTING LEVEL                       
         LA    RE,1(RE)                                                         
         SRDL  RE,1                /2 FOR ABSOLUTE TEMPSTR PAGE NUMBER          
         STC   RE,BCBYTE1          SAVE TEMPSTR PAGE NUMBER                     
         L     R2,ATIA                                                          
         LTR   RF,RF               USING SECOND HALF OF PAGE?                   
         BZ    *+8                 NO                                           
         AH    R2,=Y(TWAMAX/2)                                                  
         USING FESD,R2             R2=A(SESSION SAVE AREA)                      
*                                                                               
         ICM   RF,12,=C'L='        READ SAVED TEMPSTR PAGE                      
         ICM   RF,3,=Y(TWAMAX)                                                  
         GOTOX VDMGR,BCPARM,DMREAD,TEMPSTR,(BCBYTE1,0),ATIA,,(RF)               
         BE    *+6                                                              
         DC    H'0'                TEMPSTR READ UNHAPPY...                      
*                                                                               
*                                  ALLOW CALLED TO SET PARAMETERS               
         GOTOX AGEN,RTPARM,OSES,SXITOUT,PSSAV,FESD                              
         MVC   XSSAV,PSSAV                                                      
*                                                                               
         LA    R0,TWAD             RESTORE OVERLAY SAVE AREA                    
         AH    R0,=Y(TWSAVE-TWAD)                                               
         LA    R1,FESSAVEL                                                      
         LA    RE,FESSAVE                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,TWAD             RESTORE FILTER VALUES                        
         AH    R0,=Y(FLTELSV-TWAD)                                              
         LA    R1,FESFLSVL                                                      
         LA    RE,FESD                                                          
         AH    RE,=Y(FESFLSV-FESD)                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,CSVALS           RESTORE SESSION VALUES                       
         LA    R1,FESCSSVL                                                      
         LA    RE,FESCSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   CSNXRECN,XSNXRECN   SET NEXT SESSION HIGH RECORD                 
*                                                                               
         LA    R0,GSSAVE           RESTORE SESSION VALUES                       
         LA    R1,FESGSSVL                                                      
         LA    RE,FESGSSV                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   NSLST#1,XSLST#1                                                  
         MVC   NSLST#X,XSLST#X                                                  
         MVC   NSREC,XSREC                                                      
         MVC   NSACT,XSACT                                                      
         MVC   NSSAV,XSSAV                                                      
         MVC   GSFRP.FRPELD(FRPLNQ),XSFRPEL                                     
*                                                                               
         MVC   TWASCRN,FESSCRN     RESTORE GLOBAL VALUES                        
         MVC   TWASCRF,FESSCRF                                                  
*                                                                               
         MVC   PSHOLE,XSHOLE       RESTORE TWA                                  
         GOTOX AGEN,RTPARM,OSCRN,SREST,FESD                                     
*                                                                               
         OI    GCINDS2,GCIXITS     SET XITSES ISSUED                            
         NI    GCINDS2,FF-(GCINTRS)                                             
         NI    GCINDS1,FF-(GCIMLPRC) REDISPLAY MAINT LIST                       
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,GSDSPREC                                                    
         BZ    *+16                                                             
         A     RF,ATWA                                                          
         NI    FHAT-FHD(RF),FF-FHATPR                                           
         OI    FHOI-FHD(RF),FHOITR                                              
*                                                                               
         XR    RF,RF               UNPROTECT ACTION FIELD                       
         ICM   RF,3,GSDSPACT                                                    
         BZ    *+16                                                             
         A     RF,ATWA                                                          
         NI    FHAT-FHD(RF),FF-FHATPR                                           
         OI    FHOI-FHD(RF),FHOITR                                              
*                                                                               
         OC    CSREC,CSREC         RECORD SET?                                  
         BZ    SESX02              NO                                           
         GOTOX AGEN,RTPARM,ORTYPE,RTSET,(CSREC,0)                               
         BL    SESXX               BAD RECORD                                   
*                                                                               
         OC    CSACT,CSACT         ACTION SET?                                  
         BZ    SESX02              NO                                           
         GOTOX (RF),(R1),OACT,ASET,(CSACT,0),0                                  
         BL    SESXX               BAD ACTION                                   
*                                                                               
SESX02   TM    N.SXINDS1,SXIXRA    DON'T RESTORE RECORD/ACTION?                 
         BO    SESX04              YES                                          
         GOTOX AGEN,RTPARM,ORTYPE,RTDIS                                         
         GOTOX AGEN,RTPARM,OACT,ADIS                                            
*                                                                               
SESX04   TM    N.SXINDS1,SXIXRA    DON'T RESTORE RECORD/ACTION?                 
         BZ    SESX05              YES                                          
         TM    N.SNINDS1,SNINEWR   BCINREC ON AT TIME OF NTRSES?                
         BZ    *+10                                                             
         XC    CSRECACT,CSRECACT   ENSURE BCINREC SET ON RETURN                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,GSDSPREC                                                    
         BZ    *+12                                                             
         A     RF,ATWA                                                          
         NI    FHII-FHD(RF),FF-(FHIIVA)                                         
         XR    RF,RF                                                            
         ICM   RF,3,GSDSPACT                                                    
         BZ    *+12                                                             
         A     RF,ATWA                                                          
         NI    FHII-FHD(RF),FF-(FHIIVA)                                         
*                                                                               
SESX05   XR    R0,R0               ANY FDRELS IN THIS LIST?                     
         ICM   R0,1,GS#FDR                                                      
         BZ    SESX08              NO                                           
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         MVI   GS#FDR,0                                                         
         LA    R2,GSFDRLST         RESTORE FDREL LIST                           
         XR    R3,R3                                                            
*                                                                               
SESX06   GOTOX ('SAVFDR',AGROUTS),RTPARM,(C'N',(R2))                            
         LA    R2,L'GSFDRLST(R2)                                                
         BCT   R0,SESX06                                                        
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),GSSYS  SWITCH INTO NATIVE SYSTEM              
*                                                                               
SESX08   OC    LSLINE#,LSLINE#     CURRENT LIST LINE NUMBER?                    
         BZ    SESX10              NO                                           
         CLC   LSLINE#,LSLST#1     TEST VALIDITY                                
         BL    SESX10              INVALID                                      
         CLC   LSLINE#,LSLST#X                                                  
         BH    SESX10              INVALID                                      
*                                                                               
         L     RF,ATLST            READ CURRENT LIST LINE                       
         MVC   TLNUM-TLSTD(L'TLNUM,RF),LSLINE#                                  
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                  ALLOW CALLER TO SET PARAMETERS               
SESX10   CLI   NSREC,O#MAX                                                      
         BNH   SESX12                                                           
         GOTOX AGEN,RTPARM,OSES,SXITIN,NSSAV                                    
*                                                                               
SESX12   CLI   NSREC,O#ACV        ACTIVITY RECORD?                              
         BNE   *+8                NO                                            
         NI    N.SNINDS1,FF-SNIUSECR  FOOL CONTROLLER FOR MAINT LISTS           
*                                                                               
         OC    LSCURLIN,LSCURLIN  BEEN PROCESSING A LINE?                       
         BZ    SESX14             NO                                            
         LH    RE,LSCURLIN        '?' IN THE SUB-ACT FIELD HERE                 
         A     RE,ATWA                                                          
         USING FHD,RE                                                           
         CLI   FHDA,C'?'          CHECK FOR THE'?'                              
         BNE   SESX14                                                           
         MVI   FHDA,C'*'           SET TO IGNORE THIS LINE                      
         OI    FHOI,FHOITR         RETRANSMIT IT                                
*                                                                               
         CLC   LSSELSUB,BCSPACES   JUST SELECTED A SUB-ACTION?                  
         BNH   SESX14              NO                                           
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),LSSELSUB    SELECTED SUB-ACTION PASSED HERE              
         LA    RF,1(RF)                                                         
         STCM  RF,1,FHIL                                                        
         ST    RE,FVADDR                                                        
         DROP  RE                                                               
*                                                                               
SESX14   OC    CSREC,CSREC         RECORD FIELD SET?                            
         BNZ   SESX16                                                           
         LH    RF,GSDSPREC                                                      
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         XC    CSACT,CSACT                                                      
         B     SESXX                                                            
*                                                                               
SESX16   OC    CSACT,CSACT         ACTION FIELD SET?                            
         BNZ   SESX18                                                           
         LH    RF,GSDSPACT                                                      
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     SESXX                                                            
*                                                                               
SESX18   TM    N.SXINDS1,SXISEL    WAS SOMETHING SELECTED?                      
         BZ    SESX20              NO                                           
         OC    LSSELSUB,LSSELSUB                                                
         BZ    SESX20                                                           
*                                                                               
         LA    RF,L'LSSELSUB                                                    
         LA    R1,LSSELSUB+L'LSSELSUB-1                                         
         CLI   0(R1),C' '          LOOK FOR FIRST SPACE                         
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVXTRA(0),LSSELSUB  SET &T                                       
         XC    LSSELSUB,LSSELSUB   CLEAR SUB-ACTION                             
         NI    BCINDS1,FF-(BCINREC+BCINACT)                                     
*              *** THESE 2 FLAGS ARE SET IF AUTOSELECT PERFORMED                
         MVI   FVOMTYP,GTMINF      SET &T SELECTED MSG                          
         MVC   FVMSGNO,=AL2(GI$SELEC)                                           
         B     SESXX                                                            
*                                                                               
SESX20   MVI   FVOMTYP,GTMINF      SET PREVIOUS SESSION RESTORED MSG            
         MVC   FVMSGNO,=AL2(GI$PSRES)                                           
*                                                                               
SESXX    OI    GSINDSG1,GSG1MSAV                                                
         L     RD,BCSVRD           RETURN TO ROOT                               
         LM    RF,RC,16(RD)                                                     
         L     RE,GCAXIT                                                        
         BR    RE                                                               
         DROP  R2,N,P                                                           
         SPACE 2                                                                
***********************************************************************         
* ENTERING OUT FROM SESSION                                      *  4 *         
*                                                                ******         
* NTRY: P3 = A(NEXT SESSION DATA (SEE SSAVD))                         *         
***********************************************************************         
         SPACE 1                                                                
SESNOUT  ICM   RF,15,AOLY                                                       
         BZ    EXITOK                                                           
         L     R1,RTPARMA                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ENTERING IN TO SESSION                                         *  5 *         
*                                                                ******         
* NTRY: P3 = A(PREVIOUS SESSION DATA (SSAVD))                         *         
*       P4 = A(SAVED FESD)                                            *         
***********************************************************************         
         SPACE 1                                                                
SESNIN   ICM   RF,15,AOLY                                                       
         BZ    EXITOK                                                           
         L     R1,RTPARMA                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* EXITING OUT FROM SESSION                                       *  6 *         
*                                                                ******         
* NTRY: P3 = A(PREVIOUS SESSION DATA (SEE SSAVD))                     *         
*       P4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
         SPACE 1                                                                
SESXOUT  ICM   RF,15,AOLY                                                       
         BZ    EXITOK                                                           
         L     R1,RTPARMA                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* EXITING BACK IN TO SESSION                                     *  7 *         
*                                                                ******         
* NTRY: P3 = A(NEXT SESSION DATA (SEE SSAVD))                         *         
***********************************************************************         
         SPACE 1                                                                
SESXIN   ICM   RF,15,AOLY                                                       
         BZ    EXITOK                                                           
         L     R1,RTPARMA                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* PARSE CALLED'S FIELD RECORDS & SET DATA FROM CALLER            *  8 *         
*                                                                ******         
* DATA IS FORMATTED INTO IO8 & IO9                                    *         
***********************************************************************         
         SPACE 1                                                                
N        USING SSAVD,NSSAV                                                      
P        USING SSAVD,PSSAV                                                      
         USING FDRRECD,IOKEY                                                    
*                                                                               
SESS     GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DNTR),        *        
               GSRECKEY                                                         
         BL    SESTN                                                            
*                                                                               
         MVC   RTSPRG,GCPRGNO      SET PROGRAM                                  
         MVC   RTSREC,N.SREC       SET NEXT RECORD                              
         LA    R0,30               MAX NUMBER OF FILTERS TO PASS                
         L     R4,AIO8             WHERE THEY ARE GOING                         
         USING PRSESD,R4                                                        
*                                                                               
         OC    ASTEST,ASTEST       TEST PHASE?                                  
         BZ    SEST02                                                           
*                                                                               
K        USING FSRRECD,IOKEY                                                    
         XC    K.FSRKEY,K.FSRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,GCOVSYS                                                
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVI   K.FSRKPAGE,FSRKPKEY                                              
         CLI   GSFRA.FRATYPE,FRATLIST LIST OR KEY?                              
         BNE   *+8                                                              
         MVI   K.FSRKPAGE,FSRKPLST                                              
         MVC   K.FSRKCTRY,CUCTRY                                                
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         MVC   K.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SEST06                                                           
*                                                                               
SEST02   XC    K.FSRKEY,K.FSRKEY   READ FOR THIS COUNTRY                        
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,GCOVSYS                                                
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVI   K.FSRKPAGE,FSRKPKEY                                              
         CLI   GSFRA.FRATYPE,FRATLIST LIST OR KEY?                              
         BNE   *+8                                                              
         MVI   K.FSRKPAGE,FSRKPLST                                              
         MVC   K.FSRKCTRY,CUCTRY                                                
         XI    K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
         XC    K.FSRKTEST,K.FSRKTEST                                            
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SEST06              SCREEN FOR THIS COUNTRY                      
*                                                                               
         OC    ASTEST,ASTEST                                                    
         BZ    SEST04                                                           
*                                                                               
         XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND TEST PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,GCOVSYS                                                
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVI   K.FSRKPAGE,FSRKPKEY                                              
         CLI   GSFRA.FRATYPE,FRATLIST LIST OR KEY?                              
         BNE   *+8                                                              
         MVI   K.FSRKPAGE,FSRKPLST                                              
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SEST06                                                           
*                                                                               
SEST04   XC    K.FSRKEY,K.FSRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FSRKMIN,FSRKMINQ  AND LIVE PHASE                               
         MVI   K.FSRKTYP,FSRKTYPQ                                               
         MVC   K.FSRKSYS,GCOVSYS                                                
         MVC   K.FSRKPRG,RTSPRG                                                 
         MVC   K.FSRKREC,RTSREC                                                 
         MVI   K.FSRKPAGE,FSRKPKEY                                              
         CLI   GSFRA.FRATYPE,FRATLIST LIST OR KEY?                              
         BNE   *+8                                                              
         MVI   K.FSRKPAGE,FSRKPLST                                              
         MVI   K.FSRKCTRY,X'FF'                                                 
         MVI   K.FSRKSUB,X'FF'                                                  
*                                                                               
         L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    SEST06                                                           
         DC    H'0'                 MISSING SCREEN RECORD                       
*                                                                               
SEST06   L     R1,=AL4(XOGENFIL+XOGET+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1             SEE IF SHADOW ELEMENT                        
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   *+20                NO SHADOW ELEMENT                            
         L     RF,12(R1)           SAVE SHADOW SCREEN PROG/REC                  
         MVC   RTSPRG,SPRPRG-SPRELD(RF)                                         
         MVC   RTSREC,SPRREC-SPRELD(RF)                                         
*                                                                               
         MVI   GCBYTE1,FDRELQ      SET LIST OR MAINTAIN                         
         CLI   GSFRA.FRATYPE,FRATLIST                                           
         BNE   *+8                                                              
         MVI   GCBYTE1,FLTRLQ                                                   
*                                                                               
P        USING FDRRECD,IOKEY                                                    
         XC    P.FDRKEY,P.FDRKEY   READ FIELD RECORDS FOR CALLED RECORD         
         MVI   P.FDRKMIN,FDRKMINQ                                               
         MVI   P.FDRKTYP,FDRKTYPQ                                               
         MVC   P.FDRKSYS,GCOVSYS   SET SYSTEM                                   
         MVC   P.FDRKPRG,RTSPRG    SET PROGRAM                                  
         MVC   P.FDRKREC,RTSREC    SET NEXT RECORD                              
         XC    P.FDRKNUM,P.FDRKNUM                                              
         GOTOX ('GETFLD',AGROUTS),RTPARM,GFHIGH,AIO2                            
         BE    SEST10                                                           
         DC    H'0'                                                             
*                                                                               
SEST08   GOTOX ('GETFLD',AGROUTS),RTPARM,GFRSEQ,AIO2                            
*                                                                               
SEST10   CLC   FDRKEY(FDRKREC-FDRRECD+1),IOKEYSAV                               
         BNE   SEST22                                                           
         CLI   GSFRA.FRATYPE,FRATLIST                                           
         BNE   SEST12                                                           
         TM    FDRKSTAT,FDRKFLT    LIST ACTION - NEED FILTERS                   
         BZ    SEST08                                                           
         B     SEST14                                                           
*                                                                               
SEST12   TM    FDRKSTAT,FDRKKEY    KEY ACTION - NEED KEY FIELDS                 
         BZ    SEST08                                                           
*                                                                               
SEST14   L     R2,AIO2                                                          
         LA    R2,FDRFIRST(R2)                                                  
         USING FDRELD,R2                                                        
         XR    RF,RF                                                            
*                                                                               
SEST16   CLC   FDREL,GCBYTE1       FDREL/FLTRL DEPENDING ON LIST/MAINT          
         BE    SEST18                                                           
         CLI   FDREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,FDRLN                                                         
         BXH   R2,RF,SEST16                                                     
*                                                                               
SEST18   MVC   RTWORK,IOKEY        SAVE IOKEY IN CASE THE USER WANTS IT         
         GOTOX (SWCHFC,AGROUTS),=AL1(0)                                         
         GOTOX AGEN,RTPARM,ODATA,DNTR,FDRELD,0                                  
         BL    SESTN               BUILD DATA INTO FVIFLD                       
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         MVC   PRSNUM,FDRNUM       SAVE FIELD NUMBER                            
         MVC   PRSDATA,FVIFLD      SAVE FIELD DATA                              
         LA    R4,PRSLQ(R4)        NEXT FREE                                    
*                                                                               
         CLC   IOKEY,RTWORK        READ SEQUENCE CHANGED BY USER?               
         BE    SEST20              NO                                           
*                                                                               
         MVC   IOKEY,RTWORK        RESTORE SEQUENCE                             
         L     R1,=AL4(XOREAD+XOGENDIR+XIO2)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SEST20   BCT   R0,SEST08                                                        
*                                                                               
SEST22   GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DNTR),         *        
               GSRECKEY                                                         
         BL    SESTN                                                            
         B     SESTOK                                                           
*                                                                               
SESTN    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITL                                                            
*                                                                               
SESTOK   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         DROP  R4,N,P,R2                                                        
*                                                                               
PRSESD   DSECT                                                                  
PRSNUM   DS    XL(L'FDRNUM)                                                     
PRSDATA  DS    CL(L'FVIFLD)                                                     
PRSLQ    EQU   *-PRSESD                                                         
*                                                                               
GEFIL01  CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* PARSE DATA FROM CALLER AND SET INFO ON SCREEN                  *  9 *         
*                                                                ******         
* DATA IS DISPLAYED FROM IO8 & IO9                                    *         
***********************************************************************         
         SPACE 1                                                                
CESS     GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         L     R3,ATWA             START OF USER SCREEN                         
         AH    R3,GSDSPOVR                                                      
         USING FHD,R3                                                           
         XR    R5,R5                                                            
*                                                                               
CESS02   ICM   R5,1,FHLN                                                        
         BZ    CESSOK              END OF SCREEN REACHED                        
         TM    FHAT,FHATXH                                                      
         BZ    CESS08              IGNORE IF NO EXTENDED HEADER                 
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTENDED HEADER SAVED HERE                   
*                                                                               
         XR    R4,R4                                                            
         ICM   R4,1,FVIXUS2        FIELD NUMBER (INTERNAL REPN.)                
         BZ    CESS08                                                           
         BCTR  R4,0                MAKE IT ZERO-BASED                           
         L     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         SLL   R4,2                                                             
         LA    RF,0(R4,RF)         A(THIS FIELD ENTRY)                          
         L     R4,0(RF)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         USING FDRELD,R4                                                        
*                                                                               
         L     RF,AIO8                                                          
         USING PRSESD,RF                                                        
CESS04   OC    PRSNUM,PRSNUM       END REACHED                                  
         BZ    CESS08                                                           
         CLC   FDRNUM,PRSNUM       IS IT A KEY FIELD?                           
         BE    CESS06              NO - IGNORE IT                               
         LA    RF,PRSLQ(RF)        NEXT FREE                                    
         B     CESS04                                                           
*                                                                               
CESS06   XR    RE,RE               CLEAR FIELD                                  
         IC    RE,FHLN                                                          
         SH    RE,=Y(FHDAD+FHDAD+1)                                             
         STC   RE,RTBYTE1          RTBYTE=FIELD LENGTH-1                        
         EX    RE,*+4                                                           
         MVC   FHDA(0),PRSDATA                                                  
*                                                                               
CESS08   LA    R3,0(R5,R3)                                                      
         B     CESS02                                                           
*                                                                               
CESS10   B     EXITOK                                                           
*                                                                               
CESSN    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITL                                                            
*                                                                               
CESSOK   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         DROP  R3,R4,RF                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER OBJECT - SITS AT THE HIGHEST LEVEL                           *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(INITIAL KEY BUILD AREA)                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
FILTER   LR    R7,RF                                                            
         LM    R0,R2,0(R1)                                                      
         LA    RF,FLRTABL                                                       
         B     ITER                                                             
*                                                                               
FLRTABL  DC    AL1(FDIS),AL1(0,0,0),AL4(FLRDIS)                                 
         DC    AL1(FVAL),AL1(0,0,0),AL4(FLRVAL)                                 
         DC    AL1(FDOD),AL1(0,0,0),AL4(FLRDOD)                                 
         DC    AL1(FDOR),AL1(0,0,0),AL4(FLRDOR)                                 
         DC    AL1(FDUSR),AL1(0,0,0),AL4(FLRDUSR)                               
         DC    AL1(FFFLT),AL1(0,0,0),AL4(FLRFFLT)                               
         DC    AL1(FBLDKEY),AL1(0,0,0),AL4(FBLDKY)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTERS ONTO SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
FLRDIS   GOTOX AGEN,RTPARM,OKEY,KFDIS,RTPARMS3                                  
         BL    EXITL               ERROR ON KEY FILTER DISPLAY                  
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RFDIS,0                                        
         BL    EXITL               ERROR ON RECORD FILTER DISPLAY               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD FLTELS & INITIAL KEY FOR FIELDS ON SCREEN                     *         
***********************************************************************         
         SPACE 1                                                                
FLRVAL   GOTOX AGEN,RTPARM,OKEY,KFVAL,RTPARMS3                                  
         BL    EXITL               ERROR ON KEY FILTER VALIDATE                 
*                                                                               
         GOTOX AGEN,RTPARM,ORECH,RFVAL,0                                        
         BL    EXITL               ERROR ON RECORD FILTER VALIDATE              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ALL FLTELS FOR DIRECTORY                           *         
*                                                                     *         
* P3 = A(KEY)                                                         *         
***********************************************************************         
         SPACE 1                                                                
FLRDOD   L     R5,AFLTELSV                                                      
         USING FLTELSV,R5                                                       
         LA    R5,FLTELSDT                                                      
         USING FLTELD,R5                                                        
*                                                                               
FDO02    CLI   FLTEL,0             FINISHED PROCESSING FILTERS?                 
         BE    EXITOK              YES                                          
*                                                                               
         TM    FLTLVL,FLTLKEY                                                   
         BZ    FDO04                                                            
         GOTOX AGEN,RTPARM,(0,ODATA),DFDO,FLTELD,(R2)                           
         BNE   EXITL               RETURNS EQUAL IF FILTER IS VALID             
*                                                                               
FDO04    XR    RE,RE                                                            
         IC    RE,FLTLN                                                         
         LA    R5,0(RE,R5)                                                      
         B     FDO02                                                            
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ALL FLTELS PRESENT IN RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
FLRDOR   L     R5,AFLTELSV         WHERE FLTELS ARE IN MEMORY                   
         USING FLTELSV,R5                                                       
         LA    R5,FLTELSDT                                                      
         USING FLTELD,R5                                                        
*                                                                               
FDOR02   CLI   FLTEL,0             FINISHED PROCESSING FILTERS?                 
         BE    EXITOK              YES                                          
*                                                                               
         TM    FLTLVL,FLTLREC                                                   
         BZ    FDOR04                                                           
         GOTOX AGEN,RTPARM,(0,ODATA),DFDO,FLTELD,(R2)                           
         BNE   EXITL                                                            
*                                                                               
FDOR04   XR    RE,RE                                                            
         IC    RE,FLTLN                                                         
         LA    R5,0(RE,R5)                                                      
         B     FDOR02                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON USER TYPE FILTER WHENEVER USER WANTS A FILTER       *         
*                                                                     *         
* P3 = FIELD NUMBER TO FILTER ON                                      *         
* P4 = A(RECORD AT CORRECT LEVEL)   ??                                *         
***********************************************************************         
         SPACE 1                                                                
FLRDUSR  L     R5,AFLTELSV         WHERE FLTELS ARE IN MEMORY                   
         USING FLTELSV,R5                                                       
         LA    R5,FLTELSDT                                                      
         USING FLTELD,R5                                                        
*                                                                               
FDUS02   CLI   FLTEL,0             FINISHED PROCESSING FILTERS?                 
         BE    EXITH               FILTER NOT SET                               
*                                                                               
         TM    FLTLVL,FLTLUSR                                                   
         BZ    FDUS04              A PIECE OF GRATUITOUS SOFT CODE !!           
         CLC   FLTNUM,RTPARMS3+L'RTPARMS3-L'FLTNUM                              
         BNE   FDUS04                                                           
*                                                                               
         GOTOX AGEN,RTPARM,(0,ODATA),DFDO,FLTELD,RTPARMS4                       
         BNE   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
FDUS04   XR    RE,RE                                                            
         IC    RE,FLTLN                                                         
         LA    R5,0(RE,R5)                                                      
         B     FDUS02                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* SEE IF A PARTICULAR FILTER IS PRESENT                               *         
*                                                                     *         
* P3 = FIELD NUMBER OF FILTER                                         *         
***********************************************************************         
         SPACE 1                                                                
FLRFFLT  L     R5,AFLTELSV         WHERE FLTELS ARE IN MEMORY                   
         USING FLTELSV,R5                                                       
         LA    R5,FLTELSDT                                                      
         USING FLTELD,R5                                                        
         XR    RE,RE                                                            
         XC    RTPARMS4,RTPARMS4                                                
*                                                                               
FDFF02   CLI   FLTEL,0             FINISHED PROCESSING FILTERS?                 
         BE    EXITL               FILTER NOT SET                               
*                                                                               
         CLC   FLTNUM,RTPARMS3+L'RTPARMS3-L'FLTNUM                              
         BNE   FDFF04                                                           
         TM    FLTIND1,FDRF1NOT                                                 
         BNO   EXITOK                                                           
         MVI   RTPARMS4,FF                                                      
         B     EXITOK                                                           
*                                                                               
FDFF04   IC    RE,FLTLN                                                         
         LA    R5,0(RE,R5)                                                      
         B     FDFF02                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD INITIAL KEY FOR LIST/SELECT ON NTRSES                         *         
***********************************************************************         
         SPACE 1                                                                
FBLDKY   GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KLBUILD,(R2),PSSAV                  
         BL    EXITL                                                            
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* HEIRARCHY OBJECT                                                    *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY OF INHERITING RECORD)                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
HEIR     LR    R7,RF                                                            
         LM    R0,R3,0(R1)                                                      
         LA    RF,HERTABL                                                       
         B     ITER                                                             
*                                                                               
HERTABL  DC    AL1(HGET),AL1(0,0,0),AL4(GETHEIR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* GET HEIRARCHICAL RECORD TO INHERIT FROM                             *         
***********************************************************************         
         SPACE 1                                                                
GETHEIR  MVC   RTWORK(L'GSRECKEY),GSRECKEY   SAVE KEY WE WANT                   
         XC    GSRECKEY,GSRECKEY                                                
         XC    GSRECSTA,GSRECSTA                                                
         XC    GSRECDA,GSRECDA                                                  
         GOTOX AGEN,RTPARM,('GCBOVER',OKEY),KHEIR,GSRECKEY,RTWORK               
         BNE   EXITH                                                            
*                                                                               
         OC    GSRECKEY,GSRECKEY                                                
         BZ    EXITH                        NO KEY DEFINED                      
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IDIRGET      READ DIRECTORY RECORD               
         BL    EXITL                                                            
*                                                                               
         OC    GSRECDA,GSRECDA                                                  
         BZ    EXITH                        NO RECORD FOUND                     
*                                                                               
         GOTOX AGEN,RTPARM,OIO,IRECGET,0    READ FILE RECORD                    
         BL    EXITL                        INTO XIO11                          
*                                                                               
         LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         DIRECTORY ENTRY                              
         USING NFITABD,RF                                                       
         XR    RE,RE                                                            
         L     R2,AIOREC           WHERE RECORD IS                              
         IC    RE,NFIKEYL          LENGTH OF RECORD KEY                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECKEY(0),RTWORK  MOVE IN ORIGINAL KEY AGAIN                   
         XC    GSRECDA,GSRECDA     MOVE IN D/A                                  
*                                                                               
         L     RF,AIOREC                                                        
         EX    RE,*+4                                                           
         MVC   0(0,RF),RTWORK      MOVE IN ORIGINAL KEY TO FILE RECORD          
*                                                                               
         L     RF,AIOREC                                                        
         SH    RF,=Y(L'IODA+L'IOWORK)                                           
         XC    0(L'IODA,RF),0(RF)  CLEAR DISK ADDRESS ON FRONT OF XIO11         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARMS7 DS    A                                                                
RTPARMS8 DS    A                                                                
*                                                                               
RTPARM   DS    0XL24               * PARAMETERS 1-6 *                           
RTPARM1  DS    A                                                                
RTPARM2  DS    A                                                                
RTPARM3  DS    A                                                                
RTPARM4  DS    A                                                                
RTPARM5  DS    A                                                                
RTPARM6  DS    A                                                                
RTPARM7  DS    A                                                                
RTPARM8  DS    A                                                                
*                                                                               
RTFILDIR DS    A                   A(THIS SYSTEM DIRECTORY ENTRY)               
RTFILREC DS    A                   A(THIS SYSTEM FILE ENTRY)                    
*                                                                               
RTHALF1  DS    H                                                                
RTHALF2  DS    H                                                                
RTHALF3  DS    H                                                                
RTHALF4  DS    H                                                                
RTVFINDS DS    XL1                                                              
RTVFIVAL EQU   X'80'                                                            
RTVFINOT EQU   X'40'                                                            
RTADR    DS    A                                                                
RTTAG    DS    A                                                                
RTBYTE1  DS    X                                                                
RTFILT1  DS    X                                                                
RTFILT2  DS    X                                                                
RTFILTX  DS    X                                                                
RTFILTST EQU   X'80'                                                            
*                                                                               
RTSOVSYS DS    XL1                 SYSTEM                                       
RTSPRG   DS    XL1                 PROGRAM                                      
RTSREC   DS    XL1                 RECORD TYPE                                  
RTSPAG#  DS    XL1                 PAGE NUMBER                                  
RTSCODE  DS    XL1                 SCREEN CODE                                  
*                                                                               
RTWORK   DS    XL80                FOR ALL TO USE                               
RTWORKA  DS    XL80                FOR ALL TO USE                               
RTWORK1  DS    XL40                RESERVED FOR FILTERS                         
RTWORK2  DS    XL40                RESERVED FOR FILTERS                         
RTWORK3  DS    XL256               RESERVED FOR FILTERS                         
RTWORK4  DS    XL256               RESERVED FOR FILTERS                         
RTWORK5  DS    XL80                RESERVED FOR FILTERS                         
RTCOUNT  DS    H                                                                
RTKEY    DS    XL(L'GSRECKEY)                                                   
RTSTA    DS    XL(L'GSRECSTA)                                                   
RTDA     DS    XL(L'GSRECDA)                                                    
*                                                                               
RTSCAN   DS    (RTSCAN#)CL(SCBLKLQ)   BLOCK FOR SCANNER CALLS                   
RTSCAN#  EQU   6                   MAX NUMBER OF FIELDS IN LIST                 
RTSCANS  DS    CL(SCBLKLQ)         SCANBLK FORMATTING AREA                      
*                                                                               
RTRECACT DS    XL(L'GSRECACT)                                                   
RTCTRY   DS    XL1                                                              
RTSTACHG DS    XL1                                                              
*                                  * SCRSET ROUTINE *                           
         DS    0A                                                               
RTSSPARM DS    0XL12                                                            
RTSSINDS DS    XL1                 INDICATOR BYTE                               
RTSSISCR EQU   X'80'               BUILDING SCREEN                              
RTSSIBLK EQU   X'40'               BUILDING BLOCK                               
RTSSAREC DS    AL3                 A(SCREEN RECORD)                             
         ORG   *-1                                                              
RTSSPAG  DS    XL1                 PAGE NUMBER                                  
RTSSATWA DS    A                   A(TWA AREA)                                  
RTSSAINP DS    A                   RETURNED A(1ST INPUT FIELD)                  
         ORG   RTSSPARM+L'RTSSPARM                                              
RTSSAR1  DS    A                   A(SCRSET CALLER'S R1)                        
RTSSELQ  DS    XL1                 FDRELQ/FLTRLQ                                
*                                                                               
TITSACTN DS    XL1                 SAVED TSAR ACTION NUMBER                     
XSNXRECN DS    XL2                 SAVED CSHIRECN TO SET CSNXRECN               
XSRECACT DS    0XL2                PREVIOUS RECORD/ACTION                       
XSREC    DS    XL1                                                              
XSACT    DS    XL1                                                              
XSLST#1  DS    XL2                                                              
XSLST#X  DS    XL2                                                              
XSSAV    DS    XL(SSAVL)                                                        
XSHOLE   DS    XL4                 ??                                           
XSFRPEL  DS    XL(FRPLNQ)                                                       
*                                                                               
RTWORKL  EQU   *-RTWORKD                                                        
         EJECT                                                                  
         SPACE 1                                                                
* GEFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
GEFIL01  CSECT                                                                  
         ORG   GEFIL01+(((*-GEFIL01)/2048)+1)*2048                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038GEFIL01S  08/29/00'                                      
         END                                                                    
