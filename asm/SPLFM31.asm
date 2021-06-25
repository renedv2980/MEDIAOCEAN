*          DATA SET SPLFM31    AT LEVEL 030 AS OF 01/20/99                      
*PHASE T21931A                                                                  
T21931   TITLE 'SPLFM31 - CLIENT HEADER OPTIONS'                                
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*-DATE---------BY------------------CHANGE-----------------------------*         
*                                                                     *         
* 04/07/98     NRK                 INITIAL RELEASE                    *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
T21931   CSECT                                                                  
         NMOD1 0,T21931,R9,RR=RE                                                
         ST    RE,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING CLTHDRD,R8                                                       
         CLI   SVFMTSW,0                TEST FORMAT OR EDIT                     
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         MVI   ERRCD,SCRTYERR                                                   
         CLI   SVACT,C'D'          ACTION DISPLAY?                              
         BE    FMT0050                                                          
         CLI   SVACT,C'C'          ACTION CHANGE?                               
         BNE   FMT0100                                                          
*                                                                               
FMT0050  EQU   *                                                                
*                                                                               
         BAS   RE,CHKAUT           CHECK IF AUTHORIZED                          
*                                                                               
FMT0100  EQU   *                                                                
*                                                                               
         LA    R2,CL2NAMEH         A(1ST SCREEN FIELD TO CLEAR)                 
*                                                                               
FMT0150  EQU   *                                                                
*                                                                               
         ZIC   R1,0(R2)            GET L(SCREEN FIELD)                          
         SH    R1,=H'9'            L(DATA)                                      
         EX    R1,*+4              EXECUTE THE CLEAR                            
         MVC   8(0,R2),SPACES      CLEAR THE FIELD                              
         FOUT  (R2)                SET THE HEADER BITS                          
         ZIC   R1,0(R2)            R1 = L(SCREEN FIELD)                         
         AR    R2,R1               INC TO NEXT FIELD HEADER                     
         LA    R1,CL2LSTH          R1 = A(LAST FIELD TO CLEAR)                  
         CLR   R2,R1               PAST LAST FIELD TO CLEAR?                    
         BNH   FMT0150             NO - SO LOOP BACK UP                         
*                                                                               
         FOUT  CL2NAMEH,CNAME,20   PUT OUT THE NAME                             
*                                                                               
* GET THE CLIENT PROFILES                                                       
*                                                                               
         MVC   KEY(4),=C'S0C2'     OFFLINE SPOT PROGRAM NUMBER                  
         MVC   KEY+4(2),AGYALPHA   AGENCY CODE                                  
         MVC   KEY+6(6),SPACES     NO MED, CLIENT, OFFICE                       
         L     RF,VCOMFACS         SET UP FOR GETPROF CALL                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',KEY),PROFILE,VDATAMGR                           
         CLC   PROFILE(16),SPACES  DID WE GET ANYTHING?                         
         BH    *+6                 YES - SO CONTINUE                            
*                                                                               
         DC    H'0'                ELSE - HUH?                                  
*                                                                               
* NOW SET UP ADDRESSES AND USINGS FOR THE MAIN LOOP                             
*                                                                               
         LA    R1,TABLE1           A(TABLE OF WHAT GOES ON THE SCREEN)          
         USING TABLE1D,R1                                                       
         LA    R2,CL2FRSTH         A(FIRST SCREEN LITERAL FIELD)                
         USING FIELDD,R2                                                        
*                                                                               
FMT0200  EQU   *                                                                
*                                                                               
         CLI   PROF,X'FF'          AT END OF TABLE?                             
         BE    FMT0500             YES - SO ALL DONE                            
*                                                                               
         LA    R3,PROFILE          A(PROFILE RECORD RESPONSES)                  
         ZIC   R4,PROF             R4 = DISP INTO PROFILE RESPONSES             
         AR    R3,R4               A(THIS PROFILE REPONSE)                      
         MVC   BYTE,0(R3)          PUT IT IN BYTE                               
         CLI   BYTE,C'Y'           CLIENT USING THIS FEATURE?                   
         BNE   FMT0400             NO - SO CONTINUE                             
*                                                                               
         MVC   SCRNLIT(21),LITERAL MOVE LITERAL TO SCREEN                       
*                                                                               
         ICM   RF,15,DISPRTN       R3 = A(DISPLAY ROUTINE)                      
         A     RF,RELO             RELOCATE IT                                  
         BASR  RE,RF               AND GO                                       
*                                                                               
         LA    R2,LFIELDD(R2)      INC TO NEXT SCREEN FIELD                     
*                                                                               
FMT0400  EQU   *                                                                
*                                                                               
         LA    R1,LTABLE1D(R1)     INC TO NEXT TABLE ENTRY                      
         B     FMT0200             LOOP BACK TO CHECK IT                        
         DROP R1,R2                                                             
*                                                                               
FMT0500  EQU   *                                                                
*                                                                               
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
EDT      EQU   *                                                                
*                                                                               
         MVI   COMPCD,0                                                         
         MVC   KEY,SVKEY         CHANGE SO REREAD RECORD                        
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVC   MYBYTE,COPT1        SAVE THE OLD OPTIONS BYTE                    
         MVC   MYBYTE2,COPT2       SAVE THE OLD OPTIONS BYTE                    
         MVI   ERRCD,INVERR        INVALID INPUT                                
*                                                                               
* NOW SET UP ADDRESSES AND USINGS FOR THE MAIN LOOP                             
*                                                                               
         LA    R1,TABLE1           A(TABLE OF WHAT GOES ON THE SCREEN)          
         USING TABLE1D,R1                                                       
         LA    R2,CL2FRSTH         A(FIRST SCREEN LITERAL FIELD)                
         USING FIELDD,R2                                                        
*                                                                               
EDT0100  EQU   *                                                                
*                                                                               
         CLI   PROF,X'FF'          AT END OF TABLE?                             
         BE    EDT0300             YES - SO ALL DONE                            
*                                                                               
         LA    R3,PROFILE          A(PROFILE RECORD RESPONSES)                  
         ZIC   R4,PROF             R4 = DISP INTO PROFILE RESPONSES             
         AR    R3,R4               A(THIS PROFILE REPONSE)                      
         MVC   BYTE,0(R3)          PUT IT IN BYTE                               
         CLI   BYTE,C'Y'           CLIENT USING THIS FEATURE?                   
         BNE   EDT0200             NO - SO CONTINUE                             
*                                                                               
         ICM   RF,15,VALRTN        RF = A(DISPLAY ROUTINE)                      
         A     RF,RELO             RELOCATE IT                                  
         BASR  RE,RF               AND GO                                       
         BNZ   EDTERR              ERROR FOUND                                  
*                                                                               
         LA    R2,LFIELDD(R2)      ELSE - INC TO NEXT SCREEN FIELD              
*                                                                               
EDT0200  EQU   *                                                                
*                                                                               
         LA    R1,LTABLE1D(R1)     INC TO NEXT TABLE ENTRY                      
         B     EDT0100             LOOP BACK TO CHECK IT                        
*                                                                               
EDT0300  EQU   *                                                                
*                                                                               
         B     OUTPUT                                                           
*                                                                               
EDTERR   EQU   *                                                                
*                                                                               
         LA    R2,DATAH            GET A(SCREEN FIELD)                          
         OI    6(R2),X'40'         SET THE CURSOR POSITION                      
         B     LFMERR                                                           
         DROP  R1,R2                                                            
*                                                                               
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
         MVC   REC(13),SVKEY                                                    
         MVC   REC+13(2),=H'1280'                                               
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         B     OUT2                                                             
*                                                                               
OUT1     DS    0H                                                               
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC              REREAD CLTREC BEFORE PUTREC                  
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 CNCHASPT                                                         
*                                                                               
OUT2     DS    0H                                                               
         GOTO1 =A(OFCPTR),RR=RELO                                               
*                                                                               
REQREC   XC    REC(150),REC       GENERATE REQUEST RECORD                       
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'C'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DISPLAYS THE Y/N TO THE SCREEN                                   
*                                                                               
* NOTE: THIS ROUTINE USES R1 AND R2, DON'T MUCK WITH THEM!                      
*       IT ALSO DOESN'T SAVE/RESTORE ANY REGISTERS!                             
*                                                                               
DISPYN   EQU   *                                                                
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         MVI   DATA,C'N'           MOVE OUT DEFAULT                             
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,(3)      R1 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         NC    BYTE,OPTEQU         BIT SET?                                     
         BZ    DYN0100             NO - SO CONTINUE                             
*                                                                               
         MVI   DATA,C'Y'           ELSE - MOVE OUT 'Y'                          
*                                                                               
DYN0100  EQU   *                                                                
*                                                                               
         BR    RE                  AND RETURN TO CALLER                         
         DROP  R1,R2                                                            
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE DETERMINES IF COS2 IS A COST FACTOR OR Y/N FIELD AND             
* DISPLAYS THE APPROPRIATE VALUE.                                               
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
DISPCOS2 NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    SVAGYFL1,AGYCOS2Q   COST FACTOR?                                 
         BO    DCOS0100            YES - SO CONTINUE                            
*                                                                               
         BAS   RE,DISPYN           ELSE - SHOW Y/N                              
         B     DCOSEXIT            AND EXIT                                     
*                                                                               
DCOS0100 EQU   *                                                                
*                                                                               
         OC    CCOST2,CCOST2       ANY COST FACTOR?                             
         BZ    DCOSEXIT            NO - SO ALL DONE                             
*                                                                               
         CLI   CCOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   DCOS0200            NO - SO CONTINUE                             
*                                                                               
         MVC   DATA(3),=C'0.0'     ELSE - MOVE OUT ZERO                         
         B     DCOSEXIT            AND CONTINUE                                 
*                                                                               
DCOS0200 EQU   *                                                                
*                                                                               
         LA    R4,DATA             GET A(FIELD)                                 
         EDIT  CCOST2,(8,0(R4)),6,ALIGN=LEFT,ZERO=NOBLANK,FILL=0,DROP=5         
*                                                                               
DCOSEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE DISPLAYS THE PROFIT WITHIN % TO THE SCREEN                       
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
DISPWPCT NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CPWPCT,CPWPCT       ANY PROFIT WITHIN?                           
         BZ    DPCT0200            NO - SO CONTINUE                             
*                                                                               
         LA    R4,DATA             ELSE - GET A(PW % FIELD)                     
         CLC   CPWPCT,=X'800000'                                                
         BNE   DPCT0100                                                         
         MVI   0(R4),C'0'                                                       
         B     DPCT0200                                                         
*                                                                               
DPCT0100 EQU   *                                                                
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,CPWPCT                                                      
         EDIT  (R3),(6,0(R4)),2,ALIGN=LEFT                                      
*                                                                               
DPCT0200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE DISPLAYS THE PROFIT WITHIN % TO THE SCREEN                       
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
DISPZEN  NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CZENCLT,CZENCLT     ANY ZENITH CLIENT TO DISPLAY?                
         BZ    DZEN0200            NO - SO CONTINUE                             
*                                                                               
         LA    RE,1                ELSE - SET LENGTH FOR 2 CHARS                
         CLI   CZENCLT+2,X'00'     TEST IF IT'S 2 OR 3 CHARACTERS               
         BE    DZEN0100            YES - SO CONTINUE                            
*                                                                               
         LA    RE,2                ELSE - SET LENGTH FOR 3 CHARS                
*                                                                               
DZEN0100 EQU   *                                                                
*                                                                               
         EXMVC RE,DATA,CZENCLT     MOVE OUT THE ZENITH CLIENT                   
*                                                                               
DZEN0200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE DISPLAYS THE LIMIT ACCESS CODE TO THE SCREEN                     
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
DISPACS  NTR1                                                                   
         USING FIELDD,R2                                                        
         OC    CACCESS,CACCESS     ANY CODE                                     
         BZ    DACS0200            NO - SO CONTINUE                             
*                                                                               
         MVC   DATA,CACCESS                                                     
*                                                                               
DACS0200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE 2ND COST FEATURE                                   
*                                                                               
VALCOS2  NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    SVAGYFL1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BO    VCOS0100            YES - SO CONTINUE                            
*                                                                               
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VCOSERR             ERROR                                        
         B     VCOSDONE            ELSE - ALL DONE                              
*                                                                               
VCOS0100 EQU   *                                                                
*                                                                               
         XC    CCOST2,CCOST2       CLEAR THE FIELD                              
         CLI   DATAH+5,X'0'        ANY INPUT?                                   
         BE    VCOSDONE            NO - SO ALL DONE                             
*                                                                               
         ZIC   R4,DATAH+5                                                       
         GOTO1 VCASHVAL,DMCB,(6,DATA),(R4)                                      
         CLI   DMCB,0                                                           
         BNE   VCOSERR                                                          
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'      MAX 9.999999                                 
         BH    VCOSERR                                                          
*                                                                               
         C     R3,=F'0'             .LT. 0?                                     
         BL    VCOSERR             YES - SO ERROR                               
*                                                                               
         MVC   CCOST2,DMCB+4       ELSE - SAVE THE COST FACTOR                  
         OC    CCOST2,CCOST2       ZERO?                                        
         BNZ   VCOSDONE            NO - SO CONTINUE                             
*                                                                               
         OI    CCOST2,X'80'        ELSE - SET 'ZERO WAS INPUT' BIT              
*                                                                               
VCOSDONE EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VCOSEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VCOSERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VCOSEXIT            AND EXIT                                     
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE DOUBLE BOOK CHECK FEATURE                          
*                                                                               
VALDBL   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VDBLERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VDBLEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VDBLERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VDBLEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE INFOMERCIAL FEATURE                                
*                                                                               
VALINFO  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VINFERR             ERROR                                        
*                                                                               
         TM    MYBYTE,COP1INFQ     WAS CLIENT INFOMERCIAL?                      
         BNO   VINF0100            NO - SO CONTIUE                              
*                                                                               
         TM    COPT1,COP1INFQ      ELSE - IS IT STILL INFOMERCIAL?              
         BO    VINF0100            YES - SO CONTINUE                            
*                                                                               
         XC    LFMMSG,LFMMSG       ELSE - SET UP ERROR MESSAGE                  
         MVC   LFMMSG(32),=CL32'ERROR-CANNOT UNDO INFOMERCIAL  '                
         MVI   ERRAREA,X'FF'                                                    
         B     VINFERR                                                          
*                                                                               
VINF0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VINFEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VINFERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VINFEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE MARKET GROUP FEATURE                               
*                                                                               
VALMGR   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VMGRERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VMGREXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VMGRERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VMGREXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE J1 REPORT EXCLUDE FEATURE                          
*                                                                               
VALXJ1   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VXJ1ERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VXJ1EXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VXJ1ERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VXJ1EXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE A7 REPORT EXCLUDE FEATURE                          
*                                                                               
VALXA7   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VXA7ERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VXA7EXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VXA7ERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VXA7EXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE CTA CLIENT FEATURE                                 
*                                                                               
VALCTA   NTR1                                                                   
         USING FIELDD,R2                                                        
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VCTAERR             ERROR                                        
*                                                                               
         CLI   DATA,C'Y'           IS RESPONSE 'Y'ES?                           
         BE    VCTA0100            YES - SO CONTINUE                            
*                                                                               
         CLI   T219FFD+1,C'*'      ELSE - IS THIS A DDS TERMINAL?               
         BNE   VCTAERR             NO - SO ERROR                                
*                                                                               
VCTA0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VCTAEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VCTAERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VCTAEXIT            AND EXIT                                     
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE GMI CLIENT FEATURE                                 
*                                                                               
VALGMI   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VGMIERR             ERROR                                        
*                                                                               
         TM    MYBYTE,COP1GMI      WAS CLIENT GMI?                              
         BNO   VGMI0100            NO - SO CONTINUE                             
*                                                                               
         TM    COPT1,COP1GMI       ELSE - IS IT STILL GMI?                      
         BO    VGMI0100            YES - SO CONTINUE                            
*                                                                               
         XC    LFMMSG,LFMMSG       ELSE - SET UP ERROR MESSAGE                  
         MVC   LFMMSG(32),=C'ERROR-CANNOT UNDO GMI            '                 
         MVI   ERRAREA,X'FF'                                                    
         B     VGMIERR                                                          
*                                                                               
VGMI0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VGMIEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VGMIERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VGMIEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE UPLOAD DELETED BUYS FEATURE                        
*                                                                               
VALUPL   NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VUPLERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VUPLEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VUPLERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VUPLEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE ZENITH CLIENT FEATURE                              
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
VALZEN   NTR1                                                                   
         USING FIELDD,R2                                                        
*                                                                               
         XC    CZENCLT,CZENCLT                                                  
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VZENEXIT            NO - SO JUST EXIT (SETS CC)                  
*                                                                               
         CLI   DATAH+5,2           AT LEAST 2 CHARS I/P?                        
         BL    VZENERR             NO - SO ERROR                                
*                                                                               
         CLI   DATAH+5,3           MORE THAN 3 CHARS INPUT?                     
         BH    VZENERR             YES - SO ERROR                               
*                                                                               
         ZIC   R3,DATAH+5          ELSE - R3 = L(DATA)                          
         LA    RE,DATA             RE = A(DATA)                                 
*                                                                               
VZEN0100 EQU   *                                                                
*                                                                               
         LA    RF,ALPHANUM                                                      
*                                                                               
VZEN0200 EQU   *                                                                
*                                                                               
         CLI   0(RF),0                                                          
         BE    VZENERR                                                          
*                                                                               
         CLC   0(1,RE),0(RF)                                                    
         BE    VZEN0300                                                         
         LA    RF,1(RF)                                                         
         B     VZEN0200                                                         
*                                                                               
VZEN0300 EQU   *                                                                
*                                                                               
         LA    RE,1(RE)                                                         
         BCT   R3,VZEN0100                                                      
*                                                                               
         IC    R3,DATAH+5                                                       
         BCTR  R3,0                                                             
         EXMVC R3,CZENCLT,DATA                                                  
*                                                                               
VZEN0400 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VZENEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VZENERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VZENEXIT            AND EXIT                                     
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE DISALLOW PW BILLING FEATURE                        
*                                                                               
VALNPWB  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VPWBERR             ERROR                                        
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VPWBEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VPWBERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VPWBEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE PW PERCENT FEATURE                                 
*                                                                               
* NOTE: THIS ROUTINE USES R2, DON'T MUCK WITH IT!                               
*                                                                               
VALPWPCT NTR1                                                                   
         USING FIELDD,R2                                                        
*                                                                               
         XC    CPWPCT,CPWPCT                                                    
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VPCTEXIT            NO - SO JUST EXIT (SETS CC)                  
*                                                                               
         ZIC   R4,DATAH+5          ELSE - R4 = L(DATA)                          
         GOTO1 VCASHVAL,DMCB,(2,DATA),(R4) CHECK THE INPUT                      
         CLI   DMCB,0              NUMERIC DATA INPUT?                          
         BNE   VPCTERR             NO - SO ERROR                                
*                                                                               
         L     R3,4(R1)                                                         
         CH    R3,=H'10000'        MAX 100%                                     
         BNL   VPCTERR             TOO BIG A NUMBER INPUT                       
*                                                                               
         MVC   CPWPCT,DMCB+5                                                    
         OC    CPWPCT,CPWPCT       0% INPUT?                                    
         BNZ   VPCT0100            NO - SO CONTINUE                             
*                                                                               
         OI    CPWPCT,X'80'        ELSE - SET SPECIAL FLAG FOR PW%=0            
*                                                                               
VPCT0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VPCTEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VPCTERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VPCTEXIT            AND EXIT                                     
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE TRADE FEATURE                                      
*                                                                               
VALTRADE NTR1                                                                   
         USING FIELDD,R2                                                        
         TM    SVAGYFL1,AGYTRDQ    TRADE AGENCY?                                
         BZ    VTRD0100            NO - SO CONTINUE                             
*                                                                               
         FOUT  DATAH,=C'Y',1       ELSE - FORCE TRADE ON                        
*                                                                               
VTRD0100 EQU   *                                                                
*                                                                               
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VTRDERR             ERROR                                        
*                                                                               
         TM    MYBYTE2,COP2TRAD    WAS CLIENT TRADE?                            
         BNO   VGMI0100            NO - SO CONTINUE                             
*                                                                               
         TM    COPT2,COP2TRAD      ELSE - IS IT STILL TRADE?                    
         BO    VTRD0200            YES - SO CONTINUE                            
*                                                                               
         XC    LFMMSG,LFMMSG       ELSE - SET UP ERROR MESSAGE                  
         MVC   LFMMSG(23),=C'ERROR-CANNOT UNDO TRADE'                           
         MVI   ERRAREA,X'FF'                                                    
         B     VTRDERR                                                          
*                                                                               
VTRD0200 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VTRDEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VTRDERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VTRDEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE FREEZE FEATURE                                     
*                                                                               
VALFRZ   NTR1                                                                   
*                                                                               
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VFRZERR             ERROR                                        
*                                                                               
* WI IS NOT ALLOWED TO CHANGE FREEZE HERE                                       
*                                                                               
         CLC   AGYALPHA,=C'WI'     'WI' AGENCY?                                 
         BNE   VFRZ0100            NO - SO JUST CONTINUE                        
*                                                                               
* WELL, OK, IF THEY HAVE T219FFD+12 BIT X'04' SET, THEN THEY CAN                
* CHANGE IT.                                                                    
*                                                                               
         TM    T219FFD+12,X'04'    IS IT SET?                                   
         BO    VFRZ0100            YES - SO CONTINUE                            
*                                                                               
         MVC   FULL,COPT2          ELSE - STORE THE CURRENT BYTE                
         MVC   FULL+1,MYBYTE2      STORE THE ORIGINAL BYTE                      
         NI    FULL,COP2FRZ        TURN OFF EVERYTHING BUT FRZ BIT              
         NI    FULL+1,COP2FRZ      TURN OFF EVERYTHING BUT FRZ BIT              
         CLC   FULL(1),FULL+1      HAS BIT CHANGED?                             
         BE    VFRZ0100            NO - SO OK                                   
*                                                                               
         XC    LFMMSG,LFMMSG       ELSE - SET UP ERROR MESSAGE                  
         MVC   LFMMSG(24),=C'ERROR-CANNOT UNDO FREEZE'                          
         MVI   ERRAREA,X'FF'                                                    
         B     VFRZERR                                                          
*                                                                               
VFRZ0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VFRZEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VFRZERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VFRZEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE CROSS ESTIMATE REPORTING FEATURE, WHICH            
* CAN ONLY BE USED IF THE CLIENT IS A PROFIT W/IN CLIENT.                       
*                                                                               
VALXEST  NTR1                                                                   
         BAS   RE,VALYN            VALIDATE Y/N INPUT                           
         BNE   VXESERR             ERROR                                        
*                                                                               
         TM    COPT2,COP2XEST      CROSS EST SET?                               
         BZ    VXES0100            NO - SO CONTINUE                             
*                                                                               
         OC    CPWPCT,CPWPCT       ANY PROFIT W/IN?                             
         BNZ   VXES0100            YES - SO CONTINUE                            
*                                                                               
         XC    LFMMSG,LFMMSG       ELSE - SET UP ERROR MESSAGE                  
         MVC   LFMMSG(54),=CL54'ERROR-CANNOT HAVE CROSS ESITMATE WITHOUX        
               T PROFIT WITHIN.'                                                
         MVI   ERRAREA,X'FF'                                                    
         B     VXESERR                                                          
*                                                                               
VXES0100 EQU   *                                                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VXESEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
VXESERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VXESEXIT            AND EXIT                                     
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE LIMIT ACCESS CODE                                  
*                                                                               
VALACS   NTR1                                                                   
         USING FIELDD,R2                                                        
         XC    CACCESS,CACCESS                                                  
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VACSEXIT            NO - SO JUST EXIT                            
         CLI   DATAH+5,3           MORE THAN 3 CHARS INPUT?                     
         BH    VACSERR             YES - SO ERROR                               
*                                                                               
         ZIC   R3,DATAH+5          ELSE - R3 = L(DATA)                          
         LA    RE,DATA             RE = A(DATA)                                 
*                                                                               
VACS0100 LA    RF,ALPHANUM         VALID ALPHANUMERIC                           
VACS0200 CLI   0(RF),0                                                          
         BE    VACSERR                                                          
         CLC   0(1,RE),0(RF)                                                    
         BE    VACS0300                                                         
         LA    RF,1(RF)                                                         
         B     VACS0200                                                         
*                                                                               
VACS0300 LA    RE,1(RE)                                                         
         BCT   R3,VACS0100                                                      
*                                                                               
         IC    R3,DATAH+5                                                       
         BCTR  R3,0                                                             
         EXMVC R3,CACCESS,DATA                                                  
*                                                                               
VACSEXIT EQU   *                                                                
         XR    R0,R0               SET GOOD CC                                  
VACSX    XIT1                                                                   
*                                                                               
VACSERR  EQU   *                                                                
         LTR   RC,RC               SET ERROR CC                                 
         B     VACSX               AND EXIT                                     
         DROP  R2                                                               
         SPACE 3                                                                
*                                                                               
* THIS ROUTINE VALIDATES THE Y OR N INPUT                                       
*                                                                               
* NOTE: THIS ROUTINE USES R1 AND R2, DON'T MUCK WITH THEM!                      
*       IT ALSO DOESN'T SAVE/RESTORE ANY REGISTERS!                             
*                                                                               
VALYN    EQU   *                                                                
         USING TABLE1D,R1                                                       
         USING FIELDD,R2                                                        
         CLI   DATAH+5,1           INPUT 1 BYTE?                                
         BH    VYNERR              NO - SO ERROR                                
*                                                                               
         LA    R4,CLTHDR           R4 = A(CLIENT RECORD)                        
         ZICM  R3,OPTBYTE,(3)      R1 = DISP OF PROF BYTE IN CLIENT REC         
         AR    R4,R3               A(CLIENT RECORD PROFILE BYTE)                
         MVC   BYTE,0(R4)          STORE IT IN BYTE                             
         MVC   FULL(1),OPTEQU      SAVE THE OPTIONS MASK                        
         XI    FULL,X'FF'          TURN ON EVERYTHING BUT THE BIT               
         NC    BYTE,FULL           RE-SET THE BIT TO START                      
*                                                                               
         CLI   DATAH+5,0           ANY INPUT?                                   
         BE    VYN0100             NO - SO TREAT AS 'N'O                        
*                                                                               
         CLI   DATA,C'N'           ELSE - IS IT 'N'O?                           
         BE    VYN0100             YES - SO CONTINUE                            
*                                                                               
         CLI   DATA,C'Y'           ELSE - IS IT 'Y'ES?                          
         BNE   VYNERR              NO - SO ERROR                                
*                                                                               
         OC    BYTE,OPTEQU         ELSE - SET THE BIT                           
*                                                                               
VYN0100  EQU   *                                                                
*                                                                               
         MVC   0(1,R4),BYTE        PUT RESULT BACK IN RECORD                    
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
VYNEXIT  EQU   *                                                                
*                                                                               
         BR    RE                  RETURN TO CALLER                             
*                                                                               
VYNERR   EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     VYNEXIT             AND EXIT                                     
         DROP  R1,R2                                                            
         EJECT                                                                  
*====================================================================*          
*    CHECK IF AUTHORIZED FOR DISPLAY & CHANGE                        *          
*====================================================================*          
         SPACE 1                                                                
CHKAUT   NTR1                                                                   
         LA    R2,LFMKEYH                                                       
         CLI   T219FFD+1,C'*'      DDS TERMINAL?                                
         BE    CHKAUTX                                                          
         OC    T219FFD+6(2),T219FFD+6 TEST ANY SECURITY LIMIT                   
         BZ    CHKAUTX             NO                                           
         CLI   T219FFD+6,C'+'      TEST MKT LOCKOUT                             
         BE    CHKAUTX                                                          
         CLI   T219FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BNE   CHKAUT10                                                         
*                                                                               
         CLC   T219FFD+7(1),COFFICE TEST RIGHT OFFICE                           
         BNE   SECERR                                                           
         B     CHKAUTX                                                          
*                                                                               
CHKAUT10 CLI   T219FFD+6,C'$'      TEST OFFICE LIST LOCKOUT                     
         BNE   CHKAUTX                                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T219FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,VCOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   SECERR                                                           
CHKAUTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
SECERR   NI    LFMKEYH+4,X'FF'-X'20'    TURN OFF PREVIOUSLY VALIDATED           
         MVI   ERRCD,SCRTYERR                                                   
         B     LFMERR                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
*==============================================================*                
* ADD/DELETE OFFICE PASSIVE POINTERS                           *                
*==============================================================*                
         SPACE 1                                                                
         DS    0D                                                               
OFCPTR   NTR1  BASE=*                                                           
         B     OFCPTR0                                                          
         DC    CL8'*OFCPTR*'                                                    
*                                                                               
OFCPTR0  CLC   SVCOFFC,COFFICE     TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
* DELETE OLD OFFICE (IF ANY)                                                    
         CLI   SVCOFFC,C' '                                                     
         BNH   OFCPTR10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),SVKEY+1    A/M                                          
         MVC   KEY+9(1),SVCOFFC                                                 
         MVC   KEY+11(2),SVKEY+2   CLT                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR2                                                          
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
OFCPTR2  CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   OFCPTR10                                                         
         CLI   SVEBCMED,C'T'       TV ONLY                                      
         BNE   OFCPTR10                                                         
* NEED TO DO MEDIA N (X'03')                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR4                                                          
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
* NEED TO DO MEDIA C (X'08')                                                    
OFCPTR4  NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR10                                                         
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
* NOW ADD NEW (OR UNDELETE)                                                     
OFCPTR10 CLI   COFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX                                                          
         MVC   KEY(20),SVKEY                                                    
         BAS   RE,ADDOFC                                                        
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   OFCPTRX                                                          
         CLI   SVEBCMED,C'T'                                                    
         BNE   OFCPTRX                                                          
* DO MEDIA N                                                                    
         MVC   KEY(20),SVKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDOFC                                                        
* DO MEDIA C                                                                    
         MVC   KEY(20),SVKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDOFC                                                        
*                                                                               
OFCPTRX  NI    DMINBTS,X'F7'                                                    
         XIT1                                                                   
*                                                                               
ADDOFC   NTR1                                                                   
         MVC   WORK(20),KEY        SAVE CLTHDR KEY AND DISK ADDRESS             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),WORK+1     A/M                                          
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),WORK+2    CLT                                          
         MVC   KEY+14(4),WORK+14   SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDOFC10                                                         
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     ADDOFCX                                                          
*                                                                               
ADDOFC10 MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 DIR                                                              
*                                                                               
ADDOFCX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
TABLE1   EQU   *                                                                
         DC    AL1(PROFCOS2-PROFILED),CL21'2ND COST REQUIRED?'                  
         DC    AL4(VALCOS2,DISPCOS2),AL2(COPT1-CLTHDR),AL1(COP1COSQ)            
*                                                                               
         DC    AL1(PROFDBL-PROFILED),CL21'SUPP DBL BOOK TEST?'                  
         DC    AL4(VALDBL,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1DBLQ)               
*                                                                               
         DC    AL1(PROFINFO-PROFILED),CL21'INFOMERCIAL?'                        
         DC    AL4(VALINFO,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1INFQ)              
*                                                                               
         DC    AL1(PROFMGR-PROFILED),CL21'MARKET GROUPS?'                       
         DC    AL4(VALINFO,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1MGRQ)              
*                                                                               
         DC    AL1(PROFXJ1-PROFILED),CL21'EXCLUDE FROM J1 RPT?'                 
         DC    AL4(VALXJ1,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2EXJ1)               
*                                                                               
         DC    AL1(PROFXA7-PROFILED),CL21'EXCLUDE FROM A7 RPT?'                 
         DC    AL4(VALXA7,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2EXA7)               
*                                                                               
         DC    AL1(PROFCTA-PROFILED),CL21'CTA CLIENT?'                          
         DC    AL4(VALCTA,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1CTAQ)               
*                                                                               
         DC    AL1(PROFGMI-PROFILED),CL21'GMI CLIENT?'                          
         DC    AL4(VALGMI,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1GMI)                
*                                                                               
         DC    AL1(PROFUPL-PROFILED),CL21'UPLOAD DELETED BUYS?'                 
         DC    AL4(VALUPL,DISPYN),AL2(COPT1-CLTHDR),AL1(COP1UPLQ)               
*                                                                               
         DC    AL1(PROFZEN-PROFILED),CL21'ZENITH CLIENT CODE'                   
         DC    AL4(VALZEN,DISPZEN),XL2'0',XL1'0'                                
*                                                                               
         DC    AL1(PROFNPWB-PROFILED),CL21'DISALLOW PW BILLING?'                
         DC    AL4(VALNPWB,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2NPWB)              
*                                                                               
         DC    AL1(PROFPWP-PROFILED),CL21'PW %'                                 
         DC    AL4(VALPWPCT,DISPWPCT),XL2'0',XL1'0'                             
*                                                                               
         DC    AL1(PROFTRAD-PROFILED),CL21'TRADE CLIENT?'                       
         DC    AL4(VALTRADE,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2TRAD)             
*                                                                               
         DC    AL1(PROFFRZ-PROFILED),CL21'FREEZE CLIENT?'                       
         DC    AL4(VALFRZ,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2FRZ)                
*                                                                               
         DC    AL1(PROFXEST-PROFILED),CL21'CROSS EST REPORTING?'                
         DC    AL4(VALXEST,DISPYN),AL2(COPT2-CLTHDR),AL1(COP2XEST)              
*                                                                               
         DC    AL1(PROFACS-PROFILED),CL21'LIMIT ACCESS CODE'                    
         DC    AL4(VALACS,DISPACS),XL2'0',XL1'0'                                
*                                                                               
         DC    X'FF'               E.O.T. FLAG                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         SPACE 2                                                                
         ORG   LFMTABH                                                          
*SPLFMD1                                                                        
       ++INCLUDE SPLFMD1D                                                       
         SPACE 2                                                                
         ORG   SVAPPL                                                           
MYBYTE   DS    CL1                                                              
MYBYTE2  DS    CL1                                                              
COMPCD   DS    CL1                                                              
SVCOFFC  DS    C                   OLD OFFICE CODE                              
MYFULL   DS    F                                                                
DATADISP DS    H                                                                
PROFILE  DS    CL16                16 BYTE PROFILE AREA                         
         EJECT                                                                  
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
TABLE1D  DSECT                     DSECT TO COVER MAIN TABLE                    
PROF     DS    XL1                 DISPLACEMENT INTO PROFILE                    
LITERAL  DS    CL21                SCREEN LITERAL                               
VALRTN   DS    XL4                 VALIDATION ROUTINE                           
DISPRTN  DS    XL4                 DISPLAY ROUTINE                              
OPTBYTE  DS    XL2                 DISP OF OPTION BYTE IN CLIENT REC            
OPTEQU   DS    X                   OPTION BIT MASK                              
*                                                                               
LTABLE1D EQU   *-TABLE1D           L(TABLE ENTRY)                               
*                                                                               
PROFILED DSECT                     DSECT TO COVER PROFILE RECORD Y/N'S          
PROFCOS2 DS    C                   CLIENT USE 2ND COST FEATURE?                 
PROFDBL  DS    C                     "     "   DBL BOOK TEST?                   
PROFINFO DS    C                     "     "   INFOMERCIAL?                     
PROFMGR  DS    C                     "     "   MARKET GROUPS?                   
PROFXJ1  DS    C                     "     "   EXCLUDE J1?                      
PROFXA7  DS    C                     "     "   EXCLUDE A7?                      
PROFCTA  DS    C                     "     "   CTA CLIENT?                      
PROFGMI  DS    C                     "     "   GMI CLIENT?                      
PROFUPL  DS    C                     "     "   EXCLUDE DELETED BUYS?            
PROFZEN  DS    C                     "     "   ZENITH CLIENT?                   
PROFNPWB DS    C                     "     "   NO PW BILLING?                   
PROFPWP  DS    C                     "     "   PW PERCENT?                      
PROFTRAD DS    C                     "     "   TRADE CLIENT?                    
PROFFRZ  DS    C                     "     "   FREEZE?                          
PROFXEST DS    C                     "     "   CROSS EST RPT'ING?               
PROFACS  DS    C                     "     "   LIMIT ACCESS                     
*                                                                               
FIELDD   DSECT                     DSECT TO COVER SCREEN FIELDS                 
SCRNLITH DS    XL8                 LITERAL FIELD HEADER                         
SCRNLIT  DS    CL21                LITERAL                                      
DATAH    DS    XL8                 DATA FIELD HEADER                            
DATA     DS    CL8                 DATA (Y/N, ETC.)                             
*                                                                               
LFIELDD  EQU   *-FIELDD            L(SCREEN FIELD)                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPLFM31   01/20/99'                                      
         END                                                                    
