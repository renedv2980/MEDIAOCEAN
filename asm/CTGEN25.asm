*          DATA SET CTGEN25    AT LEVEL 054 AS OF 08/22/00                      
*PHASE TA0B25A                                                                  
*                                                                               
         TITLE 'CTGEN25 - File Maintenance - DDS Dept security'                 
GEN25    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*GEN25**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLI   APPFKEY,0           TEST FOR UP / DOWN                           
         BE    INIT010                                                          
         SR    R1,R1                                                            
         IC    R1,SAVETOP                                                       
         CLI   APPFKEY,7                                                        
         BNE   *+8                                                              
         SH    R1,=H'34'                                                        
         CLI   APPFKEY,8                                                        
         BNE   *+8                                                              
         AH    R1,=H'34'                                                        
         STC   R1,SAVETOP                                                       
         CLI   SAVETOP,64          CHECK BOUNDS                                 
         BL    *+8                                                              
         MVI   SAVETOP,0                                                        
         MVI   APPFKEY,X'FF'       FLAG PF KEY HIT                              
*                                                                               
INIT010  ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CT3REC,R2                                                        
         MVI   CT3KTYP,CT3KTYPQ    RECORD 3D                                    
         MVI   CT3DSUB,CT3DDSDL                                                 
         MVC   APWORK,SPACES                                                    
*                                                                               
         MVI   FVMINL,1            VALIDATE DEPT                                
         GOTO1 AFVAL,DDSDEPH                                                    
         LA    R1,DEPTTAB                                                       
VALK010  SR    RF,RF               EX COMP FOR NAME                             
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(R1)                                                  
         BE    VALK020                                                          
         LA    R1,L'DEPTTAB(R1)                                                 
         CLC   0(L'DEPTTAB,R1),FFS                                              
         BNE   VALK010                                                          
         B     VALKEYX                                                          
*                                                                               
VALK020  MVC   DDSDEPT,0(R1)       SAVE DEPT                                    
*                                                                               
         MVC   APWORK(8),SPACES    PRESET TO SPACES                             
         MVC   APWORK(8),1(R1)                                                  
         GOTO1 DISPFLD,DDSDEPH     DISPLAY THE DEPT NAME                        
*                                                                               
         MVI   FVMINL,1            VALIDATE LEVEL                               
         GOTO1 AFVAL,DDSLEVH                                                    
         BNE   VALKEYX                                                          
         TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALKEYX                                                          
         OC    SCFULL,SCFULL       MUST BE > 0                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
         CLC   SCFULL,=F'16'       MUST BE <=16                                 
         BH    *-16                                                             
         SR    R1,R1                                                            
         IC    R1,SCFULL+3                                                      
         BCTR  R1,0                                                             
         STC   R1,DDSLEVL          SAVE LEVEL                                   
*                                                                               
         MVC   DDSDPLV,DDSDEPT     MERGE INTO DDSDPLV                           
         OC    DDSDPLV,DDSLEVL                                                  
*                                                                               
         MVC   APRECKEY,CT3KEY                                                  
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS       DISPLAY ONLY?                                
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       NO - READ FOR UPDATE THEN                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT HERE                          
         BNE   *+12                RECORD NOT FOUND?                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD DELETED                          
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DELETED, SO NO RECORD EXISTS             
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES DELETED RECORD FOUND                    
*                                                                               
VALKEYY  EQU   *                                                                
         MVC   FVMSGNO,=AL2(FVFOK) OK EXIT POINT HERE                           
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* ROUTINE TO CHANGE THE DDS DEPT SECURITY RECORD            *                   
*************************************************************                   
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         USING CT3KEY,R2                                                        
         MVC   CT3KEY(L'CT3KEY),APRECKEY                                        
         CLC   DDSDPLV,SAVEDPLV    REDISPLAY IF KEY CHANGED                     
         BNE   DISREC                                                           
         CLI   APPFKEY,X'FF'       OR IF PFKEY HIT                              
         BE    DISREC                                                           
*                                                                               
         CLI   APACTN,ACTADD       ADD?                                         
         BE    VR010               NO ELEMENTS TO REMOVE ON AN ADD              
         XC    APELEM,APELEM                                                    
         MVI   APELEM+0,CTDDSDLQ   DDS DEPT ELEMENT                             
         MVI   APELEM+1,1                                                       
         MVC   APELEM+2(1),DDSDPLV DDS DEPT LEVEL                               
         MVI   APELEM+3,0                                                       
         GOTO1 AGETELS,CT3REC      GET EXISTING ELEMENT                         
         USING CTDDSSD,R3          R3=A(SCRIPT ELEMENT)                         
         ICM   R3,15,APPARM                                                     
         BZ    *+10                                                             
         MVC   DUB,CTDDSBIT                                                     
         GOTO1 ADELELS,CT3REC      DELETE EXISTING ELEMENT                      
*                                                                               
VR010    LA    R1,L'CT3KEY+4       DEFINE INITIAL RECORD LENGTH                 
         STCM  R1,3,CT3LEN         AND SHOVE IT INTO RECORD                     
         XC    APELEM,APELEM       CLEAR ELEMENT AREA                           
         LA    R3,APELEM                                                        
*                                                                               
         MVI   CTDDSDL,CTDDSDLQ    ELEMENT CODE                                 
         MVI   CTDDSLEN,CTDDSLNQ   LEN                                          
         MVC   CTDDSDLV,DDSDPLV    DEPT LEVEL                                   
         XC    CTDDSBIT,CTDDSBIT   CLEAR ALL BITS                               
         MVC   CTDDSBIT,DUB        SET FROM CURRENT                             
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,X'80'           SET FIRST BIT                                
         LA    R4,64                                                            
*                                                                               
         SR    R1,R1               OFFSET DATA BY SAVETOP                       
         IC    R1,SAVETOP                                                       
         LM    RE,RF,DUB                                                        
         SRDL  RE,0(R1)                                                         
         STM   RE,RF,DUB                                                        
         SR    R4,R1                                                            
*                                                                               
         LA    R1,DDSIP1H          POINT TO 1ST FIELD                           
VREC020  MVC   DUB1,FFS            TURN OFF THIS BIT                            
         XC    DUB1,DUB                                                         
         NC    CTDDSBIT,DUB1                                                    
*                                                                               
         MVI   FVMINL,1            TEST YES OR NO                               
         GOTO1 AFVAL                                                            
         BAS   RE,YAYRNEY          WAS IT YES OR NO                             
         BNE   VREC021                                                          
         OC    CTDDSBIT,DUB        TURN BIT ON IF YES                           
*                                                                               
VREC021  LM    RE,RF,DUB           SHUFFLE BIT ALONG 1                          
         SRDL  RE,1                                                             
         STM   RE,RF,DUB                                                        
         LA    R1,DDSIP2H-DDSIP1H(R1)                                           
         LA    RF,DDSPFK                                                        
         CR    R1,RF                                                            
         BNL   VREC080                                                          
         BCT   R4,VREC020          DO 64 TIMES                                  
*                                                                               
VREC080  GOTO1 AADDELS,CT3REC      ADD NEW ELEMENT                              
*                                                                               
         MVC   IOKEY(L'CT3KEY),CT3KEY                                           
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) OK MESSAGE                                   
*                                                                               
VALRECX  B     EXIT                THAT`S ALL FOLKS...                          
*                                                                               
YAYRNEY  BNE   NEYLAD              NO INPUT MEANS NO                            
         CLI   FVIFLD,C'Y'                                                      
         BNE   *+14                                                             
         MVC   8(3,R1),LCYES       ECHO BACK YES                                
         NI    1(R1),255-X'08'                                                  
         CLI   FVIFLD,C'N'                                                      
         BNE   *+14                                                             
NEYLAD   MVC   8(3,R1),LCNO        ECHO BACK NO                                 
         OI    1(R1),X'08'                                                      
         OI    6(R1),X'80'                                                      
         CLC   8(3,R1),LCYES       SET CC ON EXIT                               
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY DDS DEPT RECORD                                            
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         A(THIS RECORD)                               
         USING CT3REC,R2                                                        
         TWAXC DDSIP1H,DDSIPL,PROT=Y    CLEAR DATA PORTION OF SCREEN            
         XC    APWORK,APWORK                                                    
         XC    DUB,DUB                                                          
*                                                                               
         USING CTDDSSD,R3                                                       
         XC    APELEM,APELEM                                                    
         MVI   APELEM+0,CTDDSDLQ   DDS DEPT ELEMENT                             
         MVI   APELEM+1,1                                                       
         MVC   APELEM+2(1),DDSDPLV DDS DEPT LEVEL                               
         GOTO1 AGETELS,CT3REC      GET ELEMENT                                  
         ICM   R3,15,APPARM                                                     
         BZ    *+10                                                             
         MVC   DUB,CTDDSBIT                                                     
*                                                                               
         LA    R1,DDSPFKH          SAVE END OF SCREEN ADDRESS                   
         ST    R1,FULL                                                          
         LA    R4,AUTHTAB                                                       
*                                                                               
         LM    RE,RF,DUB           GET BITS INTO DOUBLE EF                      
         LA    R0,64               64 BITS                                      
*                                                                               
         SR    R1,R1               OFFSET DATA BY SAVETOP                       
         IC    R1,SAVETOP                                                       
         SR    R0,R1                                                            
         SLDL  RE,0(R1)                                                         
         SLL   R1,5                                                             
         AR    R4,R1                                                            
*                                                                               
         LA    R1,DDSOP1H          POINT TO 1ST FIELD                           
DREC020  MVC   8(32,R1),0(R4)      INSERT TEXT                                  
         MVC   48(3,R1),LCNO       DEFAULT TO NO                                
         OI    41(R1),X'08'                                                     
         LTR   RE,RE                                                            
         BNM   DREC021                                                          
         MVC   48(3,R1),LCYES      SET TO YES IF TOP BIT ON                     
         NI    41(R1),255-X'08'                                                 
DREC021  SLDL  RE,1                                                             
         LA    R4,32(R4)                                                        
         OI    46(R1),X'80'                                                     
         OI    6(R1),X'80'                                                      
         LA    R1,DDSOP2H-DDSOP1H(R1)                                           
         C     R1,FULL                                                          
         BE    DRECX                                                            
         BCT   R0,DREC020          DO 64 TIMES                                  
*                                                                               
DRECX    MVC   SAVEDPLV,DDSDPLV                                                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD TRANSMIT IF CHANGED                                  *          
* R1=A(TWA HEADER)                                                   *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                  EQUAL DON`T BOTHER TO MOVE IN DATA           
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW DATA                             
         BR    RE                                                               
*                                                                               
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* ENTRY      R3 = A(CURRENT ELEMENT)                                  *         
*        APELEM = ELEMENT CODE TO FIND                                *         
* EXIT    CC EQ - FOUND - R3=A(NEW ELEMENT)                           *         
*         CC NE - NOT FOUND                                           *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMENT)                              
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM      SAME AS LAST?                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS & LITERALS                                         *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
SPACES   DC    CL132' '                                                         
STARS    DC    80C'*'                                                           
FFS      DC    20X'FF'                                                          
LCYES    DC    C'Yes'                                                           
LCNO     DC    C'No '                                                           
       ++INCLUDE SEDDSDEPT                                                      
         EJECT                                                                  
***********************************************************************         
*        TABLE OF AUTH BITS                                           *         
***********************************************************************         
         SPACE 1                                                                
AUTHTAB  DC    CL32'Operator Service reqs ..........'                           
         DC    CL32'=LOAD/PATCH Service requests ...'                           
         DC    CL32'Systems Service reqs ...........'                           
         DC    CL32'Restricted Service reqs ........'                           
         DC    CL32'CTL Userid/Terminal update .....'                           
         DC    CL32'CTL Output/Profile/Currency ....'                           
         DC    CL32'CTL Fee records ................'                           
         DC    CL32'CTL Security records ...........'                           
         DC    CL32'CTL Text records ...............'                           
         DC    CL32'CTL Broadcast records ..........'                           
         DC    CL32'CTL Systems records ............'                           
         DC    CL32'SEC Systems records ............'                           
         DC    CL32'ACC Company records ............'                           
         DC    CL32'MED Acc/Bac/Media records ......'                           
         DC    CL32'DDS Security records ...........'                           
         DC    CL32'Cross agency functions .........'                           
         DC    CL32'Authorization bit 17............'                           
         DC    CL32'Authorization bit 18............'                           
         DC    CL32'Authorization bit 19............'                           
         DC    CL32'Authorization bit 20............'                           
         DC    CL32'Authorization bit 21............'                           
         DC    CL32'Authorization bit 22............'                           
         DC    CL32'Authorization bit 23............'                           
         DC    CL32'Authorization bit 24............'                           
         DC    CL32'Authorization bit 25............'                           
         DC    CL32'Authorization bit 26............'                           
         DC    CL32'Authorization bit 27............'                           
         DC    CL32'Authorization bit 28............'                           
         DC    CL32'Authorization bit 29............'                           
         DC    CL32'Authorization bit 30............'                           
         DC    CL32'Authorization bit 31............'                           
         DC    CL32'Authorization bit 32............'                           
         DC    CL32'Authorization bit 33............'                           
         DC    CL32'Authorization bit 34............'                           
         DC    CL32'Authorization bit 35............'                           
         DC    CL32'Authorization bit 36............'                           
         DC    CL32'Authorization bit 37............'                           
         DC    CL32'Authorization bit 38............'                           
         DC    CL32'Authorization bit 39............'                           
         DC    CL32'Authorization bit 40............'                           
         DC    CL32'Authorization bit 41............'                           
         DC    CL32'Authorization bit 42............'                           
         DC    CL32'Authorization bit 43............'                           
         DC    CL32'Authorization bit 44............'                           
         DC    CL32'Authorization bit 45............'                           
         DC    CL32'Authorization bit 46............'                           
         DC    CL32'Authorization bit 47............'                           
         DC    CL32'Authorization bit 48............'                           
         DC    CL32'Authorization bit 49............'                           
         DC    CL32'Authorization bit 50............'                           
         DC    CL32'Authorization bit 51............'                           
         DC    CL32'Authorization bit 52............'                           
         DC    CL32'Authorization bit 53............'                           
         DC    CL32'Authorization bit 54............'                           
         DC    CL32'Authorization bit 55............'                           
         DC    CL32'Authorization bit 56............'                           
         DC    CL32'Authorization bit 57............'                           
         DC    CL32'Authorization bit 58............'                           
         DC    CL32'Authorization bit 59............'                           
         DC    CL32'Authorization bit 60............'                           
         DC    CL32'Authorization bit 61............'                           
         DC    CL32'Authorization bit 62............'                           
         DC    CL32'Authorization bit 63............'                           
         DC    CL32'Authorization bit 64............'                           
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
       ++INCLUDE FAUTL                                                          
* CTGENWRK                                                                      
* FAGETTXTD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE CTGENWRK                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT  ON                                                              
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENCAD                                                       
         ORG   SAVOVER                                                          
SAVEDPLV DS    X                                                                
SAVETOP  DS    X                                                                
*                                                                               
*                                                                               
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
DDSDEPT  DS    X                                                                
DDSLEVL  DS    X                                                                
DDSDPLV  DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054CTGEN25   08/22/00'                                      
         END                                                                    
