*          DATA SET NEWRI31T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEWRI31    AT LEVEL 132 AS OF 04/18/91                      
*PHASE T32031A,+0                                                               
*INCLUDE KHDUMMY                                                                
*INCLUDE CLPACK                                                                 
         TITLE 'T32031 - COCA COLA TRANSFER'                                    
T32031   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**N6ED**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD        * ANETWS2 = OPEN WORK AREAS                    
         USING SPOOLD,R8                                                        
         L     R9,ASYSD          * ANETWS1 = FIXED WORK AREA                    
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING MYD,R7                                                           
         L     R6,TWADCONS                                                      
         USING TWADCOND,R6                                                      
         ST    R2,RELO                                                          
*                                                                               
         L     R1,BOOKVAL                                                       
         ST    R1,ASTDISK                                                       
         LA    R1,200(R1)                                                       
         ST    R1,ASPDISK                                                       
         LA    R1,200(R1)                                                       
         ST    R1,AUNDISK                                                       
*                                                                               
         L     R3,ANETWS2                                                       
         LA    R3,4(R3)                                                         
         ST    R3,AIO                                                           
         L     R3,ANETWS2                                                       
*                                                                               
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALREC                                                      
         BNE   RP3                                                              
*                                                                               
         BAS   RE,VALSCRN                                                       
         B     XIT                                                              
*                                                                               
RP3      CLI   MODE,PRINTREP                                                    
         BNE   RP4                                                              
*                                                                               
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   *+12                                                             
         BAS   RE,SETUP                                                         
         BAS   RE,OUTBASIC                                                      
*                                                                               
         LA    R4,BRECTAB                                                       
         BAS   RE,LOADTAPE                                                      
*                                                                               
         SR    R5,R5               PRNTBL COUNTER                               
         MVC   0(2,R3),=XL2'0010'  SET TRAILER RECORD                           
         MVC   4(3,R3),=CL3'LYN'                                                
         MVC   7(2,R3),CLI2                                                     
         MVC   9(3,R3),CLI3                                                     
         BAS   RE,PSPDISK                                                       
         BAS   RE,CLEARIO                                                       
*                                                                               
         BAS   RE,OUTUNITS                                                      
         BAS   RE,OUTPACK                                                       
*                                                                               
RP4      L     RE,ATWA                                                          
         CLI   29(RE),X'FF'        CHECK FOR REQLAST INDICATOR                  
         BNE   XIT                                                              
         CLC   4(4,R3),=CL4'EVAN'                                               
         BE    XIT                                                              
*                                                                               
         SR    R5,R5                                                            
         XC    0(4,R3),0(R3)                                                    
         MVI   1(R3),10                                                         
         MVC   4(10,R3),=10X'FF'                                                
         BAS   RE,PSTDISK          SET TRAILER RECORD STATION                   
*                                                                               
         MVI   1(R3),8                                                          
         MVC   4(4,R3),=CL4'EVAN'                                               
         BAS   RE,PSPDISK          SET TRAILER RECORD SPOT                      
*                                                                               
         BAS   RE,PUNDISK          SET TRAILER RECORD UNIT                      
*                                                                               
         MVI   1(R3),14                                                         
         MVC   4(10,R3),=10X'FF'                                                
*        BAS   RE,PSPDISK          SET EOF RECORD                               
*        BAS   RE,PUNDISK          SET EOF RECORD                               
         MVC   4(4,R3),=CL4'EVAN'                                               
*                                                                               
         L     R2,ASTDISK                                                       
         CLOSE ((R2),)                                                          
         L     R2,ASPDISK                                                       
         CLOSE ((R2),)                                                          
         L     R2,AUNDISK                                                       
         CLOSE ((R2),)                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 TPRNTBL,DMCB,=C'CLOS',(R2),C'DUMP',128,=C'1D'                    
         B     XIT                                                              
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE SCREEN                                              
VALSCRN  NTR1                                                                   
         LA    R2,SPLCLIH                CLIENT                                 
*                                                                               
         CLC   AGENCY,=CL2'MC'     MUST BE MCCANN REQUEST                       
         BNE   INVREPT                                                          
*                                                                               
         LA    R3,SPLCLI                 CLIENT                                 
         LA    R4,CLITAB                                                        
*                                                                               
VALS50   CLC   0(3,R3),0(R4)                                                    
         BE    VALS100                                                          
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    INVCLT                                                           
         B     VALS50                                                           
*                                                                               
VALS100  MVC   CLI3,SPLCLI                                                      
         GOTO1 =V(CLPACK),DMCB,SPLCLI,CLI2,RR=RELO                              
*                                                                               
         LA    R2,SPLDATH                DATE                                   
*                                                                               
         CLI   SPLDATH+5,0                                                      
         BE    INVPER                                                           
*                                                                               
         ZIC   R4,SPLDATH+5                                                     
         BCTR  R4,0                                                             
*        MVI   SDATE,X'FF'                                                      
*        EX    R4,ALLCOMP                                                       
*        BE    XIT                                                              
*                                                                               
         MVI   SDATE,0                                                          
         EX    R4,NONECOMP                                                      
         BE    XIT                                                              
*                                                                               
         GOTO1 SCANNER,DMCB,SPLDATH,(1,WORK),C',=,-'                            
         CLI   4(R1),1                                                          
         BNE   INVPER                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(0,WORK+12),DATEHLD                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    INVPER                                                           
         GOTO1 DATCON,DMCB,(0,DATEHLD),(2,SDATE)                                
*                                                                               
         GOTO1 DATVAL,DMCB,(0,WORK+22),DATEHLD                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    INVPER                                                           
         GOTO1 DATCON,DMCB,(0,DATEHLD),(2,EDATE)                                
         B     XIT                                                              
*                                                                               
ALLCOMP  CLC   SPLDAT(0),=CL3'ALL'                                              
NONECOMP CLC   SPLDAT(0),=CL4'NONE'                                             
*                                                                               
INVCLT   MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVREPT  MVI   ERROR,INVREP                                                     
         B     TRAPERR                                                          
*                                                                               
INVPER   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
CLITAB   DC    CL3'DR1'                                                         
         DC    CL3'DR2'                                                         
         DC    CL3'DR3'                                                         
         DC    CL3'DR4'                                                         
         DC    CL3'DR5'                                                         
         DC    CL3'DR6'                                                         
         DC    CL3'DR7'                                                         
         DC    CL3'DR8'                                                         
         DC    CL3'DR9'                                                         
         DC    CL3'DR0'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              SET UP THE TAPE                                                  
SETUP    NTR1                                                                   
         L     RE,ATWA                                                          
         MVI   29(RE),X'02'        SET FOR REQLAST INDICATOR                    
*                                                                               
         L     RE,BOOKVAL                                                       
         LA    RF,STDISK                                                        
         MVC   0(128,RE),0(RF)                                                  
         LA    RF,SPDISK                                                        
         MVC   200(128,RE),0(RF)                                                
         LA    RF,UNDISK                                                        
         MVC   400(128,RE),0(RF)                                                
*                                                                               
         L     R2,ASTDISK                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         L     R2,ASPDISK                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         L     R2,AUNDISK                                                       
         OPEN  ((R2),(OUTPUT))                                                  
*                                                                               
         XC    0(4,R3),0(R3)                                                    
         MVI   1(R3),10                                                         
         MVC   4(6,R3),=CL6'NELSON'     SET HEADER RECORD                       
         SR    R5,R5                                                            
         BAS   RE,PSPDISK                                                       
         BAS   RE,PUNDISK                                                       
         BAS   RE,CLEARIO                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--MOVE THE BASIC RECORDS OUT TO TAPE                                           
OUTBASIC NTR1                                                                   
         BAS   RE,CLEARIO                                                       
         MVI   1(R3),X'79'                                                      
*                                                                               
*--MOVE MARKET,RECORD                                                           
*                                                                               
         SR    R5,R5               PRNTBL COUNTER                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=CL2'MN'                                                  
         MVI   NFILE,C'T'                                                       
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
OUTB050  BAS   RE,NSEQ                                                          
         CLC   KEYSAVE(2),4(R3)                                                 
         BNE   OUTB100                                                          
         CLC   10(2,R3),=CL2'MC'                                                
         BNE   OUTB050                                                          
*                                                                               
         MVI   1(R3),X'79'         RECORD LENGTH OF 117                         
         MVC   10(2,R3),=C'CC'     CHANGE TO COCA COLA AGENCY                   
         BAS   RE,PSTDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
         B     OUTB050                                                          
*--MOVE REP RECORD                                                              
*                                                                               
OUTB100  SR    R5,R5               PRNTBL COUNTER                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=CL2'RN'                                                  
         MVI   NFILE,C'T'                                                       
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
OUTB120  BAS   RE,NSEQ                                                          
         CLC   KEYSAVE(2),4(R3)                                                 
         BNE   OUTB150                                                          
         CLC   9(2,R3),=CL2'MC'                                                 
         BNE   OUTB120                                                          
*                                                                               
         MVI   1(R3),X'79'         RECORD LENGTH OF 117                         
         MVC   9(2,R3),=C'CC'      CHANGE TO COCA COLA AGENCY                   
         BAS   RE,PSTDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
         B     OUTB120                                                          
*                                                                               
*--MOVE STATION,RECORD                                                          
*                                                                               
OUTB150  SR    R5,R5               PRNTBL COUNTER                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=CL2'SN'                                                  
         MVI   NFILE,C'T'                                                       
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
OUTB170  BAS   RE,NSEQ                                                          
         CLC   KEYSAVE(2),4(R3)                                                 
         BNE   OUTB200                                                          
         CLC   11(2,R3),=CL2'MC'                                                
         BNE   OUTB170                                                          
*                                                                               
         MVI   1(R3),X'79'         RECORD LENGTH OF 117                         
         MVC   11(2,R3),=C'CC'     CHANGE TO COCA COLA AGENCY                   
         BAS   RE,PSTDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
         B     OUTB170                                                          
*--ROUTINE TO OUTPUT SPOT RECORDS                                               
OUTB200  LA    R4,ARECTAB                                                       
         BAS   RE,LOADTAPE                                                      
         B     XIT                                                              
*                                                                               
KEYCOMP  CLC   KEY(0),KEYSAVE                                                   
         EJECT                                                                  
*--LOADS UNIT RECORDS                                                           
OUTUNITS NTR1                                                                   
         LA    R2,PACKTAB                                                       
         MVI   0(R2),X'FF'                                                      
*                                                                               
         SR    R5,R5               PRNTBL COUNTER                               
*                                                                               
         SR    R4,R4                                                            
*                                                                               
         CLI   SDATE,0             REQUEST IS FOR NONE                          
         BE    OUTU200                                                          
*                                                                               
         CLI   SDATE,X'FF'         REQUEST IS FOR ALL                           
         BNE   OUTU30                                                           
         MVI   0(R2),X'00'                                                      
*                                                                               
OUTU30   MVI   NFILE,C'U'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(4),=XL4'0453885F'                                            
         CLI   SDATE,X'FF'         REQUEST IS FOR ALL                           
         BE    *+10                                                             
         MVC   KEY+4(2),SDATE                                                   
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
OUTU50   BAS   RE,NSEQ                                                          
         CLC   KEY(4),KEYSAVE                                                   
         BNE   OUTU200                                                          
         CLI   SDATE,X'FF'         REQUEST IS FOR ALL                           
         BE    OUTU70                                                           
         CLC   KEY+4(2),SDATE                                                   
         BL    OUTU50                                                           
         CLC   KEY+4(2),EDATE                                                   
         BH    OUTU50                                                           
*                                                                               
OUTU70   BAS   RE,NGETREC          READ RECORD                                  
         SR    RF,RF                                                            
         LH    RF,24(R3)                                                        
         LA    RF,4(RF)                                                         
         STH   RF,0(R3)            RECORD LENGTH                                
*                                                                               
         L     RE,AIO                                                           
         USING NURECD,RE                                                        
*--SET UP PACKAGE TABLE                                                         
         CLI   SDATE,X'FF'         REQUEST IS FOR ALL                           
         BE    OUTU100                                                          
         LA    R2,PACKTAB                                                       
         MVC   DUB(4),NUKNET                                                    
         MVC   DUB+4(1),NUKEST                                                  
         MVC   DUB+5(1),NUPACK                                                  
*                                                                               
OUTU80   CLI   0(R2),X'FF'                                                      
         BE    OUTU90                                                           
         CLC   0(6,R2),DUB                                                      
         BE    OUTU100                                                          
         LA    R2,6(R2)                                                         
         B     OUTU80                                                           
*                                                                               
OUTU90   MVC   0(6,R2),DUB                                                      
         MVI   6(R2),X'FF'                                                      
*--PUT COCA-COLA INFO ON UNIT RECORD                                            
OUTU100  MVI   1(RE),X'53'         AGENCY CODE                                  
         MVC   2(2,RE),CLI2        2 BYTE CLIENT CODE                           
*                                                                               
         LA    R4,1(R4)            RECORD COUNT                                 
         BAS   RE,PUNDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
         B     OUTU50                                                           
*                                                                               
OUTU200  STCM  R4,15,UNITCNT                                                    
         B     XIT                                                              
         EJECT                                                                  
*--LOADS PACKAGE RECORDS OUT TO TAPE                                            
OUTPACK  NTR1                                                                   
         SR    R5,R5              PRNTBL COUNTER                                
*                                                                               
         LA    R2,PACKTAB                                                       
         GOTO1 TPRNTBL,DMCB,=C'PACK',(R2),C'DUMP',400,=C'1D'                    
         CLI   0(R2),X'FF'         END OF TABLE MARK                            
         BE    OUTP200                                                          
*                                                                               
         MVI   NFILE,C'U'                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(14),=XL14'020000000000000000000053885F'                      
         CLI   0(R2),X'00'                                                      
         BE    *+10                                                             
OUTP30   MVC   KEY+14(6),0(R2)                                                  
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
OUTP50   BAS   RE,NSEQ                                                          
         LA    RE,13                                                            
         CLI   0(R2),X'00'                                                      
         BE    *+8                                                              
         LA    RE,19                                                            
         EX    RE,KEYCOMP                                                       
         BNE   OUTP100                                                          
         BAS   RE,NGETREC          READ RECORD                                  
         SR    RF,RF                                                            
*                                                                               
         LH    RF,24(R3)                                                        
         LA    RF,4(RF)                                                         
         STH   RF,0(R3)            RECORD LENGTH                                
*                                                                               
         L     RE,AIO                                                           
*                                                                               
         MVI   11(RE),X'53'         ONE BYTE AGENCY CODE                        
         MVC   12(2,RE),CLI2        2 BYTE CLIENT CODE                          
*                                                                               
OUTP80   BAS   RE,PUNDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         CLI   0(R2),X'00'         IF REQUEST IS ALL DONT BUMP TABLE            
         BE    OUTP50                                                           
*                                                                               
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'FF'         IF END OF TABLE EXIT                         
         BE    OUTP200                                                          
         B     OUTP30                                                           
*                                                                               
OUTP100  CLI   0(R2),X'00'         IF REQUEST IS ALL EXIT                       
         BE    OUTP200                                                          
         DS    H'0'                ELSE DUMP                                    
*                                                                               
OUTP200  SR    R5,R5               PRNTBL COUNTER                               
         MVC   0(2,R3),=XL2'0010'  SET TRAILER RECORD                           
         MVC   4(3,R3),=CL3'LYN'                                                
         MVC   7(2,R3),CLI2                                                     
         MVC   9(3,R3),CLI3                                                     
         MVC   12(4,R3),UNITCNT                                                 
         BAS   RE,PUNDISK                                                       
         BAS   RE,CLEARIO                                                       
         B     XIT                                                              
         EJECT                                                                  
*--ARECTAB       BYTE 0    =FILE CODE                                           
*                BYTE 1-15 =KEY                                                 
*                BYTE 16   =KEY INPUT LENGTH                                    
*                BYTE 17   =CLIENT CODE OFFSET                                  
*                BYTE 18   =2 OR 3 BYTE CLIENT SWITCH                           
*                BYTE 19   =AGENCY OFFSET                                       
*                BYTE 20   =1 0R 2 BYTE AGENCY CODE                             
* 1)    INTEGRATION RECORD                                                      
* 2)    HUT RECORD                                                              
* 3)    PROGRAM RECORD                                                          
* 4)    UNIVERSE RECORD                                                         
* 5)    DAYPART HEADRER                                                         
ARECTAB  DC    CL1'U',XL15'0A0000000000530000000000000000'                      
         DC    XL5'07000006FF'                                                  
         DC    CL1'S',XL15'0D5053000000000000000000000000'                      
         DC    XL5'03000002FF'                                                  
         DC    CL1'S',XL15'0D2053000000000000000000000000'                      
         DC    XL5'03000002FF'                                                  
         DC    CL1'S',XL15'0D22D4C30000000000000000000000'                      
         DC    XL5'0400000200'                                                  
         DC    CL1'S',XL15'08D4C3D50000000000000000000000'                      
         DC    XL5'0400000100'                                                  
         DC    X'FF'                                                            
*--BRECTAB       BYTE 0    =FILE CODE                                           
*                BYTE 1-15 =KEY                                                 
*                BYTE 16   =KEY INPUT LENGTH                                    
*                BYTE 17   =CLIENT CODE OFFSET                                  
*                BYTE 18   =2 OR 3 BYTE CLIENT SWITCH                           
*                BYTE 19   =AGENCY OFFSET                                       
*                BYTE 20   =1 0R 2 BYTE AGENCY CODE                             
* 1)    PRODUCT, ESTIMATE, CLIENT RECORDS                                       
* 2)    GOAL RECORD                                                             
* 3)    PRODUCT GROUP RECORD                                                    
* 4)    MARKET GROUP RECORD                                                     
* 5)    NTI STATION RECORD                                                      
BRECTAB  DC    CL1'S',XL15'0053885F0000000000000000000000'                      
         DC    XL5'04020001FF'                                                  
         DC    CL1'S',XL15'0253885F0000000000000000000000'                      
         DC    XL5'04020001FF'                                                  
         DC    CL1'S',XL15'0D0153885F00000000000000000000'                      
         DC    XL5'05030002FF'                                                  
         DC    CL1'S',XL15'0D0253885F00000000000000000000'                      
         DC    XL5'05030002FF'                                                  
         DC    CL1'S',XL15'0D7553000000000000000000000000'                      
         DC    XL5'03080002FF'                                                  
         DC    X'FF'                                                            
**********************************************                                  
*--LOADS RECORDS OUT TO TAPE                                                    
*---R4 POINTS TO INPUT TABLE                                                    
LOADTAPE NTR1                                                                   
LDTP20   CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
         SR    R5,R5               PRNTBL COUNTER                               
*                                                                               
         MVC   NFILE,0(R4)                                                      
         MVC   KEY(15),1(R4)                                                    
         BAS   RE,NHIGH                                                         
         B     *+8                                                              
*                                                                               
LDTP50   BAS   RE,NSEQ                                                          
         ZIC   RE,16(R4)                                                        
         BCTR  RE,0                                                             
         EX    RE,KEYCOMP                                                       
         BNE   LDTP100                                                          
         CLI   KEY,0               CHECK PROD,EST,CLENT KEY                     
         BNE   *+14                                                             
         OC    KEY+8(5),KEY+8      MAKE SURE ONLY PROD,EST,CLIENT               
         BNZ   LDTP50                                                           
         BAS   RE,NGETREC          READ RECORD                                  
         SR    RF,RF                                                            
         LH    RF,17(R3)                                                        
         LA    RF,4(RF)                                                         
         STH   RF,0(R3)            RECORD LENGTH                                
         CLI   NFILE,C'S'                                                       
         BE    *+18                                                             
         SR    RF,RF                                                            
         LH    RF,24(R3)                                                        
         LA    RF,4(RF)                                                         
         STH   RF,0(R3)            RECORD LENGTH                                
*                                                                               
         CLI   17(R4),0            IS CLIENT OFFSET SET                         
         BE    LDTP70                                                           
         ZIC   RF,17(R4)           MOVE IN CLIENT CODE                          
         L     RE,AIO                                                           
         AR    RE,RF                                                            
*                                                                               
         CLI   18(R4),X'FF'                                                     
         BE    *+24                                                             
         CLC   0(2,RE),=XL2'885F'  MAKE SURE CLIENT COCA COLA                   
         BNE   LDTP50                                                           
         MVC   0(2,RE),CLI2        2 BYTE CLIENT CODE                           
         B     LDTP70                                                           
*--THREE BYTE CLIENT CHECK                                                      
         CLC   0(2,RE),=CL2'CC'    MAKE SURE CLIENT COCA COLA                   
         BNE   LDTP50                                                           
         MVC   0(3,RE),CLI3        3 BYTE CLIENT CODE                           
*                                                                               
LDTP70   CLI   19(R4),0            IS AGENCY OFFSET SET                         
         BE    LDTP80                                                           
         ZIC   RF,19(R4)           MOVE IN AGENCY OFFSET                        
         L     RE,AIO                                                           
         AR    RE,RF                                                            
         CLI   20(R4),X'FF'                                                     
         BE    *+14                                                             
         MVC   0(2,RE),=C'CC'      2 BYTE AGENCY CODE                           
         B     LDTP80                                                           
         MVI   0(RE),X'53'         ONE BYTE AGENCY CODE                         
*                                                                               
LDTP80   CLI   0(R4),C'U'                                                       
         BE    *+16                                                             
         BAS   RE,PSPDISK                                                       
         BAS   RE,CLEARIO                                                       
         B     *+12                                                             
         BAS   RE,PUNDISK                                                       
         BAS   RE,CLEARIO                                                       
         LA    R5,1(R5)                                                         
         B     LDTP50                                                           
*                                                                               
LDTP100  LA    R4,21(R4)                                                        
         B     LDTP20                                                           
         EJECT                                                                  
*              DATAMGR INTERFACE                                                
         SPACE 3                                                                
NHIGH    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         B     DIRALL                                                           
         SPACE 1                                                                
NSEQ     NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
NREAD    NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         SPACE 1                                                                
DIRALL   MVC   SYSFIL,=C'SPTDIR  '         DIRECTORIES                          
         CLI   NFILE,C'S'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'UNTDIR  '                                              
         CLI   NFILE,C'U'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'STATION '                                              
         CLI   NFILE,C'T'                                                       
         MVC   KEYSAVE,KEY                                                      
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,COMMAND,SYSFIL,KEY,(R2),0                           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DRL2     MVC   KEYSAVE,KEY                                                      
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,COMMAND,SYSFIL,KEY,KEY,0                            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
NGETREC  NTR1                                                                   
         L     R2,AIO                                                           
         LA    R4,KEY+14                                                        
         MVC   SYSFIL,=C'SPTFILE '     FILE                                     
         MVC   DATADISP,=H'24'                                                  
         CLI   NFILE,C'S'                                                       
         BE    GETREC2                                                          
         MVC   SYSFIL,=C'UNTFILE '                                              
         MVC   DATADISP,=H'27'                                                  
         LA    R4,KEY+21                                                        
         SPACE 1                                                                
GETREC2  L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,(X'00',=C'GETREC'),SYSFIL,(R4),(R2),DMWORK          
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
         B     XIT                                                              
*                                                                               
*--WRITE TO STATION DISK                                                        
PSTDISK  NTR1                                                                   
         L     R2,ASTDISK                                                       
         PUT   (R2),(R3)                                                        
*                                                                               
         C     R5,=F'10'                                                        
         BH    XIT                                                              
         GOTO1 TPRNTBL,DMCB,=C'STAT',(R3),C'DUMP',50,=C'1D'                     
         B     XIT                                                              
*--WRITE TO SPOT DISK                                                           
PSPDISK  NTR1                                                                   
         L     R2,ASPDISK                                                       
         PUT   (R2),(R3)                                                        
*                                                                               
         C     R5,=F'10'                                                        
         BH    XIT                                                              
         GOTO1 TPRNTBL,DMCB,=C'SPOT',(R3),C'DUMP',50,=C'1D'                     
         B     XIT                                                              
*--WRITE TO UNIT DISK                                                           
PUNDISK  NTR1                                                                   
         L     R2,AUNDISK                                                       
         PUT   (R2),(R3)                                                        
*                                                                               
         C     R5,=F'10'                                                        
         BH    XIT                                                              
         GOTO1 TPRNTBL,DMCB,=C'UNIT',(R3),C'DUMP',100,=C'1D'                    
         B     XIT                                                              
*                                                                               
PRNT     NTR1                                                                   
         GOTO1 TPRNTBL,DMCB,=C'TAPE',(R3),C'DUMP',50,=C'1D'                     
         B     XIT                                                              
*                                                                               
CLEARIO  NTR1                                                                   
         LR    RE,R3                                                            
         LA    RF,2000                                                          
         XCEF                                                                   
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
*                                                                               
* DCB FOR OUTPUT LOAD TAPE                                                      
*                                                                               
STDISK   DCB   DDNAME=STDISK,DSORG=PS,RECFM=VB,LRECL=2000,             *        
               BLKSIZE=32760,MACRF=PM                                           
SPDISK   DCB   DDNAME=SPDISK,DSORG=PS,RECFM=VB,LRECL=2000,             *        
               BLKSIZE=32760,MACRF=PM                                           
UNDISK   DCB   DDNAME=UNDISK,DSORG=PS,RECFM=VB,LRECL=2000,             *        
               BLKSIZE=32760,MACRF=PM                                           
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
PACKTAB  DS    CL20000                                                          
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
ASTDISK  DS    F                   A(TAPE DCB)                                  
ASPDISK  DS    F                   A(TAPE DCB)                                  
AUNDISK  DS    F                   A(TAPE DCB)                                  
UNITCNT  DS    F                   UNIT COUNTER                                 
CLI3     DS    CL3                 3 BYTE CLIENT                                
CLI2     DS    CL2                 2 BYTE CLIENT                                
SDATE    DS    CL2                 START DATE                                   
EDATE    DS    CL2                 END DATE                                     
DATEHLD  DS    CL6                 HOLD AREA FOR DATE                           
NFILE    DS    CL1                 FILE CODE FOR READ LOGIC                     
RELO     DS    A                   RELOCATION FACTOR                            
MYDLENE  EQU   *-MYD                                                            
*******                    ANY NEW FIELDS GO HERE                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE9D                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NEGENUNIT                                                      
NDBLK    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEWRI31T  05/01/02'                                      
         END                                                                    
