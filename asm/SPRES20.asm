*          DATA SET SPRES20    AT LEVEL 013 AS OF 04/01/14                      
*PHASE T20F20A                                                                  
         TITLE 'T20F20- RESEARCH CUSTOM DAYPART MAINT/LIST'                     
         SPACE 1                                                                
T20F20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F20                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING CONHEADH-64,R3                                                   
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT OFFLINE                         
         BE    PR                                                               
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R5,KEY                                                           
         USING CDPKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CDPKTYPE,X'0D'                                                   
         MVI   CDPKSUB,X'5A'                                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(12),=X'0C00000000020000D9C10000'                            
         LA    R2,ELEM                                                          
         GOTO1 VVALSRC             FAKE THE SOURCE FIELD TO 'RA'                
         MVC   CDPKAM,BAGYMD                                                    
*                                                                               
         LA    R2,CDPNAMH                                                       
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VKX                                                              
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     DS    0H                                                               
         GOTO1 ANY                                                              
         MVC   CDPKNAME,WORK                                                    
*                                                                               
VKX      MVC   SVKEY,KEY                                                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,DSCELQ                                                    
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,CDPDESCH         UPGRADE FIELD                                
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R6,ELEM             BUILD DESCRIPTION ELEMENT                    
         USING DSCELEM,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   DSCELEM,DSCELQ                                                   
         MVI   DSCLEN,DSCLENEQ                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DSCDESC(0),8(R2)                                                 
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       CANNOT RESEQ ON AN ADD                       
         BE    VR1B                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC              SAVE RECORD IN IO2                           
*                                                                               
         XR    R7,R7                                                            
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    VR1A                                                             
         DC    H'0'                MUST HAVE ONE '05' ELEMNT                    
*                                                                               
VR1      BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
*                                                                               
VR1A     LA    R7,1(R7)            COUNT NUMBER OF ELEMENTS '05' TYPE           
         B     VR1                                                              
*                                                                               
         MVC   AIO,AIO1            RESTORE IO AREA                              
*                                                                               
         MVI   ELCODE,DPTELQ                                                    
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
VR1B     XC    SEQNUM,SEQNUM                                                    
         XC    COUNTER,COUNTER                                                  
         LA    R2,CDPDTL1H         FIRST DAYPART LIST ENTRY                     
         MVI   FIRSTIME,C'Y'                                                    
         EJECT                                                                  
* EDIT DAY/TIME LIST ENTRY *                                                    
         SPACE 1                                                                
VR2      ST    R2,SAVEDPL          SAVE DAYPART LIST HDR ADDRESS                
         XC    ELEM,ELEM                                                        
         LH    RE,SEQNUM           INCREMENT SEQUENCE#                          
         LA    RE,1(RE)                                                         
         STH   RE,SEQNUM                                                        
         STC   RE,BYTE                                                          
         CLM   RE,1,COUNTER                                                     
         BNE   VR2A1                                                            
         LA    RE,1(RE)            YES, THEN SKIP SEQ#                          
         STH   RE,SEQNUM                                                        
         STC   RE,BYTE                                                          
*                                                                               
*                                                                               
VR2A1    CLI   8(R2),C'='          TEST FOR RESEQUENCING COMMAND                
         BNE   VR9                                                              
         CLI   FIRSTIME,C'Y'                                                    
         BNE   VR8B                                                             
         CLI   11(R2),X'00'        MUST CLEAR AFTER RESEQ#                      
         BE    *+12                                                             
         MVI   ERROR,MUSTCLR                                                    
         B     EDTERR                                                           
*                                                                               
         CLI   9(R2),C'0'          TEST FOR NUMERIC                             
         BL    VR8A                                                             
         CLI   9(R2),C'9'                                                       
         BH    VR8A                                                             
         CLI   10(R2),X'00'                                                     
         BNE   *+12                                                             
         LA    R5,1              INPUT LENGTH                                   
         B     THERE                                                            
*                                                                               
         CLI   10(R2),C'0'                                                      
         BL    VR8A                                                             
         CLI   10(R2),C'9'                                                      
         BH    VR8A                                                             
         LA    R5,2              INPUT LENGTH                                   
*                                                                               
THERE    L     R6,AIO2             LOCATION OF UNCHANGED RECORD                 
         ST    R6,AIO                                                           
         USING DPTELEM,R6                                                       
         MVI   ELCODE,DPTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    VR3                                                              
         DC    H'0'                DIE IF NO X'05' RECORDS                      
*                                                                               
VR2A     BAS   RE,NEXTEL                                                        
         BE    VR3                                                              
         DC    H'0'                DIE IF NO RECORD OF OLD SEQUENCE#            
*                                                                               
VR3      CLC   DPTSEQ,BYTE         FIND ELEMENT TO BE RESEQUENCED               
         BE    *+8                                                              
         B     VR2A                                                             
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)         PACK NEW SEQ#                                
*                                                                               
         CVB   R1,DUB              ENTER NEW SEQ# INTO ELEMENT                  
*                                                                               
         CR    R1,R7               TEST RESEQ# > TOTAL NUMBER ELEMENTS          
         BNH   *+12                                                             
         MVI   ERROR,OVERMAX                                                    
         B     EDTERR                                                           
*                                                                               
         STC   R1,DPTSEQ                                                        
         MVC   ELEM(DPTLENEQ),0(R6)      MOVE ELEMENT TO ELEM                   
         MVC   AIO,AIO1            RESTORE AIO                                  
         DROP  R6                                                               
*                                                                               
         STCM  R1,1,COUNTER        SAVE NEW SEQ#                                
         MVI   FIRSTIME,C'N'                                                    
         CLM   R1,1,BYTE                                                        
         BL    VR4                                                              
         LH    RE,SEQNUM           RESET SEQ#                                   
         BCTR  RE,0                                                             
         STH   RE,SEQNUM                                                        
         B     VR8                                                              
*                                                                               
VR4      L     R6,AIO              LOCATION UPDATED DATA                        
         USING DPTELEM,R6                                                       
         MVI   ELCODE,DPTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    VR7                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
VR5      BAS   RE,NEXTEL                                                        
         BNE   VR8                                                              
*                                                                               
VR7      CLM   R1,1,DPTSEQ         TEST IF NEED RENUMBERED                      
         BNE   VR5                                                              
         LA    R1,1(R1)            BUMP SEQ#                                    
         STCM  R1,1,DPTSEQ         RENUMBER                                     
         B     VR5                                                              
*                                                                               
VR8      DS    0H                                                               
         BAS   RE,BUMP             POINT TO APPROPRIATE FIELD                   
         B     VR39                                                             
*                                                                               
VR8A     MVI   ERROR,NUMER         RESEQ# MUST BE NUMERIC                       
         B     EDTERR                                                           
*                                                                               
VR8B     MVI   ERROR,ONEONL        ONLY ONE RESEQ AT A TIME                     
         B     EDTERR                                                           
*                                                                               
VR9      LA    R6,ELEM                                                          
         USING DPTELEM,R6                                                       
         MVC   DPTINPUT,8(R2)      SAVE INPUT DATA                              
*                                                                               
         MVI   DPTELEM,DPTELQ                                                   
         MVI   DPTLEN,DPTLENEQ                                                  
         MVC   DPTSEQ,BYTE                                                      
         LA    R7,DPTELEM+3                                                     
*                                                                               
         LA    R4,BLOCK                                                         
         XCEF  (R4),480                                                         
         MVI   SCANLEN,32                                                       
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK),C',=/='                             
*                                                                               
VR10     BAS   RE,VALDAY                                                        
         BNE   SCANERR                                                          
         MVC   0(1,R7),BYTE        MOVE DAY TO LIST                             
*                                                                               
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
*                                                                               
         CLI   0(R4),0                                                          
         BNE   VR26                                                             
         MVI   ERROR,NOTIME        TELL USER TIME IS MISSING                    
         GOTO1 VCURSERR                                                         
*                                                                               
VR26     DS    0H                                                               
         CLI   1(R4),0                                                          
         BNE   SCANERR                                                          
         BAS   RE,VALTIME                                                       
         BNE   SCANERR                                                          
*                                                                               
VR28     MVC   1(2,R7),HALF        MOVE TIME TO ELEMENT                         
*                                                                               
         LA    R7,3(R7)            ADVANCE DAY/TIME LIST POINTER                
*                                                                               
         LA    R4,32(R4)                                                        
         CLI   0(R4),0             TEST ANY MORE INPUT THIS FIELD               
         BE    VR30                                                             
         LA    RE,DPTNAME          YES - CHECK NO MARE THAN MAX                 
         CR    R7,RE                     DAYS/TIMES                             
         BNL   SCANERR                                                          
*                                                                               
         CLI   1(R4),0                                                          
         BNE   SCANERR                                                          
         BAS   RE,VALTIME          VALIDATE FOR ANOTHER TIME                    
         BNE   VR10                IF INVALID, TRY FOR ANOTHER DAY              
*                                                                               
         LR    RE,R7                                                            
         SH    RE,=H'3'                                                         
         MVC   0(1,R7),0(RE)       COPY DAY FROM PREVIOUS                       
         B     VR28                 AND CONTINUE                                
         EJECT                                                                  
VR30     MVI   ERROR,TOOMANY                                                    
         LA    R0,ELEM+DPTNAME-DPTELEM  POINT TO END OF D/T DATA                
         CR    R7,R0                    COMPARE TO CURRENT LOCATION             
         BH    EDTERR                                                           
*                                                                               
         BAS   RE,BUMP             POINT TO DAYPART OVERRIDE FIELD              
*                                                                               
         XC    DPTNAME,DPTNAME                                                  
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VR32                YES                                          
         CLI   DPTDAY1+3,0         TEST MORE THAN ONE DAY/TIME                  
         BE    VR33                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR              YES - INPUT REQUIRED                         
*                                                                               
* CHECK DAYPART OVERRIDE FIELD IS NOT A VALID DAY/TIME EXPRESSION,              
* EXCEPT IF IT'S THE SAME DAYS/TIMES AS ON LEFT SIDE OF SCREEN                  
*                                                                               
VR32     LA    R4,BLOCK                                                         
         XCEF  (R4),480                                                         
         LA    RE,DPTELEM+3                                                     
         SR    R7,RE                                                            
         STC   R7,DTLEN            SAVE LENGTH OF DAY/TIME EXPRESSIONS          
         LA    R7,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   SCANLEN,32                                                       
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK),C',=/='                             
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VR32X                                                            
VR32A    BAS   RE,VALDAY                                                        
         BNE   VR32X                                                            
         MVC   0(1,R7),BYTE                                                     
         LA    R4,32(R4)                                                        
         BCT   R0,*+8                                                           
         B     VR32X                                                            
         CLI   1(R4),0                                                          
         BNE   VR32X                                                            
         BAS   RE,VALTIME                                                       
         BNE   VR32X                                                            
VR32B    MVC   1(2,R7),HALF                                                     
         LA    R7,3(R7)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,*+8                                                           
         B     VR32C                                                            
         CLI   1(R4),0                                                          
         BNE   VR32X                                                            
         BAS   RE,VALTIME                                                       
         BNE   VR32A                                                            
         LR    RE,R7                                                            
         SH    RE,=H'3'                                                         
         MVC   0(1,R7),0(RE)                                                    
         B     VR32B                                                            
*                                                                               
VR32C    ZIC   RE,DTLEN                                                         
         LA    RF,WORK                                                          
         SR    R7,RF                                                            
         CR    R7,RE                                                            
         BNH   *+6                                                              
         LR    RE,R7                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   DPTELEM+3(0),WORK                                                
         BE    VR32X                                                            
         MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
*                                                                               
VR32X    ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DPTNAME(0),8(R2)                                                 
*                                                                               
*                                  CHECK FOR TIME OVERLAPS                      
VR33     LA    R0,DPTNAME-3        R0 = A(LAST DAY)                             
         LA    RE,3                SET UP RE,RF FOR BXLE                        
         LA    RF,DPTNAME-1                                                     
         LA    R5,DPTDAY1          R5 = A(FIRST DAY)                            
*                                                                               
VR34     LA    R7,3(R5)            R7 = A(DAY FOR COMPARE)                      
*                                                                               
VR35     MVC   BYTE,0(R5)                                                       
         NC    BYTE,0(R7)          FIND COMMON BITS,IF ANY                      
         BNZ   VR38                YES - THEN AT LEAST 1 DAY IN COMMON          
VR36     BXLE  R7,RE,VR35          COMPARE AGAINST NEXT DAY                     
*                                                                               
         LA    R5,3(R5)            INCREMENT TO NEXT DAY                        
         CR    R5,R0               TEST COMPARED ALL DAYS YET                   
         BNL   VR39                YES - THEN NO OVERLAPS                       
         B     VR34                NO                                           
*                                                                               
VR38     CLC   1(1,R5),1(R7)       TEST FOR OVERLAP TIMES                       
         BNH   VR38A                                                            
         CLC   1(1,R5),2(R7)                                                    
         BNL   VR36                TIMES DO NO OVERLAP                          
         L     R2,SAVEDPL          DAYPART LIST HEADER ADDRESS                  
         MVI   ERROR,OVERLAP                                                    
         B     TRAPERR                                                          
*                                                                               
VR38A    CLC   2(1,R5),1(R7)                                                    
         BNH   VR36                TIMES DO NOT OVERLAP                         
         LA    R1,1(R1)            BUMP SEQUENCE#                               
         ST    R1,DUB                                                           
         L     R2,SAVEDPL                                                       
         MVI   ERROR,OVERLAP                                                    
         B     TRAPERR                                                          
*                                                                               
*                                                                               
VR39     GOTO1 ADDELEM                                                          
*                                                                               
VR40     BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST E-O-S                                   
         BE    VR45                                                             
         CLI   5(R2),0             TEST DATA                                    
         BNE   VR2                 YES - GO EDIT                                
         ST    R2,SAVEDPL                                                       
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             SHOULD BE NO NAME EITHER                     
         BE    VR40                                                             
         L     R2,SAVEDPL                                                       
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR45     B     DR                  DISPLAY THE RECORD                           
         EJECT                                                                  
* ROUTINE TO VALIDATE A DAY EXPRESSION                                          
* INPUT  : R4 = A(SCANNER BLOCK)                                                
* OUTPUT : CC EQ - OK AND BYTE = DAY CODE                                       
*          CC NE - INVALID                                                      
*                                                                               
VALDAY   NTR1                                                                   
         CLI   1(R4),0                                                          
         BNE   NEQXIT                                                           
         ZIC   R0,0(R4)                                                         
         MVI   BYTE,0                                                           
         GOTO1 DAYVAL,DMCB,((R0),12(R4)),BYTE,WORK                              
         CLI   BYTE,0                                                           
         BNE   VD30                NON-ZERO MEANS VALID INPUT                   
         SPACE 1                                                                
* CODE HERE TO HANDLE SPECIAL SITUATIONS                                        
         SPACE 1                                                                
         LA    R1,SPCLDAY                                                       
         LA    R0,SPCLDAYL                                                      
         SR    RE,RE                                                            
*                                                                               
VD10     IC    RE,1(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),2(R1)                                                   
         BNE   VD20                                                             
         MVC   BYTE,0(R1)          SET VALID INPUT                              
         B     VD30                                                             
*                                                                               
VD20     LA    R1,L'SPCLDAY(R1)                                                 
         BCT   R0,VD10                                                          
         B     NEQXIT                                                           
*                                                                               
SPCLDAY  DS    0CL7                                                             
         DC    X'03',X'02',CL5'S-S  '                                           
         DC    X'03',X'03',CL5'SA-S '                                           
         DC    X'03',X'03',CL5'S-SU '                                           
         DC    X'7F',X'02',CL5'M-S  '                                           
SPCLDAYL EQU   (*-SPCLDAY)/L'SPCLDAY                                            
*                                                                               
VD30     LA    R1,DAYLIST                                                       
         LA    R0,DAYLISTL                                                      
*                                                                               
VD40     CLC   BYTE,0(R1)                                                       
         BE    VDX                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,VD40                                                          
         B     NEQXIT                                                           
*                                                                               
DAYLIST  DC    X'7C'               MO-FR                                        
         DC    X'7E'               MO-SA                                        
         DC    X'7F'               MO-SU                                        
         DC    X'03'               SA-SU                                        
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
DAYLISTL EQU   *-DAYLIST                                                        
*                                                                               
VDX      B     EQXIT                                                            
         EJECT                                                                  
VALTIME  NTR1                                                                   
         ZIC   R0,0(R4)                                                         
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),FULL                                   
         CLI   0(R1),X'FF'                                                      
         BE    NEQXIT                                                           
*                                                                               
         MVI   ERROR,BADSTTIM                                                   
         LH    R0,FULL                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TIME MUST BE HOUR ONLY                       
         BNZ   EDTERR                                                           
         STC   R1,HALF             SET START HOUR                               
*                                                                               
         MVI   ERROR,BADNDTIM                                                   
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   EDTERR                                                           
         STC   R1,HALF+1           SET END HOUR                                 
         B     EQXIT                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       L     R6,AIO                                                           
         MVI   ELCODE,DSCELQ       DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DSCELEM,R6                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(L'DSCDESC),DSCDESC                                          
*                                                                               
         LA    R2,CDPDESCH                                                      
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE DESCRIPTION                             
         OI    6(R2),X'80'         XMIT                                         
         DROP  R6                                                               
*                                                                               
         LA    R2,CDPDTL1H         FIRST DAYPART LIST FIELD                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DPTELQ       DAYPART LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    DR12                                                             
         DC    H'0'                                                             
*                                                                               
DR10     BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         USING DPTELEM,R6                                                       
*                                                                               
         BAS   RE,BUMP                                                          
*                                                                               
DR12     ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DPTINPUT    MOVE DAY/TIME LIST INPUT                     
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,BUMP                                                          
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DPTNAME     MOVE NAME                                    
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
         B     DR10                                                             
*                                                                               
DR20     BAS   RE,BUMP             CLEAR TO END OF SCREEN                       
         BZ    XIT                                                              
*                                                                               
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,TWACLC                                                        
         BNH   DR20                                                             
         EX    RE,TWAXC                                                         
         OI    6(R2),X'80'                                                      
         B     DR20                                                             
*                                                                               
TWACLC   CLC   8(0,R2),SPACES                                                   
TWAXC    XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING CDPREC,R6                                                        
*                                                                               
         LA    R2,CDPNAMH                                                       
         MVC   CDPNAM,CDPKNAME                                                  
         OI    6(R2),X'80'         SET XMT                                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
*                                                                               
         MVC   KEY,SVKEY                                                        
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     DS    0H                  NEXT RECORD                                  
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),SVKEY        TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
*                                                                               
LR40     LA    R6,KEY                                                           
         USING CDPREC,R6                                                        
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   LISTAR(8),CDPKNAME                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DSCELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING DSCELEM,R6                                                       
         MVC   LISTAR+10(L'DSCDESC),DSCDESC                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*PRINT REPORT ROUTINE                                                           
*                                                                               
*                                                                               
*                                                                               
PR       LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R4,KEY              POINT TO KEY                                 
         USING CDPKEY,R4                                                        
*                                                                               
         MVI   CDPKTYPE,X'0D'                                                   
         MVI   CDPKSUB,X'5A'                                                    
         MVC   CDPKAM,BAGYMD                                                    
*                                                                               
         LA    R2,CDPTYPH                                                       
         XC    CDPKNAME,CDPKNAME                                                
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   CDPKNAME,8(R2)                                                   
         MVC   SVKEY(13),KEY                                                    
*                                                                               
PR10     GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(3),SVKEY                                                     
         BNE   PR110                                                            
         CLC   8(3,R2),=C'ALL'     WANT ALL RECORDS                             
         BE    PR40                                                             
         LLC   RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   PR20                                                             
         CLC   8(0,R2),KEY+3                                                    
*                                                                               
*       HAVE RECORD DESIRED--PRINT ROUTINE FOLLOWS                              
*                                                                               
PR40     MVI   RECFOUND,C'Y'                                                    
*                                                                               
         LA    R7,P1                                                            
         USING PRLINE,R7                                                        
*                                                                               
         MVC   PRNAME,CDPKNAME     ENTER NAME IN PRINT                          
*                                                                               
         L     R6,AIO                                                           
         USING DSCELEM,R6                                                       
         GOTO1 GETREC                                                           
         MVI   ELCODE,DSCELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NEED DESCRIPTION ELEMENT                     
         MVC   PRDESC,DSCDESC      ENTER DESCRIPTION IN PRINT                   
*                                                                               
PR50     L     R6,AIO                                                           
         MVI   ELCODE,DPTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    PR60                                                             
         DC    H'0'                MUST HAVE DAY/TIME ENTRY                     
*                                                                               
PR55     BAS   RE,NEXTEL                                                        
         BNE   PR70                                                             
         USING DPTELEM,R6                                                       
*                                                                               
PR60     MVC   PRDATI,DPTINPUT     DAY/TIME LISTINGS                            
         MVC   PRNAM,DPTNAME       LISTINGS NAME                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR55                                                             
*                                                                               
PR70     MVI   ALLOWLIN,2          DRAW LINE BETWEEN RECORDS                    
         OC    ABOX,ABOX                                                        
         BZ    PR80                                                             
         L     R5,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
         DROP  R7                                                               
PR80     B     PR20                GET NEXT RECORD                              
*                                                                               
PR110    CLI   RECFOUND,C'Y'                                                    
         BE    PRX                 REPORT HAS DATA IN IT                        
         MVI   HDHOOKOK,C'N'                                                    
         MVC   PRNAME(16),=C'NO RECORDS FOUND'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVI   H3,0                PRINT BLANK LINES                            
         MVI   H4,0                                                             
         MVI   H5,0                                                             
*                                                                               
         LA    R5,H6                                                            
         USING PRNAME,R5                                                        
         MVC   PRNAME+4(4),=C'NAME'           INSERT TABLE HEADLINES            
         MVC   PRDESC+12(11),=C'DESCRIPTION'                                    
         MVC   PRDATI+16(17),=C'DAY/TIME LISTINGS'                              
         MVC   PRNAM(16),=C'DAYPART OVERRRIDE'                                  
*                                                                               
         OC    ABOX,ABOX                                                        
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+2,C'L'                                                   
         MVI   BOXCOLS+12,C'C'                                                  
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+106,C'C'                                                 
         MVI   BOXCOLS+130,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOKX    B     XIT                                                              
*                                                                               
HEDSPECS SSPEC H1,2,C'MEDIA      SPOT RADIO'                                    
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,60,C'CUSTOM REPORT'                                           
         SSPEC H2,60,C'-------------'                                           
         SSPEC H1,102,RUN                                                       
         SSPEC H2,102,REPORT                                                    
         SSPEC H2,120,PAGE                                                      
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
TRAPERR  GOTO1 VGETERR                                                          
*                                                                               
EDTERR   EQU   TRAPERR                                                          
*                                                                               
SCANERR  MVI   ERROR,INVALID                                                    
         GOTO1 VCURSERR                                                         
*                                                                               
BUMP     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BR    RE                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESE0D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESD0D                                                       
         SPACE 1                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
SAVEDPL  DS    A                                                                
SEQNUM   DS    H                                                                
RECFOUND DS    C                                                                
HDHOOKOK DS    C                                                                
FIRSTIME DS    C                                                                
COUNTER  DS    CL1                                                              
DTLEN    DS    X                                                                
         EJECT                                                                  
CDPRECD  DSECT                                                                  
       ++INCLUDE SPGENCUSDP                                                     
*                                                                               
SPOOLD   DSECT                                                                  
*              PRINT DSECT FOR REPORT GENERATION                                
         ORG   P                                                                
PRLINE   DS    CL4                                                              
PRNAME   DS    CL8                                                              
         DS    CL2                                                              
PRDESC   DS    CL36                                                             
         DS    CL2                                                              
PRDATI   DS    CL57                                                             
         DS    CL2                                                              
PRNAM    DS    CL18                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPRES20   04/01/14'                                      
         END                                                                    
