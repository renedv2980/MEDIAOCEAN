*          DATA SET NEMED63    AT LEVEL 010 AS OF 05/01/02                      
*          DATA SET NEMED63    AT LEVEL 048 AS OF 06/12/97                      
*PHASE T31E63A,+0                                                               
         TITLE '-   NETPAK PROGRAM LIST'                                        
T31E63   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PRPR**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ARGS PASSED FROM EDIT                        
         USING PROGCOM,R7                                                       
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         MVC   PRGFLTLN,PRGFILTH+5    SAVE SCREEN DATA                          
         MVC   PRGFLTSV,PRGFILT                                                 
         MVC   PRGSEQSV,PRGSEQ                                                  
         DROP  RA                                                               
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         MVC   BOXWIDTH,=F'165'                                                 
         L     RA,BOXAWIDE                                                      
         ST    RA,ADRWIDE                                                       
         USING WIDED,RA                                                         
         DROP  R1                                                               
*                                                                               
         CLI   MODE,PRINTREP       PXZ                                          
         BNE   XIT                 PXZ                                          
*                                  ANETWS2=WS                                   
*                                  ANETWS3/4=MKT/CALL LETTERS LIST              
         EJECT                                                                  
*              CONTROL I/O                                                      
         SPACE 3                                                                
         MVC   NETNAME,NBSELNET                                                 
         OC    PAKMKT,PAKMKT       IF MKT=ALL                                   
         BNZ   *+8                                                              
         BAS   RE,NETNAMES         GETS MKT/CALL LETTERS                        
         L     R1,ANETWS3                                                       
         ST    R1,APAKMKT                                                       
         MVI   USEIO,C'Y'          SET UP TO READ SPOT PROG REC                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         NETGO NVSETSPT,DMCB                                                    
         LA    R4,KEY                                                           
         USING NPGRECD,R4                                                       
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,NBACTAM                                                   
         OC    PAKMKT,PAKMKT       IF SPECIFIC MKT                              
         BZ    READ1                                                            
         MVC   NPGKNET,PAKMKT      PACKED MARKET NUMBER                         
READ1    CLI   PRGSEQSV,C'D'                                                    
         BNE   *+8                                                              
         MVI   NPGKTYP+1,X'A0'     OPTION TO SHOW DAY/TIME SEQUENCE             
         GOTO1 HIGH                                                             
         B     READ4                                                            
         SPACE 2                                                                
READ2    MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         SPACE 2                                                                
READ4    CLC   KEY(3),KEYSAVE                                                   
         BNE   XITPL                                                            
         OC    PAKMKT,PAKMKT       IF MKT=ALL                                   
         BZ    RD4A                SKIP MKT NUMBER CHECK                        
         CLC   KEY(5),KEYSAVE                                                   
         BNE   XITPL                                                            
RD4A     GOTO1 DATCON,DMCB,(2,NPGKEND),(0,DUB)                                  
         CLC   DUB(6),STRTFILT     DATE FILTERS                                 
         BL    READ2                                                            
         CLC   DUB(6),ENDFILT                                                   
         BH    READ2                                                            
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         CLI   DPTFILT,0           CHK DAYPART FILTER                           
         BE    READ4B                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   READ2                                                            
         USING NPGEL93,R6                                                       
         CLC   DPTFILT,NPG2DYP                                                  
         BNE   READ2                                                            
*                                                                               
READ4B   L     R6,AIO                                                           
         MVI   ELCODE,X'92'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R6                                                       
         CLI   DAYFILT,X'FF'       DAY FILTER                                   
         BE    READ6                                                            
         ZIC   R1,DAYFILT                                                       
         LA    R1,DAYLIST(R1)                                                   
         CLC   NPGDAY,0(R1)                                                     
         BNE   READ2                                                            
         SPACE 2                                                                
READ6    ZIC   R3,PRGFLTLN                                                      
         LTR   R3,R3                                                            
         BZ    READ12                                                           
         LA    R2,PRGFLTSV                                                      
         LA    R5,NPGFILT                                                       
         SPACE 2                                                                
READ8    CLI   0(R2),C'*'          FILTER FILTER                                
         BE    READ10                                                           
         CLI   0(R2),C' '                                                       
         BE    READ10                                                           
         CLC   0(1,R2),0(R5)                                                    
         BNE   READ2                                                            
         SPACE 2                                                                
READ10   LA    R2,1(R2)                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,READ8                                                         
         SPACE 2                                                                
READ12   BAS   RE,FORMAT                                                        
         B     READ2                                                            
         EJECT                                                                  
*              ROUTINE TO FORMAT A PROGRAM RECORD                               
         SPACE 3                                                                
FORMAT   NTR1                                                                   
                                                                                
         MVI   ELEM93,C'N'                                                      
         ZIC   R4,1(R6)            R6 - ADDRESS OF OLD PROGRAM ELEMENT          
         AR    R4,R6                                                            
         USING NPG2ELEM,R4                                                      
         CLI   0(R4),X'93'         TEST IF NEW PROGRAM ELEMENT PRESENT          
         BNE   *+8                                                              
         MVI   ELEM93,C'Y'                                                      
*                                                                               
         L     R3,AIO                                                           
         USING NPGKEY,R3                                                        
         MVC   XP(6),NPGKPROG       PROGRAM CODE                                
         CLI   ELEM93,C'Y'                                                      
         BNE   FORM0                                                            
         MVI   XP1+33,C'*'          ***TEMP***                                  
         GOTO1 DATCON,DMCB,(2,NPG2STD),(8,XP1+24) START DATE                    
FORM0    GOTO1 DATCON,DMCB,(2,NPGKEND),(8,XP2+24) END DATE                      
         DROP  R3                                                               
*                                                                               
         MVC   XP+7(16),NPGNAME     PROGRAM NAME                                
         GOTO1 UNDAY,DMCB,NPGDAY,XP2+7 DAY                                      
         DS    0H                                                               
         CLI   NPGROT,0                                                         
         BE    SKIPUND                                                          
         GOTO1 UNDAY,DMCB,NPGROT,XP3+7 DAY                                      
         DS    0H                                                               
SKIPUND  GOTO1 UNTIME,DMCB,NPGTIME,XP2+11 TIME                                  
         DS    0H                                                               
         EDIT  (2,NPGSHARE),(4,XP3+24),1 SHR OR RTG                             
         MVI   XP3+28,C'R'                                                      
         TM    NPGSTAT,X'80'                                                    
         BO    *+10                                                             
         MVC   XP3+26(4),SPACES                                                 
         OC    NPGCOST,NPGCOST                                                  
         BZ    FORM1                                                            
         EDIT  (4,NPGCOST),(8,XP4+24),ALIGN=LEFT,FLOAT=$ COST                   
         SPACE 2                                                                
FORM1    XC    FULL,FULL                                                        
         MVC   FULL+2(2),NPGPPNO                                                
         EDIT  (4,FULL),(5,XP3+32),ZERO=BLANK NTI CODE                          
         CLI   ELEM93,C'Y'                                                      
         BNE   *+10                                                             
         MVC   XP4+33(1),NPG2DYP    DAYPART                                     
         MVC   USERQSTR,STRTFILT                                                
         MVC   USERQEND,ENDFILT                                                 
         LA    R3,DISPTAB                                                       
         LA    R2,NPGVPHS                                                       
         CLI   ELEM93,C'Y'         ARE WE USING NEW PROGRAM ELEMENT             
         BNE   FORM2                                                            
         LA    R2,NPG2VPHS                                                      
         SPACE 2                                                                
FORM2    CLI   0(R3),X'FF'         TEST END-OF-TABLE                            
         BE    FORM3                                                            
         CLI   0(R3),X'FE'         TEST END-OF-TABLE FOR OLD ELEMENT            
         BNE   FORM4                                                            
         CLI   ELEM93,C'Y'         ARE WE USING NEW PROGRAM ELEMENT             
         BNE   FORM3                                                            
         LA    R3,2(R3)            DISPTAB (1ST ENTRY FOR NEW ELEMENT)          
         B     FORM4                                                            
         SPACE 2                                                                
FORM3    MVI   SPACING,2                                                        
         MVC   XP1+37(5),=C'WOMEN'                                              
         MVC   XP2+37(5),=C'  MEN'                                              
         MVC   XP3+37(5),=C'TOTAL'                                              
         BAS   RE,USERVPH          CHECK FOR ANY USER ELEMENTS                  
         OC    PAKMKT,PAKMKT       IF MKT=ALL                                   
         BNZ   FORM3B                                                           
         L     R1,ANETWS3          GET CALL LETTERS                             
         L     R3,AIO                                                           
         L     R0,=F'450'          MAX NUMBER OF STATIONS                       
FORM3A   CLC   3(2,R3),0(R1)                                                    
         BE    FORM3AA                                                          
         LA    R1,7(R1)                                                         
         OC    0(7,R1),0(R1)                                                    
         BZ    FORM3B                                                           
         BCT   R0,FORM3A                                                        
         XC    MKTSV,MKTSV         IF NO MATCH/MKT=0                            
         MVC   NETNAME,=C'XXXX'    AND STATNAME=XXXX                            
         B     FORM3B                                                           
FORM3AA  CLC   MKTSV,0(R1)                                                      
         BE    FORM3B                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   MKTSV,0(R1)                                                      
         MVC   NETNAME,2(R1)                                                    
FORM3B   GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   ELEM93,C'Y'         ARE WE USING NEW PROGRAM ELEMENT             
         BNE   XIT                 NO EXIT                                      
         B     XIT                                                              
*  ADD   W,MN,ALL45+    NO ROOM LEFT SO FUDGE IT                                
                                                                                
         LA    R5,XP+95                                                         
         LA    R2,NPG2VPHS         ASSUME ONLY NEW PROGRAMS                     
         LA    R2,92(R2)           BUMP TO W45+                                 
         OC    0(6,R2),0(R2)       IF NO VPHS                                   
         BZ    XIT                 DON'T PRINT DEMO                             
         MVC   0(4,R5),=C'W45+'                                                 
         EDIT  (2,0(R2)),(4,5(R5)),ALIGN=LEFT                                   
         LA    R5,10(R5)                                                        
         LA    R2,2(R2)                                                         
         MVC   0(4,R5),=C'M45+'                                                 
         EDIT  (2,0(R2)),(4,5(R5)),ALIGN=LEFT                                   
         LA    R5,10(R5)                                                        
         LA    R2,2(R2)                                                         
         MVC   0(5,R5),=C'AD45+'                                                
         EDIT  (2,0(R2)),(4,6(R5)),ALIGN=LEFT                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     AND A SPACE BEFORE NEXT STATION              
*                                                                               
         B     XIT                                                              
                                                                                
*                                                                               
FORM4    ZIC   R5,0(R3)            COMPUTE DISPLACEMENT                         
         LTR   R5,R5                                                            
         BZ    NEXTVPH             SKIP IF ZERO IN DISPTAB                      
         BCTR  R5,0                                                             
         MH    R5,=H'198'                                                       
         ZIC   R1,1(R3)                                                         
         BCTR  R1,R0                                                            
         LA    R5,XP(R5)                                                        
         AR    R5,R1                                                            
         CLI   ELEM93,C'Y'                                                      
         BNE   FORM5                                                            
         EDIT  (2,0(R2)),(4,0(R5))                                              
         B     NEXTVPH                                                          
FORM5    ZIC   RE,0(R2)            MULTIPLY NPGEL92 VPH'S BY 10                 
         LTR   RE,RE                                                            
         BZ    NEXTVPH                                                          
         MH    RE,=H'10'                                                        
         EDIT  (RE),(4,0(R5))                                                   
NEXTVPH  LA    R3,2(R3)            DISPTAB                                      
         LA    R2,1(R2)            NPGEL92                                      
         CLI   ELEM93,C'Y'                                                      
         BNE   FORM2                                                            
         LA    R2,1(R2)            NPGEL93 HAS AN ADDITIONAL BYTE               
         B     FORM2                                                            
         EJECT                                                                  
*              USER VPH DISPLAY                                                 
         SPACE 3                                                                
USERVPH  NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'C3'                                                     
         BAS   RE,GETEL                                                         
         LA    R2,XP4+43                                                        
         B     USERVPH4                                                         
         SPACE 1                                                                
USERVPH2 BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
USERVPH4 BNE   XIT                                                              
         USING UDEVPD,R6                                                        
         MVC   XP4+37(5),=C'USERS'                                              
         MVC   0(7,R2),UDEVPNAM                                                 
         MVI   7(R2),C'='                                                       
         EDIT  (2,UDEVPH),(3,8(R2)),ALIGN=LEFT                                  
         LA    R2,15(R2)                                                        
         B     USERVPH2                                                         
         EJECT                                                                  
*                                                                               
* SET UP TABLE OF PACKED MKT NUMBERS/CALL LETTERS                               
*                                                                               
NETNAMES NTR1                                                                   
         MVC   LKEY,=H'17'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         XC    FILENAME,FILENAME                                                
         XC    KEY,KEY                                                          
         USING STAREC,R4                                                        
         LA    R4,KEY                                                           
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         XC    STAKCALL,STAKCALL                                                
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT(8),=CL8'00000000'                                        
         GOTO1 HIGH                                                             
         L     R5,=F'450'          MAX NUMBER OF STATIONS IN TABLE              
         B     NN4                                                              
*                                                                               
NNSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
NN4      CLC   STAKTYPE(2),=C'SN'                                               
         BNE   NNX                                                              
         CLC   STAKAGY,NBSELAGY                                                 
         BNE   NNSEQ                                                            
         L     R4,AIO                                                           
         L     R2,ANETWS3                                                       
NN5      OC    0(7,R2),0(R2)                                                    
         BZ    NN6                                                              
         LA    R2,7(R2)                                                         
         BCT   R5,NN5              IF OVER MAX NUMBER OF STATIONS               
         B     NNX                 THEN EXIT                                    
NN6      PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,0(R2)                                                       
         MVC   2(5,R2),STAKCALL                                                 
         L     R5,=F'450'                                                       
         B     NNSEQ                                                            
NNX      XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLACEMENT TABLE FOR EDIT                                      
         SPACE 3                                                                
DISPTAB  DS    0H                                                               
         DC    AL1(1,045)          F18+                                         
         DC    AL1(2,045)          M ''                                         
         DC    AL1(3,045)          V ''                                         
         DC    AL1(1,050)          F18-34                                       
         DC    AL1(2,050)          M ''                                         
         DC    AL1(3,050)          V ''                                         
         DC    AL1(1,055)          F18-49                                       
         DC    AL1(2,055)          M ''                                         
         DC    AL1(3,055)          V ''                                         
         DC    AL1(1,070)          F25-49                                       
         DC    AL1(2,070)          M ''                                         
         DC    AL1(3,070)          V ''                                         
         DC    AL1(1,075)          F25-54                                       
         DC    AL1(2,075)          M ''                                         
         DC    AL1(3,075)          V ''                                         
         DC    AL1(0,000)          F55-64                                       
         DC    AL1(0,000)          M ''                                         
         DC    AL1(0,000)          V ''                                         
         DC    AL1(1,085)          F55+                                         
         DC    AL1(2,085)          M ''                                         
         DC    AL1(3,085)          V ''                                         
         DC    AL1(1,090)          F12-17                                       
         DC    AL1(2,090)          M ''                                         
         DC    AL1(3,090)          V ''                                         
         DC    AL1(0,000)          K2-5                                         
         DC    AL1(3,100)          K6-11                                        
         DC    AL1(3,105)          K2-11                                        
         DC    AL1(0,000)          LOH                                          
         DC    AL1(1,120)          WW                                           
         DC    AL1(1,115)          V2+                                          
         DC    AL1(1,080)          F35-64                                       
         DC    AL1(2,080)          M ''                                         
         DC    AL1(3,080)          V ''                                         
         DC    X'FEFE'             (OLD PROGRAM ELEMENT ENDS HERE)              
         DC    AL1(1,105)          F2-11                                        
         DC    AL1(2,105)          M ''                                         
         DC    AL1(1,100)          F6-11                                        
         DC    AL1(2,100)          M ''                                         
         DC    AL1(1,095)          F15-24                                       
         DC    AL1(2,095)          M ''                                         
         DC    AL1(3,095)          V ''                                         
         DC    AL1(3,125)          HWC18                                        
         DC    AL1(2,125)          HWC12                                        
         DC    AL1(1,125)          HWC06                                        
         DC    AL1(1,065)          F21-49                                       
         DC    AL1(2,065)          M ''                                         
         DC    AL1(3,065)          V ''                                         
         DC    AL1(1,135)          F45+                                         
         DC    AL1(2,135)          M45+                                         
         DC    AL1(3,135)          V45+                                         
         DC    AL1(1,060)          F21+                                         
         DC    AL1(2,060)          M ''                                         
         DC    AL1(3,060)          V ''                                         
         DC    AL1(2,115)          V9-11                                        
         DC    AL1(3,130)          C9-14                                        
         DC    AL1(0,000)          (SPARE)                                      
         DC    AL1(2,120)          WW18-49                                      
         DC    AL1(3,120)          WW25-54                                      
         DC    AL1(3,115)          MOMS                                         
         DC    AL1(0,000)          (SPARE)                                      
         DC    X'FFFF'                                                          
         SPACE 3                                                                
DISPTABX DS    0H                  OLD TABLE                                    
         DC    AL1(1,60)                                                        
         DC    AL1(2,60)                                                        
         DC    AL1(3,60)                                                        
         DC    AL1(1,64)                                                        
         DC    AL1(2,64)                                                        
         DC    AL1(3,64)                                                        
         DC    AL1(1,68)                                                        
         DC    AL1(2,68)                                                        
         DC    AL1(3,68)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(1,72)                                                        
         DC    AL1(2,72)                                                        
         DC    AL1(3,72)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(0,00)                                                        
         DC    AL1(1,84)                                                        
         DC    AL1(2,84)                                                        
         DC    AL1(3,84)                                                        
         DC    AL1(1,88)                                                        
         DC    AL1(2,88)                                                        
         DC    AL1(3,88)                                                        
         DC    AL1(1,97)                                                        
         DC    AL1(2,97)                                                        
         DC    AL1(3,97)                                                        
         DC    AL1(1,105)                                                       
         DC    AL1(2,105)                                                       
         DC    AL1(3,105)                                                       
         DC    AL1(1,76)                                                        
         DC    AL1(2,76)                                                        
         DC    AL1(3,76)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
DAYLIST  DC    X'7C'               M-F                                          
         DC    X'40'               MON                                          
         DC    X'20'               TUE                                          
         DC    X'10'               WED                                          
         DC    X'08'               THU                                          
         DC    X'04'               FRI                                          
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
         DC    X'7F'               M-S                                          
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   XHEAD4+10(4),NETNAME                                             
         B     XIT                                                              
*                                                                               
*                                                                               
XITPL    XC    FILENAME,FILENAME   RESTORE                                      
         NETGO NVSETUNT,DMCB       RESET TO READ UNITFILE                       
         MVI   USEIO,C'N'                                                       
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
*                                                                               
ELEM93   DS    C                   Y=NEW PROGRAM ELEMENT PRESENT                
         DS    0H                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENPROGA                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUSER                                                      
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE3D                                                       
*                                                                               
         EJECT                                                                  
PROGCOM  DSECT                                                                  
**  PASSED FROM EDIT                                                            
PAKMKT   DS    CL2                                                              
STRTFILT DS    CL6                                                              
ENDFILT  DS    CL6                                                              
DAYFILT  DS    CL1                                                              
DPTFILT  DS    CL1                                                              
APAKMKT  DS    F                                                                
MKTSV    DS    CL2                                                              
NETNAME  DS    CL4                                                              
*                                                                               
ADRWIDE  DS    A                                                                
PRGFLTLN DS    CL1      *          SAVE SCREEN DATA SO I CAN USE RA             
PRGFLTSV DS    CL4      *                                                       
PRGSEQSV DS    CL1      *                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NEMED63   05/01/02'                                      
         END                                                                    
