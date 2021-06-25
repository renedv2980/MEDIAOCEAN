*          DATA SET NENVA02    AT LEVEL 002 AS OF 05/01/02                      
*PHASE T31802A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31802   TITLE 'NENAV01 - STEWARD/MATCHMAKER - INIT OVERLAY'                    
T31802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV01**                                                       
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0070'                                                 
         BE    ROUT020                                                          
         CLC   SVRCVEL,=X'0072'                                                 
         BE    ROUT040                                                          
         CLC   SVRCVEL,=X'0074'                                                 
         BE    ROUT060                                                          
         CLC   SVRCVEL,=X'0076'                                                 
         BE    ROUT080                                                          
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES                                                    
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT020  GOTO1 VALIMED                                                          
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
         BAS   RE,PASDEF                                                        
*--SEND DAYPART INFO                                                            
         BAS   RE,PASDYPT                                                       
*--SEND REASON CODE INFO                                                        
         BAS   RE,PASRSN                                                        
*--SEND NETWORK, MEDIA TYPE                                                     
         BAS   RE,PASNTWK                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  PROGRAM SEARCH ROUTINES                                                      
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT220  GOTO1 VALIMED              TEMPORARY                                   
         BAS   RE,PRGSERCH                                                      
         B     EXIT                                                             
         SPACE 3                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES NAD PREFIXES, DEMO AGE BREAKS, DAY, LENGTHS                        
PASDEF   NTR1                                                                   
         LHI   R1,X'12'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  PASS NAD PREFIXES                                                            
*                                                                               
         LA    R4,PFXTAB                                                        
         LA    R3,PREFIXES                                                      
*                                                                               
PASDF20  LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,7(R4)                                                         
         BCT   R3,PASDF20                                                       
         SPACE 2                                                                
*                                                                               
*  PASS DEMO AGE BREAKS                                                         
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2                                                             
         LA    R2,254                                                           
*                                                                               
PASDF60  MVC   0(2,RE),=X'00C8'                                                 
         STCM  RF,1,2(RE)                                                       
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,PASDF60                                                       
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         XC    WORK,WORK                                                        
         L     R2,AIO3                                                          
         LA    R3,1000(R2)                                                      
         MVC   0(7,R3),=CL7'HHOMES '                                            
         LA    R3,7(R3)                                                         
         SPACE                                                                  
         PRINT GEN                                                              
         GOTO1 VDEMOCON,DMCB,(254,(R2)),(2,(R3)),(C'S',DBLOCK),WORK             
         PRINT NOGEN                                                            
*                                                                               
*  PASS AGE BREAKS                                                              
*                                                                               
         L     R4,AIO3                                                          
         LA    R4,1001(R4)                                                      
         LA    R3,255                                                           
*                                                                               
PASDF100 LHI   R1,X'03'                                                         
         CLI   0(R4),C'*'           INVALID NAME                                
         BE    *+8                                                              
         BAS   RE,SENDD                                                         
         LA    R4,7(R4)                                                         
         BCT   R3,PASDF100                                                      
         SPACE 2                                                                
*                                                                               
*  PASS DAYS                                                                    
*                                                                               
         LA    R4,DAYTAB            DAY TABLE                                   
         LA    R3,DAYNUM                                                        
*                                                                               
PASDF140 LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,PASDF140                                                      
         SPACE 2                                                                
*                                                                               
*  PASS LENGTHS                                                                 
*                                                                               
         LA    R4,LENTAB            LENGTH TABLE                                
         LA    R3,LENNUM                                                        
*                                                                               
PASDF180 LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         BCT   R3,PASDF180                                                      
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES DAYPART TABLE                                                      
PASDYPT  NTR1                                                                   
         LHI   R1,X'13'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,DPTTAB           DAYPART TABLE                                
         LA    R3,DAYPARTS                                                      
PASDY20  LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         LHI   R1,X'02'                                                         
         LA    R4,1(R4)                                                         
         BAS   RE,SENDD                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,PASDY20                                                       
         B     EXIT                                                             
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES REASON CODES                                                       
PASRSN   NTR1                                                                   
*                                                                               
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING RSNRECD,R3                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=XL2'0D77'                                              
         MVC   RSNKAGY,QAGY                                                     
         MVI   RSNKMED,C'N'                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
         LHI   R1,X'14'                                                         
         BAS   RE,SENDH                                                         
         B     PASRS50                                                          
PASRS40  GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
         CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
PASRS50  GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
         L     R3,AIO                                                           
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,RSNKCODE                                                      
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,RSNTEXT                                                       
         BAS   RE,SENDD                                                         
         B     PASRS40                                                          
         DROP  R3                                                               
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES NETWORK CODES                                                      
PASNTWK  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO                                    
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EXIT                                                             
         LHI   R1,X'15'                                                         
         BAS   RE,SENDH                                                         
         B     PASNT50                                                          
PASNT40  GOTO1 AIOCALL,DMCB,STA+FIL+SEQ,AIO                                     
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
PASNT50  L     R3,AIO                                                           
         CLC   STAKAGY,QAGY                                                     
         BNE   PASNT40                                                          
         CLC   STAKCLT,=CL3'000'                                                
         BNE   PASNT40                                                          
*                                                                               
         LHI   R1,X'01'                                                         
         LA    R4,STAKCALL                                                      
         BAS   RE,SENDD                                                         
*                                                                               
         LHI   R1,X'02'                                                         
         LA    R4,STYPE                                                         
         BAS   RE,SENDD                                                         
         B     PASNT40                                                          
         EJECT                                                                  
*                                                                               
*  ROUTINE READS PROGRAM RECORDS USING FILTERS FROM THE X'40' ELEMENT           
*                                                                               
PRGSERCH NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
         MVC   NPGKTYP,=XL2'0DA0'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,SVMKT                                                    
         CLI   SVDAYFLT,C'Y'       WAS DAY TO BE FILTERED                       
         BNE   PRGSR100                                                         
         LA    R5,1(R5)                                                         
         MVC   NPGKDAY,SVDAY                                                    
*                                                                               
PRGSR100 STCM  R5,1,SVLENGTH                                                    
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         B     PRGSR160                                                         
PRGSR120 LA    R3,KEY                                                           
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
PRGSR160 ZIC   R5,SVLENGTH                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   PRGSREXT                                                         
*                                                                               
         OC    SVTIME,SVTIME        WAS TIME TO BE FILTERED                     
         BZ    PRGSR200                                                         
         MVC   FULL,NPGKTIME                                                    
         CLC   FULL,FULL+2          CHECK IF TIME CROSSED MIDNIGHT              
         BNH   PRGSR170                                                         
         LH    RE,FULL+2                                                        
         AH    RE,=H'2400'                                                      
         STH   RE,FULL+2                                                        
PRGSR170 CLC   SVTIME(2),FULL+2     IS START TIME > RECORD END TIME             
         BH    PRGSR120                                                         
         CLC   SVTIME+2(2),FULL     IS END TIME < RECORD START TIME             
         BL    PRGSR120                                                         
*                                                                               
PRGSR200 OC    SVPRSDAT(4),SVPRSDAT  DATE INPUTTED                              
         BZ    PRGSR280                                                         
         OC    SVPREDAT,SVPREDAT     JUST START DATE INPUTTED                   
         BZ    PRGSR260                                                         
         CLC   SVPRSDAT,NPGKEND     IS START DATE > PROGRAM DATE                
         BH    PRGSR120                                                         
         CLC   SVPREDAT,NPGKEND     IS END DATE < PROGRAM DATE                  
         BL    PRGSR120                                                         
         B     PRGSR280                                                         
*                                                                               
PRGSR260 CLC   SVPRSDAT,NPGKEND     IS DATE = PROGRAM DATE                      
         BNE   PRGSR120                                                         
*                                                                               
PRGSR280 L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*        CLC   NPGKPROG,=CL6'BCIT  '                                            
*        BNE   PRGSR120                                                         
*                                                                               
         CLI   SVPRNAD,X'40'        NAD CODE FILTER REQUEST                     
         BNH   PRGSR300                                                         
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'03',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PRGSR120                                                         
         L     R6,12(R1)                                                        
         USING NPGEL03,R6                                                       
         CLC   SVPRNAD,NPGNADDM                                                 
         BNE   PRGSR120                                                         
         DROP  R6                                                               
*                                                                               
PRGSR300 GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
         CLI   SVPRFILT,X'40'       PROGRAM FILTER FILTER REQUESTED             
         BNH   PRGSR320                                                         
         CLC   SVPRFILT,NPGFILT                                                 
         BNE   PRGSR120                                                         
*                                                                               
PRGSR320 OC    SVNTI,SVNTI          NTI CODE FILTER REQUESTED                   
         BZ    PRGSR400                                                         
         CLC   SVNTI,NPGPPNO                                                    
         BNE   PRGSR120                                                         
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
PRGSR400 LHI   R1,X'42'                                                         
         BAS   RE,SENDH                                                         
*  OUTPUT THE DAY                                                               
         LA    R4,DAYTBL                                                        
PRGSR420 CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NPGRDAY,0(R4)                                                    
         BE    PRGSR460                                                         
         LA    R4,5(R4)                                                         
         B     PRGSR420                                                         
PRGSR460 LA    R4,1(R4)                                                         
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE TIME                                                              
         XC    WORK,WORK                                                        
         GOTO1 VUNTIME,DMCB,NPGTIME,WORK                                        
         LHI   R1,X'02'                                                         
         LA    R4,WORK                                                          
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PROGRAM NAME                                                      
         LHI   R1,X'03'                                                         
         LA    R4,NPGNAME                                                       
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE PROGRAM CODE                                                      
         LHI   R1,X'04'                                                         
         LA    R4,NPGKPROG                                                      
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE END DATE                                                          
         LHI   R1,X'06'                                                         
         LA    R4,NPGKEND                                                       
         BAS   RE,SENDD                                                         
*                                                                               
*  OUTPUT THE NTI CODE                                                          
         LHI   R1,X'07'                                                         
         LA    R4,NPGPPNO                                                       
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
*  OUTPUT THE START DATE                                                        
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'93',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   PRGSR120                                                         
         L     R6,12(R1)                                                        
         USING NPGEL93,R6                                                       
         OC    NPG2STD,NPG2STD                                                  
         BZ    PRGSR120             NEXT RECORD                                 
         LHI   R1,X'05'                                                         
         LA    R4,NPG2STD                                                       
         BAS   RE,SENDD                                                         
         B     PRGSR120                                                         
*                                                                               
PRGSREXT B     EXIT                                                             
         DROP  R3,R6                                                            
*                                                                               
DAYTBL   DC    X'01',CL4'MON '                                                  
         DC    X'02',CL4'TUE '                                                  
         DC    X'03',CL4'WED '                                                  
         DC    X'04',CL4'THU '                                                  
         DC    X'05',CL4'FRI '                                                  
         DC    X'06',CL4'SAT '                                                  
         DC    X'07',CL4'SUN '                                                  
         DC    X'00',CL4'M-F '                                                  
         DC    X'08',CL4'M-SU'                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
         EJECT                                                                  
SPTFILE  DC    CL8'SPTFILE'                                                     
UNTFILE  DC    CL8'UNTFILE'                                                     
*                                                                               
*                                                                               
LENTAB   DS    0CL1                                                             
         DC    X'0F'                15 SEC                                      
         DC    X'1E'                30 SEC                                      
         DC    X'2D'                45 SEC                                      
         DC    X'3C'                60 SEC                                      
         DC    X'5A'                90 SEC                                      
         DC    X'78'                120 SEC                                     
         DC    X'F0'                240 SEC                                     
LENNUM   EQU   (*-LENTAB)/L'LENTAB                                              
*                                                                               
*                                                                               
DAYTAB   DS    0CL4                                                             
         DC    CL4'MON '                                                        
         DC    CL4'TUE '                                                        
         DC    CL4'WED '                                                        
         DC    CL4'THU '                                                        
         DC    CL4'FRI '                                                        
         DC    CL4'SAT '                                                        
         DC    CL4'SUN '                                                        
         DC    CL4'M-F '                                                        
         DC    CL4'M-SU'                                                        
DAYNUM   EQU   (*-DAYTAB)/L'DAYTAB                                              
         EJECT                                                                  
*  DAYPART TABLE                                                                
DPTTAB   DS    0CL10       BYTE 9 = LENGTH OF ENTRY PLUS 1                      
         DC    C'D',CL8'DAYTIME '                                               
         DC    C'F',CL8'FRINGE  '                                               
         DC    C'P',CL8'PRIME   '                                               
         DC    C'K',CL8'KIDS    '                                               
         DC    C'T',CL8'TEENS   '                                               
         DC    C'Y',CL8'YOUTH   '                                               
         DC    C'S',CL8'SPORTS  '                                               
         DC    C'N',CL8'NEWS    '                                               
         DC    C'E',CL8'EARLY   '                                               
         DC    C'L',CL8'LATE    '                                               
         DC    C'H',CL8'OTHER   '                                               
         DC    C'J',CL8'PROMO-ID'                                               
         DC    C'C',CL8'CABLE   '                                               
         DC    C'O',CL8'OLYMPICS'                                               
         DC    C'R',CL8'RADIO   '                                               
         DC    C'X',CL8'SYND.   '                                               
         DC    C'I',CL8'SPECIAL '                                               
         DC    C'V',CL8'OVERNITE'                                               
         DC    C'W',CL8'WKNDPM  '                                               
         DC    C'M',CL8'WKNDAM  '                                               
         DC    C'A',CL8'ACCESS  '                                               
         DC    C'B',CL8'CBLSPORT'                                               
         DC    C'Q',CL8'INTRACTV'                                               
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
SVBDELEM DS    CL67                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
*                                                                               
         DS    0D                                                               
BUYDATA1 DS    CL78                                                             
BUYDATA2 DS    CL78                                                             
BUYDATA3 DS    CL78                                                             
BUYDATA4 DS    CL78                                                             
BUYDATAL EQU   *-BUYDATA1                                                       
BUYDATAX EQU   *                                                                
         ORG                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NENVA02   05/01/02'                                      
         END                                                                    
