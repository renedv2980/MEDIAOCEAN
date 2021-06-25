*          DATA SET NENAV01X   AT LEVEL 114 AS OF 05/01/02                      
*PHASE T31801A                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31801   TITLE 'NENAV01 - STEWARD/MATCHMAKER - INIT OVERLAY'                    
T31801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV01**,R8                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
* ROUTE THE ELEMENTS TO THE RIGHT ROUTINES                                      
         CLC   SVRCVEL,=X'0010'                                                 
         BE    ROUT020                                                          
         CLC   SVRCVEL,=X'0040'                                                 
         BE    ROUT220                                                          
         CLC   SVRCVEL,=X'0044'                                                 
         BE    ROUT320                                                          
         CLC   SVRCVEL,=X'0060'       WIZARD UNIT DATA                          
         BE    ROUT420                                                          
         SPACE 3                                                                
*                                                                               
*  INITIAL DOWNLOAD ROUTINES                                                    
*                                                                               
*--SEND DEMO, NAD PREFIX, DAY, LENGTH INFO                                      
ROUT020  GOTO1 VALIMED                                                          
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
ROUT220  BAS   RE,PRGSERCH                                                      
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
*  PROGRAM DETAILS ROUTINES                                                     
*                                                                               
*--GET AGENCY PROFILES                                                          
ROUT320  BAS   RE,PRGDET                                                        
         B     EXIT                                                             
         SPACE 3                                                                
*--GET WIZARD UNIT DATA                                                         
ROUT420  BAS   RE,WIZDATA                                                       
         B     EXIT                                                             
         SPACE 3                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*  ROUTINE BUILDS DEFAULT INFORMATION FOR STEWARD                               
*  IT PASSES NAD PREFIXES, DEMO AGE BREAKS, DAY, LENGTHS                        
PASDEF   NTR1                                                                   
         LHI   R1,X'16'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
*  PASS NAD PREFIXES                                                            
*                                                                               
         LA    R4,PFXTAB                                                        
         LA    R3,PREFIXES                                                      
*                                                                               
PASDF20  LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,1(R4)                                                         
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         LA    R4,7(R4)                                                         
         BCT   R3,PASDF20                                                       
         SPACE 2                                                                
*                                                                               
*  PASS DEMO AGE BREAKS                                                         
*                                                                               
         LHI   R1,X'12'                                                         
         BAS   RE,SENDH                                                         
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
PRGSR200 OC    SVSDATE(4),SVSDATE    DATE INPUTTED                              
         BZ    PRGSR280                                                         
         OC    SVEDATE,SVEDATE       JUST START DATE INPUTTED                   
         BZ    PRGSR260                                                         
         CLC   SVSDATE,NPGKEND      IS START DATE > PROGRAM DATE                
         BH    PRGSR120                                                         
         CLC   SVEDATE,NPGKEND      IS END DATE < PROGRAM DATE                  
         BL    PRGSR120                                                         
         B     PRGSR280                                                         
*                                                                               
PRGSR260 CLC   SVSDATE,NPGKEND      IS DATE = PROGRAM DATE                      
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
*                                                                               
*  OUTPUT THE RATING/SHARE                                                      
         LHI   R1,X'08'             SET AS A RATING                             
         TM    NPGSTAT,X'80'        IS VALUE A RATING                           
         BO    *+8                                                              
         LHI   R1,X'09'             SET AS A SHARE                              
         LA    R4,NPGSHARE                                                      
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
*                                                                               
*  ROUTINE READS A PROGRAM RECORD                                               
*                                                                               
PRGDET   NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R3,KEY                                                           
         USING NPGRECD,R3                                                       
         XC    KEY,KEY                                                          
         LA    R5,4                 LENGTH OF KEY COMPARE                       
         MVC   NPGKTYP,=XL2'0D20'                                               
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,SVMKT                                                    
         MVC   NPGKPROG,SVPRGCD                                                 
         MVC   NPGKEND,SVSDATE                                                  
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(11),KEYSAVE                                                  
         BNE   PRGDTEXT                                                         
*                                                                               
         L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'92',(R3)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NPGEL92,R6                                                       
*                                                                               
*  PASS THE RECORD TO THE PC                                                    
*                                                                               
         LHI   R1,X'46'                                                         
         BAS   RE,SENDH                                                         
*  OUTPUT THE DAY                                                               
         LA    R4,DAYTBL                                                        
PRGDT420 CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NPGRDAY,0(R4)                                                    
         BE    PRGDT460                                                         
         LA    R4,5(R4)                                                         
         B     PRGDT420                                                         
PRGDT460 LA    R4,1(R4)                                                         
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
*  OUTPUT THE NTI CODE                                                          
         LHI   R1,X'04'                                                         
         LA    R4,NPGPPNO                                                       
         BAS   RE,SENDD                                                         
*                                                                               
PRGDTEXT B     EXIT                                                             
         DROP  R3,R6                                                            
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
*  ROUTINE BUILDS DEFAULT WIZARD UNIT DATA                                      
*                                                                               
*  R2 -> WIZCLIENT TABLE                                                        
*  R4 -> WIZ CLIENT                                                             
*  R5 -> LENGTH OF DATA                                                         
*                                                                               
*                                                                               
*                                                                               
WIZDATA  NTR1                                                                   
         LHI   R1,X'62'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R2,SVWIZCLT               TABLE OF CLIENTS                       
         USING WIZCLTD,R2                                                       
         LA    R3,L'SVWIZCLT/LNWIZCLT    MAX NUMBER OF CLIENTS                  
WIZDATA5 MVC   CURRCLT,WIZBCLT           THIS CLIENT                            
         BAS   RE,WIZIT                  SEND CLT/PRD/EST                       
         LA    R2,LNWIZCLT(R2)           BUMP TO NEXT CLIENT                    
         CLI   0(R2),0                                                          
         BE    WIZDATAX                                                         
         LHI   R1,X'62'            SEND NEW HEADER WITH EACH CLIENT             
         BAS   RE,SENDH                                                         
         BCT   R3,WIZDATA5                                                      
WIZDATAX XIT1                                                                   
*                                                                               
                                                                                
                                                                                
* EXPECTS R2->SVWIZ CLIENT LIST                                                 
*                                                                               
WIZIT    NTR1                                                                   
* PASS CLIENT CODE                                                              
         LHI   R1,X'01'                                                         
         LA    R4,WIZCLT                                                        
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         BAS   RE,SENDD                                                         
* PASS CLIENT NAME                                                              
         LA    R5,20                                                            
         LA    R6,WIZCLTNM         POINT TO CLIENT NAME                         
         LA    R6,19(R6)           POINT R6 TO END OF CLIENT NAME               
WIZD20   CLI   0(R6),X'40'                                                      
         BH    WIZD30                                                           
         BCTR  R6,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNZ   WIZD20                                                           
         DC    H'0'                NO PRISONERS                                 
WIZD30   LHI   R1,X'02'                                                         
         LA    R4,WIZCLTNM                                                      
         BAS   RE,SENDD            R5 HAS LENGTH                                
* PASS OFFICE CODE                                                              
         LHI   R1,X'03'                                                         
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LA    R4,WIZCLTO                                                       
         BAS   RE,SENDD                                                         
* PASS N0,N1,N2 PROFILES                                                        
         XC    KEY,KEY             GET USER PROFILE INTO NBUSE                  
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),QAGY                                                    
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),WIZCLT                                                  
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),WIZCLTO                                                
         GOTO1 VGETPROF,DMCB,KEY,WORK2,VDATAMGR    N0 PROFILE                   
         MVI   KEY+3,C'1'                     GET N1 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+16,VDATAMGR                              
         MVI   KEY+3,C'2'                     GET N2 PROFILE                    
         GOTO1 VGETPROF,DMCB,KEY,WORK2+32,VDATAMGR                              
         LHI   R6,X'04'                                                         
         LA    R3,WORK2                                                         
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'05'                                                         
         LA    R3,WORK2+16                                                      
         BAS   RE,SENDPRF                                                       
         LHI   R6,X'06'                                                         
         LA    R3,WORK2+32                                                      
         BAS   RE,SENDPRF                                                       
         B     WIZD40                                                           
                                                                                
*                                                                               
* R3->PROFILE STRING                                                            
* R1->MAP CODE                                                                  
SENDPRF  NTR1                                                                   
         LA    R4,WORK             OUTPUT AREA                                  
         LA    R2,16               # OF PROFILES                                
SENDP10  LA    R5,1                                                             
         CLI   0(R3),X'40'         SEND BLANKS                                  
         BE    SENDP15                                                          
         CLI   0(R3),C'*'          SEND ASTERISK                                
         BE    SENDP15                                                          
         CLI   0(R3),C'A'          SEND ALPHAS                                  
         BNL   SENDP15                                                          
* ASSUME NUMERIC IF LESS THAN C'A'                                              
         EDIT  (B1,0(R3)),(2,WORK+20),ALIGN=LEFT,ZERO=NOBLANK                   
         MVC   WORK(3),WORK+20                                                  
         LR    R5,R0               R0 = # OF SIGNIFICANT CHARACTERS             
         B     SENDP20                                                          
*                                                                               
SENDP15  MVC   WORK(1),0(R3)                                                    
*                                                                               
SENDP20  LR    R1,R6             R1 GETS TRASHED BY SENDD                       
         BAS   RE,SENDD                                                         
         LA    R3,1(R3)                                                         
         BCT   R2,SENDP10                                                       
         XIT1                                                                   
                                                                                
* NOW DEAL WITH PRODUCTS                                                        
* EITHER PASS ONLY THOSE IN SVWIZPRD LIST OR ALL ON CLIENT RECORD               
*                                                                               
WIZD40   DS    0H                 GET CLIENT RECORD                             
         MVC   QCLT,WIZCLT        VALICLT NEEDS QCLT                            
         GOTO1 VALICLT            GETS CLT REC INOT SVCLTREC                    
         L     R3,AIO                                                           
         USING CLTHDR,R3                                                        
         CLC   QPRD,=C'POL'        ..IF POL                                     
         BE    WIZD44                                                           
         CLI   SVWIZPRD,0          ..IF BLANK                                   
         BNE   WIZD60                                                           
         MVC   QPRD,=C'POL'                                                     
*                                  ..PASS ALL PRODUCTS IN CLIST                 
WIZD44   LA    R6,CLIST                                                         
         LA    R3,220              MAX PRODUCTS                                 
WIZD45   MVC   QPRD,0(R6)          THIS PROD CODE                               
         BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                       
         LA    R4,WORK             CODE/NAME IN WORK/R5->LENGTH                 
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
         LA    R6,4(R6)            BUMP TO NEXT PROD                            
         CLI   0(R6),0             NO MORE PRODS?                               
         BE    WIZD70                                                           
         BCT   R3,WIZD45                                                        
         B     WIZD70                                                           
                                                                                
* STEP THROUGH PRODUCTS IN LIST AND SEND THOSE VALID FOR CLIENT                 
WIZD60   LA    R6,SVWIZPRD                                                      
         LA    R3,SVWZPRD#         NUMBER OF PRODS                              
WIZD65   BAS   RE,PRODMTCH         FIND MATCHING PRODUCT                        
         BNE   WIZD66              (RETURNS R4-> PROD)                          
         MVC   QPRD,0(R4)                                                       
         BAS   RE,FMTPROD          GET NAME/FORMAT OUTPUT                       
         LA    R4,WORK             WORK HAS CODE/NAME,R5->LENGTH                
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
WIZD66   LA    R6,3(R6)                                                         
         CLI   0(R6),0             EOF?                                         
         BE    WIZD70                                                           
         BCT   R3,WIZD65                                                        
         B     WIZD70              GO DO ESTIMATES                              
*                                                                               
FMTPROD  NTR1                    FORMAT PRODUCT CODE/NAME                       
         MVC   AIO,AIO2                                                         
         GOTO1 VALIPRD                                                          
         MVC   WORK(3),QPRD                                                     
         MVC   WORK+3(20),SVPNAME                                               
         LA    R5,23               DROP END BLANKS                              
         LA    R4,WORK                                                          
         LA    R4,22(R4)                                                        
FMTPRD5  CLI   0(R4),X'40'                                                      
         BH    FMTPRDX                                                          
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         CHI   R5,3                                                             
         BNE   FMTPRD5                                                          
FMTPRDX  XIT1  REGS=(R5)           PASS BACK LENGTH                             
                                                                                
*                                                                               
*                                                                               
* EXPECTS R6 -> TO PRODUCT TO BE MATCHED                                        
* EXPECTS CLIENT RECORD IN AIO1                                                 
PRODMTCH NTR1                                                                   
         LR    R3,RA                                                            
         AHI   R3,(SVCLTREC-TWAD)                                               
         USING CLTHDR,R3                                                        
         LA    R4,CLIST                                                         
         DROP  R3                                                               
         LA    R3,220                                                           
PRODM10  CLC   0(3,R4),0(R6)                                                    
         BNE   PRODM12                                                          
                                                                                
******************************************************                          
* ADD PRODUCT TO CURRPRDS LIST                                                  
         LA    R1,CURRPRDS         PRODS VALID FOR CURR CLIENT                  
PRODM11  CLI   0(R1),X'FF'                                                      
         BE    PRODM11B                                                         
         CLI   0(R1),0                                                          
         BE    PRODM11B                                                         
         LA    R1,3(R1)                                                         
         B     PRODM11                                                          
PRODM11B MVC   0(3,R1),0(R4)       ADD PROD TO LIST                             
         MVI   3(R1),X'FF'         SET EOF                                      
         SR    RE,RE               SET CC EQUAL                                 
         B     PRODMX                                                           
******************************************************                          
PRODM12  LA    R4,4(R4)                                                         
         BCT   R3,PRODM10                                                       
PRODMX   LTR   RE,RE                                                            
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
* NOW DEAL WITH ESTIMATES                                                       
* EITHER SEND ALL OR FROM REQUESTED LIST                                        
WIZD70   DS    0H                                                               
         MVI   WIZEST,0            CLEAR EST FLAG                               
         LA    R6,SVWIZEST         POINT TO EST TABLE                           
         CLI   0(R6),0             RETURN ALL ESTIMATES?                        
         BNE   WIZD76              NO                                           
         LA    R6,1                YES                                          
         LA    R7,255                                                           
WIZD75   STC   R6,CURREST                                                       
         BAS   RE,SENDEST                                                       
         LA    R6,1(R6)                                                         
         BCT   R7,WIZD75                                                        
         B     WIZD100                                                          
WIZD76   EQU   *                   USE TABLE OF REQUESTED EST                   
         LA    R7,20                                                            
WIZD77   MVC   CURREST,0(R6)       SET ESTIMATE                                 
         BAS   RE,SENDEST                                                       
         LA    R6,1(R6)            BUMP TO NEXT ESTIMATE                        
         CLI   0(R6),0                                                          
         BE    WIZD80                                                           
         BCT   R7,WIZD77                                                        
WIZD80   CLI   WIZEST,0            RETURN NO DATA IF NO MATCH                   
****     BNE   WIZD100                                                          
****     MVC   ERROR,=AL2(237)                                                  
****     GOTO1 SENDMSG                                                          
WIZD100  XIT1                                                                   
                                                                                
         EJECT                                                                  
*                                                                               
SENDEST  NTR1                                                                   
         CLC   QPRD,=C'POL'                                                     
         BNE   *+12                                                             
SENDE00  BAS   RE,SENDE04                                                       
         B     SENDEX                                                           
                                                                                
* NOT POL - CHECK THAT AT LEAST ONE PRODUCT SATISFIES                           
*           DATE RESTRICTIONS                                                   
         LA    R2,CURRPRDS                                                      
SENDE01  CLI   0(R2),X'FF'         EOF?                                         
         BE    SENDEX                                                           
         CLI   0(R2),0             EOF?                                         
         BE    SENDEX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         MVC   KEY+4(3),0(R2)      PRODUCT                                      
         MVC   KEY+7(1),CURREST    ESTIMATE                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SENDE02                                                          
         L     R3,AIO1                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         USING ESTHDR,R3                                                        
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SENDE00                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SENDE02                                                          
         CLC   SVSDATE,FULL+2                                                   
         BH    *+8                                                              
         B     SENDE00             OK- SEND DATA                                
SENDE02  LA    R2,3(R2)            BUMP TO NEXT PROD                            
         B     SENDE01                                                          
*                                                                               
*                                                                               
* SEND POL ESTIMATE DATA FOR CURRENT CLI/ESTIMATE                               
SENDE04  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),CURRCLT                                                 
         MVC   KEY+4(3),=C'POL'    PRODUCT                                      
         MVC   KEY+7(1),CURREST    ESTIMATE                                     
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SENDEX                                                           
         L     R3,AIO1                                                          
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO1                                    
         USING ESTHDR,R3                                                        
         CLI   SVSDATE,0           ANY DATE RESTRICTIONS?                       
         BE    SENDE05                                                          
         GOTO1 VDATCON,DMCB,ESTART,(2,FULL)                                     
         GOTO1 VDATCON,DMCB,EEND,(2,FULL+2)                                     
         CLC   SVEDATE,FULL        CHECK DATE RESTRICTIONS                      
         BL    SENDEX                                                           
         CLC   SVSDATE,FULL+2                                                   
         BH    SENDEX                                                           
*                                                                               
SENDE05  DS    0H                                                               
         MVI   WIZEST,1            SET EST FLAG                                 
         EDIT  (B1,EKEYEST),(3,WORK2),ALIGN=LEFT  SEND EST #                    
         LA    R5,3                                                             
         CLI   WORK2+2,X'40'       DROP BLANKS                                  
         BH    SENDE07                                                          
         BCTR  R5,0                                                             
         CLI   WORK2+1,X'40'                                                    
         BH    SENDE07                                                          
         BCTR  R5,0                                                             
SENDE07  LA    R4,WORK2                                                         
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R6,EDESC            SEND EST DESCRIPTION                         
         LA    R6,19(R6)                                                        
         LA    R5,20                                                            
SENDE10  CLI   0(R6),X'40'                                                      
         BH    SENDE20                                                          
         BCTR  R5,0                                                             
         BCTR  R6,0                                                             
         CHI   R5,X'01'                                                         
         BNE   SENDE10                                                          
SENDE20  LHI   R1,X'09'                                                         
         LA    R4,EDESC            SEND EST DESCRIPTION                         
         BAS   RE,SENDD                                                         
                                                                                
         LA    R2,WORK+20          SEND DEMO LIST                               
         XC    WORK,WORK                                                        
         MVC   0(L'EDEMLST,R2),EDEMLST    20 DEMOS                              
         LA    R2,L'EDEMLST(R2)           BUMP                                  
         MVC   0(3,R2),EDEM21             21ST DEMO                             
         LA    R2,WORK+20                 REPOSITION R2                         
*                                                                               
         LA    R4,BLOCK            INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         XC    WORK2(250),WORK2    OUTPUT FOR 11 CHAR DEMO NAMES                
                                                                                
         LA    R5,EUSRNMS                                                       
         GOTO1 VDEMOCON,DMCB,(21,(R2)),(13,WORK2),(C'S',DBLOCK),(R5)            
                                                                                
         LA    R2,WORK+20          R2->DEMO LIST INPUT                          
         LA    R6,WORK2            R6->DEMO DESCRIPTION OUTPUT                  
SENDE25  CLI   1(R2),0             LAST DEMO?                                   
         BNH   SENDE30                                                          
         CLI   2(R2),1             IF HOMES?                                    
         BE    SENDE27             SKIP SENDING                                 
         BAS   RE,FMTDEMO          FORMAT DESCRIPTION                           
         ZIC   R5,WORK             WORK=LENGTH                                  
         LA    R4,WORK+1           WORK+1=DEMO DESCRIPTION                      
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
SENDE27  LA    R2,3(R2)            NEXT DEMO IN BLOCK                           
         LA    R6,11(R6)           NEXT OUPUT DEMO DESCRIPTION                  
         B     SENDE25                                                          
                                                                                
SENDE30  GOTO1 VDATCON,DMCB,ESTART,(X'20',WORK)                                 
         GOTO1 VDATCON,DMCB,EEND,(X'20',WORK+6)                                 
         LA    R4,WORK                                                          
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   WORK,C'Y'           ESTIMATE LOCKED?                             
         TM    ECNTRL,X'08'                                                     
         BO    *+8                                                              
         MVI   WORK,C'N'                                                        
         LA    R4,WORK                                                          
         LHI   R1,X'0C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SENDEX   XIT1                                                                   
         DROP  R4                                                               
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
*         FMTDEMO                                                     *         
***********************************************************************         
* ROUTINE TO FORMAT DEMOS ... R6 POINTS TO 10 CHARACTER DESCRIPTION   *         
* ... R2 POINTS TO 3 BYTE DEMO ... WORK(1) RETURNS LENGTH ...         *         
* WORK+1 RETURNS DESCRIPTION                                          *         
***********************************************************************         
FMTDEMO  NTR1                                                                   
         USING ESTHDR,R3                                                        
         MVC   WORK(11),SPACES       INITIALIZE WORK                            
         MVI   WORK,0                                                           
         CLI   0(R6),C' '            IF NO DEMO TO FORMAT ... EXIT              
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,10(R6)                                                        
FMTD5    CLI   0(R4),C' '            SCAN BACKWARDS FOR NON-SPACE               
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK               LENGTH OF DEMO INTO WORK                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(R6)       DEMO DESCRIPTION INTO WORK+1               
*                                                                               
         CLI   1(R2),X'21'           IF DOING A USER DEMO, INSERT               
         BNE   FMTD20                USER DEMO HEADER                           
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
         MVC   WORK+4(7),WORK+11                                                
         ZIC   R0,2(R2)              USER NAME NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,3                                                             
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM                                                 
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1     IF DEMO MATCHES WEIGHTED DEMO              
         MVC   WORK+1(2),=C'W/'      INSERT HEADER                              
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,2                                                             
         STC   R1,WORK                                                          
FMTDEMOX XIT1                                                                   
         DROP  R3                                                               
*                                                                               
*                                                                               
                                                                                
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
DPTTAB   DS    0CL9                                                             
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
         DC    C'U',CL8'UNWIRED '                                               
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
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114NENAV01X  05/01/02'                                      
         END                                                                    
