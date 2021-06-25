*          DATA SET SPBUY03    AT LEVEL 086 AS OF 05/16/13                      
*PHASE T21103C                                                                  
         TITLE 'T21103 - SPOTPAK BUY - RECALL'                                  
*&&ONLIN SET   Y                  ONLINE-ONLY MODULE                            
T21103   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21103                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21103+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         MVI   RCLOPT,0                                                         
         XC    STDTP,STDTP                                                      
         XC    SVSPLMKT,SVSPLMKT   INIT AGENCY MKT CODE FOR SPILL               
         XC    SVSPLALP,SVSPLALP   INIT ALPHA  MKT CODE FOR SPILL               
         XC    STSPLMKT,STSPLMKT                                                
*                                                                               
         LA    R4,8(R2)            ON ENTRY R2 HAS FLDHDR ADDRESS               
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',='                                                 
*                                                                               
         CLI   PFKEY,0             TEST PFKEY THIS TIME                         
         BE    RCLX                NO                                           
         CLI   SVRCLOPT,RCLPAY     TEST LAST WAS NP                             
         BE    RCLR                                                             
         CLI   SVRCLOPT,RCLROT     TEST LAST WAS NR                             
         BE    RCLR                                                             
         CLI   SVRCLOPT,RCLPAYDT   TEST LAST WAS NX                             
         BE    RCLR                                                             
         CLI   SVRCLOPT,RCLHIST    TEST LAST WAS HIST                           
         BE    RCLR                                                             
         CLI   SVRCLOPT,C'N'       TEST LAST WAS NN                             
         BE    RCLN                                                             
         CLI   SVRCLOPT,C'S'       TEST LAST WAS SORT                           
         BE    RCLS                                                             
         MVI   PFKEY,0             <==== IGNORE INVALID PFKEY                   
         B     RCLX                                                             
*                                                                               
RCLS     BRAS  RE,BRSORT                                                        
         B     BR202                                                            
*                                                                               
RCLR     LA    R5,4                SET LENGTH SO FAR                            
*                                                                               
         XC    ELEM,ELEM                                                        
         SR    R0,R0                                                            
         ICM   R0,3,SVKEY+11       GET BUY LINE NUMBER                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM(3),DUB                                                      
*                                                                               
         MVI   ELEM+3,C'R'                                                      
         CLI   SVRCLOPT,RCLROT                                                  
         BE    RCLR2                                                            
*                                                                               
         MVI   ELEM+3,C'P'                                                      
         CLI   SVRCLOPT,RCLPAY                                                  
         BE    RCLR2                                                            
*                                                                               
         MVI   ELEM+3,C'X'                                                      
         CLI   SVRCLOPT,RCLPAYDT                                                
         BE    RCLR2                                                            
*                                                                               
         MVI   ELEM+3,C'H'                                                      
         CLI   SVRCLOPT,RCLHIST                                                 
         BE    RCLR2                                                            
         DC    H'0'                                                             
*                                                                               
RCLR2    CLI   PFKEY,6             TEST TOP                                     
         BNE   *+10                                                             
         XC    SVRCLEND,SVRCLEND                                                
*                                                                               
         OC    SVRCLEND(2),SVRCLEND  TEST SAVED LAST DATE                       
         BZ    RCLR4                 NO                                         
         GOTO1 VDATCON,DMCB,(2,SVRCLEND),(4,ELEM+4)                             
         LA    R5,9                SET CURRENT OUTPUT LENGTH                    
*                                                                               
         CLI   SVRCLEND+2,1                                                     
         BNH   RCLR4                                                            
         MVI   ELEM+9,C'-'                                                      
         ZIC   R0,SVRCLEND+2                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+10(2),DUB                                                   
         LA    R5,12                                                            
*                                                                               
RCLR4    MVC   8(78,R2),ELEM       MOVE TO BUY INPUT AREA                       
         STC   R5,5(R2)            SET  INPUT LENGTH                            
         MVI   4(R2),0             RESET VALIDITY BITS                          
         B     RCLX                                                             
*                                                                               
* PF6=TOP, 8=FORWARD                                                            
RCLN     CLI   PFKEY,6             TEST TOP                                     
         BL    RCLX                                                             
         BH    *+10                                                             
         MVC   SVRCLEND(2),=H'1'   SET TO START AT LINE 1                       
         CLI   PFKEY,8             TEST SCROLL FORWARDS                         
         BH    RCLX                                                             
* SAVE OLD INPUT LINE FLDHDR AND DATA                                           
         XC    ELEM,ELEM                                                        
         SR    R0,R0                                                            
         ICM   R0,3,SVRCLEND       GERT LAST LINE DISPLAYED                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM(3),DUB                                                      
         MVI   ELEM+3,C'N'                                                      
* NOW NEED TO FIND ANY FILTERS IN OLD INPUT AREA                                
         LA    R4,8(R2)                                                         
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)          GET OLD INPUT LENGTH                         
         BZ    RCLN6                                                            
RCLN2    CLI   0(R4),C','          TEST SEPARATOR                               
         BE    RCLN4                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,RCLN2                                                         
         B     RCLN6               NO FILTERS ENTERED                           
*                                                                               
RCLN4    MVI   ELEM+4,C','         ADD A NEW SEPARATOR                          
         BCTR  R5,0                SUBTRACT 1 FOR ,                             
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,*+8              MOVE FILTERS                                 
         B     *+10                                                             
         MVC   ELEM+5(0),1(R4)     MOVE FILTER VALUES                           
         LA    R5,2(R5)            ADD 1 FOR EX AND 1 FOR COMMA                 
*                                                                               
RCLN6    MVC   8(78,R2),ELEM       MOVE TO BUY INPUT AREA                       
         LA    R5,4(R5)            ADD LENGTH OF MY DATA                        
         STC   R5,5(R2)            SET AS INPUT LENGTH                          
         MVI   4(R2),0             RESET VALIDITY BITS                          
*                                                                               
RCLX     XC    SVRCLEND,SVRCLEND   CLEAR SAVED RECALL DATA                      
         MVI   SVDSPMOD,0          CLEAR DISPLAY MODE                           
         CLI   8(R2),C'0'          TEST NUMERIC INPUT                           
         BNL   BR5                 YES - USE NEW EDIT ROUTINES                  
*                                                                               
         CLI   SVRCLOPT,RCLNLST    DISPLAY NETWORK LIST                         
         BNE   BR0                                                              
         BRAS  RE,BRNL                                                          
         BNE   BUYERR                                                           
         B     EXIT                                                             
*                                                                               
BR0      XC    SVKEY+14(4),SVKEY+14  NO VALID RECORD NOW                        
         MVI   ERRCD,BADLINE                                                    
         GOTO1 FLDVAL              GET 'LI='                                    
         CLC   =C'LI=',0(R4)                                                    
         BE    BR2                                                              
         CLI   PFKEY,0             IF PFKEY=0, ERROR IS BAD LINE                
         BE    BRBADK2                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(BADPFKEY)                                            
         B     BRBADK2                                                          
*                                                                               
BR2      MVI   FSTOPS+1,C'+'                                                    
         GOTO1 FLDVAL              GET LINE NUMBER                              
*                                                                               
         CLI   FLEN+1,1                                                         
         BNE   BR3                                                              
         CLI   0(R4),C'*'                                                       
         BNE   BR3                                                              
         OC    PRVDSPLN,PRVDSPLN                                                
         BZ    *+10                                                             
         MVC   SVLIN,PRVDSPLN                                                   
         OC    SVLIN,SVLIN                                                      
         BZ    BRBADKEY                                                         
         B     BR4                                                              
         EJECT                                                                  
BR3      LTR   R5,R5                                                            
         BZ    BRBADKEY                                                         
         TM    FVAL,X'08'          VALID NUMERIC                                
         BZ    BRBADKEY                                                         
         CHI   R5,4                                                             
         BH    BRBADKEY                                                         
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BRBADKEY                                                         
         CHI   R0,MAXBUYS+1         500 MAX BUYS TO ALLOW THEM TO SEE           
         BH    BRBADKEY                                                         
         CLI   SV1OR2,2                                                         
         BE    *+12                                                             
         CHI   R0,255                                                           
         BH    BRBADKEY                                                         
         STCM  R0,3,SVLIN                                                       
         OC    PRVDSPLN,PRVDSPLN                                                
         BZ    BR4                                                              
         CLC   SVLIN,PRVDSPLN                                                   
         BNE   BRBADKEY                                                         
*                                                                               
BR4      CLI   FSTOP,C'+'                                                       
         BE    BR10                                                             
         B     BR40                                                             
         EJECT                                                                  
* SPECIAL EDITS FOR SHORT INPUT FORMAT --  14R = LI=14+R                        
*                                                                               
BR5      XC    SVKEY+14(4),SVKEY+14  NO VALID RECORD NOW                        
         SR    R5,R5               CLEAR LEN COUNTER                            
         LA    R4,8(R2)                                                         
BR6      CLI   0(R4),C'0'                                                       
         BL    BR7                                                              
         CLI   0(R4),C'9'                                                       
         BH    BR7                                                              
         LA    R4,1(R4)                                                         
         BCT   R5,BR6                                                           
BR7      LPR   R5,R5               R5 NOW HAS LEN                               
         CHI   R5,4                                                             
         BH    BRBADKEY                                                         
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BRBADKEY                                                         
         CHI   R0,MAXBUYS+1         500 MAX BUYS TO ALLOW THEM TO SEE           
         BH    BRBADKEY                                                         
         CLI   SV1OR2,2                                                         
         BE    *+12                                                             
         CHI   R0,255                                                           
         BH    BRBADKEY                                                         
         STCM  R0,3,SVLIN                                                       
         OC    PRVDSPLN,PRVDSPLN                                                
         BZ    *+14                                                             
         CLC   SVLIN,PRVDSPLN                                                   
         BNE   BRBADKEY                                                         
*                                                                               
         LA    R4,8(R5,R2)         POINT TO NEXT INPUT CHAR                     
         CLI   0(R4),C'+'                                                       
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         ST    R4,FADDR                                                         
         ZIC   R0,5(R2)                                                         
         CR    R0,R5               TEST ALL INPUT PROCESSED                     
         BE    BR40                                                             
         EJECT                                                                  
* EDIT RECALL OPTION (CHAR FOLLOWING '+')                                       
*                                                                               
BR10     XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',-'                                                 
         GOTO1 FLDVAL                                                           
*                                                                               
         MVI   ERRCD,BADOPTN                                                    
         LTR   R5,R5                                                            
         BZ    BRBADK2                                                          
*                                                                               
         CLI   FLEN+1,1                                                         
         BNE   BR10A                                                            
         MVI   RCLOPT,C'N'                                                      
         CLI   0(R4),C'N'          +N                                           
         BE    BR100                                                            
*                                                                               
BR10A    CLC   =C'PKG',0(R4)                                                    
         BE    BR300                                                            
*                                                                               
         MVI   RCLOPT,5                                                         
         CLC   =C'REF',0(R4)                                                    
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,9                                                         
         CLC   =C'ORB',0(R4)                                                    
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,RCLXCH                                                    
         CLC   =C'XCH',0(R4)                                                    
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,RCLZCUT                                                   
         OI    RCLOPT,X'80'                                                     
         CLC   =C'ZCUT',0(R4)                                                   
         BNE   BR10B                                                            
         LA    R4,3(R4)            SIMULATE 1 CHAR INPUT                        
         AHI   R5,-3                                                            
         B     BR12                                                             
*                                                                               
BR10B    MVI   RCLOPT,RCLCUT                                                    
         OI    RCLOPT,X'80'                                                     
         CLC   =C'CUT',0(R4)                                                    
         BE    BR10C                                                            
*                                                                               
         MVI   RCLOPT,RCLRSVP                                                   
         OI    RCLOPT,X'80'                                                     
         CLC   =C'RSV',0(R4)                                                    
         BNE   BR11                                                             
BR10C    LA    R4,2(R4)            SIMULATE 1 CHAR INPUT                        
         AHI   R5,-2                                                            
         B     BR12                                                             
*                                                                               
BR11     DS    0H                                                               
         CHI   R5,2                TEST AT LEAST 2 CHARS                        
         BL    BR11B                                                            
         MVC   BUTRCODE(2),0(R4)   SAVE FIRST 2 CHARS OF INPUT                  
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         EX    RE,TESTMS                                                        
         BE    BR11A                                                            
         EX    RE,TESTSORT                                                      
         BNE   BR11B                                                            
*                                                                               
BR11A    MVI   RCLOPT,C'S'                                                      
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOTFOR2)                                               
         CLI   SV1OR2,2                                                         
         BE    BUYERR                                                           
         BRAS  RE,BRSORT                                                        
         B     BR202                                                            
*                                                                               
TESTMS   CLC   0(0,R4),=C'MS'      MULTILINE SKED                               
TESTSORT CLC   0(0,R4),=C'SORT'    SORT                                         
*                                                                               
BR11B    MVI   RCLOPT,X'81'                                                     
         CLI   0(R4),C'R'          +R                                           
         BE    BR12                                                             
         MVI   RCLOPT,RCLPDEMX                                                  
         CLC   0(3,R4),=C'PDX'     +PDX                                         
         BE    BR38                                                             
         MVI   RCLOPT,RCLPDEM                                                   
         CLC   0(2,R4),=C'PD'      +PD                                          
         BE    BR38                                                             
         MVI   RCLOPT,X'82'                                                     
         CLI   0(R4),C'P'          +P                                           
         BE    BR12                                                             
         MVI   RCLOPT,X'83'                                                     
         CLI   0(R4),C'X'          +X                                           
         BE    BR12                                                             
         MVI   RCLOPT,X'84'                                                     
         CLI   0(R4),C'I'          +I                                           
         BE    BR12                                                             
         MVI   RCLOPT,6                                                         
         CLI   0(R4),C'C'          +C                                           
         BE    BR12                                                             
         MVI   RCLOPT,RCLDT        CNNET DAYS/TIMES                             
         CLC   0(2,R4),=C'DT'                                                   
         BE    BR12                                                             
         CLC   =C'SDEC',0(R4)      TRY TO ALLOW DEC SCHED INPUT                 
         BE    BR11C                                                            
         MVI   RCLOPT,RCLDSK       SCHEDULE DAYS                                
         CLC   0(2,R4),=C'SD'                                                   
         BE    BR12                                                             
*                                                                               
BR11C    MVI   RCLOPT,RCLDEMS                                                   
         CLC   0(2,R4),=C'DS'      +DS                                          
         BE    BR12                                                             
         MVI   RCLOPT,RCLDEM                                                    
         CLI   0(R4),C'D'          +D                                           
         BE    BR38                                                             
         MVI   RCLOPT,RCLACTV                                                   
         CLI   0(R4),C'A'          +A                                           
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,RCLHIST                                                   
         CLI   0(R4),C'H'          +H                                           
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,RCLFLM                                                    
         OI    RCLOPT,X'80'        SET DATE ALLOWED FLAG                        
         CLI   SVPRD,X'FF'                                                      
         BNE   *+12                                                             
         CLI   0(R4),C'F'                                                       
         BE    BR12                                                             
         MVI   RCLOPT,RCLSPILL                                                  
         CLC   0(2,R4),=C'SP'      SPILL MARKETS                                
         BE    BR12                                                             
         MVI   RCLOPT,RCLSTA                                                    
         CLC   0(2,R4),=C'ST'                                                   
         BNE   BR11D                                                            
         XC    SVRCLSTA,SVRCLSTA                                                
         CLI   2(R4),C'/'          TEST STATION PRESENT                         
         BNE   BR12                                                             
         MVC   SVRCLSTA,3(R4)                                                   
         OC    SVRCLSTA,SPACES                                                  
         B     BR12                                                             
*                                                                               
BR11D    MVI   RCLOPT,RCLSCH                                                    
         OI    RCLOPT,X'80'                                                     
         CLI   0(R4),C'S'          SCHEDULE                                     
         BE    BR12                                                             
*                                                                               
         MVI   RCLOPT,0                                                         
*                                                                               
BR12     TM    RCLOPT,X'80'        TEST DATE PERMITTED                          
         BZ    BR40                                                             
*                                                                               
BR12A    MVC   STDTB(6),SVSTARTB   SAVE DEFAULT DATES                           
         CHI   R5,1                TEST ANY MORE DATA                           
         BE    BR40                NO - MUST BE RECALL CHAR ONLY                
*                                                                               
* EDIT FOR MMM OR MMMDD                                                         
BR14     CHI   R5,4                TEST RCL CHAR + 3 MONTH CHARS                
         BE    BR16                                                             
* EDIT MMMDD                                                                    
         GOTO1 VDATVAL,DMCB,(1,1(R4)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   BR14A                                                            
         CLI   RCLOPT,RCLCUT+X'80' TEST CUT-IN DATA                             
         BE    BR30                YES - STATION MAY FOLLOW                     
         CLI   RCLOPT,RCLZCUT+X'80' TEST CUT-IN DATA                            
         BE    BR30                YES - STATION MAY FOLLOW                     
BR14A    MVI   BYTE,C'S'           INDICATE START DATE ONLY                     
*                                                                               
         CLI   FSTOP,C'-'          TEST END DATE TOO                            
         BNE   BR20                NO                                           
* EDIT END DATE OR SPOT NUMBER                                                  
         XC    FSTOPS,FSTOPS                                                    
         MVC   FSTOPS(2),=C',/'                                                 
         GOTO1 FLDVAL                                                           
         CLI   SVPRD,X'FF'         TEST POL                                     
         BNE   BR15                                                             
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BR15                                                             
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BRBADKEY                                                         
         CHI   R0,99                                                            
         BH    BRBADKEY                                                         
         STC   R0,BUELEMNO         SAVE SPOT NUMBER FOR DSPLY                   
         B     BR20                                                             
*                                                                               
BR15     GOTO1 VDATVAL,DMCB,(1,(R4)),WORK+6                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    BRBADKEY                                                         
         MVI   BYTE,C'E'                                                        
         B     BR20                                                             
*                                                                               
* EDIT MONTH ENTRY ONLY                                                         
*                                                                               
BR16     MVC   DUB(3),1(R4)                                                     
         MVC   DUB+3(2),=C'15'                                                  
         GOTO1 VDATVAL,DMCB,(1,DUB),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BRBADKEY                                                         
         MVI   BYTE,C'M'           INDICATE MONTH REQ                           
*                                                                               
* COMPLETE DATES WITH YEAR                                                      
*                                                                               
BR20     GOTO1 VDATCON,DMCB,WORK,(3,STDTB)   GET 3 BYTE DATE                    
*                                                                               
         MVC   STDTB(1),SVSTARTB   USE EST START YEAR                           
         MVC   WORK(2),SVSTART                                                  
         CLC   SVSTARTB(1),SVENDB  EST ALL IN ONE YEAR                          
         BE    BR22                YES                                          
         CLC   STDTB+1(2),SVSTARTB+1  TEST RCL MMDD AFTER ESTART                
         BNL   BR22                YES                                          
         MVC   STDTB(1),SVENDB        ELSE USE END YEAR                         
         MVC   WORK(2),SVEND                                                    
*                                                                               
BR22     MVC   ENDDTB,SVMGDATB      DEFAULT END IS LAST MG DATE                 
         CLI   BYTE,C'S'           TEST ONLY START DATE ENTERED                 
         BE    BR28                YES                                          
         CLI   BYTE,C'M'           TEST MONTH ENTERED                           
         BE    BR24                YES                                          
         GOTO1 (RF),(R1),WORK+6,(3,ENDDTB)   GET 3 BYTE END DATE                
*                                                                               
         MVC   ENDDTB(1),STDTB                                                  
         CLC   ENDDTB+1(2),STDTB+1 IF END MMMDD GE START MMMDD,                 
         BNL   *+16                 USE SAME YEAR                               
         IC    RE,ENDDTB            ELSE NEXT YEAR                              
         LA    RE,1(RE)                                                         
         STC   RE,ENDDTB                                                        
         B     BR28                                                             
*                                                                               
* GET BROADCAST MONTH START AND END DATES                                       
*                                                                               
BR24     GOTO1 VCALLOV,DMCB,0,X'D9000A1D'   GET GETBROAD ADDRESS                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(1,WORK),WORK+6,VGETDAY,VADDAY                         
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    BRBADKEY                                                         
         GOTO1 VDATCON,DMCB,WORK+6,(3,STDTB)                                    
         GOTO1 (RF),(R1),WORK+12,(3,ENDDTB)                                     
*                                                                               
* TEST DATES IN EST PERIOD                                                      
*                                                                               
BR28     MVI   ERRCD,ESPERERR                                                   
         CLC   STDTB,ENDDTB         TEST START BEFORE END                       
         BH    BRBADK2                                                          
         CLC   STDTB,SVMGDATB       TEST RCL START AFTER MG END                 
         BH    BRBADK2                                                          
         CLC   ENDDTB,SVSTARTB      TEST RCL ENDS BEFORE EST START              
         BL    BRBADK2                                                          
         EJECT                                                                  
* INPUT MAY BE FOLLOWED BY STATION FOR CUT-IN DISPLAY                           
*                                                                               
BR30     CLI   RCLOPT,RCLZCUT+X'80'                                             
         BE    *+12                                                             
         CLI   RCLOPT,RCLCUT+X'80'                                              
         BNE   BR40                                                             
*                                                                               
         MVI   FSTOPS+2,C'/'                                                    
         XC    FLEN,FLEN           REREAD                                       
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'/'                                                       
         BNE   BR40                                                             
         XC    FSTOPS,FSTOPS                                                    
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,STAERR                                                     
         CLI   FLEN+1,4                                                         
         BH    BRBADK2                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=4C'0'                                                   
         MVC   WORK+4(4),0(R4)                                                  
         MVI   WORK+8,C'T'                                                      
         OC    WORK(9),SPACES                                                   
         GOTO1 STAPACK,DMCB,(C'P',WORK),WORK+4,WORK+16                          
         MVC   BUCUTSTA,WORK+18    SAVE STARTING STATION                        
         MVI   BUCUTSTA+2,0        DROP NETWORK BITS                            
         OC    BUCUTSTA+2(1),SVNETBTS                                           
         B     BR40                                                             
         EJECT                                                                  
* RCLDEM MAY BE FOLLOWED BY /MKT                                                
*                                                                               
BR38     MVI   FSTOPS+2,C'/'                                                    
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL              REREAD                                       
         CLI   FSTOP,C'/'                                                       
         BNE   BR40                                                             
*                                                                               
         MVI   FSTOPS+2,0                                                       
         GOTO1 FLDVAL              SCAN FOR SPILL MARKET NUMBER                 
*                                                                               
         LTR   R5,R5                                                            
         BZ    BRBADKEY                                                         
*                                                                               
         TM    FVAL,X'08'          SKIP IF SPILL MARKET IS NOT NUMERIC          
         BZ    BR38B                                                            
*                                                                               
         CLI   FLEN+1,4                                                         
         BH    BRBADKEY                                                         
*                                                                               
         CVB   R0,DUB                                                           
         STH   R0,SVSPLMKT                                                      
         STH   R0,STSPLMKT                                                      
*                                                                               
         B     BR40                                                             
*                                                                               
BR38B    DS    0H                  ASSUME ENTRY IS ALPHA MARKET CODE            
*                                                                               
         CLI   FLEN+1,3            MAX 3 LONG                                   
         BH    BRBADKEY                                                         
*                                                                               
         MVC   SVSPLALP,0(R4)      SAVE AS MARKET ALPHA                         
         OC    SVSPLALP,SPACES     SPACE FILL                                   
         MVC   SVSPLMKT,=X'FFFF'   INDICATE SPILL MKT ENTERED                   
         B     BR40                BUT AS ALPHA CODE                            
*                                                                               
BRBADKEY MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADRCL)                                                
*                                                                               
BRBADK2  XC    SVKEY+14(4),SVKEY+14  SET NO VALID LINE RECALLED                 
         B     BUYERR                                                           
         EJECT                                                                  
* EDITING COMPLETE - SHOULD BE NO MORE DATA *                                   
         SPACE 1                                                                
BR40     MVI   ERRCD,BADOPTN                                                    
         XC    FSTOPS,FSTOPS                                                    
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'                                                      
         BNE   BRBADK2                                                          
* SEE IF BUY EXISTS                                                             
         TM    UPSW,UPON           TEST UPLOADING                               
         BZ    BR40A                                                            
         OI    DMINBTS,X'08'       SET TO PASS DELETES !                        
*                                                                               
BR40A    XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+8                                                              
         OI    KEY,X'08'           SET TO READ ORIGINAL BUY                     
         XC    SVKEY+14(4),SVKEY+14  CLEAR IN CASE NOT FOUND                    
         XC    DMWORK,DMWORK       CLEAR GETWORK TABLE TOO !                    
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
* UPLOAD MAY HAVE READ A DELETED RECORD -- UNDELETE IT NOW !                    
         TM    KEY+13,X'80'                                                     
         BZ    BR41                                                             
         NI    KEY+13,X'7F'        SET UNDELETED                                
         MVC   COMMAND,=C'DMWRT'                                                
         MVI   GBYACT,C'W'         TELL SPGETBUY IT'S A WRITE                   
         GOTO1 DIR                                                              
* DISPLAY BUY                                                                   
BR41     MVC   SVKEY,KEY           SAVE KEY AND DISK ADDR                       
         TM    RCLOPT,X'80'        TEST DATES ALLOWED                           
         BZ    BR42                                                             
         GOTO1 VDATCON,DMCB,(3,STDTB),(2,STDTP)                                 
         GOTO1 (RF),(R1),(3,ENDDTB),(2,ENDDTP)                                  
BR42     DS    0H                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         TM    REC+15,X'80'        TEST RECORD DELETED (UPLOAD)                 
         BZ    BR42B                                                            
         TM    UPSW,UPON           TEST UPLOADING                               
         BZ    BR42A               NO - ERROR                                   
         NI    REC+15,X'7F'        UNDELETE                                     
         GOTO1 PUTREC                                                           
         B     BR42B                                                            
*                                                                               
BR42A    MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(RECISDEL)                                              
         XC    SVKEY+14(4),SVKEY+14                                             
         B     BUYERR                                                           
*                                                                               
BR42B    NI    DMINBTS,X'F7'       RESET                                        
         MVI   ERRCD,BADOPTN                                                    
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BZ    *+12                                                             
         CLI   RCLOPT,X'84'        TEST LI=N+I                                  
         BE    BUYERR                                                           
*                                                                               
         CLI   SVPOLPRD,0          TEST BRAND POL BY BRAND                      
         BE    BR43                                                             
         MVI   ERRCD,MASNOTEQ                                                   
         CLC   BDMASPRD,SVPOLPRD                                                
         BNE   BUYERR                                                           
         B     BR45                                                             
*                                                                               
BR43     OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    BR44                NO                                           
         MVI   ERRCD,RGNNOTEQ                                                   
         CLC   SVNRGN,BDNRGN       TEST SAME RGN                                
         BNE   BUYERR                                                           
         CLI   RCLOPT,RCLDT                                                     
         BNE   BR44                                                             
         BAS   RE,BLDDT                                                         
         B     BR45                                                             
*                                                                               
BR44     CLI   RCLOPT,RCLZCUT+X'80'   CAN NET CUT-IN OPTION                     
         BE    *+12                                                             
         CLI   RCLOPT,RCLCUT+X'80'   CAN NET CUT-IN OPTION                      
         BNE   BR45                                                             
         MVI   ERRCD,NOTNETWK                                                   
         OC    SVNDEF(16),SVNDEF                                                
         BZ    BUYERR                                                           
         BAS   RE,BLDCUT                                                        
*                                                                               
BR45     CLI   RCLOPT,RCLHIST                                                   
         BNE   *+8                                                              
         BRAS  RE,BRHIST           GET HISTORY REC IN IO2                       
*                                                                               
         GOTO1 CALLDSP                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVKEY+11        PASS LINE NUMBER TO GLOBBER                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FULL,3,GLVSPBUY                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCTACON,SVCTACON                                                
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    BR46                                                             
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BR46                                                             
         MVC   SVCTACON,3(R6)      SAVE LAST CONTRACT RECALLED                  
*                                                                               
BR46     CLI   BUYMSG,0                                                         
         BNE   *+10                                                             
         MVC   BUYMSG(24),=C'REQUESTED DATA DISPLAYED'                          
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR               NO RETURN HERE                               
         EJECT                                                                  
*======================================================*                        
* MULTIPLE LINE RECALL                                 *                        
*======================================================*                        
         SPACE 1                                                                
BR100    DS    0H                                                               
         ZIC   R4,0(R2)                                                         
         AR    R4,R2                                                            
         C     R4,FLAST                                                         
         BH    *+8                                                              
         MVI   5(R4),0             SUPPRESS FURTHER INPUT                       
         CLI   FSTOP,C'-'          TEST FOR PRD SEPARATOR                       
         BE    BR146                                                            
*                                                                               
BR140    MVC   FSTOPS(2),=C',='                                                 
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADKEYWD                                                   
         CLI   FSTOP,X'FF'                                                      
         BE    BR160                                                            
*                                                                               
         CLI   0(R4),C'F'          TEST 'FIND'                                  
         BNE   BR144                                                            
         MVI   BUREFTYP,C'F'       INDICATE 'FIND' ACTIVE                       
         MVC   BUYKEY(13),SVKEY    SET A KEY IN BUYKEY                          
         MVI   BUYKEY+3,X'FF'      FORCE TO ACCEPT COST OVERRIDE                
*                                                                               
         LA    R4,1(R4)            POINT TO DATE                                
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN           CLEAR FIELD LENGTH                           
         MVI   EDTVAL,ELDTEDT                                                   
         GOTO1 CALLEDT                                                          
*                                                                               
         L     R4,FADDR            UPDATE POINTER                               
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         AHI   R4,1                                                             
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
*                                                                               
         CLI   0(R4),C'$'          TEST COST OVERRIDE PRESENT                   
         BNE   BR140                                                            
*                                                                               
         MVI   EDTVAL,ALLOEDT                                                   
         GOTO1 CALLEDT                                                          
         L     R4,FADDR            UPDATE POINTER                               
         AH    R4,FLEN                                                          
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         AHI   R4,1                                                             
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         B     BR140                                                            
*                                                                               
BR144    CLC   =C'MAS',0(R4)       TEST PKG MASTER DISPLAY                      
         BNE   *+12                                                             
         MVI   BUREFTYP,C'M'                                                    
         B     BR140                                                            
*                                                                               
         CLI   FSTOP,C'='                                                       
         BNE   BUYERR                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,4                                                         
         BH    BUYERR                                                           
*                                                                               
BR146    MVI   EDTVAL,ALLOEDT                                                   
         CLI   SVPRD,X'FF'                                                      
         BNE   BR148                                                            
         CLI   SVPOLPRD,0                                                       
         BNE   *+14                                                             
         CLC   =C'M=',0(R4)                                                     
         BE    BR150                                                            
         CLI   FSTOP,C'-'                                                       
         BE    BR150                                                            
BR148    MVI   EDTVAL,DAYEDT                                                    
         CLC   =C'DAY',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,TIMEDT                                                    
         CLC   =C'TIM',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,SLNEDT                                                    
         CLC   =C'SLN',0(R4)                                                    
         BE    BR150                                                            
         CLC   =C'LEN',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,ADJEDT                                                    
         CLC   =C'ADJ',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,REPEDT                                                    
         CLC   =C'REP',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,DPTEDT                                                    
         CLC   =C'DPT',0(R4)                                                    
         BE    BR150                                                            
         MVI   EDTVAL,COSTEDT                                                   
         CLC   =C'COS',0(R4)                                                    
         BE    BR150                                                            
         CLC   =C'DATE',0(R4)                                                   
         BE    BR152                                                            
         CLC   =C'ID',0(R4)                                                     
         BE    BR158ID                                                          
         CLC   =C'PUR',0(R4)                                                    
         BE    BR158PUR                                                         
         CLC   =C'PGM',0(R4)                                                    
         BE    BR157                                                            
         CLC   =C'PRO',0(R4)                                                    
         BE    BR157                                                            
         MVI   ERRCD,BADKEYWD                                                   
         B     BUYERR                                                           
*                                                                               
BR150    GOTO1 CALLEDT                                                          
         B     BR140                                                            
         SPACE 2                                                                
* EDIT ACTIVITY DATE FILTER                                                     
*                                                                               
BR152    LA    R4,5(R4)                                                         
         CLC   =C'TODAY',0(R4)                                                  
         BNE   BR154                                                            
         GOTO1 VDATCON,DMCB,(5,0),(3,BUSTARTB)                                  
         MVC   BUENDB,BUSTARTB                                                  
         B     BR156                                                            
*                                                                               
BR154    MVI   ERRCD,INVDATE                                                    
         GOTO1 VDATVAL,DMCB,(R4),BUSTART                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
         A     R4,0(R1)            POINT PAST DATE                              
*                                                                               
         GOTO1 VDATCON,DMCB,BUSTART,(3,BUSTARTB)                                
         MVC   BUENDB,BUSTARTB                                                  
*                                                                               
         CLI   0(R4),C' '                                                       
         BNH   BR156                                                            
         CLI   0(R4),C','                                                       
         BE    BR156                                                            
*                                                                               
         LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(R4),BUEND                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
         GOTO1 VDATCON,DMCB,BUEND,(3,BUENDB)                                    
*                                                                               
         CLC   BUSTARTB,BUENDB                                                  
         BH    BUYERR                                                           
*                                                                               
BR156    MVI   FSTOPS+1,0          UPDATE FLDVAL FOR NEXT FILTER                
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         B     BR140                                                            
         SPACE 2                                                                
* EDIT PROGRAM FILTER                                                           
*                                                                               
BR157    XC    BUPROG,BUPROG                                                    
         GOTO1 FLDVAL                                                           
         ZIC   R1,FLEN+1                                                        
         AHI   R1,-1                                                            
         BM    BUYERR                                                           
         CHI   R1,16                                                            
         BNH   *+8                                                              
         LA    R1,16               ONLY MOVE FOR UP TO 17                       
         EX    R1,*+4                                                           
         MVC   BUPROG(0),0(R4)                                                  
         STC   R1,BUPROG+17        SAVE 1 BYTE FOR COMPARE LENGTH-1             
         B     BR140                                                            
         SPACE 2                                                                
* EDIT CONTRACT FILTER                                                          
*                                                                               
BR158ID  LA    R4,3(R4)            FILTER ON ID                                 
         B     *+8                                                              
BR158PUR LA    R4,4(R4)            FILTER ON PURPOSE CODE                       
*                                                                               
         MVI   FSTOPS+1,0                                                       
         GOTO1 FLDVAL                                                           
         MVI   ERRCD,BADIDERR                                                   
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         CLI   FLEN+1,12                                                        
         BH    BUYERR                                                           
         MVC   BUORB,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     BR140                                                            
         MVC   BUORB(0),0(R4) *EXECUTED*                                        
         EJECT                                                                  
BR160    MVI   ERRCD,NORECS                                                     
         LA    R2,BUYINP1H                                                      
         MVI   BYTE2,C'N'          INDICATE NO DATA FOUND                       
         MVC   KEY,SVKEY                                                        
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+8                                                              
         OI    KEY,X'08'           SET TO READ ORIGINAL BUY                     
         CLI   SVPOLPRD,0          TEST BRD POL BY BRD                          
         BE    *+10                NO                                           
         MVC   BUELPRD,SVPOLPRD    YES - FORCE ONLY BUYS FOR THIS BRD           
         GOTO1 HIGH                                                             
BR161    CLC   KEY(10),KEYSAVE     A-M/C/P/MKT-STA/EST                          
         BNE   BUYERR                                                           
         CLI   KEY+10,0                                                         
         BE    BR164                                                            
         GOTO1 SEQ                                                              
         B     BR161                                                            
*                                                                               
BR162    GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   BR202                                                            
         CLI   KEY+10,0                                                         
         BNE   BR162                                                            
*                                                                               
BR164    LA    RE,BUYREC                                                        
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVNRGN,C'*'                                                      
         BE    BR166                                                            
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BZ    *+14                                                             
         CLC   SVNRGN,BDNRGN       TEST SAME RGN                                
         BNE   BR162                                                            
*                                                                               
* CHECK FILTERS                                                                 
*                                                                               
BR166    OC    BUELPRD(2),BUELPRD                                               
         BZ    BR168                                                            
         CLC   BUELPRD(2),BDMASPRD                                              
         BNE   BR162                                                            
*                                                                               
BR168    OC    BUDAYS,BUDAYS                                                    
         BZ    *+14                                                             
         CLC   BUDAYS,BDDAY                                                     
         BNE   BR162                                                            
*                                                                               
         OC    BUTIME,BUTIME                                                    
         BZ    BR170                                                            
         MVC   DUB(4),BUTIME                                                    
         LA    R1,DUB                                                           
         BAS   RE,BRFIXTIM                                                      
         MVC   DUB+4(4),BDTIMST                                                 
         LA    R1,DUB+4                                                         
         BAS   RE,BRFIXTIM                                                      
*                                                                               
         CLC   DUB(2),DUB+6        FILTER START AFTER BUY END                   
         BH    BR162                                                            
         CLC   DUB+2(2),DUB+4      FILTER END BEFORE BUY START                  
         BL    BR162                                                            
*                                                                               
BR170    DS    0H                                                               
         OC    BUDPT,BUDPT                                                      
         BZ    *+14                                                             
         CLC   BUDPT,BDDAYPT                                                    
         BNE   BR162                                                            
         OC    BUSLN,BUSLN                                                      
         BZ    *+14                                                             
         CLC   BUSLN,BDSEC                                                      
         BNE   BR162                                                            
         OC    BUCOST(4),BUCOST                                                 
         BZ    *+14                                                             
         CLC   BUCOST(4),BDCOST    SAME COST/COST IND                           
         BNE   BR162                                                            
         OC    BUPROG,BUPROG                                                    
         BZ    BR172                                                            
         LA    R4,BDPROGRM                                                      
         CLI   0(R4),C'='          SKIP OVER =                                  
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         SR    R1,R1                                                            
         IC    R1,BUPROG+17        LENGTH OF ENTRY-1                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BUPROG(0),0(R4)      SAME PROGRAM                                
         BNE   BR162                                                            
*                                                                               
BR172    OC    BUADJ,BUADJ                                                      
         BZ    *+14                                                             
         CLC   BUADJ,BDPROGT                                                    
         BNE   BR162                                                            
*                                                                               
         OC    BUREP,BUREP                                                      
         BZ    BR180                                                            
         MVC   HALF,BUREP                                                       
         NI    HALF,X'7F'          DROP BUREP=0 FLAG                            
         CLC   HALF,BDREP                                                       
         BNE   BR162                                                            
*                                                                               
BR180    OC    BUSTARTB,BUSTARTB   TEST ACTIVITY FILTER                         
         BZ    BR182                                                            
         CLC   BDCHG,BUSTARTB                                                   
         BL    BR162                                                            
         CLC   BDCHG,BUENDB                                                     
         BH    BR162                                                            
*                                                                               
BR182    CLI   BUREFTYP,C'M'       PKG MSTR ONLY                                
         BNE   BR186                                                            
* FIND PKG ELEM AND CHECK MASTER                                                
         LA    R6,BDELEM                                                        
BR184    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BR162                                                            
         CLI   0(R6),5                                                          
         BNE   BR184                                                            
         TM    2(R6),X'01'         TEST MASTER                                  
         BZ    BR162                                                            
*                                                                               
BR186    OC    BUORB,BUORB         TEST ID FILTER                               
         BZ    BR190                                                            
         LA    R6,BDELEM                                                        
BR188    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BR162                                                            
         CLI   0(R6),X'70'                                                      
         BNE   BR188                                                            
         CLC   3(12,R6),BUORB      MATCH ID                                     
         BNE   BR162                                                            
         EJECT                                                                  
BR190    CLI   BUREFTYP,C'F'       TEST FIND                                    
         BNE   BR200                                                            
         BRAS  RE,FSPOT                                                         
         BNE   BR162                                                            
*                                                                               
BR200    DS    0H                                                               
         GOTO1 CALLDSP                                                          
         MVI   BYTE2,C'Y'                                                       
         CLI   ERRCD,X'FE'         TEST NO MORE ROOM                            
         BE    BR202                                                            
         MVC   SVRCLEND(2),KEY+11  SAVE LAST LINE NUMBER DISPLAYED              
         MVI   SVRCLEND+2,0                                                     
         B     BR162               AND CONTINUE                                 
*                                                                               
BR202    DS    0H                                                               
         CLI   BYTE2,C'Y'          TEST ANY DATA DISPLAYED                      
         BE    BR204                                                            
         LA    R2,BUYOUTH                                                       
         MVC   0(3,R2),=X'000101'  FORCE NEW SCREEN TO CLEAR IT                 
         MVI   ERRCD,NORECS                                                     
         LA    R2,BUYINP1H                                                      
         GOTO1 ERROR                                                            
*                                                                               
BR204    DS    0H                                                               
         CLI   BUYMSG,C' '                                                      
         BH    *+10                                                             
         MVC   BUYMSG(24),=C'REQUESTED DATA DISPLAYED'                          
         L     RE,ADRSVKEY                                                      
         XC    14(4,RE),14(RE)     INDICATE NO VALID LINE RECALL                
         B     EXIT                                                             
         EJECT                                                                  
BRFIXTIM NTR1                                                                   
*                                                                               
         OC    2(2,R1),2(R1)                                                    
         JNZ   *+10                                                             
         MVC   2(2,R1),0(R1)                                                    
         LH    R0,0(R1)                                                         
         CHI   R0,600                                                           
         JNL   *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,0(R1)                                                         
         LH    R0,2(R1)                                                         
         CHI   R0,600                                                           
         JNL   *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,2(R1)                                                         
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* DISPLAY PACKAGE LINES                                                         
*================================================================               
         SPACE 1                                                                
BR300    DS    0H                                                               
         MVI   RCLOPT,C'N'         INDICATE MULT LINE RECALL                    
*                                                                               
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'/'                                                      
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,C'/'                                                       
         BNE   BR302                                                            
* EDIT STARTING LINE NUMBER                                                     
         MVI   FSTOPS,0                                                         
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         CHI   R0,255                                                           
         BH    BUYERR                                                           
         STC   R0,BUREF                                                         
*                                                                               
* SHOULD BE NO MORE DATA                                                        
*                                                                               
BR302    MVI   ERRCD,BADOPTN                                                    
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'                                                      
         BNE   BUYERR                                                           
* NOW CHECK LINE IS A PACKAGE MASTER                                            
         MVC   KEY,SVKEY                                                        
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+8                                                              
         OI    KEY,X'08'                                                        
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTFOUND                                                   
         CLC   KEY(12),KEYSAVE                                                  
         BNE   BUYERR                                                           
         GOTO1 GETREC                                                           
* LOOK FOR PKGEL                                                                
         LA    R6,BDELEM                                                        
         MVI   ERRCD,NOTMSTR                                                    
BR310    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BUYERR                                                           
         CLI   0(R6),5                                                          
         BNE   BR310                                                            
*                                                                               
         TM    2(R6),X'01'         TEST MASTER                                  
         BZ    BUYERR                                                           
* SAVE PKGEL                                                                    
         XC    MSTRBUY,MSTRBUY                                                  
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MSTRBUY(0),0(R6)  *EXECUTED*                                     
*                                                                               
         TM    MSTRBUY+2,X'10'     TEST 2-BYTE LINE NUMBERS                     
         BO    BR315                                                            
*                                                                               
         LA    R6,MSTRBUY+2        POINT TO FIRST LINE -1                       
         CLI   BUREF,0             TEST START AT MASTER                         
         BE    BR320               YES                                          
* FIND STARTING LINE NUM IN PKGEL                                               
         LA    R6,MSTRBUY+3                                                     
         MVI   ERRCD,NOTINPKG                                                   
BR312    CLI   0(R6),0                                                          
         BE    BUYERR                                                           
         CLC   BUREF(1),0(R6)                                                   
         BE    BR330                                                            
         LA    R6,1(R6)                                                         
         B     BR312                                                            
                                                                                
* FIND STARTING 2-BYTE LINE NUM IN PKGEL                                        
                                                                                
BR315    LA    R6,MSTRBUY+1        POINT TO FIRST PKGLINE -2                    
         OC    BUREF,BUREF                                                      
         BZ    BR320                                                            
*                                                                               
         LA    R6,MSTRBUY+3                                                     
         MVI   ERRCD,NOTINPKG                                                   
BR317    OC    0(2,R6),0(R6)                                                    
         BZ    BUYERR                                                           
         CLC   BUREF(2),0(R6)                                                   
         BE    BR330                                                            
         LA    R6,2(R6)                                                         
         B     BR317                                                            
*                                                                               
BR320    GOTO1 CALLDSP                                                          
         CLI   ERRCD,X'FE'         TEST NO MORE ROOM                            
         BE    BR204                                                            
*                                                                               
         TM    MSTRBUY+2,X'10'     TEST 2-BYTE LINE NUMS IN EL                  
         BO    BR322                                                            
         LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    BR204                                                            
         B     BR330                                                            
*                                                                               
BR322    LA    R6,2(R6)                                                         
         OC    0(2,R6),0(R6)                                                    
         BZ    BR204                                                            
*                                                                               
BR330    XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         CLI   SVORIGNL,C'Y'                                                    
         BNE   *+8                                                              
         OI    KEY,X'08'                                                        
         MVC   KEY+12(1),0(R6)                                                  
         TM    MSTRBUY+2,X'10'     TEST 2-BYTE LINE NUMS IN EL                  
         BZ    *+10                                                             
         MVC   KEY+11(2),0(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     BR320                                                            
         EJECT                                                                  
*=========================================================*                     
* BUILD LIST OF EXPLODED DAYS/TIMES FOR CANADIAN NETWORK  *                     
*=========================================================*                     
         SPACE 1                                                                
BLDDT    NTR1                                                                   
         MVC   BUSVSTB(6),BDSTART  SAVE NETWORK START/END DATES                 
*                                                                               
         MVI   ERRCD,NODTLST                                                    
         CLI   BDPROGRM,C'='       TEST SIMULCAST                               
         BE    BUYERR              YES - ALL MUST BE THE SAME                   
* SAVE NTWK BUY IN REC3                                                         
         L     R0,AREC             'FROM' ADDR                                  
         LHI   R1,REC2-REC         'FROM' LEN                                   
         L     RE,AREC3            'TO' ADDR                                    
         LR    RF,R1               'TO' LEN = 'FROM' LEN                        
         MVCL  RE,R0                                                            
* CLEAR REC4 TO CONSTRUCT STATION/DAY/TIME TABLE                                
         SR    R0,R0               'FROM' ADDR                                  
         SR    R1,R1               'FROM' LEN                                   
         L     RE,AREC4            'TO' ADDR                                    
         LHI   RF,REC5-REC4        'TO' LEN                                     
         MVCL  RE,R0                                                            
         L     R4,AREC4            POINT TO TABLE START                         
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
*                                                                               
BLDDT2   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BLDDTX                                                           
*                                                                               
* READ EXPLODED KEY/REC                                                         
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
*                                                                               
         XC    BUEXPDAY,BUEXPDAY                                                
*                                                                               
         BAS   RE,GETRELST         SET RELATIVE START DAY                       
*                                                                               
         MVC   0(5,R4),KEY+4       MOVE MARKET/STATION TO BUFFER                
         MVC   5(2,R4),BUEXPDAY    SET SIGNED REL DAY                           
         MVC   7(2,R4),BDTIMST     SET START TIME                               
         LA    R4,9(R4)            ADVANCE POINTER                              
         B     BLDDT2                                                           
         EJECT                                                                  
*========================================================*                      
* RESTORE NETWORK BUY TO IOA1 BEFORE DISPLAY             *                      
*========================================================*                      
         SPACE 1                                                                
BLDDTX   MVC   KEY,SVKEY           RESTORE KEY                                  
* MOVE NTWK BUY TO REC                                                          
         L     R0,AREC             'TO' ADDR                                    
         LHI   R1,REC2-REC         'TO' LEN                                     
         L     RE,AREC3            'FROM' ADDR                                  
         LR    RF,R1               'FROM' LEN = 'TO' LEN                        
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
*======================================================*                        
* SUBROUTINE COMPUTES RELATIVE START DATE FOR EXPLODED *                        
* CANADIAN NETWORK BUYS                                *                        
*======================================================*                        
         SPACE 1                                                                
GETRELST NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK+6  EXPLODED BUY START              
         LA    R4,BUSVSTB                       NETWORK BUY START               
         GOTO1 (RF),(R1),(3,(R4)),WORK                                          
*                                                                               
         SR    R4,R4               CLEAR COUNTER                                
         LA    R0,1                SET TO ADVANCE DAYS                          
         CLC   WORK(6),WORK+6                                                   
         BE    GETRELS4                                                         
         BL    *+6                                                              
         LCR   R0,R0               UNLESS HIGH ALREADY                          
*                                                                               
GETRELS2 LA    R4,1(R4)            BUMP COUNTER                                 
         GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         MVC   WORK(6),WORK+12                                                  
         CLC   WORK(6),WORK+6                                                   
         BNE   GETRELS2                                                         
*                                                                               
GETRELS4 LTR   R0,R0                                                            
         BNM   *+6                                                              
         LCR   R4,R4                                                            
         STH   R4,BUEXPDAY         SET SIGNED RELATIVE DAY                      
         XIT1                                                                   
*                                                                               
GETDATE  NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,(R4)),WORK                                       
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(3,(R4))                                     
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* BUILD LIST OF CUT-IN STATIONS AND PRODUCTS             *                      
*    FOR CANADIAN NETWORK                                *                      
*========================================================*                      
         SPACE 1                                                                
BLDCUT   NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,STDTB),(2,BUCUTDT)  GET 2 BYTE ST DT             
* CLEAR CUT-IN DATA SAVE AREA                                                   
         LA    R0,SVCUTDTA         'TO' ADDRESS                                 
         LA    R1,SVCUTDTX-SVCUTDTA 'TO' LEN                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* COMPUTE STARTING ELEMENT NUMBER AND BUILD DATE LIST                           
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         MVI   SVCUTEL,0                                                        
         XC    HALF,HALF                                                        
         CLI   BUELEMNO,0                                                       
         BNE   *+8                                                              
         MVI   BUELEMNO,1                                                       
*                                                                               
BLDCUT2  BRAS  RE,NEXTEL                                                        
         BNE   BLDCUT8                                                          
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    BLDCUT2                                                          
         CLC   HALF,2(R6)          TEST SAME DATE                               
         BE    *+8                                                              
         MVI   BYTE,0              RESET DATE COUNTER                           
         IC    RE,BYTE             BUMP SPOT COUNT                              
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         MVC   HALF,2(R6)          AND SAVE DATE                                
*                                                                               
         IC    RE,SVCUTEL                                                       
         LA    RE,1(RE)                                                         
         STC   RE,SVCUTEL                                                       
         STC   RE,SVCUTELX                                                      
*                                                                               
         CLC   2(2,R6),BUCUTDT     ELEMENT DATE TO REQ START                    
         BL    BLDCUT2                                                          
         BH    BLDCUT3                                                          
         CLC   BYTE,BUELEMNO       REACHED STARTING SPOT YET                    
         BL    BLDCUT2             NO                                           
         MVC   SVCUTLOW,BUELEMNO   SET LOW SPOT FIRST DATE                      
* THIS LEAVES STARTING ELEMENT NUMBER IN SVCUTEL                                
BLDCUT3  CLI   RCLOPT,RCLZCUT+X'80'                                             
         BNE   BLDCUT3A                                                         
         LA    R4,SVZCUTDT                                                      
         XC    SVZCUTDX,SVZCUTDX                                                
         LA    R5,18                                                            
         B     BLDCUT6                                                          
BLDCUT3A LA    R4,SVCUTDTS         NOW BUILD DATE LIST                          
         LA    R5,8                                                             
         B     BLDCUT6                                                          
*                                                                               
BLDCUT4  BRAS  RE,NEXTEL                                                        
         BNE   BLDCUT8                                                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BO    BLDCUT4                                                          
*                                                                               
BLDCUT6  MVC   0(2,R4),2(R6)                                                    
         IC    RE,SVCUTELX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,SVCUTELX                                                      
         LA    R4,2(R4)                                                         
         BCT   R5,BLDCUT4                                                       
*                                                                               
BLDCUT8  CLI   RCLOPT,RCLZCUT+X'80'                                             
         BNE   BLDCUT9                                                          
         OC    SVZCUTDT,SVZCUTDT                                                
         BNZ   BLDCUT10                                                         
         B     BLDCUT9A                                                         
BLDCUT9  OC    SVCUTDTS,SVCUTDTS   TEST ANY DATES IN LIST                       
         BNZ   BLDCUT10                                                         
BLDCUT9A MVI   ERRCD,NOCUTDTA                                                   
         B     BUYERR                                                           
*                                                                               
* SAVE NETWORK BUY IN REC3                                                      
*                                                                               
BLDCUT10 L     R0,AREC                                                          
         LHI   R1,REC2-REC         'FROM' LEN                                   
         L     RE,AREC3                                                         
         LR    RF,R1               'TO' LEN = 'FROM' LEN                        
         MVCL  RE,R0                                                            
*                                                                               
* NEED TO FIND OUT IF USER SPECIFIED CUT-IN LIST                                
*                                                                               
         MVI   BUCUTFLG,0          CLEAR CUT-IN LIST FLAG                       
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
*                                                                               
BLDCUT12 TM    SVNDSTAT,SVNDCUTQ                                                
         BZ    BLDCUT14                                                         
         OI    BUCUTFLG,X'80'      SET FLAG CUT-IN LIST ACTIVE                  
         B     BLDCUT20                                                         
*                                                                               
BLDCUT14 LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST   TEST MORE IN LIST                            
         BNZ   BLDCUT12                                                         
*                                                                               
BLDCUT20 OC    BUCUTSTA,BUCUTSTA   DID USER INPUT START STATION                 
         BZ    BLDCUT30            NO                                           
*                                                                               
* FIND THE STARTING STATION IN SVNDEF LIST                                      
*                                                                               
         LA    R7,SVNDEF                                                        
*                                                                               
BLDCUT22 CLC   BUCUTSTA,2(R7)                                                   
         BE    BLDCUT30                                                         
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   BLDCUT30                                                         
         MVI   ERRCD,STNETERR      STATION NOT IN NETWORK                       
         B     BUYERR                                                           
         EJECT                                                                  
*=========================================================*                     
* BUILD A LIST OF THE NEXT 7 STATIONS IN THE BUY RECORD   *                     
*=========================================================*                     
         SPACE 1                                                                
BLDCUT30 L     R6,AREC3            POINT TO NETWORK BUY                         
         LA    R6,24(R6)                                                        
*                                                                               
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         L     R6,AREC3                                                         
         LA    R6,BDELEM-BUYREC(R6)                                             
*                                                                               
BLDCUT32 BRAS  RE,NEXTEL                                                        
         BNE   BLDCUT50                                                         
*                                                                               
         OC    BUCUTSTA,BUCUTSTA   TEST STARTING STATION SPECIFIED              
         BZ    BLDCUT34                                                         
         CLC   BUCUTSTA,4(R6)                                                   
         BNE   BLDCUT32            NOT YET                                      
         XC    BUCUTSTA,BUCUTSTA   NOW CLEAR STARTING POINT                     
*                                                                               
BLDCUT34 TM    BUCUTFLG,X'80'      TEST CUT-IN LIST SPECIFIED                   
         BZ    BLDCUT40            NO                                           
*                                                                               
* FIND STATION IN SVNDEF AREA                                                   
*                                                                               
         LA    R7,SVNDEF                                                        
*                                                                               
BLDCUT36 CLC   SVNDSTA,4(R6)       MATCH STATION                                
         BE    BLDCUT38                                                         
         LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   BLDCUT36                                                         
         B     BLDCUT32            IGNORE MISSING STATION                       
*                                                                               
BLDCUT38 TM    SVNDSTAT,SVNDCUTQ   TEST CUT-IN THIS STATION                     
         BZ    BLDCUT32                                                         
*                                                                               
BLDCUT40 LA    R1,SVCUTLST         FIND NEXT SLOT IN LIST                       
         LA    R0,6                                                             
*                                                                               
BLDCUT42 OC    0(5,R1),0(R1)                                                    
         BZ    BLDCUT44                                                         
         LA    R1,L'SVCUTLST(R1)                                                
         BCT   R0,BLDCUT42                                                      
         B     BLDCUT50                                                         
*                                                                               
BLDCUT44 MVC   0(5,R1),2(R6)       SAVE MKT/STA                                 
         CLI   RCLOPT,RCLZCUT+X'80'                                             
         BNE   BLDCUT32                                                         
         MVI   BYTE,0                                                           
         DROP  R7                                                               
*                                                                               
BLDCT44A L     R6,AREC3                                                         
         LA    R6,BDELEM-BUYREC(R6)                                             
         XC    SVZCUTST(200),SVZCUTST                                           
         LA    R7,SVZCUTST                                                      
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
BLDCUT45 BRAS  RE,NEXTEL                                                        
         BNE   BLDCT49A                                                         
         LA    RE,SVZCUTSX                                                      
         CR    R7,RE                                                            
         BH    BLDCT49A                                                         
         CLI   BYTE,1                IF BYTE=1, ADD ALL STATIONS                
         BE    BLDCT49B              TO CUTLIST                                 
         LA    RE,SVNDEF                                                        
         USING SVNDEFD,RE                                                       
BLDCUT48 CLC   SVNDMKST,2(R6)                                                   
         BE    BLDCUT49                                                         
         LA    RE,26(RE)                                                        
         CLC   SVNDMKST,=5X'00'                                                 
         BNE   BLDCUT48                                                         
         B     BLDCUT45                                                         
BLDCUT49 TM    SVNDSTAT,SVNDCUTQ     CHECK IF STATION IS SELECTED               
         BZ    BLDCUT45              IF YES, ADD TO CUTLIST                     
BLDCT49B MVC   0(L'SVZCUTST,R7),2(R6)                                           
         LA    R7,L'SVZCUTST(R7)                                                
         B     BLDCUT45                                                         
         DROP  RE                                                               
*                                                                               
BLDCT49A OC    SVZCUTST,SVZCUTST     IF NO STATIONS ARE SELECTED                
         BNZ   BLDCUT50              ADD ALL STATIONS TO CUTLIST                
         MVI   BYTE,1                                                           
         B     BLDCT44A                                                         
*                                                                               
*==========================================================*                    
* READ EXPLODED BUYS AND BUILD TABLE OF CUT-IN PRODUCTS    *                    
*==========================================================*                    
         SPACE 1                                                                
BLDCUT50 MVI   ERRCD,NOCUTDTA                                                   
         LA    R7,SVCUTLST                                                      
         OC    0(5,R7),0(R7)       TEST ANY STATIONS IN LIST                    
         BZ    BUYERR                                                           
*                                                                               
BLDCUT52 XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+4(5),0(R7)      MOVE MKT/STA                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,8(R7)            FIRST PRODUCT SLOT                           
         LA    R5,8                SET MAX SPOT COUNT                           
         CLI   RCLOPT,RCLZCUT+X'80'                                             
         BNE   BLDCUT53                                                         
         XC    0(108,R4),0(R4)                                                  
         LA    R5,18                                                            
*                                                                               
BLDCUT53 MVI   0(R4),X'01'         SET 'NO SPOT THIS DATE' DEFAULT              
         LA    R4,6(R4)                                                         
         BCT   R5,*-8                                                           
*                                                                               
         LA    R4,8(R7)            FIRST PRODUCT SLOT                           
         LA    R5,8                MAX SPOTS FOR CUT                            
         CLI   RCLOPT,RCLZCUT+X'80'                                             
         BNE   *+8                                                              
         LA    R5,18               MAX SPOTS FOR ZCUT                           
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         MVI   BYTE,0              RESET ELEMENT COUNTER                        
*                                                                               
BLDCUT54 BRAS  RE,NEXTEL                                                        
         BNE   BLDCUTX                                                          
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    BLDCUT54            YES - SKIP                                   
         IC    RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
         CLC   BYTE,SVCUTEL        ELEMENT NUMBER TO REQ START                  
         BL    BLDCUT54                                                         
         TM    6(R6),X'40'         TEST MINUSSED                                
         BZ    BLDCUT56                                                         
         MVI   0(R4),X'80'         SET NOT-ALLOCABLE FLAG                       
         B     BLDCUT58                                                         
         EJECT                                                                  
BLDCUT56 NI    0(R4),X'FF'-X'01'   RESET NO SPOT FLAG                           
         TM    6(R6),X'04'         TEST HIATUS                                  
         BZ    *+8                                                              
         MVI   5(R4),X'04'         SET HIATUS FLAG                              
         CLI   1(R6),10            TEST UNALLOCATED                             
         BNH   BLDCUT58                                                         
*                                                                               
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    *+8                                                              
         MVI   0(R4),X'40'         SET PAID FLAG                                
         MVC   1(1,R4),10(R6)      MOVE PRD1                                    
         MVC   3(1,R4),11(R6)      MOVE SLN1                                    
         CLI   1(R6),14                                                         
         BNH   BLDCUT58                                                         
         MVC   2(1,R4),14(R6)      MOVE PRD2                                    
         MVC   4(1,R4),15(R6)      MOVE SLN2                                    
*                                                                               
BLDCUT58 LA    R4,6(R4)                                                         
         BCT   R5,BLDCUT54                                                      
*        EJECT                                                                  
BLDCUTX  CLI   RCLOPT,RCLZCUT+X'80'                                             
         BE    BLDDTX                                                           
         LA    R7,L'SVCUTLST(R7)   NEXT STATION                                 
         LA    R0,SVCUTDTS                                                      
         CR    R7,R0               TEST GONE PAST END                           
         BNL   BLDDTX                                                           
         OC    0(5,R7),0(R7)       TEST MORE STATIONS IN LIST                   
         BNZ   BLDCUT52                                                         
         B     BLDDTX              RESTORE NETWORK BUY AND RETURN               
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* DISPLAY BUYS IN SORTED SEQUENCE                                               
* SVSORT+1(255) CONTAINS SORTED LIST OF BUYLINE NUMBERS                         
* SVSORT CONTAINS NEXT BUYLINE TO BE DISPLAYED                                  
*>>>>>>>>>>>>>                                                                  
* NOTE THAT FEATURE IS ONLY SUPPORTED FOR 1-BYTE LINE NUMBERS!                  
*>>>>>>>>>>>>>                                                                  
*================================================================               
         SPACE 1                                                                
BRSORT   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE2,C'N'          SET NO BUYS DISPLAYED                        
         MVI   RCLOPT,C'S'         INDICATE SORTED RECALL                       
         MVI   BUDPT,0             CLEAR POTENTIAL FILTER VALUES                
         MVI   BUDAYS,0                                                         
         MVI   BUSLN,0                                                          
         XC    BUPROG,BUPROG                                                    
         XC    BUCOST,BUCOST                                                    
         XC    BUTIME,BUTIME                                                    
         XC    BLDROW,BLDROW       FIRST TIME SWITCH FOR SPBUY23                
*                                                                               
         CLI   SVDSPMOD,0          TEST FIRST TIME                              
         BNE   BRS1                                                             
         GOTO1 VDATCON,DMCB,SVSTART,(2,SVSKPER) SET TO EST START                
*                                                                               
BRS1     CLI   PFKEY,2             TEST CALL MIS                                
         BNE   BRS1A                                                            
         BRAS  RE,GOMIS                                                         
         J     EXIT                                                             
*                                                                               
BRS1A    CLI   PFKEY,5             TEST TOGGLE DISPLAY MODE                     
         BNE   BRS1B                                                            
         CLI   SVDSPMOD,C'S'       IS IT SKEVAL NOW                             
         BE    BRS1OFF             YES - TURN IT OFF                            
         MVI   SVDSPMOD,C'S'       SET FOR SKEVAL                               
         B     BRS1ON                                                           
*                                                                               
BRS1B    CLI   PFKEY,7             TEST TOGGLE DEMO MODE                        
         BNE   BRS1C               NO                                           
         CLI   SVDSPMOD,C'D'       TEST DEMO DISPLAY                            
         BE    BRS1OFF                                                          
         MVI   SVDSPMOD,C'D'       SET FOR DEMO DISPLAY                         
         B     BRS1ON                                                           
*                                                                               
BRS1C    CLI   PFKEY,9             TEST TOGGLE CPP MODE                         
         BNE   BRS1D                                                            
         CLI   SVDSPMOD,C'C'                                                    
         BE    BRS1OFF                                                          
         MVI   SVDSPMOD,C'C'                                                    
         B     BRS1ON                                                           
*                                                                               
BRS1D    LHI   R0,7                SET TO ADVANCE 7 WEEKS                       
         CLI   PFKEY,11            TEST SCROLL RIGHT                            
         BE    BRS1E                                                            
         LHI   R0,-7                                                            
         CLI   PFKEY,10            TEST SCROLL LEFT                             
         BNE   BRS1X                                                            
BRS1E    CLI   SVDSPMOD,0          CAN'T SCROLL FROM BASE                       
         BE    BRS1X                                                            
         BRAS  RE,ADJPER                                                        
         MVI   SVDSPMOD,C'S'       FORCE SKED DISPLAY                           
         B     BRS1ON                                                           
*                                                                               
BRS1OFF  MVI   SVDSPMOD,C'S'       FORCE SKED DISPLAY                           
*                                                                               
BRS1ON   LHI   R4,SVSORT-BUYSAVE                                                
         AR    R4,RA                                                            
         MVC   0(1,R4),SVSORTST    MOVE PREVIOUS DISPLAY START                  
         B     BRS51               GO FIND STARTING LINE IN LIST                
*                                                                               
BRS1X    CLI   PFKEY,6             TEST TOP                                     
         BNE   BRS2                NO                                           
         LHI   R4,SVSORT-BUYSAVE                                                
         AR    R4,RA                                                            
         MVI   0(R4),0                                                          
         B     BRS60                                                            
*                                                                               
BRS2     CLI   PFKEY,8             TEST NEXT                                    
         BNE   BRS6                                                             
         LHI   RE,SVSORT-BUYSAVE                                                
         AR    RE,RA                                                            
         CLI   0(RE),0             TEST NONE DISPLAYED BEFORE                   
         BE    BRS6                YES - IGNORE PFKEY                           
         LA    R4,1(RE)                                                         
*                                                                               
BRS4     CLC   0(1,R4),0(RE)       FIND LINE IN LIST                            
         BE    BRS61                                                            
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   BRS4                                                             
         DC    H'0'                                                             
*                                                                               
BRS6     CLC   =C'MS',BUTRCODE     TEST MULTISKED DISPLAY                       
         BNE   *+8                                                              
         MVI   SVDSPMOD,C'S'       THEN SET FOR MULTISKED                       
*                                                                               
         XC    FLEN,FLEN           SKIP TO FIRST SEQ FIELD                      
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
*                                                                               
         XC    ELEM,ELEM           CLEAR SORT LIST                              
*                                                                               
         CLI   FSTOP,C','                                                       
         BE    BRS10                                                            
         MVC   ELEM(6),SORTDPT     SET DEFAULT = DPT                            
         MVC   ELEM+6(12),SORTDAYS DAYS                                         
         MVC   ELEM+18(6),SORTTIME TIMES                                        
         MVC   ELEM+24(6),SORTPROG PROGRAM                                      
         B     BRS30                                                            
*                                                                               
BRS10    MVI   FSTOPS,C','         SET TO EDIT SORT SEQ                         
         MVI   FSTOPS+1,C'='                                                    
         GOTO1 FLDVAL                                                           
         CHI   R5,4                                                             
         BH    BRSERR2                                                          
         BCTR  R5,0                                                             
*                                                                               
         LA    R1,SORTFLDS                                                      
         LA    R0,(SORTFLDX-SORTFLDS)/L'SORTFLDS                                
*                                                                               
BRS12    EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(R4) *EXECUTED*                                         
         BE    BRS14                                                            
         LA    R1,L'SORTFLDS(R1)                                                
         BCT   R0,BRS12                                                         
         B     BRSERR2                                                          
*                                                                               
BRS14    LA    RE,ELEM                                                          
*                                                                               
BRS16    CLI   0(RE),0                                                          
         BE    BRS20                                                            
         CLC   4(1,RE),4(R1)       TEST DUP IN LIST                             
         BE    BRSERR3                                                          
         LA    RE,6(RE)                                                         
         B     BRS16                                                            
*                                                                               
BRS20    MVC   0(6,RE),0(R1)       SAVE ENTRY IN LIST                           
*                                                                               
         CLI   FSTOP,C'='                                                       
         BNE   BRS28                                                            
*                                                                               
         MVI   EDTVAL,COSTEDT                                                   
         CLI   4(R1),BDCOST-BUYREC   TEST COST FIELD                            
         BE    BRS24                                                            
*                                                                               
         MVI   EDTVAL,DPTEDT                                                    
         CLI   4(R1),BDDAYPT-BUYREC  TEST DAYPART                               
         BE    BRS24                                                            
*                                                                               
         MVI   EDTVAL,SLNEDT                                                    
         CLI   4(R1),BDSEC-BUYREC  TEST SLN                                     
         BE    BRS24                                                            
*                                                                               
         MVI   EDTVAL,TIMEDT                                                    
         CLI   4(R1),BDTIMST-BUYREC   TEST TIME                                 
         BE    BRS24                                                            
*                                                                               
         MVI   EDTVAL,DAYEDT                                                    
         CLI   4(R1),BDSEDAY-BUYREC                                             
         BNE   BRS22                                                            
* EDIT DAY FILTER                                                               
         GOTO1 FLDVAL                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'  GET ADDRESS OF DAYPAK                
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),((R5),(R4)),BUDAYS,BUDAYNUM                            
         CLI   BUDAYS,0                                                         
         BNE   BRS28                                                            
         MVI   ERRCD,DAYERR                                                     
         GOTO1 ERROR                                                            
*                                                                               
BRS22    CLI   4(R1),BDPROGRM-BUYREC                                            
         BNE   BRSERR6                                                          
* EDIT PROGRAM NAME AND SAVE LENGTH                                             
         GOTO1 FLDVAL                                                           
         MVC   NERRCD,=Y(PGMERR)                                                
         LTR   R5,R5                                                            
         BNP   BRSERRX                                                          
         BCTR  R5,0                                                             
         CHI   R5,16                                                            
         BNH   *+8                                                              
         LA    R5,16               ONLY MOVE FOR UP TO 17                       
         EX    R5,*+4                                                           
         MVC   BUPROG(0),0(R4)                                                  
         STC   R5,BUPROG+17        SAVE 1 BYTE FOR COMPARE LENGTH-1             
         B     BRS28                                                            
*                                                                               
BRS24    GOTO1 CALLEDT                                                          
*                                                                               
BRS28    CLI   FSTOP,C','                                                       
         BE    BRS10                                                            
* FOR MULTISKED, ONLY INPUT IS FILTERS - ALWAYS DO DAY/TIME/PROG                
         CLC   =C'MS',BUTRCODE     TEST MULTISKED DISPLAY                       
         BNE   BRS30                                                            
         MVC   ELEM(12),SORTDAYS   SET DAYS                                     
         MVC   ELEM+6(6),SORTTIME  TIMES                                        
         MVC   ELEM+12(6),SORTPROG PROGRAM                                      
         B     BRS30                                                            
*                                                                               
SORTFLDS DS    0CL6                                                             
SORTDAYS DC    CL4'DAYS',AL1(BDSEDAY-BUYREC),AL1(1) USE SEDAY/NOT BDDAY         
SORTTIME DC    CL4'TIME',AL1(BDTIMST-BUYREC),AL1(4)                             
SORTDPT  DC    CL4'DPT ',AL1(BDDAYPT-BUYREC),AL1(1)                             
         DC    CL4'SLN ',AL1(BDSEC-BUYREC),AL1(1)                               
         DC    CL4'LEN ',AL1(BDSEC-BUYREC),AL1(1)                               
SORTPROG DC    CL4'PROG',AL1(BDPROGRM-BUYREC),AL1(17)                           
         DC    CL4'PRGM',AL1(BDPROGRM-BUYREC),AL1(17)                           
         DC    CL4'PGM ',AL1(BDPROGRM-BUYREC),AL1(17)                           
         DC    CL4'COST',AL1(BDCOST-BUYREC),AL1(3)                              
SORTFLDX EQU   *                                                                
*                                                                               
BRSERR1  MVC   NERRCD,=Y(NOSORT)     NO SORT SEQ SPECIFIED                      
         B     BRSERRX                                                          
*                                                                               
BRSERR2  MVC   NERRCD,=Y(BADSORT)    SORT ON DAY,TIM,DPT,SLN,PRG,COS            
         B     BRSERRX                                                          
*                                                                               
BRSERR3  MVC   NERRCD,=Y(DUPSORT)    CAN'T SORT ON SAME THING TWICE             
         B     BRSERRX                                                          
*                                                                               
BRSERR4  MVC   NERRCD,=Y(BADSTART)   STARTING LINE NUMBER NOT VALID             
         B     BRSERRX                                                          
*                                                                               
BRSERR5  MVC   NERRCD,=Y(NOBUYS)     NO BUYS TO DISPLAY                         
         B     BRSERRX                                                          
*                                                                               
BRSERR6  MVC   NERRCD,=Y(BADFILT)    FILTER NOT AVAILABLE                       
         MVC   ERRTEXT(4),0(R1)      MOVE FIELD NAME                            
         B     BRSERRX                                                          
*                                                                               
BRSERRX  MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
         EJECT                                                                  
*===================================================================            
* READ BUYLINES AND BUILD SORT STRINGS IN REC2 AND REC3                         
* START READING AT LINE NUMBER ENTERED                                          
*===================================================================            
         SPACE 1                                                                
BRS30    L     R0,AREC3                                                         
         LHI   R1,REC4-REC3                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R7,AREC3            SORT AREA                                    
*                                                                               
         LA    RE,REC                                                           
         ST    RE,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(12),SVKEY                                                    
         CLI   SVPOLPRD,0                                                       
         BE    *+10                                                             
         MVC   KEY+3(1),SVPOLPRD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/MKT/STA/EST                      
         BNE   BRSERR5                                                          
         B     BRS34                                                            
*                                                                               
BRS32    GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE                                                  
         BNE   BRS40                                                            
*                                                                               
BRS34    GOTO1 GETREC                                                           
*                                                                               
         CLI   BUDPT,0             CLEAR POTENTIAL FILTER VALUES                
         BE    *+14                                                             
         CLC   BDDAYPT,BUDPT                                                    
         BNE   BRS32   <=== WAS BL FOR SORT                                     
*                                                                               
         CLI   BUSLN,0                                                          
         BE    *+14                                                             
         CLC   BDSEC,BUSLN                                                      
         BNE   BRS32   <=== WAS BL FOR SORT                                     
*                                                                               
         CLI   BUDAYS,0                                                         
         BE    BRS34A                                                           
         SR    R0,R0                                                            
         IC    R0,BUDAYS                                                        
         SR    R1,R1                                                            
         IC    R1,BDDAY                                                         
         NR    R0,R1               TEST ANY DAYS IN COMMON                      
         BZ    BRS32                                                            
*                                                                               
BRS34A   OC    BUCOST,BUCOST                                                    
         BZ    *+14                                                             
         CLC   BDCOST,BUCOST                                                    
         BNE   BRS32    <=== WAS BL FOR SORT                                    
*                                                                               
         OC    BUTIME,BUTIME                                                    
         BZ    BRS35                                                            
*                                                                               
         MVC   DUB(4),BUTIME                                                    
         CLC   DUB(2),DUB+2        IF START TIME=END TIME                       
         BNE   *+10                THE USER IS AN IDIOT                         
         XC    DUB+2(2),DUB+2      AND GET RID OF THE END TIME                  
         OC    DUB+2(2),DUB+2      TEST NO END TIME                             
         BNZ   *+10                                                             
******** MVC   DUB+2(2),=AL2(3000) MAKE FILTER END TIME HIGH                    
         MVC   DUB+2(2),DUB        SET END TIME = START TIME                    
         LA    R1,DUB                                                           
         BRAS  RE,BRFIXTIM                                                      
*                                                                               
         MVC   DUB+4(4),BDTIMST                                                 
         OC    DUB+6(2),DUB+6      TEST NO END TIME                             
         BNZ   *+10                                                             
         MVC   DUB+6(2),DUB+4                                                   
         LA    R1,DUB+4                                                         
         BRAS  RE,BRFIXTIM                                                      
* DUB=FILTER START/END, DUB+4=BUY START/END                                     
         CLC   DUB+2(2),DUB+4      FILTER END BEFORE BUY START                  
         BL    BRS32                                                            
         CLC   DUB+0(2),DUB+6      FILTER START AFTER BUY END                   
         BH    BRS32                                                            
*                                                                               
BRS35    CLI   BUPROG,0            TEST ANY PROGRAM FILTER                      
         BE    BRS36               NO                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BUPROG+17        GET EX LEN FOR PROG COMPARE                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BDPROGRM(0),BUPROG                                               
         BNE   BRS32     <<< WAS BL FOR SORT                                    
*                                                                               
BRS36    MVC   0(1,R7),KEY+12      MOVE LINE NUM TO COL 0                       
         LA    R7,1(R7)                                                         
*                                                                               
         LA    R6,ELEM             POINT TO FIELD LIST                          
*                                                                               
BRS38    SR    R4,R4                                                            
         IC    R4,4(R6)            GET FIELD DSPL                               
         A     R4,AREC             POINT TO FIELD                               
         SR    R5,R5                                                            
         IC    R5,5(R6)            GET FIELD LEN                                
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R4)       MOVE STRING TO SORT AREA                     
*                                                                               
         LA    R7,1(R5,R7)         NEXT FIELD POSN                              
         LA    R6,6(R6)            NEXT FIELD                                   
         CLI   0(R6),0             TEST EOL                                     
         BNE   BRS38                                                            
         B     BRS32                                                            
         SPACE 1                                                                
*=============================================================                  
* SORT THE DATA                                                                 
*=============================================================                  
         SPACE 1                                                                
BRS40    LHI   R0,1                LINE NUM IS FIRST BYTE                       
         SR    RF,RF               ADD 1 FOR LINE NUM                           
         LA    R6,ELEM                                                          
*                                                                               
BRS42    IC    RF,5(R6)            FIELD LEN                                    
         AR    R0,RF                                                            
         LA    R6,6(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   BRS42                                                            
*                                                                               
         MVC   DMCB+4(4),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 VCALLOV,DMCB,0                                                   
         ICM   RF,15,0(R1)                                                      
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,AREC3            START OF SORT STRINGS                        
         SR    R7,RE               GIVES LENGTH OF SORT DATA                    
         BZ    BRSERR5             NOTHING PASSED FILTERS                       
         SR    R6,R6                                                            
         DR    R6,R0               GIVES NUMBER OF RECORDS IN R7                
         ST    R7,DMCB+4                                                        
*                                                                               
         ST    R0,DMCB+8           SET RECORD LENGTH                            
         BCTR  R0,0                                                             
         ST    R0,DMCB+12          SET KEYLEN (=RECLEN-1)                       
         LA    R0,1                                                             
         ST    R0,DMCB+16          SET DSPL OF KEY IN REC                       
*                                                                               
         GOTO1 (RF),DMCB,AREC3                                                  
* SAVE SORTED LIST                                                              
         LHI   R4,SVSORT-BUYSAVE                                                
         AR    R4,RA                                                            
         XC    0(256,R4),0(R4)                                                  
         LA    R4,1(R4)            +0 IS DISPLAY START                          
*                                                                               
         L     R1,AREC3            POINT TO SORTED DATA                         
*                                                                               
BRS50    MVC   0(1,R4),0(R1)       MOVE BUYLINE NUMBER                          
         LA    R4,1(R4)                                                         
         A     R1,DMCB+8           ADD RECORD LEN                               
         CLI   0(R1),0             TEST EOL                                     
         BNE   BRS50                                                            
* FIND THIS LINE NUMBER IN LIST                                                 
BRS51    LHI   R5,SVSORT-BUYSAVE                                                
         AR    R5,RA                                                            
         LA    R4,1(R5)            POINT TO FIRST                               
         CLI   0(R5),0             TEST ANY PREVIOUS                            
         BE    BRS61               NO                                           
*                                                                               
BRS52    CLC   0(1,R5),0(R4)       FIND FIRST IN LIST                           
         BE    BRS61                                                            
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   BRS52                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
*============================================================                   
* NOW READ THE BUYS AND DISPLAY THEM                                            
*============================================================                   
         SPACE 1                                                                
BRS60    LHI   R4,SVSORT-BUYSAVE                                                
         AR    R4,RA                                                            
         CLI   0(R4),0             TEST ANY PREVIOUS                            
         BNZ   *+8                 YES                                          
         LA    R4,1(R4)                                                         
*                                                                               
BRS61    MVC   SVSORTST,0(R4)      SAVE FIRST DISPLAYED LINENUM                 
*                                                                               
BRS62    XC    KEY,KEY                                                          
         MVC   KEY(10),SVKEY                                                    
         MVC   KEY+12(1),0(R4)                                                  
*                                                                               
         CLI   SVPOLPRD,0          TEST BRAND POL                               
         BNE   BRS64               YES                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BRS66                                                            
         DC    H'0'                                                             
*                                                                               
BRS64    MVC   KEY+3(1),SVPOLPRD   BRAND POL KEYS                               
         MVI   KEY+10,X'FF'                                                     
         MVI   KEY+11,0                                                         
         MVC   KEY+12(1),0(R4)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BRS66                                                            
         DC    H'0'                                                             
*                                                                               
BRS66    GOTO1 GETREC                                                           
*                                                                               
         GOTO1 CALLDSP                                                          
         MVI   SVRCLOPT,C'S'       SET SORTED RECALL ACTIVE                     
         MVI   BYTE2,C'Y'          INDICATE DATA DISPLAYED                      
*                                                                               
         LHI   RE,SVSORT-BUYSAVE                                                
         AR    RE,RA                                                            
         MVC   0(1,RE),0(R4)       SAVE LAST LINE DISPLAYED                     
         LA    R4,1(R4)                                                         
*                                                                               
         CLI   ERRCD,X'FE'         TEST NO MORE ROOM                            
         BE    BRSX                                                             
*                                                                               
         CLI   0(R4),0             TEST NO MORE TO DISPLAY                      
         BNE   BRS62                                                            
*                                                                               
BRSX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* CALL MIS                                                                      
*============================================================                   
         SPACE 1                                                                
GOMIS    NTR1  BASE=*,LABEL=*                                                   
* DELETE DAYPART VALUE IF ANY                                                   
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVSPDPT                                
*                                                                               
         CLI   SVDPTOPT,0          ANY DAYPART FILTER ACTIVE                    
         BE    GOMIS20             NO                                           
         GOTO1 (RF),(R1),=C'PUTD',SVDPTOPT,1,GLVSPDPT                           
*                                                                               
* BUILD TRANSFER CONTROL ELEMENT AND EXIT !                                     
*                                                                               
GOMIS20  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'BUY'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXTOPR,=C'MIS'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         MVC   BUYMSG(22),=C'** BACK TO SPOT BUY **'                            
         J     EXIT                                                             
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*========================================================*                      
* BUILD LIST OF NETWORKS BOUGHT FOR THIS STATION BECAUSE *                      
*  IT'S JUST TOO DIFFICULT BEING IN SYSTEMS              *                      
* 4 BYTES FOR EACH STATION                               *                      
*========================================================*                      
         SPACE 1                                                                
BRNL     NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRCD,BADOPTN                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   BRNLERR                                                          
         CLI   BUYMD,C'N'                                                       
         BNE   BRNLERR                                                          
         OC    SVNDEF(16),SVNDEF                                                
         BNZ   BRNLERR                                                          
         L     R1,AREC3                                                         
         LA    R0,4                                                             
         XC    0(256,R1),0(R1)     CLEAR TABLE BUILD AREA                       
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC KEY(8),SVKEY          MOVE A-M/CLT/PRD/MKT/STA(2)                  
*                                                                               
BRNL2    GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BRNL20                                                           
*                                                                               
         MVC   KEY+9(1),SVKEY+9     READ FOR EST                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(10),KEYSAVE     DID WE FIND THE ESTIMATE                     
         BNE   BRNL10                                                           
* FLAG ENTRY IN TABLE                                                           
         SR    R0,R0                                                            
         IC    R0,KEY+8            GET NETWORK NUMBER                           
         LR    R1,R0                                                            
         SLL   R1,2                X 4                                          
         A     R1,AREC3                                                         
         ST    R0,0(R1)            STORE NETWORK NUMBER                         
*                                                                               
BRNL10   MVC   KEY,KEYSAVE         RESTORE LAST STATION WE READ FOR             
         MVC   KEY+9(4),=4X'FF'    AND FORCE THE NEXT ONE                       
         B     BRNL2                                                            
*                                                                               
BRNL20   MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(AINTNONE)                                            
         LA    R0,4                                                             
         L     R1,AREC3                                                         
*                                                                               
BRNL22   OC    0(256,R1),0(R1)                                                  
         BNZ   BRNL30                                                           
         LA    R1,256(R1)                                                       
         BCT   R0,BRNL22                                                        
BRNLERR  LA    R0,1                SET CC NEQ                                   
         B     BRNLX                                                            
*                                                                               
* DECODE NETWORK NUMBERS VIA PASSIVE NETWORK POINTERS                           
*                                                                               
BRNL30   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D91'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         GOTO1 HIGH                                                             
         B     BRNL34                                                           
*                                                                               
BRNL32   GOTO1 SEQ                                                              
*                                                                               
BRNL34   CLC   KEY(4),KEYSAVE                                                   
         BNE   BRNL36                                                           
         SR    RE,RE                                                            
         IC    RE,KEY+4            NETWORK SEQNUM                               
         SLL   RE,2                X 4                                          
         A     RE,AREC3            POINT TO ENTRY                               
         OC    0(4,RE),0(RE)                                                    
         BZ    *+10                                                             
         MVC   0(4,RE),KEY+5       MOVE NETWORK CALL LETTERS                    
         B     BRNL32                                                           
*                                                                               
BRNL36   MVI   RCLOPT,RCLNLST      SET RECALL OPTION FOR DISPLAY                
         GOTO1 CALLDSP                                                          
         SR    R0,R0               SET CC EQ                                    
*                                                                               
BRNLX    LTR   R0,R0               SET CC                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* FIND A SPOT THAT MATCHES DATE IN BUELDT AND                    *              
*                  AND     COST IN BUELCOS IF BUELPRSW=X'20'     *              
*================================================================*              
         SPACE 1                                                                
FSPOT    NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         CLI   BUYREC+3,X'FF'      TEST POL BUY                                 
         BNE   FSPOT10                                                          
*                                                                               
         LA    R6,BDELEM                                                        
FSPOT2   BRAS  RE,NEXTEL                                                        
         BNE   FSPOTNO                                                          
*                                                                               
         CLI   1(R6),10            TEST UNALL                                   
         BNH   FSPOT2                                                           
         TM    6(R6),X'C0'         TEST MINUS/MINUSSED                          
         BNZ   FSPOT2                                                           
         CLC   BUELDT,2(R6)                                                     
         BL    FSPOTNO                                                          
         BH    FSPOT2                                                           
* DATES AGREE                                                                   
         TM    BUELPRSW,X'20'      TEST COST SPECIFIED                          
         BZ    FSPOTYES            NO - SPOT FOUND                              
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
*                                                                               
         OC    BUYKEY+4(2),BUYKEY+4  TEST CANAD NTWK BUY                        
         BZ    FSPOT4                YES - MATCH BUYLINE COST ONLY              
*                                                                               
         TM    6(R6),X'20'         TEST COST OVERRIDE THIS SPOT                 
         BZ    *+8                                                              
         ICM   R0,7,7(R6)          GET COST OVERRIDE                            
*                                                                               
FSPOT4   CLM   R0,7,BUELCOS        MATCH COSTS                                  
         BE    FSPOTYES            YES - DONE                                   
         B     FSPOT2              NO - CONTINUE                                
*                                                                               
FSPOT10  MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'07'                                                     
*                                                                               
         LA    R6,BDELEM                                                        
FSPOT12  BRAS  RE,NEXTEL                                                        
         BNE   FSPOTNO                                                          
*                                                                               
         CLC   BUELDT,2(R6)                                                     
         BL    FSPOTNO                                                          
         BH    FSPOT12                                                          
         TM    BUELPRSW,X'20'                                                   
         BZ    FSPOTYES                                                         
         CLC   BUELCOS,BDCOST                                                   
         BE    FSPOTYES                                                         
         B     FSPOT12                                                          
*                                                                               
FSPOTYES CR    RB,RB                                                            
         B     *+6                                                              
FSPOTNO  LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* ADJUST PERIOD START DATE BY NUMBER OF WEEKS IN R0              *              
*================================================================*              
                                                                                
ADJPER   NTR1  BASE=*,LABEL=*                                                   
         XC    BUDATA,BUDATA                                                    
         GOTO1 VDATCON,DMCB,SVSTART,(2,BUDATA)                                  
*                                                                               
         LA    R4,BUDATA+2                                                      
         MVC   WORK(6),SVSTART                                                  
         GOTO1 VGETDAY,DMCB,SVSTART,WORK+6                                      
         CLI   0(R1),1             TEST MONDAY                                  
         BE    ADJPER4                                                          
         SR    R5,R5                                                            
         IC    R5,0(R1)                                                         
         BCTR  R5,0                NUMBER OF DAYS TO BACK UP                    
         LCR   R5,R5                                                            
         GOTO1 VADDAY,DMCB,SVSTART,WORK,(R5)  BACK UP TO MONDAY                 
*                                                                               
ADJPER4  GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'   AND GET NEXT MONDAY               
         MVC   WORK(6),WORK+6                                                   
         GOTO1 VDATCON,DMCB,WORK,(2,(R4))                                       
         CLC   WORK(6),SVEND                  TEST PAST EST END                 
         BNL   ADJPER6                                                          
         LA    R4,2(R4)                                                         
         B     ADJPER4                                                          
*                                                                               
ADJPER6  XC    0(2,R4),0(R4)       PREVIOUS WEEK IS LAST                        
*                                                                               
         LR    R5,R4               SAVE ADDR OF LAST TABLE ENTRY                
         AHI   R5,-2                                                            
         LA    R4,BUDATA                                                        
*                                                                               
ADJPER8  CLC   0(2,R4),SVSKPER     FIND CURRENT WEEK IN TABLE                   
         BE    ADJPER10                                                         
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNZ   ADJPER8                                                          
         DC    H'0'                                                             
*                                                                               
ADJPER10 LTR   R0,R0                                                            
         BM    ADJPER20                                                         
         LR    RE,R0               GET NUMBER OF WEEKS TO ADVANCE               
         AR    RE,RE               X 2                                          
         AR    RE,R4               POINT TO WEEK                                
         MVC   SVSKPER(2),0(RE)    AND SET AS DISPLAY START                     
         LA    RF,28(RE)           POINT 14 WEEKS LATER (MAX DISPLAY)           
         CR    RF,R5               TEST PAST END OF TABLE                       
         BNH   ADJPERX                                                          
         LR    RF,R5                                                            
         AHI   RF,-26              BACK UP 13 MORE WEEKS FROM END               
         MVC   SVSKPER(2),0(RF)    AND USE AS START DATE                        
*                                                                               
         LA    RE,BUDATA           MAKE SURE NOT BEFORE TABLE START             
         CR    RF,RE                                                            
         BNL   *+10                                                             
         MVC   SVSKPER(2),BUDATA   ELSE USE EST START DATE                      
         B     ADJPERX                                                          
*                                                                               
ADJPER20 LR    RE,R0               GET NUMBER OF WEEKS TO BACK UP               
         AR    RE,RE               X 2                                          
         AR    RE,R4               POINT TO WEEK                                
         LA    RF,BUDATA                                                        
         CR    RE,RF               TEST PRIOR TO EST START                      
         BH    *+8                 NO                                           
         LA    RE,BUDATA           USE EST START DATE                           
         MVC   SVSKPER(2),0(RE)                                                 
*                                                                               
ADJPERX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* READ BUY HISTORY RECORD                                        *              
*================================================================*              
                                                                                
K        USING HISTRECD,WORK                                                    
*                                                                               
BRHIST   NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         MVI   K.HISTTYP,HISTTYQ                                                
         MVI   K.HISTSTYP,HISTSTYQ                                              
         MVC   K.HISTBUYK(12),SVKEY  MOVE 12 BYTES ONLY                         
         MVC   WORK2,WORK                                                       
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WORK2,WORK                   
         CLC   WORK(31),WORK2                                                   
         BE    BRHIST10                                                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOHIST)                                                
         GOTO1 ERROR                                                            
*                                                                               
BRHIST10 GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'XSPFIL',K.HISDDA,AREC2,     X        
               DMWORK                                                           
         XIT1                                                                   
         DROP  K                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE SPGENHIST                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086SPBUY03   05/16/13'                                      
         END                                                                    
