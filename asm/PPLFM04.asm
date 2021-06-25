*          DATA SET PPLFM04    AT LEVEL 051 AS OF 05/01/02                      
*PHASE T40404A,+0                                                               
         TITLE 'T40404 - PRINTPAK - BILLING FORMULA MAINTENANCE'                
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 07/24/97  DISALLOW BILL TYPE AT ED22 IF NO COMMENT ENTERED               
*                                                                               
* SMYE 07/23/97  FIX BUG IN ROUTINE TO LEFT-JUSTIFY COMMENT NUMBER              
*                ENTRY AT DISP9                                                 
*                                                                               
* SMYE 12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                     
*                                                                               
* BPLA 10/26/94  ALLOW FOR BILL TYPES 4567 - DISPLAY AS ALL                     
*                                                                               
* SMUR 8/2/94    ADD BILL TYPES FIELD                                           
*                                                                               
* BPLA 9/6/91    ONLY ALLOW "SHOW ON BILL AS" DATA IF REAL ADJ IS A.C.          
*                                                                               
* BPLA 5/21/91   CODE FOR CD SEP AND REG/DST SEP AND AGY OF RECORD FEE          
*                REMOVED, CODE FOR BILPADJ AND BILPBASB ADDED                   
*                "SHOW ON BILL AS" DATA                                         
*                                                                               
T40404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40404                                                         
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T404FFD,RA                                                       
         SPACE 2                                                                
         CLI   SCRNUM,X'F4'        TEST ALREADY HAVE SCREEN                     
         BE    BILL2                                                            
*                                  FETCH VIRGIN SCREEN                          
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F4'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SCRNUM,X'F4'                                                     
BILL2    DS    0H                                                               
*                                  SELECT PRD OR EST                            
         LA    R8,PPRDBILP                                                      
         MVC   KEY+27(4),PRDADDR                                                
         CLI   BREC,6                                                           
         BE    *+14                                                             
         LA    R8,PESTBILP                                                      
         MVC   KEY+27(4),ESTADDR                                                
         BAS   RE,GETREC                                                        
*                                                                               
         USING BILPROF,R8                                                       
         CLI   BACT,5              DISB                                         
         BE    DISP                                                             
         CLI   DSPSW,1             FIRST CHANGE = DISP                          
         BNE   EDTSCRN                                                          
         EJECT                                                                  
*                                  DISPLAY                                      
DISP     DS    0H                                                               
         MVC   WORK(1),BILBASA                                                  
         CLI   BILCMSW,C'C'        COMMISSION ONLY                              
         BNE   *+8                                                              
         OI    WORK,X'10'                                                       
         CLI   BILNBSW,C'Y'        NOT FOR BILLING                              
         BNE   *+8                                                              
         OI    WORK,X'20'                                                       
         GOTO1 FMTBAS,DMCB,WORK,BSCBASAH       BASE A                           
*                                                                               
         GOTO1 (RF),(R1),BILBASB,BSCBASBH        BASE B                         
*                                                                               
         MVC   BSCADJ,SPACES                                                    
         OC    BILADJ,BILADJ                                                    
         BNZ   DISP2               HAVE ADJ                                     
         CLI   BILBASA,0           SEE IF I HAVE FORMULA                        
         BE    DISP4               NO                                           
         MVC   BSCADJ(2),=C'+0'                                                 
         B     DISP4                                                            
*                                                                               
DISP2    DS    0H                                                               
         EDIT  (B3,BILADJ),(8,BSCADJ+1),4,ALIGN=LEFT                            
*                                                                               
         OI    BSCADJ+6,C'0'                                                    
         OI    BSCADJ+7,C'0'                                                    
         OI    BSCADJ+8,C'0'                                                    
         LA    RF,BSCADJ+8                                                      
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
*                                                                               
         MVI   BSCADJ,C'+'                                                      
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   BSCADJ,C'-'                                                      
DISP4    DS    0H                                                               
         FOUT  BSCADJH                                                          
*                                                                               
         MVC   BSCPADJ,SPACES                                                   
         OC    BILPADJ,BILPADJ                                                  
         BNZ   DISP4E              HAVE ADJ                                     
         CLI   BILPBASB,0          SEE IF I HAVE FORMULA OVERRIDE               
         BE    DISP4X              NO                                           
         MVC   BSCPADJ(2),=C'+0'                                                
         B     DISP4X                                                           
*                                                                               
DISP4E   DS    0H                                                               
         LA    R6,BSCPADJ+1                                                     
         EDIT  (B3,BILPADJ),(8,0(R6)),4,ALIGN=LEFT                              
*                                                                               
         OI    BSCPADJ+6,C'0'                                                   
         OI    BSCPADJ+7,C'0'                                                   
         OI    BSCPADJ+8,C'0'                                                   
         LA    RF,BSCPADJ+8                                                     
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
*                                                                               
         MVI   BSCPADJ,C'+'                                                     
         TM    BILPADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   BSCPADJ,C'-'                                                     
DISP4X   DS    0H                                                               
         FOUT  BSCPADJH                                                         
         GOTO1 FMTBAS,DMCB,BILPBASB,BSCPBASH                                    
*                                                                               
         MVC   BSCADAT,SPACES                ADJUSTMENT EFF DATE                
         OC    BILADAT,BILADAT                                                  
         BZ    DISP5                                                            
*        GOTO1 VDTCNV,DMCB,(1,BILADAT),(5,BSCADAT)                              
         GOTO1 VDATCON,DMCB,(3,BILADAT),(9,BSCADAT)                             
*                                                                               
DISP5    DS    0H                                                               
         FOUT  BSCADATH                                                         
*                                                                               
         MVC   BSCDETS,BILDETS                    DETAILS ON BILL               
         FOUT  BSCDETSH                                                         
*                                                                               
*                                                                               
         LA    R4,BILCMNTS                        COMMENTS                      
         LA    R2,BSCCOM1H                                                      
         LA    R6,3                               UP TO 3                       
DISP9    DS    0H                                                               
*****    OC    0(7,R4),0(R4)                                                    
         OC    1(6,R4),1(R4)       IF COMMENT NUMBER PRESENT                    
         BNZ   DISP10              GO LEFT JUSTIFY IT                           
         MVC   8(6,R2),SPACES      CLEAR COMMENT NUMBER                         
         FOUT  (R2)                                                             
         BAS   RE,BUMPFLD                                                       
         MVC   8(3,R2),SPACES      CLEAR CODE(S)                                
         FOUT  (R2)                                                             
         BAS   RE,BUMPFLD                                                       
         MVC   8(4,R2),SPACES      CLEAR BILL TYPES                             
*****    B     DISP11                                                           
         B     DISP12A                                                          
DISP10   DS    0H                                                               
         MVC   8(6,R2),1(R4)       COMMENT NUM                                  
*                                  LEFT JUSTIFY                                 
DISP10B  DS    0H                                                               
         CLI   8(R2),C' '                                                       
         BH    DISP10D                                                          
         MVC   8(5,R2),9(R2)                                                    
         MVI   8+5(R2),C' '                                                     
         B     DISP10B                                                          
DISP10D  DS    0H                                                               
         FOUT  (R2)                                                             
         BAS   RE,BUMPFLD                                                       
         MVC   8(3,R2),SPACES                                                   
         LA    RE,8(R2)                                                         
         TM    0(R4),X'80'                                                      
         BZ    *+12                                                             
         MVI   0(RE),C'R'          REGULAR                                      
         LA    RE,1(RE)                                                         
         TM    0(R4),X'40'                                                      
         BZ    *+12                                                             
         MVI   0(RE),C'C'          CD                                           
         LA    RE,1(RE)                                                         
         TM    0(R4),X'20'                                                      
         BZ    *+12                                                             
         MVI   0(RE),C'A'          ADJ                                          
         LA    RE,1(RE)                                                         
*                                                                               
         FOUT  (R2)                                                             
DISP11   DS    0H                                                               
         BAS   RE,BUMPFLD          BILL TYPES FLD                               
         MVC   8(4,R2),SPACES                                                   
         TM    0(R4),X'EE'         WAS X'E0'                                    
         BZ    DISP12A             NO COMMENT AND NO BILLTYPE                   
         TM    0(R4),X'0F'                                                      
         BZ    DISP12                                                           
         LA    RE,8(R2)                                                         
         TM    0(R4),X'01'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'4'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'02'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'5'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'04'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'6'                                                       
         LA    RE,1(RE)                                                         
         TM    0(R4),X'08'                                                      
         BO    *+12                                                             
         MVI   0(RE),C'7'                                                       
         LA    RE,1(RE)                                                         
         B     *+10                                                             
*                                                                               
DISP12   MVC   8(3,R2),=C'ALL'                                                  
DISP12A  FOUT  (R2)                TRANSMIT                                     
         BAS   RE,BUMPFLD                                                       
         LA    R4,7(R4)                                                         
         BCT   R6,DISP9                                                         
*                                                                               
         CLI   BACT,4                                                           
         BE    DISPX                                                            
         MVI   DONESW,1            SET ACTION COMPLETE                          
         LA    R2,HDRACTH          ON DISPLAY - CURSOR TO ACTION                
         B     EXIT                                                             
*                                                                               
DISPX    LA    R2,BSCBASAH         POSITION CURSOR FOR INPUT                    
         B     EXIT                                                             
         EJECT                                                                  
*                                  EDIT                                         
         SPACE 2                                                                
EDTSCRN  DS    0H                                                               
         NI    BSCBASAH+6,X'BF'    SET OFF CURSOR                               
         XC    BILPROF,BILPROF                                                  
         MVI   BILCMSW,0                                                        
         MVI   BILNBSW,0                                                        
*                                  TEST FORMULA INPUT                           
         LA    R2,BSCBASAH                                                      
         CLI   BSCBASAH+5,0                                                     
         BNE   ED2                                                              
         CLI   BSCBASBH+5,0                                                     
         BNE   ED2                                                              
         CLI   BSCADJH+5,0                                                      
         BE    ED8                                                              
         CLI   BSCADATH+5,0                                                     
         BE    ED8                 NO FORMULA                                   
ED2      DS    0H                                                               
         CLI   CPROFLE+10,C'0'                                                  
         BNE   *+12                                                             
         LA    R3,INCMPERR         CLT PROF NOT COMPATIBLE                      
         B     ERROR                                                            
         GOTO1 EDBAS,DMCB,BILBASA,(R2)            BASE A                        
*                                                                               
         LA    R3,INVERR                                                        
         CLI   BILBASA,X'FF'                                                    
         BE    ERROR                                                            
         TM    BILBASA,X'10'          CHK FOR COMMISSION ONLY                   
         BNO   *+12                                                             
         NI    BILBASA,X'EF'       SET OFF X'10' BIT                            
         MVI   BILCMSW,C'C'                                                     
         TM    BILBASA,X'20'          CHK IF NOT FOR BILLING                    
         BNO   *+12                                                             
         NI    BILBASA,X'DF'       SET OFF X'20' BIT                            
         MVI   BILNBSW,C'Y'                                                     
ED2B     DS    0H                                                               
         LA    R2,BSCBASBH                                                      
         GOTO1 (RF),(R1),BILBASB,(R2)             BASE B                        
*                                                                               
         LA    R3,INVERR                                                        
         CLI   BILBASB,X'FF'                                                    
         BE    ERROR                                                            
         TM    BILBASB,X'10'       COMMISSION ONY                               
         BO    ERROR               INVALID ON BILBASB                           
         TM    BILBASB,X'20'       NOT FOR BILLING                              
         BO    ERROR               INVALID ON BILBASB                           
         LA    R2,BSCADJH                         ADJUSTMENT                    
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         LA    R3,MISSERR                                                       
         B     ERROR                                                            
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 VCASHVAL,DMCB,(4,BSCADJ+1),(R0)                                  
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
ED3      DS    0H                                                               
         LA    R3,ADJERR                                                        
         B     ERROR                                                            
         CLI   DMCB+4,0                                                         
         BNE   ED3                 DOESN'T FIT IN 3 BYTES                       
         L     R0,DMCB+4                                                        
         CLI   BSCADJ,C'+'                                                      
         BE    ED4                                                              
         CLI   BSCADJ,C'-'                                                      
         BNE   ED3                                                              
         LCR   R0,R0                                                            
*                                                                               
ED4      DS    0H                                                               
         ST    R0,FULL                                                          
         MVC   BILADJ,FULL+1                                                    
*                                                                               
ED5      DS    0H                                                               
**NEW 4/24/89                                                                   
         CLI   BREC,7              SEE IF ESTIMATE                              
         BNE   ED5D                                                             
         CLI   PESTRTYP,C'C'       SEE IF 'C' RATE ESTIMATE                     
         BNE   ED5D                                                             
         CLI   BILCMSW,C'C'        MUST BE COMMISSION ONLY                      
         BE    ED5D                                                             
         LA    R2,BSCBASAH                                                      
EDFERR   LA    R3,INVERR           INVALID FIELD                                
         B     ERROR                                                            
*                                                                               
ED5D     LA    R2,BSCADATH         ADJ DATE                                     
         CLI   5(R2),0                                                          
         BE    ED8                                                              
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,BSCADAT),WORK                                    
*                                                                               
         OC    DMCB(4),DMCB                                                     
         BNZ   *+12                                                             
ED6      DS    0H                                                               
         LA    R3,DATERR                                                        
         B     ERROR                                                            
         CLI   5(R2),6                                                          
         BH    ED6                                                              
*        GOTO1 VDTCNV,DMCB,WORK,(1,BILADAT)                                     
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BILADAT)                                
*                                                                               
***                                                                             
*** NOTE - BOTH OR NEITHER "SHOW AS" FIELDS MUST BE ENTERED                     
***                                                                             
ED8      DS    0H                                                               
         LA    R2,BSCPADJH              CHECK FOR "SHOW AS" ADJ                 
         CLI   5(R2),0                                                          
         BE    ED8K                                                             
         OC    BILADJ(3),BILADJ     FORMULA MUST HAVE ADJUSTMENT                
         BZ    ED8E                 TO ALLOW "SHOW AS" DATA                     
*                                                                               
         CLC   BSCPADJ(1),BSCADJ      MUST HAVE SAME SIGN +,-                   
         BNE   ED8E                                                             
*                                                                               
         TM    BILBASB,X'08'          REAL ADJ MUST BE AC                       
         BO    ED8B                                                             
         B     ED8E                                                             
*                                                                               
ED8B     SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 VCASHVAL,DMCB,(4,BSCPADJ+1),(R0)                                 
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
ED8E     DS    0H                                                               
         LA    R3,ADJERR                                                        
         B     ERROR                                                            
         CLI   DMCB+4,0                                                         
         BNE   ED8E                DOESN'T FIT IN 3 BYTES                       
         L     R0,DMCB+4                                                        
         LTR   R0,R0              CAN'T BE 0                                    
         BZ    ED8E                                                             
         CLI   BSCPADJ,C'+'                                                     
         BE    ED8G                                                             
         CLI   BSCPADJ,C'-'                                                     
         BNE   ED8E                                                             
         LCR   R0,R0                                                            
*                                                                               
ED8G     DS    0H                                                               
         ST    R0,FULL                                                          
         MVC   BILPADJ,FULL+1                                                   
*                                                                               
ED8K     DS    0H                                                               
         LA    R2,BSCPBASH          "SHOW AS" PCT OF BASE                       
         CLI   5(R2),0                                                          
         BE    ED8X                                                             
         OC    BILADJ(3),BILADJ    FORMULA MUST HAVE ADJUSTMENT                 
         BZ    EDFERR              TO ALLOW "SHOW AS" DATA                      
         GOTO1 EDBAS,DMCB,BILPBASB,(R2)                                         
*                                                                               
         CLI   BILPBASB,X'06'        MUST BE G,N,G-CD OR N-CD                   
         BH    EDFERR                                                           
*                                                                               
         CLI   BSCPADJH+5,0          IF "SHOW AS" OF FIELD ENTERED              
         BNE   ED9                                                              
         LA    R2,BSCPADJH           REQUIRE "SHOW AS" ADJ                      
         LA    R3,MISSERR                                                       
         B     ERROR                                                            
*                                                                               
ED8X     DS    0H                                                               
         CLI   BSCPADJH+5,0           SEE IF "SHOW AS" ADJ ENTERED              
         BE    ED9                                                              
         LA    R3,MISSERR             REQUIRE "SHOW AS" OF                      
         B     ERROR                                                            
*                                                                               
ED9      DS    0H                                                               
*                                                                               
ED11     DS    0H                                                               
         LA    R2,BSCDETSH              COLUMNS TO PRINT ON BILL                
         CLI   5(R2),0                                                          
         BE    ED12                                                             
         MVC   BILDETS,BSCDETS                                                  
         LA    R3,INVERR                                                        
         CLI   BSCDETS,C'A'                                                     
         BL    ERROR                                                            
*                                                                               
ED12     DS    0H                                                               
*                                                                               
ED14     DS    0H                                                               
         LA    R2,BSCCOM1H          COMMENTS                                    
         LA    R4,BILCMNTS                                                      
         LA    R6,3                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),HDRMED                                                  
ED15     MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),SPACES                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    ED17                                                             
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1                                                            
         AH    RF,=H'6'                                                         
         LA    RF,KEY+4(RF)                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)       MOVE TO KEY+4+(6-L)                          
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ED16                                                             
*                                                                               
         LA    R3,CMNTERR          COMMENT NOT ON FILE                          
         B     ERROR                                                            
*                                                                               
ED16     DS    0H                                                               
         MVC   1(6,R4),KEY+4                                                    
*                                                                               
ED17     DS    0H                                                               
         BAS   RE,BUMPFLD                                                       
         LA    R3,INVERR                                                        
         CLI   5(R2),0                                                          
         BNE   ED18                                                             
*                                                                               
         OC    1(6,R4),1(R4)                                                    
         BNE   ERROR               COMMENT BUT NO CODES                         
         B     ED22                                                             
*                                                                               
ED18     DS    0H                                                               
         OC    1(6,R4),1(R4)                                                    
         BE    ERROR               CODES BUT NO COMMENT                         
         LA    R1,8(R2)                                                         
         LA    RF,3                                                             
ED18B    CLI   0(R1),C' '                                                       
         BNH   ED19                                                             
         CLI   0(R1),C'R'          REG                                          
         BNE   *+12                                                             
         OI    0(R4),X'80'                                                      
         B     ED19                                                             
         CLI   0(R1),C'C'          CD                                           
         BNE   *+12                                                             
         OI    0(R4),X'40'                                                      
         B     ED19                                                             
         CLI   0(R1),C'A'          ADJ                                          
         BNE   *+12                                                             
         OI    0(R4),X'20'                                                      
         B     ED19                                                             
*                                                                               
         B     ERROR                                                            
*                                                                               
ED19     DS    0H                                                               
         LA    R1,1(R1)                                                         
         BCT   RF,ED18B                                                         
*                                                                               
ED22     DS    0H                                                               
         BAS   RE,BUMPFLD          BILL TYPES FLD                               
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    ED26                                                             
         OC    1(6,R4),1(R4)                                                    
         BNZ   ED22B                                                            
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   ERROR               BILL TYPE BUT NO COMMENT                     
         MVC   8(4,R2),SPACES      CLEAR BILL TYPE IF ALL                       
         FOUT  (R2)                TRANSMIT                                     
         B     ED26                                                             
ED22B    CLC   8(3,R2),=C'ALL'                                                  
         BE    ED26                                                             
         LA    R1,8(R2)                                                         
*                                  CHECK FOR DUPLICATE ENTRIES                  
         CLC   0(1,R1),1(R1)                                                    
         BNE   *+12                                                             
         CLI   0(R1),C' '                                                       
         BH    ERROR                                                            
         CLC   0(1,R1),2(R1)                                                    
         BNE   *+12                                                             
         CLI   0(R1),C' '                                                       
         BH    ERROR                                                            
         CLC   1(1,R1),2(R1)                                                    
         BNE   *+12                                                             
         CLI   1(R1),C' '                                                       
         BH    ERROR                                                            
*                                                                               
         LA    RF,4                                                             
         OI    0(R4),X'0F'         WILL TURN THE BITS OFF                       
*                                  AS NEEDED ONE BY ONE                         
ED24     CLI   0(R1),C' '                                                       
         BNH   ED25                                                             
         CLI   0(R1),C'4'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'01'                                                      
         B     ED25                                                             
         CLI   0(R1),C'5'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'02'                                                      
         B     ED25                                                             
         CLI   0(R1),C'6'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'04'                                                      
         B     ED25                                                             
         CLI   0(R1),C'7'                                                       
         BNE   *+12                                                             
         XI    0(R4),X'08'                                                      
         B     ED25                                                             
*                                                                               
         B     ERROR                                                            
*                                                                               
ED25     DS    0H                                                               
         LA    R1,1(R1)                                                         
         BCT   RF,ED24                                                          
*                                                                               
ED26     BAS   RE,BUMPFLD                                                       
         LA    R4,7(R4)                                                         
         BCT   R6,ED15                                                          
*                                                                               
*                                                                               
         MVC   KEY+27(4),PRDADDR                                                
         CLI   BREC,6                                                           
         BE    *+10                                                             
         MVC   KEY+27(4),ESTADDR                                                
*                                                                               
         BAS   RE,PUTREC                                                        
         MVI   DONESW,1                                                         
         LA    R2,HDRACTH          CURSOR TO ACTION                             
         B     EXIT                                                             
         SPACE 3                                                                
BUMPFLD  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         BYPASS PROTECTED                             
         BZR   RE                                                               
         B     BUMPFLD                                                          
         SPACE 3                                                                
FMTBAS   NTR1                      EXPAND BASE CODE                             
         SPACE 2                                                                
         LM    R2,R3,0(R1)                                                      
         LA    R5,BASLST                                                        
         LA    R6,BASLSTN                                                       
FB2      DS    0H                                                               
         CLC   0(1,R2),0(R5)                                                    
         BE    FB3                                                              
         LA    R5,6(R5)                                                         
         BCT   R6,FB2                                                           
         MVC   8(5,R3),SPACES                                                   
         B     FB4                                                              
FB3      DS    0H                                                               
         MVC   8(5,R3),1(R5)                                                    
FB4      DS    0H                                                               
         FOUT  (R3)                                                             
         B     EXXMOD                                                           
         SPACE 3                                                                
EDBAS    NTR1                      EDIT BASE                                    
         SPACE 2                                                                
         LM    R2,R3,0(R1)                                                      
         OC    8(5,R3),SPACES                                                   
         LA    R5,BASLST                                                        
         LA    R6,BASLSTN                                                       
EB2      DS    0H                                                               
         CLC   8(5,R3),1(R5)                                                    
         BE    EB3                                                              
         LA    R5,6(R5)                                                         
         BCT   R6,EB2                                                           
         MVI   0(R2),X'FF'         ERROR                                        
         B     EB4                                                              
EB3      DS    0H                                                               
         MVC   0(1,R2),0(R5)                                                    
EB4      DS    0H                                                               
         B     EXXMOD                                                           
         SPACE 3                                                                
BASLST   DS    0C                                                               
         DC    X'01',C'G    '                                                   
         DC    X'02',C'N    '                                                   
         DC    X'05',C'G-CD '                                                   
         DC    X'06',C'N-CD '                                                   
         DC    X'08',C'AC   '                                                   
         DC    X'11',C'CG   '      PRECEED BASE WITH 'C' FOR COMM               
         DC    X'12',C'CN   '                                                   
         DC    X'15',C'CG-CD'                                                   
         DC    X'16',C'CN-CD'                                                   
         DC    X'18',C'CAC  '                                                   
         DC    X'21',C'EG   '      PRECEED WITH E IF NOT FOR BILLING            
         DC    X'22',C'EN   '       ('EST' ONLY)                                
         DC    X'25',C'EG-CD'                                                   
         DC    X'26',C'EN-CD'                                                   
         DC    X'28',C'EAC  '                                                   
BASLSTN  EQU   (*-BASLST)/6        NUMBER IN LIST                               
         SPACE 3                                                                
SPACES   DC    40C' '                                                           
         SPACE 3                                                                
*                                  ERROR EQUATES                                
MISSERR  EQU   1                                                                
INVERR   EQU   2                                                                
CMNTERR  EQU   53                                                               
INCMPERR EQU   179                                                              
ADJERR   EQU   180                                                              
DATERR   EQU   20                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF4D                                                       
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051PPLFM04   05/01/02'                                      
         END                                                                    
