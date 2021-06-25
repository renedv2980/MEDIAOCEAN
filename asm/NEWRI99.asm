*          DATA SET NEWRI99    AT LEVEL 001 AS OF 08/20/02                      
*PHASE T32099A,+0                                                               
*INCLUDE DDUCOM                                                                 
         TITLE 'GENERAL ROUTINES FOR NETWORK WRITERS'                           
**********************************************************                      
*                                                                               
*  FOR CROSS-AGY READS  NDCROSS  AND  DRATTRIB +2                               
*                                                                               
*  NDCROSS = SET FOR CROSS AGENCY READS IN NEWRI20                              
*            X'01' GENERAL READ                                                 
*            X'02' =@YAL                                                        
*            X'04' =@CHR                                                        
*            X'10' =@GI2 AND @RES                                               
*                                                                               
*  KEYWORD ATTRIBUTES                                                           
*                          DRATTRIB+2=X'FF' FOR ALL AGENCIES                    
*                          DRATTRIB+2=X'03' FOR YAL/ X'05' FOR CHR ETC.         
*                                                                               
* ************* TOMBSTONE ***************************                           
* NOV 24/98    PXZ    SKIP OUTPUT OF PHONY TARGET HEADS                         
* JUN 23/99    PXZ    3D DEMO TARGET CHANGES                                    
* NOV 9/99     PXZ    SET FLAGS FOR NEW DEMO EXTENSION                          
*************************************************************                   
T32099   CSECT                                                                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         USING *,RF                                                             
GEN      NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00A4A,RB,R7,R6,RA                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         LA    RA,2048(R6)                                                      
         LA    RA,2048(RA)                                                      
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         SPACE 1                                                                
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
* BRANCH TABLE BELOW RESOLVED IN NEWRI00                                        
* PORTION OF NEDRVBLKD COVERS THE TABLE BELOW                                   
VBRANCH  B     VVALOPTS                                                         
         B     VVALTITS                                                         
         B     VVALLEFT                                                         
         B     VVALRGHT                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         B     VVALOTH                                                          
         B     VVALPLN                                                          
         DC    16X'00'                                                          
         SPACE 1                                                                
         B     VINTDRIV                                                         
         B     VINTDRON                                                         
         B     VWRPDRON                                                         
         B     VINTHEAD                                                         
****     DC    8X'00'                                                           
         SPACE 1                                                                
         B     VGENHEAD                                                         
         B     VGETNAME                                                         
         B     VGETBM                                                           
         DC    16X'00'                                                          
         SPACE 1                                                                
         B     VERR                                                             
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VVALOPTS GOTO1 =A(OVERFLOW),DMCB,(0,(RC)),RR=RELO                               
         DS    0H                                                               
         XIT1                                                                   
VVALOTH  GOTO1 =A(OVERFLOW),DMCB,(1,(RC)),RR=RELO                               
         DS    0H                                                               
         XIT1                                                                   
*                                                                               
VVALUDEF NTR1                                                                   
         GOTO1 =A(OVERFLOW),DMCB,(3,(RC)),RR=RELO                               
         XIT1                                                                   
VVALUCOM NTR1                                                                   
         GOTO1 =A(OVERFLOW),DMCB,(4,(RC)),RR=RELO                               
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE PLAN                                         
         SPACE 3                                                                
VVALPLN  MVC   WORK(16),SYSFIL                                                  
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         CLI   5(R2),0             IS IT ALL                                    
         BE    VPL0                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VPL1                                                             
VPL0     XC    NDPLNCDE,NDPLNCDE                                                
         B     VPL2B                                                            
*                                                                               
VPL1     CLC   =C'F=',8(R2)        OR FILTER                                    
         BNE   VPL2                                                             
         MVC   NDPLNFLT,10(R2)                                                  
         XC    NDPLNCDE,NDPLNCDE                                                
         B     VPL2B                                                            
*                                                                               
VPL2     MVC   NDPLNCDE,8(R2)      A CODE                                       
         LA    R1,4                                                             
         LA    R3,NDPLNCDE                                                      
*                                                                               
VPLOOP   CLI   0(R3),0             SET END ZEROS TO BLANKS                      
         BNE   *+8                                                              
         MVI   0(R3),X'40'                                                      
         LA    R3,1(R3)                                                         
         BCT   R1,VPLOOP                                                        
VPL2B    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLKEY,R4                                                        
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,NBACTAM                                                   
         CLC   NBSELCLI,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   NPLKCLT,NBACTCLI                                                 
         MVC   NPLKNET,NBSELNET                                                 
         MVC   NPLKDPT,NBSELDP                                                  
         MVC   NPLKPLAN,NDPLNCDE                                                
         GOTO1 HIGH                                                             
         B     VPL3B                                                            
         SPACE 1                                                                
VPL3     GOTO1 SEQ                                                              
         SPACE 1                                                                
VPL3B    CLC   KEY(2),KEYSAVE      AGY/MED                                      
         BNE   BADPLAN                                                          
         CLC   NBSELCLI,=C'ALL'                                                 
         BE    *+14                                                             
         CLC   KEY+2(2),KEYSAVE+2                                               
         BNE   VPL3                                                             
         CLI   NBSELNET,0                                                       
         BE    *+14                                                             
         CLC   NPLKNET,KEYSAVE+5                                                
         BNE   VPL3                                                             
         CLI   NBSELDP,0                                                        
         BE    *+14                                                             
         CLC   NPLKDPT,KEYSAVE+10                                               
         BNE   VPL3                                                             
         OC    NDPLNCDE,NDPLNCDE                                                
         BZ    *+14                                                             
         CLC   NPLKPLAN,KEYSAVE+11                                              
         BNE   VPL3                                                             
         MVC   SYSFIL(16),WORK                                                  
*--IF NO START OR END DATES USE PLAN YEAR TO CREATE A DATE RANGE                
         OC    NDPLNCDE,NDPLNCDE   WAS SINGLE PLAN INPUTTED                     
         BZ    VPLEX                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFIL  ',KEY+21,AIO,DMWORK           
*                                                                               
         DROP  R4                                                               
*                                                                               
         XC    WORK,WORK                                                        
         L     RE,AIO                                                           
         USING NPLRECD,RE                                                       
         MVC   BYTE,NPLNYEAR                                                    
         MVC   WORK+27(6),=CL6'SEP30/'                                          
         EDIT  (1,BYTE),(2,WORK+33),ZERO=NOBLANK                                
*                                                                               
         MVC   WORK+20(5),=CL5'SEP1/'                                           
         ZIC   RF,BYTE                                                          
         BCTR  RF,0                                                             
         STCM  RF,1,BYTE                                                        
         EDIT  (1,BYTE),(2,WORK+25),ZERO=NOBLANK                                
*                                                                               
VPLEX    B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              VALIDATE TITLES                                                  
         SPACE 3                                                                
VVALTITS MVC   NDTITLE,SPACES                                                   
         CLI   NDFLAVOR,C'M'                                                    
         BNE   *+10                                                             
         MVC   NDTITLE(5),=C'MEDIA'                                             
         CLI   NDFLAVOR,C'E'                                                    
         BNE   *+10                                                             
         MVC   NDTITLE(8),=C'ESTIMATE'                                          
         CLI   NDFLAVOR,C'P'                                                    
         BNE   *+10                                                             
         MVC   NDTITLE(8),=C'POST-BUY'                                          
         CLI   NDFLAVOR,C'V'                                                    
         BNE   *+10                                                             
         MVC   NDTITLE(10),=C'EVALUATION'                                       
         CLC   NDTITLE,SPACES                                                   
         BE    VVALTIT2                                                         
         MVC   NDTITLE2,NDTITLE                                                 
         MVC   NDTITLE+12(6),=C'REPORT'                                         
         MVC   NDTITLE2+12(5),=C'RECAP'                                         
         GOTO1 SQUASHER,DMCB,NDTITLE,20                                         
         GOTO1 SQUASHER,DMCB,NDTITLE2,20                                        
         CLI   NDANYREC,C'Y'       IF THERE ARE NO RECAPS                       
         BE    VVALTIT2                                                         
         MVC   NDTITLE2,SPACES     SECOND TITLE IS FOR DETAILS                  
         SPACE 1                                                                
VVALTIT2 CLI   5(R2),0                                                          
         BE    VVALTIT4                                                         
         GOTO1 ANY                                                              
         MVC   NDTITLE,WORK                                                     
         SPACE 1                                                                
VVALTIT4 CLI   NDNAROPT,C'Y'                                                    
         BE    VVALTIT6                                                         
         GOTO1 CENTER,DMCB,NDTITLE,34                                           
         SPACE 1                                                                
VVALTIT6 BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VVALTIT8                                                         
         GOTO1 ANY                                                              
         MVC   NDTITLE2,WORK                                                    
         SPACE 1                                                                
VVALTIT8 CLI   NDNAROPT,C'Y'                                                    
         BE    XIT                                                              
         GOTO1 CENTER,DMCB,NDTITLE2,34                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LEFT HAND HEADERS                                       
         SPACE 3                                                                
VVALLEFT LA    R1,NDWDR1           NOT CHECKING REPORT WIDTH YET                
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,NDWRR1                                                        
         XC    0(8,R1),0(R1)                                                    
         MVI   NDTOTWID,0                                                       
         MVI   NDCURLEV,1                                                       
         LA    R3,4                MAX 4 FIELDS                                 
         LA    R4,4                (START ON HEAD 4)                            
         SPACE 1                                                                
VVL2     CLI   5(R2),0                                                          
         BE    VVL4                                                             
         MVI   NDMYPOSO,C'H'                                                    
         STC   R4,NDMYPOSO+1                                                    
         MVI   NDMYPOSO+2,2        (COLUMN 2)                                   
         BAS   RE,VALROW                                                        
         CLC   =C'HIGHCOM4',8(R2)                                               
         BNE   VVL3                                                             
         MVI   NDCMHEAD,C'Y'                                                    
         LA    R4,3(R4)                                                         
VVL3     LA    R4,1(R4)                                                         
         SPACE 1                                                                
VVL4     BAS   RE,BUMP                                                          
         BCT   R3,VVL2                                                          
         SPACE 1                                                                
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         STC   R4,NDMYFRST                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RIGHT SIDE HEADERS                                      
         SPACE 3                                                                
VVALRGHT LA    R3,3                MAX 3 FIELDS                                 
         LA    R4,5                (START ON HEAD 5)                            
         SPACE 1                                                                
VVRT2    CLI   5(R2),0                                                          
         BE    VVRT4                                                            
         MVI   NDMYPOSO,C'H'                                                    
         STC   R4,NDMYPOSO+1                                                    
         MVI   NDMYPOSO+2,96       (COLUMN 96)                                  
         CLI   NDNAROPT,C'Y'       (NOT ALLOWED FOR NARROW)                     
         BE    BADROW                                                           
         CLI   NDWIDOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   NDMYPOSO+2,129      (COLUMN 129 FOR WIDE)                        
         BAS   RE,VALROW                                                        
         CLC   =C'HIGHCOM4',8(R2)                                               
         BNE   VVRT3                                                            
         LA    R4,3(R4)                                                         
VVRT3    LA    R4,1(R4)                                                         
         SPACE 1                                                                
VVRT4    BAS   RE,BUMP                                                          
         BCT   R3,VVRT2                                                         
         SPACE 1                                                                
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         IC    R3,NDMYFRST                                                      
         CR    R4,R3                                                            
         BL    VVRT5                                                            
         STC   R4,NDMYFRST                                                      
VVRT5    IC    R3,NDMYFRST         GET FIRST P LINE                             
         AHI   R3,-2               MINUS 2                                      
         STC   R3,NDLASTHD         = LAST HEADLINE                              
VVRTX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE MID                                                     
         SPACE 3                                                                
VVALMID  MVI   NDMYPOSO,C'M'                                                    
         MVI   NDMYPOSO+1,1                                                     
         MVI   NDMYPOSO+2,1                                                     
         BAS   RE,VALROW                                                        
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE ROWS                                                    
         SPACE 1                                                                
VVALROWS MVI   NDTOTWID,1          START CHECKING REPORT WIDTH NOW              
         CLI   5(R2),0                                                          
         BE    BADNEED1            NEED AT LEAST 1 DETAIL OR RECAP              
         LA    R3,8                (MAX 8 FOR DETAILS)                          
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R3,4                (MAX 4 FOR RECAPS)                           
         LR    R0,R3                                                            
         BAS   RE,DELINS           (CHECK FOR INSERTS/DELETES)                  
         CLI   NDANYREC,C'Y'       IF RECAPS ARE REQUESTED                      
         BNE   VVR2                                                             
         TM    NDDOWNL,X'80'          (DOWNLOAD NOT ON!)                        
         BO    BADDWNRC                                                         
         CLI   OFFLINE,C'Y'                                                     
         BNE   VVR2                                                             
         L     R1,DRCURBUF         PUT IN AN ENTRY FOR DRIVER HERE              
         MVC   0(2,R1),=X'800A'                                                 
         MVC   2(8,R1),=C'DRIVER  '                                             
         LA    R1,10(R1)                                                        
         MVC   0(2,R1),=X'810A'                                                 
         MVC   2(8,R1),=C'RECORD  '                                             
         LA    R1,10(R1)                                                        
         ST    R1,DRCURBUF                                                      
         SPACE 1                                                                
VVR2     XC    NDMYPOSO,NDMYPOSO                                                
         BAS   RE,VALROW                                                        
         LA    R1,NDWDR1           ADDRESS W(ROW1/ALL)                          
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,NDWRR1                                                        
         ZIC   R0,NDTOTWID         PICK UP TOTAL LENGTH                         
         BCTR  R0,0                (-1)                                         
         CLI   0(R1),0             AND NOTE WIDTH ROW1 FIRST TIME               
         BNE   *+8                                                              
         STC   R0,0(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R3,VVR2                                                          
         SPACE 1                                                                
         STC   R0,1(R1)            AND NOTE WIDTH OF ALL ROWS                   
         CLI   OFFLINE,C'Y'        MAY BE EXTRAS IF OFF-LINE                    
         BNE   XIT                                                              
         SPACE 1                                                                
         CLI   NDSEPOPT,C'Y'       WAS UNIQUE FEATURE SELECTED?                 
         BNE   VVR4                                                             
         CLI   NDRPTYPE,C'D'       AND WE ARE DOING DETAILS                     
         BNE   VVR4                                                             
         SPACE 1                                                                
         XC    BLOCK(12),BLOCK     GENERATE A UNIQUE ENTRY                      
         MVC   BLOCK+12(30),SPACES                                              
         MVI   BLOCK,6                                                          
         MVC   BLOCK+12(6),=C'UNIQUE'                                           
         LA    R4,BLOCK                                                         
         GOTO1 VROWDRON                                                         
         NI    DRFLAGO,X'7F'       (ENSURE IT DOESN'T PRINT!)                   
         GOTO1 GROWDRON                                                         
         SPACE 1                                                                
VVR4     CLI   NDFLAVOR,C'E'       IF WE ARE NOT DOING ESTIMATES                
         BE    VVR6                NO TARGETS                                   
         CLI   NDFLAV3,C'N'        NO TARGETS?                                  
         BE    VVR6                YES/NO TARGETS                               
*                                                                               
*                                  NOW ACTUALLY DOING COLUMNS                   
         LA    R4,BLOCK                                                         
         XC    BLOCK(12),BLOCK     GENERATE TARGET ENTRIES                      
         MVC   BLOCK+12(30),SPACES                                              
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(7),=C'TARGET1'                                          
         GOTO1 VCOLDRON                                                         
         MVC   DRPOSO,=X'C80101'   (PHONY HEAD LINE)                            
         GOTO1 GCOLDRON                                                         
         MVC   BLOCK+12(7),=C'TARGET2'                                          
         GOTO1 VCOLDRON                                                         
         MVC   DRPOSO,=X'C80101'   (PHONY HEAD LINE)                            
         GOTO1 GCOLDRON                                                         
         MVC   BLOCK+12(7),=C'TARGET3'                                          
         GOTO1 VCOLDRON                                                         
         MVC   DRPOSO,=X'C80101'   (PHONY HEAD LINE)                            
         GOTO1 GCOLDRON                                                         
         SPACE 1                                                                
VVR6     B     XIT                                                              
         EJECT                                                                  
*              ROWS - FIRST VALIDATE FOR KEYWORD                                
         SPACE 3                                                                
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         BAS   RE,CHKSEC           CHECK SECURITY LOCKOUT                       
         AI    NDCURLEV,1                                                       
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK),0                               
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
*                                                                               
*                                                                               
         GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
*                                                                               
         CLC   =C'DEMO',12(R4)                                                  
         BE    BADROW                                                           
                                                                                
         CLC   12(6,R4),=C'ALLDPT' ALLDPT KEYWORD?                              
         BNE   *+12                                                             
         OI    NBINDS2,NBALLDPT    SET ALLDPT FILTER                            
         B     VROWNXT                                                          
                                                                                
         CLC   12(6,R4),=C'BLDPRD' BILLED PROD KEYWORD?                         
         BNE   *+12                                                             
         OI    NBINDS3,NBI3BPRD    SET BILLED PROD MODE                         
         B     VROWNXT                                                          
         CLC   12(6,R4),=C'COMRTN' COMMERCIAL ROTATION PROD KEYWRD              
         BNE   *+12                                                             
         OI    NBINDS3,NBI3CMRT    SET COMMERCIAL ROTATION MODE                 
         B     VROWNXT                                                          
                                                                                
                                                                                
* BUILD PQINDEX                                                                 
         ICM   RF,15,NDAPQIND                                                   
         BZ    PQINDEND                                                         
         USING PQINDEX,RF                                                       
         LA    RE,MAXROWS+MAXMIDS+MAXHEADS                                      
                                                                                
         OC    PQKEYWRD,PQKEYWRD   IN USE                                       
         BZ    *+16                                                             
         LA    RF,PQINDXEQ(RF)                                                  
         BCT   RE,*-14                                                          
         B     PQINDEND                                                         
         MVC   PQKEYWRD,BLOCK+12   KEYWORD NAME                                 
         MVC   PQOUTLEN,DRLENO                                                  
         MVC   PQPOSO,NDMYPOSO                                                  
         MVC   PQHEAD1,DRH1LIT                                                  
         MVC   PQHEAD2,DRH2LIT                                                  
         MVC   PQHEAD3,DRH3LIT                                                  
         MVC   PQHEAD4,DRH4LIT                                                  
         DROP  RF                                                               
                                                                                
PQINDEND EQU   *                                                                
*                                                                               
                                                                                
         CLI   DRATTRIB+3,C'I'       IS IT INVOICE HEADER KEYWORD?              
         BNE   *+8                                                              
         OI    NBVARIND,X'20'      YES/SET FLAG                                 
*                                                                               
         TM    NDCROSS,X'01'       ..IS IT CROSS AGY READ                       
         BNO   VVROW10                                                          
*                                                                               
         TM    NDCROSS,X'10'       IF FOR GI2/RES                               
         BO    VVROW10             ALL KEYWORDS OK                              
*                                                                               
* - TAKE OUT DRATTRIB+1 CHECK FOR CROSS AGY READ                                
* - BYTE USED FOR OTHER CHECKS - DRATTRIB+2 IS SUFFICIENT CHECK                 
***      CLI   DRATTRIB+1,C'X'     ..YES/IS IT VALID KEYWORD                    
***      BNE   BADROW                                                           
         CLI   DRATTRIB+2,0        ..IS IT SET FOR CROSS AGY                    
         BE    BADROW                                                           
         MVC   BYTE,DRATTRIB+2     ..TEST FOR SPECIFIC ADVERTISER               
         OC    BYTE,NDCROSS                                                     
         CLC   BYTE,DRATTRIB+2                                                  
         BNE   BADROW                                                           
VVROW10  BAS   RE,ENTARG           CHECK FOR ENTRY ARGUMENTS                    
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         BAS   RE,NEWROW           MAY SWITCH TO A NEW ROW                      
         CLI   DRTYPEI+1,C'+'                                                   
         BE    BADROW2             ADDITIVE FIELDS NO GOOD IN ROWS              
*                                                                               
         CLC   =C'ACTNP',12(R4)    IF ACTNP, NO PRINT IT                        
         BE    VROW21                                                           
         CLC   =C'ASSNP',12(R4)    IF ASSNP, NO PRINT IT                        
         BE    VROW21                                                           
*                                                                               
         CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW2                                                            
         CH    R0,=H'1'            IF ON ITS OWN, MAKE NO PRINT                 
         BE    VROW21                                                           
         BAS   RE,RANKNP           ..IS IT RANK AND NO PRINT                    
         BNE   *+8                                                              
         MVI   DRPOSO,C'N'         ..YES                                        
         LA    R4,42(R4)           RANK OPTIONALLY HAS COMPUTE                  
         BCTR  R0,0                                                             
         AI    FIELDERR,1                                                       
         MVI   DRCMPMAX,C'P'                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   VROW1                                                            
         GOTO1 GROWDRON                                                         
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         B     XIT                                                              
         SPACE 1                                                                
RANKNP   NTR1                    IS IT RANK AND NO PRINT                        
RNP01    LA    R4,42(R4)                                                        
         CLC   12(2,R4),=C'NP'                                                  
         BE    RNPX                                                             
         BCT   R0,RNP01                                                         
RNPX     B     XIT                                                              
         SPACE 1                                                                
VROW1    GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   NDTOTWID,0          IF WE ARE IN THE ROWS                        
         BE    VROW2                                                            
         CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
         SPACE 1                                                                
VROW2    CLC   12(4,R4),=C'UDEF'   USER DEFINITION RECORDS                      
         BNE   VROW2AA                                                          
         BAS   RE,VVALUDEF                                                      
         BNE   BADROW                                                           
         B     VROW2A                                                           
VROW2AA  CLC   12(4,R4),=C'UCOM'   USER DEFINITION RECORDS                      
         BNE   VROW2A                                                           
         BAS   RE,VVALUCOM                                                      
         BNE   BADROW                                                           
         SPACE 1                                                                
VROW2A   DS    0H                                                               
         CLI   NDMYPOSO,C'H'       SPECIAL FOR HEADS                            
         BNE   VROW3                                                            
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW3    CLC   12(4,R4),=C'VEND'   VENDOR NAME/CODE                             
         BNE   VROW3B                                                           
         OI    NDRDBCEL,X'05'                                                   
         B     VROWNXT                                                          
*                                                                               
VROW3B   CLC   12(4,R4),=C'$TYP'   $TYP KEYWORD?                                
         BNE   VROW4                                                            
         OI    NBINDS2,NB$TYPSP    $TYP SPECIAL CHARGES                         
         B     VROWNXT                                                          
*                                                                               
VROW4    DS    0H                                                               
         CLC   =C'MERGE',12(R4)    MERGE SITUATION                              
         BNE   VROWMRGN                                                         
*                                                                               
         MVI   NSMRGSW,C'Y'           TURN ON SWITCH                            
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' TURN OFF PRINT IND ON ALL HEADLINES          
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
VROWMRGN DS    0H                                                               
*                                                                               
         CLI   NDMYPOSO,C'M'       SPECIAL FOR MID                              
         BNE   VROWNXT                                                          
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROWNXT                                                          
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY ROW EXPRESSIONS                             
         SPACE 3                                                                
VROW12   CLC   12(2,R4),=C'* '     TOTAL EXPRESSION                             
         BNE   VROW14                                                           
         OI    DRTOTAL,X'80'                                                    
*******  MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         LR    RE,R2                                                            
         SPACE 1                                                                
VROW12B  ZIC   RF,0(RE)                                                         
         AR    RE,RF               HAVE A LOOK AT THE NEXT LEVEL                
         CLI   0(RE),0                                                          
         BE    VROWNXT                                                          
         TM    1(RE),X'20'                                                      
         BO    VROW12B             FIND AN UNPROTECTED FIELD                    
         ZIC   RF,5(RE)                                                         
         LTR   RF,RF               WITH SOME DATA                               
         BZ    VROW12B                                                          
         LA    RE,8(RE)                                                         
         SPACE 1                                                                
VROW12D  CLI   0(RE),C'*'          IF A TOTAL IS NOT SPECIFIED                  
         BE    VROWNXT                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VROW12D                                                       
******   OI    DRLAST,X'80'        GENERATE A SPACE BEFORE TOTALS               
******   MVI   DRLSPACE,1                                                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW14   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW16                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW16   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW18                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROWNXT                                                          
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW18   CLC   12(2,R4),=C'U '                                                  
         BNE   VROW20                                                           
         BAS   RE,VUSRDRON                                                      
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW20   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW22                                                           
         SPACE 1                                                                
VROW21   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         MVC   DRH1LIT,SPACES                                                   
         MVC   DRH2LIT,SPACES                                                   
         MVC   DRH3LIT,SPACES                                                   
         MVC   DRH4LIT,SPACES                                                   
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW22   CLC   12(4,R4),=C'DET '   TOTAL DETAIL EXPRESSION                      
         BE    VROW23                                                           
         CLC   12(2,R4),=C'D '     DET=N OR D=N FORMAT                          
         BNE   VROW24                                                           
         SPACE 1                                                                
VROW23   OI    DRTOTAL,X'80'                                                    
*******  MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    PICK UP NUMBER OF DETAILS                    
         CLI   DRTDET,0            MUST BE SOMETHING NUMERIC                    
         BE    BADROW                                                           
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW24   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW26                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW26   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VROW28                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VROW28                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VROW28                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VROW28                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VROW32                                                           
         SPACE 1                                                                
VROW28   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),2                                                          
         BL    VROWNXT             HN=X CAUSES REMOVAL                          
         OI    0(R1),X'80'         OTHERWISE TURN IT BACK ON                    
         MVC   27(1,R1),1(R4)      PASS LITERAL LENGTH TO DRONE                 
         CLC   1(1,R4),DRLENO                                                   
         BNH   VROW30              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   HALF,=H'652'        INVALID ROW EXPRESSION                       
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
VROW30   MVC   28(24,R1),22(R4)    PASS DRONE THE LITERAL                       
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW32   DS    0H                   PRINT HORIZONTAL LINE                       
         CLC   12(5,R4),=C'HORIZ'                                               
         BNE   VROW34                                                           
         OI    DRLAST,X'80'                                                     
         OI    DRLOPTS,DRLHORIZ    PRINT HORIZONTAL                             
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW34   DS    0H                                                               
         CLC   =C'DUE',12(R4)      DUE/RUN OPTION ON ACTVDAT                    
         BNE   VROW35                                                           
         MVI   DRARGSI+11,1                                                     
         MVI   DRNARGSI,16                                                      
         B     VROWNXT                                                          
VROW35   CLC   =C'RUN',12(R4)                                                   
         BNE   VROW36                                                           
         MVI   DRARGSI+11,2                                                     
         MVI   DRNARGSI,16                                                      
         B     VROWNXT                                                          
         SPACE 1                                                                
VROW36   B     BADROW                                                           
         SPACE 1                                                                
VROWNXT  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW12                                                        
         EJECT                                                                  
*              FINAL ADJUSTMENTS                                                
         SPACE 3                                                                
         CLI   NDTOTWID,0          IF WE ARE CHECKING WIDTH                     
         BZ    VROWGEN                                                          
         TM    DRFLAGO,X'80'                                                    
         BNO   VROWGEN                                                          
         ZIC   R0,NDTOTWID         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,NDTOTWID                                                      
         SPACE 1                                                                
VROWGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRPOSO,NDMYPOSO                                                  
         SPACE 1                                                                
         TM    DRTOTAL,X'80'       WAS TOTAL REQUESTED?                         
         BNO   VROWADJ2                                                         
         CLI   DRRTNO,X'41'        IF OUT ROUTINE SPECIFIED                     
         BL    VROWADJ2                                                         
         MVC   DRTRTN,DRRTNO       USE THIS FOR TOTAL AS WELL                   
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
         SPACE 1                                                                
VROWADJ2 GOTO1 GROWDRON                                                         
         BAS   RE,ANYMOROW         MAY GENERATE SOME MORE ROWS                  
         BAS   RE,INSROW           INSERT APPENDED ROW                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
INSROW   NTR1                                                                   
         CLC   8(4,R2),=C'SD,*'    IF SUBDAYPART TOTALS                         
         BNE   *+8                                                              
         BAS   RE,DPTINS           SUBDAYPART INSERT                            
         CLC   8(3,R2),=C'DT2'     IF DT2                                       
         BNE   *+8                                                              
         BAS   RE,DT2INS           DAY/TIME/TIMEC                               
         CLC   8(4,R2),=C'MDT2'    IF MDT2                                      
         BNE   *+8                                                              
         BAS   RE,MDT2INS          DAY/MILITARY TIME/MILIT TIMEC                
INSRX    B     XIT                                                              
*                                                                               
DPTINS   NTR1                                                                   
         XC    P,P                                                              
         LA    R2,P                                                             
         MVI   5(R2),5                                                          
         MVC   8(5,R2),=C'SDP,*'   INSERT SUBDAYPART N ROW                      
         BAS   RE,VALROW                                                        
         XC    P,P                                                              
         LA    R2,P                                                             
         MVI   5(R2),3                                                          
         MVC   8(3,R2),=C'SDN'     INSERT SUBDAYPART NN ROW                     
         BAS   RE,VALROW                                                        
         B     XIT                                                              
*                                                                               
*                                                                               
DT2INS   NTR1                                                                   
         LA    R2,P                BORROW PRINT LINE                            
         XC    P,P                                                              
         MVI   5(R2),4                                                          
         MVC   8(4,R2),=C'TIME'  INSERT TIME                                    
         BAS   RE,VALROW                                                        
         XC    P,P                                                              
         MVI   5(R2),5                                                          
         MVC   8(5,R2),=C'TIMEC' INSERT CENTRAL TIME                            
         BAS   RE,VALROW                                                        
         B     XIT                                                              
*                                                                               
MDT2INS  NTR1                                                                   
         LA    R2,P                BORROW PRINT LINE                            
         XC    P,P                                                              
         MVI   5(R2),5                                                          
         MVC   8(5,R2),=C'MTIME'  INSERT MILITARY TIME                          
         BAS   RE,VALROW                                                        
         XC    P,P                                                              
         MVI   5(R2),6                                                          
         MVC   8(6,R2),=C'MTIMEC'  INSERT MILITARY CENTRAL TIME                 
         BAS   RE,VALROW                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
         USING T320FFD,R4                                                       
BADROW   MVC   HALF,=H'652'      INVALID ROW EXPRESSION                         
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADROW2  MVC   HALF,=H'658'        NOT ALLOWED AS ROW                           
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLRANK MVC   HALF,=H'646'        RANK CANNOT BE LAST ROW                      
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         MVI   BYTE,0                                                           
         B     ERR2                                                             
         SPACE 1                                                                
BADDWNRC MVC   HALF,=H'647'        CAN NOT DOWNLOAD RECAPS                      
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         MVI   BYTE,0                                                           
         B     ERR2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              ROW ADJUSTMENT ROUTINES                                          
         SPACE 3                                                                
NEWROW   NTR1                                                                   
         CLI   DRATTRIB,1          ATTRIB 1                                     
         BNE   NEWROW2                                                          
         CLI   NDMYPOSO,C'H'       IF WE ARE IN THE HEADLINES                   
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK          CHANGE TO HEADX                         
         MVC   BLOCK+12(30),SPACES      WHERE X IS SECOND ATTRIB                
         MVI   BLOCK,5                                                          
         MVC   BLOCK+12(4),=C'HEAD'                                             
         MVC   BLOCK+12+4(1),DRATTRIB+1 (SECOND ATTRIB)                         
         B     NEWROWX                                                          
         SPACE 1                                                                
NEWROW2  CLI   DRATTRIB,2          ATTRIB 2                                     
         BNE   NEWROW3                                                          
         OI    DRTOTAL,X'80'       DATE TOTAL EXPRESSIONS                       
         MVI   DRLSPACE,1                                                       
         LA    R1,NDDETPTL         NOTE LEVEL OF PERIOD TOTALS                  
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,NDRECPTL                                                      
         CLI   0(R1),0                                                          
         BNE   XIT                                                              
         MVC   0(1,R1),NDCURLEV                                                 
         B     XIT                                                              
         SPACE 1                                                                
NEWROW3  CLI   DRATTRIB,3          ATTRIB 3                                     
         BNE   NEWROWSL                                                         
         LA    R1,NDDETTTL         NOTE LEVEL OF TARGET TOTALS                  
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+8                                                              
         LA    R1,NDRECTTL                                                      
         CLI   0(R1),0                                                          
         BNE   XIT                                                              
         MVC   0(1,R1),NDCURLEV                                                 
         B     XIT                                                              
         SPACE 1                                                                
NEWROWSL CLC   DRRTNI(5),=C'NILEN'      CHANGE LEN TO SL                        
         BNE   NEWROWNE                                                         
         XC    BLOCK(12),BLOCK                                                  
         MVC   BLOCK+12(30),SPACES                                              
         MVI   BLOCK,2                                                          
         MVC   BLOCK+12(2),=C'SL'                                               
         B     NEWROWX                                                          
         SPACE 1                                                                
NEWROWNE CLC   BLOCK+12(4),=C'NET '     CHANGE NET TO NETWORK!                  
         BNE   NEWROWCO                                                         
         XC    BLOCK(12),BLOCK                                                  
         MVC   BLOCK+12(30),SPACES                                              
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(7),=C'NETWORK'                                          
         XC    DRH2ALL,DRH2ALL     (STOP SECOND HEADING)                        
         B     NEWROWX                                                          
         SPACE 1                                                                
NEWROWCO CLC   BLOCK+12(5),=C'COMM '    CHANGE COMM TO COMMNUM                  
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK                                                  
         MVC   BLOCK+12(30),SPACES                                              
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(7),=C'COMMNUM'                                          
         B     NEWROWX                                                          
         SPACE 1                                                                
NEWROWX  LA    R4,BLOCK                                                         
         GOTO1 VROWDRON                                                         
         CLI   DRERROR,0                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ANY MORE ROWS TO GENERATE                                        
         SPACE 3                                                                
ANYMOROW NTR1                                                                   
         CLI   DRATTRIB,2          ATTRIB 2                                     
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,3                                                          
         MVI   BLOCK+12+2,C'2'     APPEND A 2 TO EXPRESSION                     
         LA    R4,BLOCK                                                         
         GOTO1 VROWDRON                                                         
         NI    DRFLAGO,X'7F'            NO PRINT                                
         GOTO1 GROWDRON                                                         
         AI    NDCURLEV,1                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL COLUMN VALIDATION                                        
         SPACE 3                                                                
VVALCOLS LA    R0,12               (MAX 12 COLUMNS)                             
         BAS   RE,DELINS           (CHECK FOR INSERTS/DELETES)                  
         CLI   5(R2),0                                                          
         BE    BADNEED1            NEED AT LEAST 1 COLUMN                       
         MVI   NDMYLABL,C'A'                                                    
         ST    R2,NDALSTCL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         LA    R3,EDITLIST                                                      
         MVI   NDPREFIX,0          PRESET PREFIX TO X'00'                       
         SPACE 1                                                                
VVALCOLE CLI   NDFLAVOR,C'E'       FOR ESTIMATES                                
         BNE   VVALCOLV                                                         
         MVC   NDPREFIX,NDUSEOPT   USE ACTUAL OR EST IF USE=E REQUESTED         
         B     VVALCOL2                                                         
         SPACE 1                                                                
VVALCOLV CLI   NDFLAVOR,C'V'       FOR EVALUATIONS                              
         BNE   VVALCOLP                                                         
         MVI   NDPREFIX,C'E'       USE ESTIMATED                                
         CLI   NDFLAVOR+1,C'3'     UNLESS IT IS V3                              
         BNE   VVALCOL2                                                         
         MVI   NDPREFIX,C'A'       WHEN WE USE ACTUAL                           
         B     VVALCOL2                                                         
         SPACE 1                                                                
VVALCOLP CLI   NDFLAVOR,C'P'       FOR ALL POSTS,                               
         BNE   VVALCOL2                                                         
         CLI   NDFLAVOR+1,C'2'         EXCEPT P2 (WHICH ALTERNATES)             
         BE    VVALCOL2                                                         
         MVI   NDPREFIX,C'E'       VALIDATE ESTIMATED COLUMNS                   
         BAS   RE,VALSET                                                        
         MVI   NDPREFIX,C'A'       AND THEN DO ACTUALS                          
         SPACE 1                                                                
VVALCOL2 BAS   RE,VALSET                                                        
         SPACE 1                                                                
         TM    NDDOWNL,X'80'       SKIP LENGTH CHECKING                         
         BO    XIT                 IF WE ARE DOWNLOADING                        
         CLI   NDTOTWID,80         CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   NDNAROPT,C'Y'       ONLY 80 ALLOWED WITH NARROW OPT              
         BE    VVALCBIG                                                         
         CLI   NDTOTWID,132        CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   NDWIDOPT,C'Y'                                                    
         BNE   VVALCBIG                                                         
         CLI   NDTOTWID,165        UP TO 165 ALLOWED WITH WIDE OPTION           
         BNH   XIT                                                              
         SPACE 1                                                                
         USING T320FFD,R4                                                       
VVALCBIG MVC   HALF,=H'651'        DET(RECAP) HAS NN CHAR TOO WIDE              
         MVI   BYTE,X'FF'          SOFT ERROR MESSAGE                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM+1(6),=C'DETAIL'                                             
         MVI   ELEM,7                                                           
         CLI   NDRPTYPE,C'R'                                                    
         BNE   *+10                                                             
         MVC   ELEM+1(6),=C'RECAP '                                             
         MVI   ELEM+7,4                                                         
         ZIC   R1,NDTOTWID                                                      
         EDIT  (R1),(3,ELEM+8)                                                  
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     ERR2                                                             
         DROP  R4                                                               
         SPACE 1                                                                
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
         SPACE 1                                                                
SETMAX2  CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETMAX2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A SET OF COLUMNS                                        
         SPACE 3                                                                
VALSET   NTR1                                                                   
         SPACE 1                                                                
VALSET2  XC    NDMYPOSO,NDMYPOSO                                                
         MVC   0(1,R3),NDMYLABL    SAVE LABEL IN EDIT LIST                      
         CLI   NDFLAVOR,C'M'       PRESET MEDIA FLAVOR PREFIX TO X'00'          
         BNE   *+8                                                              
         MVI   NDPREFIX,0                                                       
         CLC   NDFLAVOR,=C'P2'     CONTROL ALTERNATING POSTS                    
         BNE   VALSET4                                                          
         MVI   NDPREFIX,C'E'       VALIDATE AN ESTIMATED COLUMN                 
         BAS   RE,VALMAC                                                        
         MVI   NDPREFIX,C'A'       AND THEN AN ACTUAL                           
         SPACE 1                                                                
VALSET4  BAS   RE,VALMAC                                                        
         BAS   RE,BUMP                                                          
         CLI   NDEXTEND,2          DON'T ADVANCE IF EXTENSION                   
         BE    VALSET6                                                          
         MVC   NDMYLABL,8(R2)                                                   
         LA    R3,4(R3)                                                         
         SPACE 1                                                                
VALSET6  BAS   RE,BUMP                                                          
         BCT   R0,VALSET2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR MACRO EXPRESSIONS                                      
         SPACE 3                                                                
VALMAC   NTR1                                                                   
         CLC   8(2,R2),=C'MC'      ALL MACROS START WITH MC...                  
         BE    VALMAC2                                                          
         CLC   8(2,R2),=C'BH'      BILL HEADER INVOICE READ                     
         BNE   *+8                                                              
         OI    NDRDBCEL,X'01'                                                   
         CLC   8(2,R2),=C'CS'      READ CASH APPLIED DATA                       
         BNE   *+8                                                              
         OI    NDRDBCEL,X'21'                                                   
         CLC   8(4,R2),=C'VEND'    BILL HEADER INVOICE READ(VENDOR)             
         BNE   *+8                                                              
         OI    NDRDBCEL,X'05'                                                   
         CLC   8(3,R2),=C'CHQ'     CHECK NUMBER READ                            
         BNE   *+8                                                              
         OI    NDRDBCEL,X'02'                                                   
         CLC   8(5,R2),=C'AUTH$'   CHECK NUMBER READ                            
         BNE   *+8                                                              
         OI    NDRDBCEL,X'10'                                                   
         CLC   8(5,R2),=C'EARNC'   EARNED COST                                  
         BNE   *+8                                                              
         OI    NBSBKEND,NBEXPDM6   EXPAND/NO TRUC                               
         CLC   8(6,R2),=C'ERCADJ'    EARNED COST ADJUSTMENT                     
         BNE   *+8                                                              
         OI    NBSBKEND,NBEXPDM6   EXPAND/NO TRUC                               
         BAS   RE,VALCOL                                                        
         B     XIT                                                              
         SPACE 1                                                                
VALMAC2  L     R4,=A(MACTABLE)                                                  
         A     R4,RELO                                                          
         GOTO1 ANY                                                              
         SPACE 1                                                                
VALMAC4  CLI   0(R4),X'FF'                                                      
         BE    BADMAC                                                           
         CLC   0(8,R4),WORK        CHECK FOR MATCH IN TABLE                     
         BE    VALMAC6                                                          
         LA    R4,L'MACENTRY(R4)                                                
         B     VALMAC4                                                          
         SPACE 1                                                                
VALMAC6  L     R3,12(R4)           PICK UP END ADDRESS                          
         A     R3,RELO                                                          
         L     R4,8(R4)                    AND START ADDRESS                    
         A     R4,RELO                                                          
         SPACE 1                                                                
VALMAC8  BAS   RE,GENMAC           GENERATE A COLUMN FOR EACH ENTRY             
         CR    R4,R3               UNTIL WE GET TO THE END                      
         BE    XIT                                                              
         LA    R4,4(R4)                                                         
         B     VALMAC8                                                          
         SPACE 1                                                                
BADMAC   MVC   HALF,=H'657'      INVALID MACRO EXPRESSION                       
         B     MYCURSOR                                                         
         EJECT                                                                  
*              GENERATE A COLUMN DERIVED FROM MACRO                             
         SPACE 3                                                                
*              INPUT               R4=A(CL4 ENTRY NAME)                         
*                                  R2=A(FIELD HEADER)                           
         SPACE 1                                                                
GENMAC   NTR1                                                                   
         XC    P,P                 BORROW P FOR THIS ROUTINE                    
         LA    R1,4                FIGURE L'ENTRY                               
         MVC   P+100(4),0(R4)                                                   
         LA    R3,P+103                                                         
         SPACE 1                                                                
GENMAC2  CLI   0(R3),C' '                                                       
         BH    GENMAC4                                                          
         BCTR  R3,0                                                             
         BCT   R1,GENMAC2                                                       
         DC    H'0'                                                             
         SPACE 1                                                                
GENMAC4  STC   R1,P+5              THIS BECOMES THE NEW LENGTH                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),P+100        PUT IN DERIVED ENTRY NAME                    
         LA    R2,P                                                             
         BAS   RE,VALCOL                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FIRST COLUMN EXPRESSION                                 
         SPACE 3                                                                
VALCOL   NTR1                                                                   
         ZIC   R1,NDGLRGCT         BUMP INDEX INTO GLRG EXTENSION               
         LA    R1,1(R1)                                                         
         STC   R1,NDGLRGCT                                                      
*                                                                               
SKIPX    MVI   FIELDERR,1                                                       
         CLI   NDEXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOLB                                                            
         CLI   5(R2),0                                                          
         BE    VCOLNXT2                                                         
         LA    R4,BLOCK+42+42                                                   
         B     VCOL2                                                            
         SPACE 1                                                                
VCOLB    L     R5,NDADATES                                                      
         USING DATELSTD,R5                                                      
         XC    EXPLIST,EXPLIST     CLEAR PERIOD EXPRESSIONS                     
         DROP  R5                                                               
         SPACE 1                                                                
VCOL1    CLI   5(R2),0             MAY COME BACK FOR ANOTHER PERIOD             
         BE    XIT                                                              
         ST    R2,NDALSTCL                                                      
         XC    BLOCK(252),BLOCK                                                 
         LA    R4,BLOCK+42                                                      
         MVI   NDEXTEND,0                                                       
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL2                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   NDEXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
         SPACE 1                                                                
VCOL2    DS    0H                                                               
         BAS   RE,CHKSEC           SECURITY LOCKOUT                             
         GOTO1 SCANNER,DMCB,(20,(R2)),(8,(R4)),0                                
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         CLI   NDEXTEND,1          IF WE JUST TOOK AWAY THE COMMA               
         BNE   VCOL4                                                            
         ZIC   R1,5(R2)                                                         
         LA    R1,1(R1)            ADJUST THE LENGTH AND                        
         STC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       PUT IT BACK AGAIN!                           
         MVI   0(R1),C','                                                       
         SPACE 1                                                                
VCOL4    CLI   NDEXTEND,2                                                       
         BE    VCOL12                                                           
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
*                                                                               
* BUILD PQINDEX                                                                 
         ICM   RF,15,NDAPQIND                                                   
         BZ    PQINDND                                                          
         USING PQINDEX,RF                                                       
         LA    RE,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS                              
                                                                                
         OC    PQKEYWRD,PQKEYWRD   IN USE                                       
         BZ    *+16                                                             
         LA    RF,PQINDXEQ(RF)                                                  
         BCT   RE,*-14                                                          
         B     PQINDND                                                          
         MVC   PQKEYWRD,12(R4)     KEYWORD NAME                                 
         MVC   PQOUTLEN,DRLENO                                                  
         MVI   PQPOSO,C'C'                                                      
         MVC   PQHEAD1,DRH1LIT                                                  
         MVC   PQHEAD2,DRH2LIT                                                  
         MVC   PQHEAD3,DRH3LIT                                                  
         MVC   PQHEAD4,DRH4LIT                                                  
         DROP  RF                                                               
                                                                                
PQINDND  EQU   *                                                                
*                                                                               
         CLC   12(4,R4),=C'UDEF'   USER DEFINITION RECORDS                      
         BNE   *+16                                                             
         BAS   RE,VVALUDEF                                                      
         BNE   BADCOL                                                           
         B     VCOL4A                                                           
                                                                                
         CLC   12(4,R4),=C'UCOM'   USER DEFINITION RECORDS                      
         BNE   VCOL4A                                                           
         BAS   RE,VVALUCOM                                                      
         BNE   BADCOL                                                           
                                                                                
VCOL4A   TM    NDLOCAL,NDMINUS         ...IF FLOAT = MINUS OPTION               
         BZ    VCOL4B                                                           
         TM    DROPTSO,DRMINUSO        ...AND IF DROPTSO SET                    
         BZ    VCOL4B                                                           
         NI    DROPTSO,255-DRMINUSO    ...ASSUME $ COL AND TURN IT OFF          
         MVI   DRFLOATO,C'-'            ...AND TURN ON FLOAT=-                  
*                                                                               
VCOL4B   DS    0H                                                               
*                                                                               
         CLI   DRATTRIB,C'I'       IS IT INVOICE HEADER KEYWORD?                
         BNE   *+8                                                              
         OI    NBVARIND,X'20'      YES/SET FLAG                                 
                                                                                
         OC    DROPTSO,NSOPTSO1    ADD IN ANY GLOBAL OPTIONS                    
*                                                                               
         CLI   NSMRGSW,C'Y'        IF IN A MERGE SITUATION                      
         BNE   VCOLMRGX                                                         
*                                                                               
         CLI   DRTYPEI+1,C' '         IF THIS INFO NOT SPECIFIED                
         BH    *+8                                                              
         MVI   DRTYPEI+1,C'+'            FORCE ADDITIVE NATURE                  
*                                                                               
         CLI   DRTYPEI,C'P'        SKIP IF A NUMERIC FIELD                      
         BE    *+8                                                              
         CLI   DRTYPEI,C'B'        SKIP IF A NUMERIC FIELD                      
         BE    *+8                                                              
         CLI   DRTYPEI,C'M'        SKIP IF A NUMERIC FIELD                      
         BE    *+8                                                              
         OI    DROPTSO+1,DRNOTOT   ELSE DO NOT TOTAL FIELD                      
*                                                                               
VCOLMRGX DS    0H                                                               
*                                                                               
         TM    NDCROSS,X'01'       ..IS IT CROSS AGY READ                       
         BNO   VVCOL10                                                          
         TM    NDCROSS,X'10'       ..FOR GI2 ALL KEYWORDS OK                    
         BO    VVCOL10                                                          
*        CLI   DRATTRIB+1,C'X'     ..YES/IS IT VALID KEYWORD                    
*        BNE   BADCOL                                                           
         CLI   DRATTRIB+2,0        ..IS IT VALID KEYWORD                        
         BE    BADCOL                                                           
         MVC   BYTE,DRATTRIB+2     ..TEST FOR SPECIFIC ADVERTISER               
         OC    BYTE,NDCROSS                                                     
         CLC   BYTE,DRATTRIB+2                                                  
         BNE   BADCOL                                                           
VVCOL10  BAS   RE,ENTARG           CHECK FOR ENTRY ARGUMENTS                    
         CLI   DRERROR,0                                                        
         BE    VCOLNXT                                                          
         EJECT                                                                  
*              TRY OTHER EXPRESSIONS                                            
         SPACE 3                                                                
VCOLM    CLI   NDFLAVOR,C'M'       IF MEDIA FLAVOR                              
         BNE   VCOLC                                                            
         MVC   NDPREFIX,BLOCK+42+12                                             
         CLI   NDPREFIX,C'A'       AND EXPRESSION STARTS WITH A OR E            
         BE    VCOLM2                                                           
         CLI   NDPREFIX,C'E'                                                    
         BNE   VCOLC                                                            
         SPACE 1                                                                
VCOLM2   ZIC   R1,BLOCK+42         SEE IF THERE IS AN EXPRESSION                
         BCTR  R1,0                WITHOUT THIS CHARACTER                       
         STC   R1,BLOCK                     (LENGTH-1)                          
         MVC   BLOCK+12(29),BLOCK+42+13     (STRIP CHARACTER)                   
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   VCOLC                                                            
         BAS   RE,ESTACPOP         POP IN EST OR ACT                            
         LA    R4,BLOCK+42                                                      
         B     VCOLNXT                                                          
         SPACE 1                                                                
*                                  NOW CHECK FOR COMPUTES                       
VCOLC    CLI   NDFLAVOR,C'P'       COMPUTES NOT ALLOWED FOR POSTS               
         BE    BADCOL                                                           
         XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOLC2                                                           
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOLC2   BAS   RE,COMPEDIT         AUTO EDIT FOR COMPUTES                       
         B     VCOLNXT                                                          
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                          
         SPACE 3                                                                
VCOL12   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL16                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL16   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL20                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL24                                                           
         SPACE 1                                                                
VCOL20   XC    0(64,R1),0(R1)      TURN OFF EVERYTHING!                         
         CLI   1(R4),2                                                          
         BL    VCOLNXT             HN=X CAUSES REMOVAL                          
         OI    0(R1),X'80'         OTHERWISE TURN IT BACK ON                    
         MVC   27(1,R1),1(R4)      PASS LITERAL LENGTH TO DRONE                 
         CLC   1(1,R4),DRLENO                                                   
         BNH   VCOL22              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   HALF,=H'660'        HEADING OVERRIDE > THAN COLUMN               
         B     MYCURSOR                                                         
         SPACE 1                                                                
VCOL22   MVC   28(24,R1),22(R4)    PASS DRONE THE LITERAL                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL26                                                           
         MVI   NDMYPOSO,C'N'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         MVC   DRH1LIT,SPACES                                                   
         MVC   DRH2LIT,SPACES                                                   
         MVC   DRH3LIT,SPACES                                                   
         MVC   DRH4LIT,SPACES                                                   
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL26   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL28                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOL28   CLC   12(4,R4),=C'RANK'   (COLUMN) RANK                                
         BNE   VCOL30                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOLNXT                                                          
         MVI   COLRANK,2                                                        
         B     VCOLNXT                                                          
         SPACE 1                                                                
COLRANK  DC    X'00'                                                            
         SPACE 1                                                                
VCOL30   CLC   12(2,R4),=C'NT'          NO TOTAL FOR COLUMN                     
         BNE   VCOL32                                                           
         OI    DROPTSO+1,DRNOTOT                                                
         B     VCOLNXT                                                          
*                                                                               
VCOL32   CLC   12(2,R4),=C'ND'          NO DETAILS (GIVES TOTALS)               
*        BNE   VCOLFRAW                                                         
         BNE   VCOLFILT                                                         
         OI    DROPTSO+1,DRNODET                                                
         B     VCOLNXT                                                          
         EJECT                                                                  
*              SUPPORT COLUMN FILTERS                                           
         SPACE 3                                                                
*              ARGUMENTS                                                        
*                                                                               
* ***NOTE***    DRARGSI+14  HAS GLARG EXTENSION INDEX                           
* ***NOTE***    NDGLRGCT =  EXTENSION INDEX                                     
* ***NOTE***    NDGLRGXE =  LENGTH OF EACH EXTENSION                            
* ***NOTE***    NDAGLARG =  ADDRESS OF EXTENSION AREAS                          
*                                                                               
*                                  GLRGX+0    DAYPART                           
*                                                                               
*                                                                               
* - FOLLOWING ARE DIRECTLY ADDRESSABLE WITH DRARGSI                             
*                                                                               
*                                   8 R=RAW E=EQUIV                             
*                                   9 DAY CODE                                  
*                                  10 PRODUCT NUMBER                            
*                                     (FOR PUP PLAN FILTER NUMBER)              
*                                  11 OFFICE CODE                               
*                                  12 1=ABC  2=CBS  3=NBC                       
*                                     11=NET 12=CAB 13=SYD 14=OTH               
*                                     ALSO USED FOR MEDIA FILTER                
*                                     ALSO USED BY ACTVDAT KEYWORD FILT         
*                                  13 A=ACTUAL E=ESTIMATED                      
*                                  14 SPOT LENGTH                               
*                                  15 DRARGSI+14 = GLARG EXTENSION INDX         
*                                  16 PERIOD NUMBER                             
*                                  PG=V111 PRODUCT GROUPS                       
         SPACE 1                                                                
VCOLFILT DS    0H                                                               
*                                                                               
VCOLFRAW CLC   12(3,R4),=C'RAW'    RAW/EQUIV = ARG 8                            
         BNE   VCOLFEQU                                                         
*                                  ..SHARE ARG WITH BILLDATE FILTER             
         OI    DRARGSI+7,RAW                                                    
         OI    NDANYDEM,RAW        ADJUSTED NOT EQUIVALENCED                    
         B     VCOLDEMX                                                         
         SPACE 1                                                                
VCOLFEQU CLC   12(3,R4),=C'EQU'                                                 
         BNE   VCOLNN                                                           
*                                  ..SHARE ARG WITH BILLDATE FILT               
         OI    DRARGSI+7,EQU                                                    
         OI    NDANYDEM,EQU        ADJUSTED AND EQUIVALENCED                    
         B     VCOLDEMX                                                         
*                                                                               
VCOLNN   CLC   12(2,R4),=C'NN'      NOT ADJ NOT EQU                             
         BNE   VCOLNQ                                                           
*                                                                               
         OI    DRARGSI+7,NN                                                     
         OI    NDANYDEM,NN         NOT ADJ AND NOT EQUIV                        
         B     VCOLDEMX                                                         
*                                                                               
VCOLNQ   CLC   12(2,R4),=C'NQ'      NOT ADJUSTED BUT EQUIVALENCED               
         BNE   VCOLBDF                                                          
*                                                                               
         OI    DRARGSI+7,NQ                                                     
         OI    NDANYDEM,NQ         NOT ADJ BUT EQUIV                            
VCOLDEMX OI    NDANYDEM,X'20'      NOTE OVERRIDE EXPRESSION                     
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLBDF  DS    0H                                                               
         CLC   12(3,R4),=C'BDF'            BILL DATE FILTER                     
         BNE   VCOLFDA                                                          
         GOTO1 DATVAL,DMCB,(0,22(R4)),WORK                                      
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    BADCOL                                                           
         MVC   WORK+6(6),WORK      ASSUME END = START                           
         LA    R1,22(R1,R4)                                                     
         CLI   0(R1),C' '                                                       
         BE    VBDF10                                                           
         CLI   0(R1),C'-'          CHECK DELIMITER                              
         BNE   BADCOL                                                           
         LA    R1,1(R1)                                                         
         ST    R1,DMCB                                                          
         GOTO1 DATVAL,DMCB,,WORK+6                                              
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    BADCOL                                                           
         CLC   WORK+6(6),WORK      CHECK END V START                            
         BL    BADCOL                                                           
VBDF10   L     R5,NBABDFLT         GET BILL DATE FILT TABLE                     
         LTR   R5,R5                                                            
         BZ    VCOLNXT             MUST BE ON-LINE/SKIP                         
         LA    R1,1                                                             
VBDF15   CLI   0(R5),0             FIND NEXT AVAILABLE SLOT                     
         BE    VBDF20                                                           
         LA    R1,1(R1)            BUMP SLOT NUMBER                             
         LA    R5,4(R5)            BUMP TABLE                                   
         C     R1,=F'10'                                                        
         BH    BADCOL              MAX OF 10 COLS                               
         B     VBDF15                                                           
VBDF20   STC   R1,DRARGSI+7                     SET DATE POSTION NUMBER         
         GOTO1 DATCON,DMCB,WORK,(2,0(R5))       SET COMPRESSED DATES            
         GOTO1 DATCON,DMCB,WORK+6,(2,2(R5))                                     
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLFDA  CLC   12(3,R4),=C'DAY'    DAY FILTER = ARG 9                           
         BNE   VCOLFPR                                                          
         LA    R1,VCOLFDAL                                                      
         B     VCOLFDA2                                                         
         SPACE 1                                                                
VCOLFDAL DS    0H                                                               
         DC    X'7C',C'M-F'                                                     
         DC    X'40',C'MON'                                                     
         DC    X'20',C'TUE'                                                     
         DC    X'10',C'WED'                                                     
         DC    X'08',C'THU'                                                     
         DC    X'04',C'FRI'                                                     
         DC    X'02',C'SAT'                                                     
         DC    X'01',C'SUN'                                                     
         DC    X'7F',C'M-S'                                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
VCOLFDA2 CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+8(1),0(R1)                                               
         CLC   22(3,R4),1(R1)                                                   
         BE    VCOLNXT                                                          
         LA    R1,4(R1)                                                         
         B     VCOLFDA2                                                         
         SPACE 1                                                                
VCOLFPR  CLC   12(2,R4),=C'PR'     PRODUCT FILTER = ARG 10                      
         BNE   VCOLFOF                                                          
         CLI   NDPRGFLT,0          SEE IF USING PRD GROUP FILT                  
         BNE   BADCOL             (PROD AND PRODGRP SHARE BYTE)                 
         L     R1,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R1                                                        
         LA    R1,CLIST                                                         
         DROP  R1                                                               
         LA    RF,220                                                           
         SPACE 1                                                                
VCOLFPR2 MVC   DRARGSI+9(1),3(R1)                                               
         CLC   0(3,R1),22(R4)                                                   
         BE    VCOLNXT                                                          
         LA    R1,4(R1)                                                         
         BCT   RF,VCOLFPR2                                                      
         B     BADCOL                                                           
         EJECT                                                                  
*              COLUMN FILTERS - OFFICE NETWORK                                  
         SPACE 3                                                                
VCOLFOF  CLC   12(2,R4),=C'OF'     OFFICE FILTER = ARG 11                       
         BNE   VCOLFNE                                                          
         MVC   DRARGSI+10(1),22(R4)                                             
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLFNE  CLC   12(2,R4),=C'NE'     NETWORK FILTERS = ARG12                      
         BNE   VCOLFME                                                          
         CLI   14(R4),C'Q'         IS IT REALLY 'NEQ' FILTER?                   
         BE    VCOLFME                 YES/KEEP ON TRUCKIN                      
         MVI   DRARGSI+11,1                ABC=1                                
         CLC   22(3,R4),=C'ABC'                                                 
         BE    VCOLNXT                                                          
         MVI   DRARGSI+11,2                CBS=2                                
         CLC   22(3,R4),=C'CBS'                                                 
         BE    VCOLNXT                                                          
         MVI   DRARGSI+11,3                NBC=3                                
         CLC   22(3,R4),=C'NBC'                                                 
         BE    VCOLNXT                                                          
         MVI   DRARGSI+11,4                FOX=4                                
         CLC   22(3,R4),=C'FOX'                                                 
         BE    VCOLNXT                                                          
         B     BADCOL                                                           
         SPACE 1                                                                
VCOLFME  CLC   12(2,R4),=C'ME'     MEDIA FILTERS = ARG12 AS WELL                
         BNE   VCOLFDR                                                          
         MVC   DRARGSI+11(1),22(R4)                                             
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLFDR  CLC   =C'DUE',12(R4)      DUE/RUN ON ACTVDAT KEYWORD                   
         BNE   *+12                                                             
         MVI   DRARGSI+11,1                                                     
         B     VCOLNXT                                                          
         CLC   =C'RUN',12(R4)                                                   
         BNE   VCOLFSL                                                          
         MVI   DRARGSI+11,2                                                     
         B     VCOLNXT                                                          
         EJECT                                                                  
*              COLUMN FILTERS - SPOT LENGTH, DAYPART, PERIOD                    
         SPACE 3                                                                
* - SL FILTER AND BH READ AOR/NAOR FILT SHARE SAME DRARGSI                      
VCOLFSL  CLC   12(3,R4),=C'SL '    SPOT LENGTH FILTER = ARG 14                  
         BNE   VCSL20                                                           
         CLI   11(R4),0                                                         
         BE    BADCOL                                                           
         MVC   DRARGSI+13(1),11(R4)                                             
         B     VCOLNXT                                                          
VCSL20   CLC   12(3,R4),=C'AOR'    BH READ AOR FILT                             
         BNE   *+12                                                             
         OI    DRARGSI+13,X'80'    AOR ONLY                                     
         B     VCOLNXT                                                          
         CLC   12(4,R4),=C'NAOR'   EXCLUDE AOR                                  
         BNE   VCOLFDP                                                          
         OI    DRARGSI+13,X'08'    NO ARO                                       
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLFDP  CLC   12(3,R4),=C'DP '    DAYPART FILTER =ARG 15                       
         BNE   VCOLSTA                                                          
         SR    R5,R5               DAYPART LEN = 1                              
         BAS   RE,GLXRTN                                                        
         B     VCOLNXT                                                          
                                                                                
VCOLSTA  CLC   12(3,R4),=C'STA'    STATION FILTER                               
         BNE   VCOLCLI                                                          
         LA    R1,3                STATION LENGTH = 4                           
         BAS   RE,GLXRTN                                                        
         B     VCOLNXT                                                          
                                                                                
VCOLCLI  CLC   12(3,R4),=C'CLI'    CLIENT FILTER                                
         BNE   VCOLSTS                                                          
         LA    R1,2                CLIENT LENGTH = 3                            
         BAS   RE,GLXRTN                                                        
         B     VCOLNXT                                                          
                                                                                
VCOLSTS  CLC   12(3,R4),=C'STS'    STATUS FILTER                                
         BNE   VCOLFPE                                                          
         MVI   12(R4),C'T'         SWITCH TO T                                  
         LA    R1,STSTBL                                                        
VSTS10   CLI   0(R1),0                                                          
         BE    BADCOL                                                           
         CLC   0(3,R1),22(R4)                                                   
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     VSTS10                                                           
         LA    R1,2                STATUS LENGTH = 3                            
         BAS   RE,GLXRTN                                                        
         B     VCOLNXT                                                          
*                                                                               
STSTBL   DC    C'PREMISADUMKGPFBEST'                                            
         DC    X'00'                                                            
*                                                                               
GLXRTN   NTR1                                                                   
         MVC   DRARGSI+14(1),NDGLRGCT   PASS INDEX NUMBER                       
         ZIC   R5,NDGLRGCT                                                      
         MH    R5,=Y(NDGLRGXE)     MULTIPLY BY LENGTH OF FILTER                 
         MH    R5,=Y(NDGLRXNM)     MULTIPLY BY NUM OF FILTERS                   
         L     RE,NDAGLARG         GET GLARG EXTENSION AREA                     
         LTR   RE,RE               DO WE HAVE ADDR (NONE ON-LINE)               
         BZ    GLXX                                                             
         AR    RE,R5               POINT INTO TABLE                             
                                                                                
         LA    RF,NDGLRXNM         NUMBER OF FILTERS IN EXTENSION               
GLX05    CLI   0(RE),0             ROOM HERE?                                   
         BE    GLX10                                                            
         LA    RE,NDGLRGXE(RE)     LENGTH OF EACH FILTER AREA                   
         BCT   RF,GLX05                                                         
         B     GLXX                NO MORE ROOM /EXIT FOR NOW                   
GLX10    MVC   0(1,RE),12(R4)      SET FIRST CHARACTER AS INDICATOR             
         EX    R1,*+8              SET FILTER FIELD                             
         B     *+10                                                             
         MVC   1(0,RE),22(R4)                                                   
GLXX     XIT1                                                                   
                                                                                
                                                                                
*                                                                               
VCOLFPE  DS    0H                                                               
         L     R1,NDADATES                                                      
         USING DATELSTD,R1                                                      
                                                                                
         CLC   =C'RFP',0(R1)       IF RFP FLAG (SO MUST BE ON-LINE)             
         BE    VCOLFPE2            SKIP PERIOD CHECK - SYMBOLIC                 
*                                  NAMES CAN NOT BUILD DATELIST                 
                                                                                
         BAS   RE,PEREXP           CHECK FOR PERIOD EXPRESSION                  
         CLI   EXPLIST,0                                                        
         BZ    VCOLDEC                                                          
         CLI   EXPLIST,X'FF'       CHECK EXPRESSION CONSISTENT                  
         BE    BADPERD                   WITH REQUEST PERIOD                    
         MVC   DRARGSI+15(1),EXPLIST    PERIOD NUMBER = ARG16                   
VCOLFPE2 BAS   RE,HEADFILT                                                      
         DROP  R1                                                               
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLDEC  CLC   12(R3,R4),=C'DEC'   DECIMAL FOR COMPUTES                         
         BNE   VCOLCMS                                                          
         CLI   11(R4),0                                                         
         BL    BADCOL                                                           
         CLI   11(R4),5                                                         
         BH    BADCOL                                                           
         MVC   DRDECO,11(R4)                                                    
         B     VCOLNXT                                                          
VCOLCMS  CLC   12(R3,R4),=C'CMS'   COMMAS                                       
         BE    DOCOMMA                                                          
         CLC   12(R3,R4),=C'COMMAS'                                             
         BNE   VCOLFPG                                                          
DOCOMMA  OI    DROPTSO,DRCOMMAO                                                 
         B     VCOLNXT                                                          
         EJECT                                                                  
         SPACE 1                                                                
VCOLFPG  CLC   12(3,R4),=C'PG '    PRODUCT FILTER = PRODUCT GROUP               
         BNE   VCOLFPL             (SHARES POSITION WITH PROD FILT)             
         XC    KEY,KEY                GET PRDGRP ID                             
         MVC   WORK(3),=X'FFFFFF'                                               
         MVC   KEY+5(1),22(R4)     DEFAULT IS LETTER ONLY                       
         LA    RE,23(R4)              IS IT FILTER                              
         LA    RF,3                                                             
         CLI   0(RE),X'C1'                                                      
         BL    VCPG5                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         MVC   WORK(3),23(R4)         NO FILTER/ID NUMBER-CHECK IT              
         PACK  WORK+10(3),WORK(5)       ALIGN PWOS LEFT                         
         MVC   WORK(1),22(R4)                                                   
         MVC   WORK+1(2),WORK+10                                                
         MVI   WORK+3,0                                                         
         MVC   KEY+5(3),WORK       SET ID NUMBER                                
* - READ PROD GROUP REC TO SEE IF PGRDEF/PGROUP EXISTS                          
VCPG5    MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBEFFCLI                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(8),KEYSAVE                                                   
**       BNE   OPTERR                                                           
         BNE   BADCOL                                                           
* - SCHEME/GROUP  EXISTS, NOW SET INTO DRIVER TABLE                             
         LA    R1,1                                                             
         LA    R5,NDPRGFLT                                                      
         LA    RF,10           MAX NUMBER OF PRD GROUP COLUMN FILTER            
VCPG10   CLI   0(R5),0                                                          
         BE    VCPG20                                                           
         LA    R5,3(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,VCPG10                                                        
**       B     OPTERR                                                           
         B     BADCOL                                                           
VCPG20   STCM  R1,1,DRARGSI+9          SET POSITION                             
         MVC   0(3,R5),WORK            SET PGROUP CODE                          
* - NOTE PGROUP CODE IS NO LONGER USED BY WRITER SYSTEM SINCE                   
*   IT NOW BUILDS LIST OF PRODUCTS FOR PRODUCT GROUPS                           
*                                                                               
* - GET PRODUCTS FOR THIS PRODGROUP INTO NBAPLIST                               
         BAS   RE,BUILDLST                                                      
         B     VCOLNXT                                                          
BUILDLST NTR1                                                                   
         L     R5,NBAPLIST                                                      
         LTR   R5,R5               ARE WE OFFLINE                               
         BZ    VCPG40                                                           
*                                  YES/FILL NBAPLIST                            
         LR    R0,R1               SAVE BUMP POINTER                            
         MVC   KEY(2),=X'0D81'                                                  
         XC    KEY+8(5),KEY+8                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
VCPG22   CLC   KEY(6),KEYSAVE      AM/CLT/GROUP ID                              
         BNE   VCPG40              NEXT COLUMN                                  
         UNPK  WORK+10(5),KEY+6(3)      UNPACK PWOS OF KEY                      
         MVC   WORK(3),WORK+10                                                  
         LA    RE,WORK             CHECK IT AGAINST REQUESTED ID                
         LA    RF,23(R4)                                                        
         LA    R1,3                                                             
VCPG30   CLI   0(RF),C'*'                                                       
         BE    VCPG35                                                           
         CLI   0(RF),X'40'                                                      
         BNH   VCPG35                                                           
         CLC   0(1,RF),0(RE)                                                    
         BNE   VCPG37                                                           
VCPG35   LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,VCPG30                                                        
* - FIND 1 BYTE PROD NUMBER                                                     
         L     RE,NBACLI                                                        
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING CKEY,RE                                                          
         LA    RE,CLIST                                                         
         DROP  RE                                                               
         LA    R1,256                                                           
         CLC   0(3,RE),KEY+8                                                    
         BE    VCPG36                                                           
         LA    RE,4(RE)                                                         
         BCT   R1,*-14                                                          
         DC    H'0'                DUMP IF NOT FOUND                            
VCPG36   DS    0H                                                               
         LR    R1,R0               RESTORE POINTER                              
         BCTR  R1,0                                                             
         MH    R1,=H'256'          BUMP TO PRDGRP AREA                          
         L     R5,NBAPLIST                                                      
         AR    R5,R1                                                            
         LA    R1,256                                                           
         CLI   0(R5),0                                                          
         BE    VCPG36A                                                          
         LA    R5,1(R5)                                                         
         BCT   R1,*-12                                                          
         DC    H'0'                                                             
VCPG36A  MVC   0(1,R5),3(RE)       SET ONE BYTE PROD NUMBER                     
*                                                                               
VCPG37   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR  ',KEY,KEY                     
         B     VCPG22                                                           
VCPG40   XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
VCOLFPL  CLC   12(3,R4),=C'PLF'    PRODUCT FILTER = PLAN FILTER                 
         BNE   VCOLMNMX            (SHARES POSITION WITH PROD FILT)             
         L     RF,ATWA                                                          
         USING T320FFD,RF                                                       
         CLC   CONREC(2),=CL2'PW'   CHECK FOR PUP WRITER                        
         BNE   BADCOL                                                           
         DROP  RF                                                               
* - LOAD PLAN FILTER INTO TABLE                                                 
         LA    R1,1                                                             
         LA    R5,NDPRGFLT                                                      
         LA    RF,10           MAX NUMBER OF PRD GROUP COLUMN FILTER            
VCFPL10  CLI   0(R5),0                                                          
         BE    VCFPL20                                                          
         LA    R5,3(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,VCFPL10                                                       
         B     BADPERD                                                          
VCFPL20  STCM  R1,1,DRARGSI+9          SET POSITION                             
         MVC   0(3,R5),22(R4)          SET PLAN FILTER                          
         B     VCOLNXT                                                          
         SPACE 1                                                                
         SPACE 1                                                                
VCOLMNMX BAS   RE,VCOLMXMN          MAX/MIN EXPRESSION                          
         BE    VCOLNXT                                                          
         B     VCOLRTAL                                                         
         SPACE 1                                                                
VCOLRTAL CLC   12(5,R4),=C'RIGHT'  RIGHT ALIGN OUTPUT                           
         BNE   VCOLMAT                                                          
         OI    DROPTSO,DRALGNRO                                                 
         B     VCOLNXT                                                          
         SPACE 1                                                                
VCOLMAT  CLC   12(3,R4),=C'MAT'    MATCH/UNMATCH                                
         BNE   *+12                                                             
         OI    DRARGSI+12,X'04'    MATCH                                        
         B     VCOLNXT                                                          
         CLC   12(3,R4),=C'UNM'    UNMATCH                                      
         BNE   VCOLFEND                                                         
         OI    DRARGSI+12,X'08'                                                 
         B     VCOLNXT                                                          
*                                                                               
VCOLFEND B     BADCOL                                                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDVALMNMX                                                      
         EJECT                                                                  
*              WRAP UP COLUMNS                                                  
         SPACE 3                                                                
VCOLNXT  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL12                                                        
         CLI   NDEXTEND,1          IF THERE IS AN EXTENSION PENDING             
         BNE   VCOLNXT2                                                         
         MVI   NDEXTEND,2             NOT TIME TO WRAP UP YET                   
         B     XIT                                                              
         SPACE 1                                                                
VCOLNXT2 MVI   NDEXTEND,0                                                       
         CLC   DRRTNI,=C'NIACCGEN' UNLESS ACCOUNTING                            
         BE    VCOLNXT4                                                         
         CLC   DRRTNI,=C'NIAST   '                                              
         BE    VCOLNXT4                                                         
*        MVC   DRARGSI+12(1),NDPREFIX   A=ACTUAL E=ESTIMATED (ARG13)            
         CLI   NDPREFIX,C'A'                                                    
         BNE   *+8                                                              
         OI    DRARGSI+12,X'01'    X'01'=ACTUAL                                 
         CLI   NDPREFIX,C'E'                                                    
         BNE   *+8                                                              
         OI    DRARGSI+12,X'02'    X'02'=ESTIMATED                              
         SPACE 1                                                                
VCOLNXT4 MVI   DRNARGSI,16         ALWAYS PASS ALL ARGUMENTS                    
         MVC   DRARGSO+12(1),NDPREFIX                                           
         MVI   DRNARGSO,16                                                      
         BAS   RE,MOREVAL          ODD VALIDATION ROUTINES                      
         BAS   RE,ARGADJ           MAY ADJUST ARGUMENTS FOR SPECIALS            
         BAS   RE,HEADPOP          MAY POP IN ADDITIONAL HEADINGS               
         SPACE 1                                                                
         TM    DRFLAGO,X'80'       IF THERE IS ANY PRINTING                     
         BNO   VCOLGEN                                                          
         ZIC   R1,NDTOTWID         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STC   R1,NDTOTWID                                                      
         SPACE 1                                                                
VCOLGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VCOLGEN2                                                         
         MVC   DRLABELI,NDMYLABL                                                
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,NDMYPOSO                                                  
         GOTO1 GCOLDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOLGEN2                                                         
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE 1                                                                
VCOLGEN2 BAS   RE,ANYMOCOL         ANU COLUMNS TO SNEAK IN HERE?                
         L     R1,NDADATES         SLIDE DATE EXPRESSION LIST                   
         USING DATELSTD,R1                                                      
         MVC   EXPLIST(16),EXPLIST+1                                            
         MVI   EXPLIST+16,0                                                     
         CLI   EXPLIST,0           IF THERE IS ANOTHER LEFT                     
         BE    XIT                                                              
         B     VCOL1                  AND GO BACK TO GEN ANOTHER                
         DROP  R1                                                               
         SPACE 1                                                                
         USING T320FFD,R4                                                       
BADCOL   MVC   HALF,=H'656'       IMVLAID COLUMN EXPRESSION                     
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUSER  MVC   HALF,=H'653'        CAN'T FIND USER RECORD                       
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADPLAN  MVC   HALF,=H'654'        INVLAID PLAN FOR PUP REQUEST                 
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADPERD  MVC   HALF,=H'661'        PERIOD INCONSISTET WITH REQUEST              
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADCOL2  MVC   HALF,=H'655'        NOT ALLOWED AS A COLUMN                      
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADNEED1 MVC   HALF,=H'648'        MUST BE AT LEAST 1 ROW/1 COLUMN              
         MVI   BYTE,0                                                           
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     ERR2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              ANY MORE COLUMNS TO GENERATE                                     
         SPACE 3                                                                
ANYMOCOL NTR1                                                                   
         CLI   COLRANK,0           COLUMN RANKING                               
         BE    XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         MVC   DRARGSI(1),COLRANK       PASS ARGUMENT                           
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
*****    OI    DROPTSO+1,DRNOLBOX  NO BOX TO LEFT OF RANK                       
         GOTO1 GCOLDRON                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK FOR ENTRY ARGUMENTS                             
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER ENTRY)                          
         SPACE 1                                                                
ENTARG   NTR1                                                                   
         CLC   DRRTNI,=C'NIHICOMM' HIGH LEVEL COMMENTS                          
         BNE   ENTARG2                                                          
         MVC   NDCOMDEF,22(R4)     NEED COMMENT DEFINITION                      
         CLI   1(R4),2             AT LENGTH 2 OR MORE                          
         BNL   XIT                                                              
         MVC   HALF,=H'649'        NEED TO SPECIFY SET OF COMMENTS              
         MVI   BYTE,0                                                           
         MVI   WORK+3,0                                                         
         B     ERR2                                                             
         SPACE 1                                                                
ENTARG2  CLC   12(5,R4),=C'TEXT '  TEXT= NEEDS (SURPRISE) TEXT                  
         BNE   ENTARG4                                                          
         ZIC   R1,1(R4)            (L'TEXT)                                     
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                                                             
         CH    R1,=H'24'                                                        
         BL    *+8                                                              
         LA    R1,24                                                            
         STC   R1,DRLITLNO                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   DRLITO(0),22(R4)                                                 
         SPACE 1                                                                
ENTARG4  CLC   DRRTNI,=C'NISTACC ' COLUMN STACK NEEDS ARGS                      
         BNE   ENTARG6                                                          
*        BAS   RE,VALACC                                                        
         GOTO1 =A(OVERFLOW),DMCB,(2,(RC)),RR=RELO                               
         DS    0H                                                               
         XIT1                                                                   
         B     XIT                                                              
         SPACE 1                                                                
ENTARG6  CLC   DRRTNI,=C'NIAST   ' ACCOUNTING STACK FLESH OUT ARGS              
         BNE   ENTARG8                                                          
         BAS   RE,FLESHAST                                                      
         B     XIT                                                              
         SPACE 1                                                                
ENTARG8  B     XIT                                                              
         EJECT                                                                  
*              FLESH OUT ARGUMENTS FOR ACCOUNTING STACK                         
         SPACE 3                                                                
*              INPUT               NDASTDEF HAS STACK DEF                       
*                                  ARG 1 HAS COLUMN DEFINITION                  
*              OUTPUT              SET ARGS 1-8 WITH ACCGEN ROUTINES            
         SPACE 1                                                                
FLESHAST NTR1                                                                   
         LA    R2,NDASTDEF                                                      
         LA    R3,DRARGSI          ADDRESS INPUT ARGS                           
         MVI   DRNARGSI,16                                                      
         MVC   WORK(1),DRARGSI     COLUMN DEFINITION                            
         LA    R0,8                (UP TO 8 TERMS)                              
         SPACE 1                                                                
FLESH2   MVC   0(1,R3),1(R2)       PASS ARGUMENT STRAIGHT THROUGH               
         CLI   1(R2),250           IF ROUTINE > 250 (SPACE ETC)                 
         BH    FLESHNXT               ITS OK FOR ANY STACK                      
         CLI   WORK,219          IF COL IS PCT OF PREVIOUS COLUMN               
         BNE   *+12              (ENTRY=ASTPPCT)                                
         MVI   0(R3),219            SET IT                                      
         B     FLESHNXT               OK                                        
         CLI   WORK,0              OR, IF STACK IS GENERAL                      
         BNE   FLESH3                                                           
         CLI   1(R2),200               ANY ACCGEN ROUTINE IS OK                 
         BL    FLESHNXT                                                         
         SPACE 1                                                                
FLESH3   L     R1,=A(ASCOMTAB)                                                  
         A     R1,RELO                                                          
         MVC   DUB(1),1(R2)                                                     
         MVC   DUB+1(1),WORK                                                    
         SPACE 1                                                                
FLESH4   MVC   0(1,R3),2(R1)       PASS ROUTINE NUMBER                          
         CLC   DUB(2),0(R1)        CHECK MATCH                                  
         BE    FLESHNXT                                                         
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   FLESH4                                                           
         SPACE 1                                                                
         L     R1,=A(ASCOMTAB)     TRY COMBO ROUND THE OTHER WAY                
         A     R1,RELO                                                          
         MVC   DUB(1),WORK                                                      
         MVC   DUB+1(1),1(R2)                                                   
         SPACE 1                                                                
FLESH6   MVC   0(1,R3),2(R1)       PASS ROUTINE NUMBER                          
         CLC   DUB(2),0(R1)        CHECK MATCH                                  
         BE    FLESHNXT                                                         
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   FLESH6                                                           
         MVI   0(R3),X'FF'         NO MATCH - PASS BACK N/A                     
         SPACE 1                                                                
FLESHNXT DS    0H                                                               
         LA    R2,2(R2)                                                         
         LA    R3,1(R3)                                                         
         CLI   1(R2),0                                                          
         BE    XIT                                                              
         BCT   R0,FLESH2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD EXPRESSIONS                           
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER ENTRY)                          
*              OUTPUT              RETURN IN EXPLIST                            
*                                  X'00' NOT VALID                              
*                                  X'FF' VALID BUT DATE MISSED                  
*                                  X'NN00' PERIOD NUMBER                        
*                                  OR LIST OF PERIOD NUMBERS                    
         SPACE 1                                                                
*              VALID               W1 THRU W105   SINGLE WEEK                   
*                                  M1 THRU M25           MONTH                  
*                                  Q1 THRU Q8            QUARTER                
*                                  D1 THRU D14           DAYS                   
*                                  WN-N           FLOW   WEEK                   
*                                  MN-N                  MONTH                  
*                                  QN-N                  QUARTER                
*                                  DN-N                  DAYS                   
*                                                                               
         SPACE 1                                                                
PEREXP   NTR1                                                                   
         L     R5,NDADATES                                                      
         USING DATELSTD,R5                                                      
         CLI   EXPLIST,0           IF THERE IS ONE THERE ALREADY                
         BNE   XIT                                                              
*                                                                               
         SR    R2,R2                                                            
         LA    R3,105                                                           
         CLI   12(R4),C'W'         EXPRESSIONS MUST START W/M/Q/D               
         BE    PEX2                                                             
         LA    R2,105                                                           
         LA    R3,25                                                            
         CLI   12(R4),C'M'                                                      
         BE    PEX2                                                             
         LA    R2,130                                                           
         LA    R3,8                                                             
         CLI   12(R4),C'Q'                                                      
         BE    PEX2                                                             
         LA    R2,138                                                           
         LA    R3,14                                                            
         CLI   12(R4),C'D'                                                      
         BE    PEX2                                                             
         LA    R2,152                                                           
         LA    R3,2                                                             
         CLI   12(R4),C'Y'                                                      
         BE    PEX2                                                             
         B     XIT                                                              
         SPACE 1                                                                
PEX2     LA    R4,13(R4)           R4=A(FIRST POTENTIAL NUMBER)                 
         BAS   RE,PEXNUM                                                        
         LTR   R0,R0               DID WE GET A VALID NUMBER                    
         BZ    XIT                                                              
         CR    R0,R3               THAT WASN'T TOO BIG?                         
         BH    XIT                                                              
         AR    R0,R2                                                            
         STC   R0,EXPLIST          THEN SAVE THAT NUMBER                        
         CLI   0(R4),C'-'          IS THERE A RANGE SPECIFIED?                  
         BNE   PEX6                                                             
         LA    R4,1(R4)                                                         
         BAS   RE,PEXNUM           WAS THE SECOND NUMBER VALID?                 
         LTR   R0,R0                                                            
         BZ    PEXNO                                                            
         CR    R0,R3                                                            
         BH    PEXNO                                                            
         AR    R0,R2                                                            
         ZIC   R1,EXPLIST                                                       
         CR    R0,R1                                                            
         BNH   PEXNO               SECOND MUST BE HIGHER THAN FIRST             
         SR    R1,R0                                                            
         LCR   R1,R1                                                            
         CH    R1,=H'16'           AND NOT MORE THAT 16 MORE                    
         BH    PEXNO                                                            
         ZIC   R1,EXPLIST                                                       
         LA    R1,1(R1)                                                         
         LA    R2,EXPLIST+1                                                     
         SPACE 1                                                                
PEX4     STC   R1,0(R2)            GENERATE A LIST OF NUMBERS                   
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         CR    R1,R0                                                            
         BNH   PEX4                                                             
         SPACE 1                                                                
PEX6     LA    R2,EXPLIST          NOW HAVE A LIST OF NUMBERS                   
         LA    R3,17               IN EXPLIST                                   
         SPACE 1                                                                
PEX8     CLI   0(R2),0                                                          
         BE    XIT                                                              
         ZIC   R1,0(R2)            CHECK THAT EACH NUMBER                       
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         AR    R1,R5                                                            
         OC    0(4,R1),0(R1)       PRODUCES A VALID DATE RANGE                  
         BZ    PEXMISS                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,PEX8                                                          
         B     XIT                                                              
         SPACE 1                                                                
PEXMISS  MVI   EXPLIST,X'FF'       PASS BACK X'FF'                              
         B     XIT                                                              
         SPACE 1                                                                
PEXNO    XC    EXPLIST,EXPLIST                                                  
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 1                                                                
PEXNUM   SR    R0,R0               VALIDATE FOR NUMERIC                         
*                                  R4=A(POTENTIAL EXPRESSION)                   
*                                  PASS BACK NUMBER IN R0                       
         SPACE 1                                                                
PEXNUM2  CLI   0(R4),C' '          DELIMITED BY SPACE OR DASH                   
         BER   RE                                                               
         CLI   0(R4),C'-'                                                       
         BER   RE                                                               
         CLI   0(R4),C'0'          CHECK FOR NUMERIC DIGIT                      
         BL    PEXNUMNO                                                         
         CLI   0(R4),C'9'                                                       
         BH    PEXNUMNO                                                         
         MH    R0,=H'10'           MULTIPLY PREVIOUS BY 10                      
         ZIC   R1,0(R4)            AND ADD THIS DIGIT                           
         SLL   R1,28               (STRIP TOP BITS)                             
         SRL   R1,28                                                            
         AR    R0,R1                                                            
         LA    R4,1(R4)                                                         
         B     PEXNUM2                                                          
         SPACE 1                                                                
PEXNUMNO SR    R0,R0               NO GOOD - RETURN ZERO                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT EDITS FOR COMPUTES                         
         SPACE 3                                                                
COMPEDIT NTR1                                                                   
*                                  R4=A(SCANNER TABLE ENTRY)                    
         TM    NDLOCAL,NDMINUS         ...IF FLOAT = MINUS OPTION               
         BZ    COMPED                                                           
         NI    DROPTSO,255-DRMINUSO    ...TURN OF  MINUS                        
         MVI   DRFLOATO,C'-'            ...AND TURN ON FLOAT=-                  
COMPED   ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BE    COMPPCT                                                          
         LA    RE,8(R2)            RE=A(START OF INPUT STRING)                  
         ZIC   R0,5(R2)            R0=LENGTH OF INPUT STRING                    
         SPACE 1                                                                
COMPED1  LA    R1,EDITLIST         ELSE LOOK FOR FIRST OPERAND                  
         SPACE 1                                                                
COMPED2  CLI   0(R1),0                                                          
         BNE   COMPED2B                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,COMPED1                                                       
         B     XIT                 NOTHING THERE YET                            
         SPACE 1                                                                
COMPED2B CLC   0(1,R1),0(RE)                                                    
         BE    COMPED3                                                          
         LA    R1,4(R1)                                                         
         B     COMPED2                                                          
         SPACE 1                                                                
COMPED3  MVC   DRDECO,1(R1)        PICK UP ITS EDIT CHARACTERISTIC              
         MVC   DRDIVO,2(R1)                                                     
         B     XIT                                                              
         SPACE 1                                                                
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     XIT                                                              
         SPACE 1                                                                
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
         EJECT                                                                  
*              A BIT MORE VALIDATION FOR COLUMNS                                
         SPACE 3                                                                
MOREVAL  NTR1                                                                   
         CLC   DRRTNI,=CL8'NISTACK'      CHECK STACK SPECIFIED                  
         BE    MV2                                                              
         CLC   DRRTNI,=CL8'NISTDATA'                                            
         BE    MV2                                                              
         CLC   DRRTNI,=CL8'NIAST'                                               
         BE    MV4                                                              
         CLC   DRRTNI,=CL8'NIASTDTA'                                            
         BE    MV4                                                              
         B     XIT                                                              
         SPACE 1                                                                
MV2      OC    NDSTADEF,NDSTADEF                                                
         BNZ   XIT                                                              
         B     NEEDSTAK                                                         
         SPACE 1                                                                
MV4      OC    NDASTDEF,NDASTDEF                                                
         BNZ   XIT                                                              
         SPACE 1                                                                
NEEDSTAK MVC   HALF,=H'650'        STACK NOT DEFINED                            
         MVI   BYTE,0                                                           
         B     ERR2                                                             
         EJECT                                                                  
*              ROUTINE TO ADJUST ARGUMENTS                                      
         SPACE 3                                                                
ARGADJ   NTR1                                                                   
         SPACE 1                                                                
ARGADJ2  CLI   DRATTRIB,C'D'       DEMO EXPRESSION?                             
         BNE   ARGADJ4                                                          
         MVI   BYTE,X'80'           X'80' DENOTES ESTIMATED                     
         CLI   NDPREFIX,C'A'                                                    
         BNE   *+8                                                              
         MVI   BYTE,X'40'           X'40' DENOTES ACTUAL                        
         OC    NDANYDEM(1),BYTE                                                 
         CLI   DRARGSI,21          CHECK TARGET REQUEST                         
         BL    ARGADJ4                                                          
         MVI   NDPLNHED,C'Y'       SET FOR PUP TARGET READ                      
         SPACE 1                                                                
ARGADJ4  CLI   DRATTRIB+1,C'G'     GOAL EXPRESSION?                             
         BNE   ARGADJ6                                                          
         OI    NDCOLIND,X'80'                                                   
         CLI   DRATTRIB+2,C'H'     GOAL HIST COLUMN?                            
         BNE   XIT                                                              
         OI    NDCOLIND,X'01'                                                   
         B     XIT                                                              
         SPACE 1                                                                
ARGADJ6  CLI   DRATTRIB,C'B'       BUDGET EXPRESSION                            
         BNE   ARGADJ8                                                          
         OI    NDCOLIND,X'10'                                                   
         SPACE 1                                                                
ARGADJ8  CLC   DRRTNO,=C'NOTOTSTK' WAS TOTSTACK SPECIFIED?                      
         BNE   ARGADJ10                                                         
         OI    NDCOLIND,X'40'      NOTE THIS                                    
         SPACE 1                                                                
ARGADJ10 DS    0H                                                               
         SPACE 1                                                                
ARGADJ12 B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT IN ADDITIONAL HEADINGS                            
         SPACE 3                                                                
*                                  JUST USED BY M FLAVOR                        
ESTACPOP NTR1                                                                   
         MVC   DRH4ALL,DRH3ALL     SHUFFLE HEADERS DOWN                         
         MVC   DRH3ALL,DRH2ALL                                                  
         MVC   DRH2ALL,DRH1ALL                                                  
         XC    DRH1ALL,DRH1ALL     AND PUT LITERAL IN LINE 1                    
         MVI   DRH1LITL,3                                                       
         OI    DRHEAD1,X'80'                                                    
         MVC   DRH1LIT(3),=C'EST'                                               
         CLI   NDPREFIX,C'A'                                                    
         BNE   XIT                                                              
         MVC   DRH1LIT(3),=C'ACT'                                               
         B     XIT                                                              
         SPACE 1                                                                
HEADPOP  NTR1                                                                   
         TM    DRFLAGO,X'80'       NOT NEEDED IF NOT PRINTING                   
         BNO   XIT                                                              
         CLI   DRARGSI+12,0        CHECK FOR ACTUAL/ESTIMATE                    
         BE    HEADPOP2                                                         
         CLI   NDFLAVOR,C'P'       JUST NEEDED BY POSTS                         
         BNE   HEADPOP2                                                         
         TM    DRARGSI+12,X'01'    (AND MAKE SURE ITS 01 OR 02)                 
         BO    *+12                                                             
         TM    DRARGSI+12,X'02'    (SINCE ARGS ARE SHARED)                      
         BNO   HEADPOP2                                                         
         MVC   DRH4ALL,DRH3ALL     SHUFFLE HEADERS DOWN                         
         MVC   DRH3ALL,DRH2ALL                                                  
         MVC   DRH2ALL,DRH1ALL                                                  
         XC    DRH1ALL,DRH1ALL     AND PUT LITERAL IN LINE 1                    
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,3                                                       
         MVC   DRH1LIT(3),=C'EST'                                               
         TM    DRARGSI+12,X'01'                                                 
         BNO   HEADPOP2                                                         
         MVC   DRH1LIT(3),=C'ACT'                                               
         SPACE 1                                                                
HEADPOP2 CLI   DRARGSI+7,0         RAW/EQUIV ETC ETC                            
         BE    HEADPOP8                                                         
         TM    DRARGSI+7,RAW+EQU+NN+NQ                                          
         BZ    HEADPOP8                                                         
         MVC   DRH4ALL,DRH3ALL     SHUFFLE HEADERS DOWN                         
         MVC   DRH3ALL,DRH2ALL                                                  
         MVC   DRH2ALL,DRH1ALL                                                  
         XC    DRH1ALL,DRH1ALL     AND PUT LITERAL IN LINE 1                    
         OI    DRHEAD1,X'80'                                                    
         TM    DRARGSI+7,EQU                                                    
         BO    HEADPOP6                                                         
         TM    DRARGSI+7,NN                                                     
         BO    HEADPP6A                                                         
         TM    DRARGSI+7,NQ                                                     
         BO    HEADPP6B                                                         
         MVI   DRH1LITL,3                   RAW                                 
         MVC   DRH1LIT(3),=C'RAW'                                               
         B     HEADPOP8                                                         
         SPACE 1                                                                
HEADPOP6 MVI   DRH1LITL,6                   EQU                                 
         MVC   DRH1LIT(6),=C'BASE30'                                            
         CLI   NDQBASE,60                                                       
         BNE   *+8                                                              
         MVI   DRH1LIT+4,C'6'                                                   
         B     HEADPOP8                                                         
*                                                                               
HEADPP6A MVI   DRH1LITL,7                   NN                                  
         MVC   DRH1LIT(7),=C'NOG-RAW'                                           
         B     HEADPOP8                                                         
                                                                                
HEADPP6B MVI   DRH1LITL,6                   NQ                                  
         MVC   DRH1LIT(6),=C'NOG-30'                                            
         CLI   NDQBASE,60                                                       
         BNE   *+8                                                              
         MVI   DRH1LIT+4,C'6'                                                   
         B     HEADPOP8                                                         
         SPACE 1                                                                
HEADPOP8 L     RF,ATWA                                                          
         USING T320FFD,RF                                                       
         CLC   CONREC(2),=CL2'PW'   CHECK FOR PUP WRITER                        
         BE    HEDPOP10                                                         
         DROP  RF                                                               
         CLI   NDPRGFLT,0          PRODUCT GROUP HEADS                          
         BE    HEDPOP10                                                         
         ZIC   R1,DRARGSI+9                                                     
         LTR   R1,R1                                                            
         BZ    HEDPOP10                                                         
         BCTR  R1,0                                                             
         LA    R2,NDPRGFLT         GET PGROUP SCHEME                            
         MH    R1,=H'3'                                                         
         AR    R2,R1                                                            
         MVC   WORK(3),0(R2)                                                    
         UNPK  WORK+5(5),1(3,R2)   GET LENGTH OF CODE                           
         LA    R2,1                INTO R2                                      
         LA    R1,WORK+5                                                        
         CLI   0(R1),X'40'                                                      
         BNH   *+16                                                             
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         B     *-16                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),NBACTAM                                                 
         MVC   KEY+3(2),NBEFFCLI                                                
         MVC   KEY+5(1),WORK       SET SCHEME                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(8),KEYSAVE                                                   
         BNE   HEDPOP10                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,AIO,DMWORK           
         L     R4,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=H'24'                                                  
         BAS   RE,GETEL                                                         
         BNE   HEDPOP10                                                         
         USING PRGEL01,R4                                                       
         ZIC   R3,PRGBK1LN         GET 1 BRK LENGTH                             
         MVC   KEY+5(3),WORK       SET SCHEME+ID NUMBER                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(8),KEYSAVE                                                   
         BNE   HEDPOP10                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',KEY+14,AIO,DMWORK           
         L     R4,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         USING PRGEL10,R4                                                       
         BAS   RE,GETEL                                                         
         BNE   HEDPOP10                                                         
         MVC   DATADISP,=H'27'     RESET DATADISP FOR UNITS                     
         MVC   WORK(12),PRGNAM1                                                 
         CR    R3,R2               R2 HAS SAVED LENGTH OF COLUMN                
         BE    *+10                                                             
         MVC   WORK(12),PRGNAM2                                                 
         MVC   DRH4ALL,DRH3ALL     SHUFFLE HEADERS DOWN                         
         MVC   DRH3ALL,DRH2ALL                                                  
         MVC   DRH2ALL,DRH1ALL                                                  
         XC    DRH1ALL,DRH1ALL     AND PUT LITERAL IN LINE 1                    
         OI    DRHEAD1,X'80'                                                    
         MVI   DRH1LITL,12                                                      
         MVC   DRH1LIT(12),WORK                                                 
         B     HEDPOP10                                                         
         SPACE 1                                                                
*                                                                               
HEDPOP10 B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT IN ADDITIONAL HEADINGS                            
         SPACE 3                                                                
HEADFILT NTR1                                                                   
         TM    DRFLAGO,X'80'       NOT NEEDED IF NOT PRINTING                   
         BNO   XIT                                                              
         OC    DRARGSI+4(8),DRARGSI+4     LOOK FOR ANY FILTERS                  
         BNZ   HEADFLT2                                                         
         OC    DRARGSI+13(3),DRARGSI+13   EXCEPT ACTUAL/ESTIMATED               
         BZ    XIT                                                              
         SPACE 1                                                                
HEADFLT2 LA    R1,DRHEAD1          FIND AN EMPTY HEADING                        
         BAS   RE,HEADFLT4                                                      
         LA    R1,DRHEAD2                                                       
         BAS   RE,HEADFLT4                                                      
         LA    R1,DRHEAD3                                                       
         BAS   RE,HEADFLT4                                                      
         LA    R1,DRHEAD4                                                       
         BAS   RE,HEADFLT4                                                      
         B     XIT                 NO ROOM!                                     
         SPACE 1                                                                
HEADFLT4 TM    0(R1),X'80'         LOOK FOR EMPTY SLOT                          
         BNO   HEADFLT6                                                         
         CLC   1(8,R1),=C'NHFILTER'     OR A PREVIOUS VISIT                     
         BNER  RE                                                               
         SPACE 1                                                                
HEADFLT6 OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'NHFILTER'                                           
         MVC   9(16,R1),DRARGSI    PASS ALL THE ARGUMENTS                       
         MVI   25(R1),16           N'ARGUMENTS IS 16                            
         B     XIT                                                              
         EJECT                                                                  
*              GENERAL HEADLINE HOOK ROUTINES                                   
         SPACE 3                                                                
VGENHEAD L     R2,NDAH1            DEAL WITH MAIN TITLE                         
         LA    R2,47(R2)                                                        
         CLI   NDWIDOPT,C'Y'                                                    
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         CLI   NDNAROPT,C'Y'                                                    
         BNE   VGENHB                                                           
         SH    R2,=H'47'                                                        
         L     R1,NDAH4                                                         
         SPACE 1                                                                
VGENHB   MVC   0(34,R2),NDTITLE    REPORT                                       
         MVI   NDRPTYPE,C'D'                                                    
         L     R1,NDGLOBAL                                                      
         USING GLOBALD,R1                                                       
         CLI   GLRECNO,1                                                        
         DROP  R1                                                               
         BE    VGENHB2                                                          
         MVI   NDRPTYPE,C'R'                                                    
         MVC   0(34,R2),NDTITLE2   OR RECAP TITLE                               
         SPACE 1                                                                
VGENHB2  LR    R3,R2                                                            
         A     R3,NDPWIDTH                                                      
         GOTO1 UNDERLIN,DMCB,(34,(R2)),(X'BF',(R3))                             
                                                                                
         OC    NBBILSTR,NBBILSTR           IF FILTERING ON BILLING              
         BZ    VGENHB3                                                          
         LR    R1,R3                                                            
         A     R1,NDPWIDTH         3D HLINE                                     
         A     R1,NDPWIDTH         4TH HLINE                                    
         A     R1,NDPWIDTH         5TH LINE                                     
         LA    R1,3(R1)                                                         
         STCM  R1,15,DUB           SAVE ADDRESS OF PRINT LINE                   
VGENHB3  CLI   NDRPTYPE,C'D'       DETAILS CAN HAVE SUB-TITLE                   
         BNE   VGENHB4                                                          
         CLI   NDNAROPT,C'Y'       (NOT WITH NARROW OPTION)                     
         BE    VGENHB4                                                          
         CLI   NDANYREC,C'Y'       (NOT IF RECAPS ALSO REQUESTED)               
         BE    VGENHB4                                                          
         A     R3,NDPWIDTH                                                      
         MVC   0(34,R3),NDTITLE2   (THIS GOES ON H3)                            
         SPACE 1                                                                
VGENHB4  L     R2,NDAH4            LINE UP THE LEFT SIDE                        
         MVC   BLOCK(80),50(R2)    GET PERIOD OUT OF THE WAY                    
         MVC   50(80,R2),SPACES                                                 
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,NDGLOBAL                                                      
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'6'                                                         
         CLI   NDCMHEAD,C'Y'        .ARE COMMENTS IN HEADLINE                   
         BNE   VGENHB5                                                          
         MVI   NDCMHEAD,0                                                       
         B     VGENHBX                                                          
         SH    R4,=H'4'             .DONT SHUFFLE THESE                         
VGENHB5  BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,6                                                             
         BAS   RE,GETLONG                                                       
         LA    R3,7(R2)                                                         
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         SPACE 1                                                                
VGENHBX  L     R2,NDAH4            RESTORE PERIOD ETC. TO H4                    
         MVC   50(80,R2),BLOCK                                                  
         OC    NBBILSTR,NBBILSTR   BILL FILTERING                               
         BZ    XIT                 NO                                           
                                                                                
         ICM   R3,15,DUB           YES/GET SAVED ADDRESS OF PRINT LINE          
         MVC   0(9,R3),=C'BILLS RUN'        SET THIS AS SUB-TITLE               
         GOTO1 DATCON,DMCB,(2,NBBILSTR),(5,10(R3))                              
         MVI   18(R3),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,NBBILEND),(5,19(R3))                              
         B     XIT                                                              
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GENHEAD                                  
         SPACE 3                                                                
GETLONG  NTR1                                                                   
*              INPUTS              R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=NUMBER OF LINES                           
*              OUTPUT              FULL=WIDEST FOUND                            
         SPACE 1                                                                
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
         SPACE 1                                                                
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XIT                                                              
         A     R1,NDPWIDTH         ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 3                                                                
SHUFFLE  NTR1                                                                   
*              INPUTS              R2=A(START DATA ON FIRST LINE)               
*                                  R3=A(FROM DATA)                              
*                                  R4=NUMBER OF LINES                           
         SPACE 1                                                                
SHUFFLE2 MVC   WORK,0(R3)                                                       
         MVC   0(64,R3),SPACES                                                  
         MVC   0(64,R2),WORK                                                    
         A     R2,NDPWIDTH                                                      
         A     R3,NDPWIDTH                                                      
         BCT   R4,SHUFFLE2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MAINTAIN NAME BUFFER                                  
         SPACE 3                                                                
*              PARAMETERS          1 A(16 BYTE KEY)                             
*                                  2 A(24 BYTE OUTPUT AREA)                     
         SPACE 1                                                                
VGETNAME LM    R2,R3,0(R1)         R2=INPUT R3=OUTPUT                           
         L     R4,NDANAMES         R4=POOL HEADER                               
*                                  CONTAINS PRESENT/MAX/WIDTH                   
         LA    R5,12(R4)           R5=POOL ENTRY                                
         L     R0,4(R4)            R0=MAX N'ENTRIES                             
         SPACE 1                                                                
VGN2     CLI   0(R5),0             IS ENTRY EMPTY                               
         BE    VGNGET              YES - GO AND GET RECORD                      
         CLC   0(16,R5),0(R2)      CAN WE FIND RECORD                           
         BE    VGNEND              YES - PASS BACK NAME                         
         A     R5,8(R4)                                                         
         BCT   R0,VGN2                                                          
         SPACE 1                                                                
VGNGET   L     R1,0(R4)            PICK UP POOL POINTER                         
         LA    R1,1(R1)            +1                                           
         C     R1,4(R4)            CHECK AGAINST POOL MAX                       
         BNH   *+8                                                              
         LA    R1,1                RESET TO START OF POOL                       
         ST    R1,0(R4)            AND SAVE THIS                                
         BCTR  R1,0                                                             
         MH    R1,10(R4)           DISPLACE TO THIS ENTRY                       
         LA    R5,12(R4,R1)                                                     
         MVC   0(16,R5),0(R2)      SAVE ACCOUNT NUMBER                          
         MVC   16(24,R5),SPACES    PRECLEAR NAME                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(16),0(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BNE   VGNREST                                                          
         SPACE 1                                                                
VGNREST  DS    0H                  RESTORE KEY IF NECESSARY                     
         SPACE 1                                                                
VGNEND   MVC   0(24,R3),16(R5)     RETURN NAME TO USER                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT MONTH                                      
         SPACE 3                                                                
*              INPUT               R2=A(COMPRESSED DATE)                        
*              OUTPUT              RETURN YYMM EBCDIC IN WORK                   
*                                         M    BINARY IN WORK+4                 
         SPACE 1                                                                
VGETBM   GOTO1 DATCON,DMCB,(2,0(R2)),(X'20',WORK)                               
         CLI   NDPEROPT,C'C'       IF CALENDAR, SKIP BROADCAST BIT              
         BE    VGETBM6                                                          
         CLI   NDPEROPT,C'B'       IF BROADCAST, DO BROADCAST BIT               
         BE    VGETBM4                                                          
         CLI   NDFLAVOR,C'E'       IF FLAV=EST USE NBUSER+2                     
         BNE   VGETBM2                                                          
         CLI   NBUSER+2,C'C'       IF CALENDAR SKIP BROADCAST BIT               
         BE    VGETBM6                                                          
         B     VGETBM4                                                          
         SPACE 1                                                                
*                                  ELSE USE NBUSER+3                            
VGETBM2  CLI   NBUSER+3,C'C'       IF CALENDAR, SKIP BROADCAST BIT              
         BE    VGETBM6             ELSE MAKE SURE BY BUMPING END                
         SPACE 1                                                                
*                                  BROADCAST BIT                                
VGETBM4  GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   R4,DMCB             GET TO THE END OF THE BROADCAST WEEK         
         ZIC   R5,NBUSER+4         (START DOW)                                  
         SLL   R5,28                                                            
         SRL   R5,28                                                            
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,1                (DEFAULT IS MONDAY)                          
         BCT   R5,*+8              (PREVIOUS DAY TO GET EOW)                    
         LA    R5,7                (BUT MONDAY ENDS ON SUNDAY)                  
         SR    R5,R4                                                            
         BZ    VGETBM6                                                          
         BP    *+8                                                              
         AH    R5,=H'7'                                                         
         GOTO1 ADDAY,DMCB,WORK,(X'20',WORK+6),(R5)                              
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
VGETBM6  PACK  WORK+8(8),WORK+2(2) YYMM NOW IN WORK                             
         CVB   R1,WORK+8                                                        
         STC   R1,WORK+4           RETURN MONTH NUMBER WORK+4                   
         B     XIT                                                              
         EJECT                                                                  
*              DRONE UTILITIES                                                  
         SPACE 3                                                                
VINTDRON DS    0H                  INITIALIZATION                               
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'NETWRITE'                                            
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         CLC   CONREC(2),=CL2'PW'   CHECK FOR PUP WRITER                        
         BNE   VINTD50                                                          
         MVC   DRDICT(3),=CL3'PUP'  DICTIONARY GOES AFTER PUP                   
         OI    NDCOLIND,X'20'       SET PUP INDICATOR                           
VINTD50  GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         CLI   NDGRAND,C'N'        OPTION NOT TO GET GRAND TOTALS               
         BE    XIT                                                              
         L     R1,DRCURBUF         PUT IN REPORT TOTALS HERE                    
         MVC   0(2,R1),=X'4802'                                                 
         LA    R1,2(R1)                                                         
         MVC   0(2,R1),=X'820A'                                                 
         MVC   2(8,R1),=C'NOREPORT'                                             
         LA    R1,10(R1)                                                        
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
VROWDRON NTR1                      VALIDATE A ROW                               
         MVI   DRACTION,DRROW                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GROWDRON NTR1                      GENERATE A ROW                               
         MVI   DRACTION,DRGENROW                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCOLDRON NTR1                      VALIDATE A COLUMN                            
         MVI   DRACTION,DRCOL                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCOLDRON NTR1                      GENERATE A COLUMN                            
         MVI   DRACTION,DRGENCOL                                                
         B     ALLDRONE                                                         
         SPACE 1                                                                
VCMPDRON NTR1                      VALIDATE A COMP                              
         MVI   DRACTION,DRCMP                                                   
         B     ALLVAL                                                           
         SPACE 1                                                                
GCMPDRON NTR1                      GENERATE A COMP                              
         MVI   DRACTION,DRGENCMP                                                
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
         SPACE 1                                                                
VWRPDRON DS    0H                  WRAP UP                                      
         MVI   DRACTION,DRWRAPUP                                                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
         B     XIT                                                              
         SPACE 1                                                                
VUSRDRON NTR1                      VALIDATE USER RECORD                         
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),NDAGYABR         KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         CLI   DRERROR,0                                                        
         BNE   BADUSER                                                          
         TM    DROPTSO,DRBKMINO    IF BRACKET=M SPECIFIED                       
         BNO   XIT                                                              
         NI    DROPTSO,X'DF'       DON'T NEED MINUS=YES                         
         CLI   DRFLOATO,C'-'                                                    
         BNE   XIT                                                              
         MVI   DRFLOATO,0                OR FLOAT=-                             
         B     XIT                                                              
         EJECT                                                                  
*              MORE DRONE UTILITIES                                             
         SPACE 3                                                                
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 NDDRONE,DMCB,DRGEN                                               
         BAS   RE,SETREAD                                                       
         B     XIT                                                              
         SPACE 1                                                                
ALLDRONE GOTO1 NDDRONE,DMCB,DRGEN                                               
         B     XIT                 USER NEEDS TO TEST DRERROR                   
         SPACE 1                                                                
BADDRONE MVC   HALF,=H'664'        ERROR MESSAGES FROM DRONE                    
         MVI   BYTE,X'FF'                                                       
         XC    ELEM,ELEM                                                        
         MVC   ELEM+1(30),DRERRMSG                                              
         MVI   ELEM,31                                                          
         MVI   WORK+3,0            SET MESSAGE TYPE                             
         B     MYCURSOR                                                         
         SPACE 1                                                                
TRACDRON NTR1                                                                   
         CLI   NDTRAOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     R3,NDADPG                                                        
         SPACE 1                                                                
TRACD2   CLI   0(R3),0                                                          
         BE    XIT                                                              
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRACD2                                                           
         EJECT                                                                  
*              ROUTINE TO SET READ SWITCHES FROM DICTIONARY                     
         SPACE 3                                                                
SETREAD  NTR1                                                                   
         CLI   DRATTRIB,C'P'       PACKAGE RECORD                               
         BNE   *+8                                                              
         OI    NBACCFLT,X'04'      YES/SET NETBLOCK TO GET PKGS                 
         CLI   DRATTRIB,C'S'       S=STARCOM=DAPA REPORT                        
         BNE   *+8                                                              
         OI    NBACCFLT,X'08'                                                   
         CLI   DRATTRIB,C'D'       REQUESTING DEMOS                             
         BNE   *+8                                                              
         MVI   NBDODEMS,C'Y'       YES/SET NETBLOCK                             
         CLI   DRATTRIB,C'A'       DL DOWNLOAD                                  
         BNE   SRTX                                                             
         OI    NDDOWNL,GLDLALPH    SEND ALL AS ALPHA                            
         OI    NDDOWNL,GLDLNOTR    NO TRUNCATION                                
         OI    NDDOWNL,GLDLNOHD    NO HEADINGS                                  
SRTX     B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE 1                                                                
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE 1                                                                
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T320B1(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,NDGLOBAL                                                      
         SPACE 1                                                                
*                                  THIS ALSO CONTAINS BUFFERS                   
         LA    R2,16(R4)                                                        
         L     R1,0(R2)                                                         
         LA    R2,4(R1,R2)                                                      
         SPACE 1                                                                
         LA    R2,8(R2)            NAME POOL                                    
         ST    R2,NDANAMES                                                      
         L     R1,4(R2)            MAX NUMBER OF ENTRIES                        
         M     R0,8(R2)            WIDTH OF EACH ENTRY                          
         LA    R2,12(R1,R2)                                                     
         SPACE 1                                                                
         LA    R2,8(R2)            COST POOL                                    
         L     R1,0(R2)            L'THIS ENTRY                                 
         LA    R2,4(R2)                                                         
         ST    R2,NDACPOOL                                                      
         AR    R2,R1                                                            
         SPACE 1                                                                
         USING GLOBALD,R4                                                       
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,NDDRIVER                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A4D'   LOAD T00A4D (NEWRIDRIVE)             
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,NDADPG                                                   
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,NDMYFRST                                                
         MVC   GLSPACE,NDSPAOPT     PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,NDBOXOPT              BOX OPTION                        
         MVC   GLLFTOPT,NDLFTOPT          AND LEFT OPTION                       
         MVI   GLNORBOX,X'40'       TURN OFF ROW BOXES FOR TOTALS               
         MVC   GLDOWNLD,NDDOWNL     PASS DOWN-LOAD INDICATORS                   
         MVC   GLDWNLD2,NDDWNL2     PASS DOWN-LOAD INDICATORS2                  
         MVC   GLINDS,NDDRINDS      PASS THROUGH OTHER INDICATORS               
         MVC   GLINDS2,NDDRIND2                                                 
         MVC   GLINDS3,NDDRIND3                                                 
         SPACE 1                                                                
DRI2     CLI   NDTRAOPT,C'Y'        OPTION TO TRACE                             
         BNE   DRI4                                                             
         MVI   GLTRACE,C'Y'                                                     
         SPACE 1                                                                
DRI4     MVI   GLAUTOCH,C'N'       TURN OFF AUTOCHUNK                           
         CLI   NDFLAVOR,C'P'        UNLESS REAL POST                            
         BNE   DRI6                                                             
         CLI   NDFLAVOR+1,C'2'                                                  
         BE    DRI6                                                             
         MVI   GLAUTOCH,C'Y'                                                    
         SPACE 1                                                                
DRI6     MVI   GLWPAPER+3,132      SET PAPER WIDTH                              
         CLI   NDWIDOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   GLWPAPER+3,165                                                   
         EJECT                                                                  
*              INITIALIZATION OF PRINT RELATED FIELDS                           
         SPACE 3                                                                
VINTHEAD L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   NDWIDOPT,C'Y'                                                    
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,NDAH1                                                         
         LA    R1,H4                                                            
         ST    R1,NDAH4                                                         
         LA    R1,P                                                             
         ST    R1,NDAP1                                                         
         MVC   NDPWIDTH,=F'132'                                                 
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NDNAROPT,C'Y'                                                    
         BNE   XIT                                                              
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         SPACE 1                                                                
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,NDAH1                                                         
         LA    R1,XHEAD4                                                        
         ST    R1,NDAH4                                                         
         LA    R1,XP                                                            
         ST    R1,NDAP1                                                         
         MVC   NDPWIDTH,=F'198'                                                 
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              INSERT DELETE UNPROTECTED FIELDS                                 
         SPACE 3                                                                
*              INPUT               R2=A(FIRST UNPROTECTED FIELD)                
*                                  R0=NUMBER OF INPUT FIELDS                    
         SPACE 1                                                                
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         CLI   PFAID,3             WAS PF3 OR PF4 HIT                           
         BE    DI2                                                              
         CLI   PFAID,15                (15/16 EQUIVALENT)                       
         BE    DI2                                                              
         CLI   PFAID,4                                                          
         BE    DI2                                                              
         CLI   PFAID,16                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
DI2      L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R4,ATWA             INTO TWA                                     
         SPACE 1                                                                
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE 1                                                                
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,15                                                         
         BE    DEL2                                                             
         XC    BLOCK(80),BLOCK                                                  
         SPACE 1                                                                
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
         SPACE 1                                                                
INSFOUND MVC   HALF,=H'509'        NEW FIELD INSERTED ON SCREEN                 
         LA    R4,WORK                                                          
         USING GBLOCK,R4                                                        
         MVI   GMSGTYPE,C'I'       INFORMATION MESSAGE                          
         DROP  R4                                                               
         USING TIOBD,R4                                                         
         L     R4,ATIOB                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R2,ATWA             INTO TWA                                     
         B     ERR2                                                             
         SPACE 1                                                                
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE 1                                                                
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         MVC   HALF,=H'510'        FIELD DELETED ON SCREEN                      
         LA    R4,WORK                                                          
         USING GBLOCK,R4                                                        
         XC    GBLOCK,GBLOCK                                                    
         MVI   GMSGTYPE,C'I'       INFORMATION MESSAGE                          
         B     ERR2                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE 1                                                                
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         ZIC   R0,5(R2)            R0 HAS FIELD LENGTH                          
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
         SPACE 1                                                                
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
         SPACE 1                                                                
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
         SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
         SPACE 1                                                                
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VERRXIT                                                          
         EJECT                                                                  
* - 2 BYTE ERROR MESSAGES                                                       
*                                                                               
VERR     LA    R3,WORK                                                          
         USING GBLOCK,R3                                                        
         OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTASUBST,GASUBST    A(TABLE OF SUBST TEXT(S))                    
         MVI   GTMSYS,X'03'        ALL MESSAGES FROM NETWORK (X'03')            
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF,R3                                                            
         GOTO1 ERREX                                                            
         SPACE                                                                  
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
FINV     MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
         SPACE 1                                                                
MYCURSOR DS    0H                                                               
         MVI   ERROR,X'FE'                                                      
         GOTO1 NDCURSOR                                                         
         SPACE 1                                                                
* - 2 BYTE ERROR MESSAGES                                                       
ERR2     LA    R3,WORK                                                          
         USING GBLOCK,R3                                                        
         CLI   GMSGTYPE,C'I'       ..INFORMATION MESSAGE                        
         BE    *+10                                                             
         XC    GBLOCK,GBLOCK       ..NO/CLEAR BLOCK(DEFAULT=ERRORMSG)           
         MVC   GERROR,HALF         SET ERROR MSG CODE                           
         CLI   BYTE,X'FF'          .SOFT ERROR MESSAGE                          
         BNE   ERR2X                                                            
         LA    R1,ELEM             .YES/PASS ADDRESS OF SUPPLEMENT MSG          
         STCM  R1,7,GASUBST                                                     
         MVI   BYTE,0                                                           
         PRINT GEN                                                              
ERR2X    GOTO1 NDERR                                                            
         PRINT NOGEN                                                            
         DROP  R3                                                               
         SPACE 1                                                                
VEXIT    DS    0H                                                               
VERRXIT  OI    6(R2),X'40'         POSITION CURSOR                              
         CLI   ERROR,X'FE'                                                      
         BE    ERR2                                                             
         GOTO1 ERREX               SYSTEM MESSAGE                               
         SPACE 1                                                                
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         PRINT GEN                                                              
         GETEL (R4),DATADISP,ELCODE                                             
         PRINT NOGEN                                                            
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
*                                  ERROR MESSAGES                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
RELOC    DC    A(*)                                                             
         SPACE 1                                                                
REGSPECS DS    0C                  SPECS FOR REGULAR PRINTING                   
         SSPEC H1,2,C'NETWORK REPORT WRITER'                                    
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H2,17,PAGE                                                       
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,96,AGYADD                                                     
         SSPEC H4,52,PERIOD                                                     
         SSPEC H4,96,NETREP                                                     
         DC    X'00'                                                            
         SPACE 1                                                                
NARSPECS DS    0C                  SPECS FOR NARROW PRINTING                    
         SSPEC H3,2,PERIOD                                                      
         SSPEC H1,54,NETREP                                                     
         SSPEC H2,54,PAGE                                                       
         DC    X'00'                                                            
         SPACE 1                                                                
WIDSPECS DS    0C                  SPECS FOR WIDE PRINTING                      
         WSPEC H1,2,C'NETWORK REPORT WRITER'                                    
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H2,17,PAGE                                                       
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,68,PERIOD                                                     
         WSPEC H4,129,NETREP                                                    
         DC    X'00'                                                            
         SPACE 1                                                                
EDITLIST DC    XL64'00'            ONLY USED OFFLINE                            
         EJECT                                                                  
* - CHECK SECURITY LOCKOUT                                                      
CHKSEC   NTR1                                                                   
*                                                                               
         CLI   NDSECFLG,C'Y'      SECURITY ON?                                  
         BNE   CHKSEX                                                           
         L     R1,ATWA                                                          
         USING T320FFD,R1                                                       
         CLI   TWAOFFC,C'*'        DDS                                          
         BE    CHKSEX                                                           
         TM    NDAUTH,X'40'       ,,AUTHORIZED                                  
         BNZ   CHKSEX                                                           
         TM    4(R2),X'20'   ,,ONLY PREVIOULY VALIDATED FIELDS ALLOWED          
         BO    CHKSEX                                                           
         MVC   HALF,=H'673'  ,,SECURITY LOCKOUT-CANNOT CHANGE FIELD             
         B     ERR2                                                             
CHKSEX   B     XIT                                                              
         DROP  R1                                                               
*                                                                               
RAW      EQU   X'01'          USED FOR EXTENDED DEMO LIST                       
EQU      EQU   X'02'                                                            
NN       EQU   X'04'                                                            
NQ       EQU   X'08'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
         DS    0F                                                               
         CSECT                                                                  
OVERFLOW NMOD1 0,**NEWRIO,RA,R7,RR=RE                                           
         L     RC,0(R1)            REESTABLISH WORKING STORAGE                  
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         ST    RE,RELOO                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVFLOW(RF)                                                       
*                                                                               
OVFLOW   B     VOPTIONS            0 - VALIDATE OPTIONS                         
         B     VOTHERS             1 - VALIDATE OTHERS                          
         B     VVALACC             2 - VALIDATE ACC STACK                       
         B     VALUDEF             3 - VALIDATE USER DEFINITION RECS            
         B     VALUCOM             4 - VALIDATE USER DEFINITION RECS            
*                                                                               
VOPTIONS DS    0H                                                               
         MVI   NDSPAOPT,1          PRESET OPTIONS                               
         MVI   NDACCPRD,0                                                       
         MVI   NDSEPOPT,C'N'                                                    
         MVI   NDBOXOPT,C'Y'                                                    
         MVI   NDLFTOPT,0                                                       
*        MVI   NDLFTOPT,C'N'                                                    
         MVI   NDAVEOPT,C'N'                                                    
         MVI   NDPEROPT,0                                                       
         MVI   NDTHOOPT,0                                                       
         TM    NDCOLIND,X'20'       CHECK FOR PUP WRITER                        
         BZ    *+8                                                              
         MVI   NDTHOOPT,C'Y'                                                    
         MVI   NDREROPT,0                                                       
         MVI   NDAFFOPT,0                                                       
         MVI   NDDOWNL,0                                                        
         MVI   NDDWNL2,0                                                        
         MVI   NDDRINDS,0                                                       
         MVI   NDPREOPT,0                                                       
         XC    NDCMDEF2,NDCMDEF2                                                
         XC    NDPROGFT,NDPROGFT                                                
         CLI   NDFLAVOR,C'E'       UNLESS ESTIMATE FLAVOR,                      
         BE    VVALOPT3                                                         
         CLI   NDFLAVOR,C'B'       UNLESS BH FLAVOR,                            
         BE    VVALOPT3                                                         
         MVI   NDDRINDS,X'02'      PRESET TO GET ALL DETAILS                    
VVALOPT3 MVI   NDDRIND2,0                                                       
         MVI   NDWIDOPT,C'N'                                                    
         MVI   NDNAROPT,C'N'                                                    
         MVI   NDUSEOPT,C'A'                                                    
         MVI   NDTRAOPT,C'N'                                                    
         MVI   NDGRAND,C'Y'        DEFAULT TO GET GRAND TOTALS                  
         MVI   NDCPPOPT,0                                                       
         MVI   NDRCPOPT,0                                                       
         MVI   NDVPHOPT,C'C'       DEFAULT TO COMPUTE                           
         MVI   NDA$OPT,0                                                        
         MVI   NDE$OPT,0                                                        
         MVI   NDG$OPT,0                                                        
         MVI   NDSPLOPT,0                                                       
         XC    NDPRIOR,NDPRIOR                                                  
         XC    NDAFTER,NDAFTER                                                  
         XC    NDSPLPRD,NDSPLPRD                                                
         XC    NDGOALDT,NDGOALDT                                                
         MVI   NDCOLIND,0          RESET COLUMN INDICATORS                      
         XC    NDSTADEF,NDSTADEF                                                
         MVI   NDANYDEM,0                                                       
         MVC   NDQBASE,NBN0B2      USE AS DEFAULT EQUIV BASE                    
         CLI   NDQBASE,0                                                        
         BNE   *+8                                                              
         MVI   NDQBASE,30          IF NONE, USE 30                              
         MVI   BYTE,0                                                           
         CLI   NBN0B2,0            SET EQUIVALENCY MASK                         
         BE    *+8                                                              
         OI    BYTE,X'C0'          (TURN BOTH ON)                               
*        CLI   NBN2B1,0            (THIS ONLY CONFUSED THE ISSUE                
*        BE    *+8                 SO I TOOK IT OUT 11/1/88)                    
*        OI    BYTE,X'40'          TURN ON GRP EQUIV                            
         MVI   NDDETDEF,1          SET UP DEFAULT DETAIL DEF DATA               
         OC    NDDETDEF(1),BYTE    WHICH MAY BE EQUIVALENCED                    
         MVC   NDSUBDEF,NDDETDEF   COPY THIS TO SUBS                            
         MVC   NDTOTDEF,NDDETDEF             AND TOTALS                         
         MVI   NDTOTDEF+1,2        TOTALS HAVE CPP/CPM AS WELL                  
         OC    NDTOTDEF+1(1),BYTE  WHICH MAY BE EQUIVALENCED AS WELL            
         XC    NDRNKMAX,NDRNKMAX                                                
         MVI   NSMRGSW,0           INIT MERGE SWITCH                            
         MVI   NSOPTSO1,0          INIT FORMAT OPTIONS                          
         MVI   NSOPTSO2,0                                                       
*                                                                               
         EJECT                                                                  
*              OPTIONS/OTHERS SHARE SAME ROUTINE                                
         SPACE 3                                                                
VOTHERS  DS     0H                                                              
         CLI   5(R2),0                                                          
         BE    XIT2                                                             
         GOTO1 SCANNER,DMCB,(40,(R2)),(10,BLOCK),0                              
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
* CEST=CLI/PROD/EST                                                             
OPT00    CLC   =C'CEST',12(R3)         CONTROL ESTIMATE?                        
         BNE   OPT2                                                             
         LR    R0,R2                   SAVE R2                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
*                                                                               
         LA    R2,22(R3)               DEAL WITH CLIENT                         
         CLI   0(R2),C'/'                                                       
         BE    OPT00X                                                           
         MVC   WORK(3),0(R2)                                                    
         CLI   WORK+2,C'/'                                                      
         BNE   *+8                                                              
         MVI   WORK+2,X'40'                                                     
         GOTO1 NBCLPACK,DMCB,WORK,KEY+2                                         
         CLI   DMCB,0                                                           
         BNE   OPT00X                                                           
         LA    R2,2(R2)                CLIENT IS AT LEAST 2                     
         CLI   0(R2),C'/'                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                IT WAS 3                                 
         CLI   0(R2),C'/'                                                       
         BNE   OPT00X                                                           
         LA    R2,1(R2)                BUMP OVER C'/'                           
*                                                                               
         MVC   KEY+4(3),0(R2)          SHOULD BE PRODUCT                        
         CLI   KEY+6,C'/'                                                       
         BNE   *+8                                                              
         MVI   KEY+6,X'40'             IN CASE 2 CHAR PROD                      
         LA    R2,2(R2)                                                         
         CLI   0(R2),C'/'                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'/'                                                       
         BNE   OPT00X                                                           
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R1,1                    LENGTH OF NUMERIC INPUT                  
         MVC   WORK(3),0(R2)           3 IS MAX                                 
         TM    WORK,X'F0'           NUMERIC?                                    
         BNO   OPT00X                                                           
         CLI   WORK+1,X'40'                                                     
         BNH   OPT1A                                                            
         TM    WORK+1,X'F0'                                                     
         BNO   OPT00X                                                           
         LA    R1,1(R1)                IT'S 2                                   
         CLI   WORK+2,X'40'                                                     
         BNH   OPT1A                                                            
         TM    WORK+2,X'40'                                                     
         BNO   OPT00X                                                           
         LA    R1,3                    IT'S 3                                   
OPT1A    XC    FULL,FULL               MOVE NUMBER TO FULL                      
         LA    RE,FULL+1               MAX IS 3 NUMBERS                         
         LA    RF,3                                                             
         SR    RF,R1                   R1 HAS LENGTH OF INPUT                   
         AR    RE,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         PACK  DUB,FULL                                                         
         CVB   R1,DUB                                                           
         STC  R1,KEY+7                                                          
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OPT00X                                                           
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING EKEY,R1                                                          
         L     RE,NBADEM                                                        
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NDDEMBLK,RE                                                      
         OI    NDOVER,X'80'            SET FLAG                                 
         MVC   NDDEMOS,EDEMLIST                                                 
         MVC   NDDEMOS+60(3),EDEM21                                             
         LA    RF,EDEMLIST                                                      
         SR    R1,R1                                                            
OPT1C    OC    0(3,RF),0(RF)           DEMO?                                    
         BZ    OPT1F                                                            
         LA    R1,1(R1)                                                         
         CHI   R1,20                   END OF LIST                              
         BE    OPT1D                                                            
         LA    RF,3(RF)                BUMP DEMO LIST                           
         B     OPT1C                                                            
OPT1D    OC    EDEM21,EDEM21           21ST DEMO?                               
         BZ    OPT1F                                                            
         LA    R1,1(R1)                YES                                      
OPT1F    STC   R1,NDNDEMOS             SET NDNDEMOS                             
         DROP  RE                                                               
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         LR    R2,R0                   RESET R2                                 
         B     OPTNEXT                                                          
*                                                                               
OPT00X   LR    R2,R0                   RESET R2                                 
         B     OPTERR                                                           
         SPACE 1                                                                
OPT2     CLC   12(3,R3),=C'SEP'                                                 
         BNE   OPT4                                                             
         MVI   NDSEPOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT4     CLC   12(3,R3),=C'BOX'   BOX PRINTING OPTION                           
         BNE   OPT6                                                             
         MVC   NDBOXOPT,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT6     CLC   12(5,R3),=C'PCODE'  PROGRAM FILTER                               
         BNE   OPT8                                                             
         MVC   NBSELPRG,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT8     CLC   12(4,R3),=C'LEFT'   LEFT ALIGN OPTION                            
         BNE   OPT9                                                             
         MVI   NDLFTOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT9     CLC   12(3,R3),=C'AVE'    AVERAGE OPTION                               
         BNE   OPT10                                                            
         MVI   NDAVEOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT10    CLC   12(3,R3),=C'PFB'    PFB OPTION                                   
         BNE   OPT12                                                            
         MVI   NDPFBOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT12    CLC   12(3,R3),=C'PER'    PERIOD OPTION                                
         BNE   OPT16                                                            
         MVC   NDPEROPT,22(R3)                                                  
         CLI   NDPEROPT,C'C'       CAN BE CALENDAR                              
         BE    OPTNEXT                                                          
         CLI   NDPEROPT,C'B'       BROADCAST                                    
         BE    OPTNEXT                                                          
         CLI   NDPEROPT,C'S'       OR SPECIAL                                   
         BE    OPTNEXT                                                          
         CLC   =C'NTI',22(R3)      OR 'NTI'                                     
         BE    OPTNEXT                                                          
         B     OPTERR              OTHERWISE ITS NO GOOD                        
         SPACE 1                                                                
OPT16    CLC   12(4,R3),=C'THOU'   THOUSAND OPTION                              
         BNE   OPT18                                                            
         MVI   NDTHOOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT18    CLC   12(6,R3),=C'RERATE' OPTION TO RERATE WITH OTHER BOOK             
         BNE   OPT19                                                            
         MVC   NDREROPT,22(R3)                                                  
         CLI   NDREROPT,C'D'       MUST BE D C I OR A                           
         BE    OPTNEXT                                                          
         CLI   NDREROPT,C'I'                                                    
         BE    OPTNEXT                                                          
         CLI   NDREROPT,C'C'                                                    
         BE    OPTNEXT                                                          
         CLI   NDREROPT,C'A'                                                    
         BE    OPTNEXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
OPT19    CLC   12(5,R3),=C'AFFID'  OPTION TO LOOK UP AFFIDS                     
         BNE   OPT20                                                            
         MVC   NDAFFOPT,22(R3)                                                  
         CLI   NDAFFOPT,C'Y'       MUST BE Y OR N                               
         BE    OPTNEXT                                                          
         CLI   NDAFFOPT,C'N'                                                    
         BE    OPTNEXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
OPT20    CLC   12(5,R3),=C'DOWN '  DOWNLOADING OPTION                           
         BNE   OPT22                                                            
         TM    GENSTAT2,NOREQDET   IS NOREQDET ON                               
         BO    OPT21               YES/ERROR                                    
         OI    NDDOWNL,GLDLACTV                                                 
         B     OPTNEXT                                                          
OPT21    MVC   HALF,=H'665'        CAN'T HAE DONW AND NOREQDET                  
         MVI   BYTE,0                                                           
         MVI   WORK+3,0                                                         
         B     ERR22                                                            
         SPACE 1                                                                
OPT22    CLC   12(4,R3),=C'WIDE'   WIDE PRINTING OPTION                         
         BNE   OPT23                                                            
         MVI   NDWIDOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT23    CLC   12(6,R3),=C'NARROW' NARROW PRINTING OPTION                       
         BNE   OPT24                                                            
         MVI   NDNAROPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT24    CLC   12(3,R3),=C'USE'    USE= OPTION                                  
         BNE   OPT26                                                            
         MVC   NDUSEOPT,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT26    CLC   12(2,R3),=C'PB'    PIGGY OPTION                                  
         BNE   OPT28                                                            
         CLI   22(R3),C'S'         ALWAYS SPLIT OPTION                          
         BNE   OPT26B                                                           
         OI    NBSBKEND,X'20'                                                   
         B     OPTNEXT                                                          
OPT26B   MVC   NBPIGOPT,22(R3)     PIGGYS ONLY (Y/N)                            
         CLI   NBPIGOPT,C'N'                                                    
         BE    OPTNEXT                                                          
         CLI   NBPIGOPT,C'Y'                                                    
         BE    OPTNEXT                                                          
         CLI   NBPIGOPT,C'S'                                                    
         BE    OPTNEXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
OPT28    CLC   12(2,R3),=C'BB'     BILLBOARD OPTION                             
         BNE   OPT30                                                            
         MVC   NBBLBOPT,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT30    CLC   12(5,R3),=C'ROUND'  ROUNDING OPTION - TOTALS ONLY                
         BNE   OPT31                                                            
         OI    NDDRINDS,X'01'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT31    CLC   12(5,R3),=C'ALLROUND'  ROUNDING TOTALS AND DETAILS               
         BNE   OPT32                                                            
         OI    NDDRIND3,X'01'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT32    CLC   12(5,R3),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT34                                                            
         MVI   NDTRAOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT34    CLC   12(3,R3),=C'TOTAL'  TOTAL DEFINITION                             
         BNE   OPT36                                                            
         LA    R1,NDTOTDEF                                                      
         BAS   RE,VALDEFS                                                       
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT36    CLC   12(3,R3),=C'SUB'    SUB-TOTAL DEFINITION                         
         BNE   OPT38                                                            
         LA    R1,NDSUBDEF                                                      
         BAS   RE,VALDEFS                                                       
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT38    CLC   12(3,R3),=C'DET'    DETAIL DEFINITION                            
         BNE   OPT39                                                            
         LA    R1,NDDETDEF                                                      
         BAS   RE,VALDEFS                                                       
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT39    CLC   12(4,R3),=C'ALL '   ALL (DET, SUB AND TOT)                       
         BNE   OPT40                                                            
         LA    R1,NDDETDEF                                                      
         BAS   RE,VALDEFS                                                       
         MVC   NDSUBDEF,NDDETDEF                                                
         MVC   NDTOTDEF,NDDETDEF                                                
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT40    CLC   12(4,R3),=C'BASE'   EQUIVALENCY BASE                             
         BNE   OPT42                                                            
         MVI   NDQBASE,30                                                       
         CLC   22(2,R3),=C'30'     30                                           
         BE    OPTNEXT                                                          
         MVI   NDQBASE,60                                                       
         CLC   22(2,R3),=C'60'     AND 60 SUPPORTED                             
         BE    OPTNEXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
OPT42    CLC   12(5,R3),=C'STACK'  STACK DEFINITION                             
         BNE   OPT43                                                            
         BAS   RE,VALSTACK                                                      
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT43    CLC   12(3,R3),=C'AST'    ACCOUNTING STACK DEFINITION                  
         BNE   OPT44                                                            
         BAS   RE,VALAST                                                        
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT44    DS    0H                                                               
         LA    R5,NBBILSTR         DATE FILTERING                               
         CLC   12(4,R3),=C'BILL'   BILLED                                       
         BE    OPT46                                                            
         CLC   12(5,R3),=C'IBILL'  BILLED INTEGRATION                           
         BNE   OPT44B                                                           
         OI    NBACCFLT,X'01'                                                   
         B     OPT46                                                            
OPT44B   LA    R5,NBPAYSTR         PAID FILTERING                               
         CLC   12(4,R3),=C'PAID'                                                
         BE    OPT46                                                            
         LA    R5,NDGOALDT         GOAL FILTERING                               
         CLC   12(5,R3),=C'GOAL '                                               
         BE    OPT46                                                            
         LA    R5,NBACTSTR         CHANGE                                       
         CLC   12(3,R3),=C'CHA'                                                 
         BE    OPT46                                                            
         CLC   12(3,R3),=C'ACT'    OR ACTIVITY FILTERING                        
         BE    OPT46                                                            
         B     OPT50                                                            
         SPACE 1                                                                
OPT46    GOTO1 DATVAL,DMCB,(0,22(R3)),WORK                                      
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    OPTDERR                                                          
         MVC   WORK+6(6),WORK      ASSUME END = START                           
         LA    R1,22(R1,R3)                                                     
         CLI   0(R1),C' '                                                       
         BE    OPT48                                                            
         CLI   0(R1),C'-'          CHECK DELIMITER                              
         BNE   OPTDERR                                                          
         LA    R1,1(R1)                                                         
         ST    R1,DMCB                                                          
         GOTO1 DATVAL,DMCB,,WORK+6                                              
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    OPTDERR                                                          
         CLC   WORK+6(6),WORK      CHECK END V START                            
         BL    OPTEBFRS                                                         
         CLC   12(4,R3),=C'GOAL'                                                
         BNE   OPT48                                                            
         SPACE 1                                                                
         GOTO1 GETDAY,DMCB,WORK,WORK+20                                         
         CLI   0(R1),1             CHECK START DAY IS MONDAY                    
         BNE   OPTDERR                                                          
         GOTO1 GETDAY,DMCB,WORK+6,WORK+20                                       
         CLI   0(R1),7             CHECK END DAY IS SUNDAY                      
         BNE   OPTDERR                                                          
         SPACE 1                                                                
OPT48    GOTO1 DATCON,DMCB,(0,WORK),(2,0(R5))                                   
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,2(R5))                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT50    CLC   12(6,R3),=C'ALLTOT' OPTION TO GET ALL THE TOTALS                 
         BNE   OPT54                                                            
         OI    NDDRINDS,X'04'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT54    CLC   12(6,R3),=C'ALLDET' OPTION TO GET ALL THE DETAILS                
         BNE   OPT56                                                            
         OI    NDDRINDS,X'02'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT56    CLC   12(4,R3),=C'ZERO'   SAME AS ABOVE                                
         BNE   OPT58                                                            
         OI    NDDRINDS,X'02'      DEFAULT IS TO GET ZEROS                      
         CLI   22(R3),C'N'         ZERO=NO TURNS THIS OFF                       
         BNE   *+8                                                              
         NI    NDDRINDS,X'FD'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT58    CLC   12(2,R3),=C'CP'     CPP/CPM BASIS                                
         BNE   OPT60                                                            
         CLC   22(3,R3),=C'ASS'    CPP=ASS                                      
         BNE   *+12                                                             
         OI    NDCPPOPT,1                                                       
         B     OPT59                                                            
         CLC   22(5,R3),=C'ASACT'  CPP=ASACT (ASSIGNED/ACTUAL)                  
         BNE   *+12                                                             
         OI    NDCPPOPT,9                                                       
         B     OPT59                                                            
         CLC   22(3,R3),=C'ACT'    CPP=ACT                                      
         BNE   *+12                                                             
         OI    NDCPPOPT,2                                                       
         B     OPT59                                                            
         OI    NDCPPOPT,4                                                       
         CLC   22(3,R3),=C'MAX'    CPP=MAX                                      
         BE    OPT59                                                            
         B     OPTERR                                                           
         SPACE 1                                                                
OPT59    CLI   25(R3),C'+'         MAY BE + INTEGRATION                         
         BNE   OPTNEXT                                                          
         OI    NDCPPOPT,X'80'                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT60    CLC   12(5,R3),=C'SPLIT'  USE SPLIT BILLING                            
         BNE   OPT61                                                            
         MVI   NDSPLOPT,1                                                       
         CLI   22(R3),C'$'                                                      
         BNE   OPTNEXT                                                          
         MVI   NDSPLOPT,100                                                     
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT61    CLC   12(6,R3),=C'SPLPRD' USE THIS PRODUCT FOR SPLITS                  
         BNE   OPT62                                                            
         MVC   NDSPLPRD,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT62    CLC   12(5,R3),=C'SOLID'  OPTION TO GET ALL THE DETAILS                
         BNE   OPT64                                                            
         OI    NDDRIND2,GLPWHOLE                                                
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT64    CLC   12(5,R3),=C'GRAND'  OPTION TO SUPPRESS GRAND TOTALS              
         BNE   OPT66                                                            
         MVC   NDGRAND,22(R3)                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT66    CLC   12(6,R3),=C'NOHEAD' OPTION TO SUPPRESS DL HEADS                  
         BNE   OPT67                                                            
         OI    NDDOWNL,GLDLNOHD                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT67    CLC   12(6,R3),=C'DOWNHEAD' OPTION TO DOWNLOAD ALL HEADS               
         BE    OPT67B                                                           
         CLC   12(6,R3),=C'DLHEAD'   OPTION TO DOWNLOAD ALL HEADS               
         BNE   OPT68                                                            
OPT67B   OI    NDDOWNL,GLDLHEAD                                                 
         NI    NDDOWNL,255-GLDLNOHD                                             
         TM    GENSTAT2,NOREQDET   IS NOREQDET ON                               
         BO    OPT21               YES/ERROR                                    
         OI    NDDOWNL,GLDLACTV                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT68    CLC   12(4,R3),=C'XBOX'   EXTRA BOX OPTION                             
         BNE   OPT70                                                            
         OI    NDDRIND2,GLEXTBOX                                                
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT70    CLC   12(4,R3),=C'MIDH'   MIDHEAD OPTION                               
         BNE   OPT72                                                            
         OI    NDDRIND2,GLMIDHED                                                
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT72    CLC   12(5,R3),=C'RECAP'  (GRAND) RECAP OPTION                         
         BNE   OPT74                                                            
         MVI   NDRCPOPT,C'Y'                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT74    CLC   12(2,R3),=C'A$'     ACTUAL DOLLAR OPTION                         
         BNE   OPT76                                                            
         LA    R1,22(R3)                                                        
         BAS   RE,LUPACC                                                        
         CLI   NDSFTARG,0                                                       
         BE    OPTERR                                                           
         MVC   NDA$OPT,NDSFTARG                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT76    CLC   12(2,R3),=C'E$'     EST DOLLAR OPTION                            
         BNE   OPT78                                                            
         LA    R1,22(R3)                                                        
         BAS   RE,LUPACC                                                        
         CLI   NDSFTARG,0                                                       
         BE    OPTERR                                                           
         MVC   NDE$OPT,NDSFTARG                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT78    CLC   12(2,R3),=C'G$'     GOAL DOLLAR OPTION                           
         BNE   OPT80                                                            
         LA    R1,22(R3)                                                        
         BAS   RE,LUPACC                                                        
         CLI   NDSFTARG,0                                                       
         BE    OPTERR                                                           
         MVC   NDG$OPT,NDSFTARG                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT80    CLC   12(3,R3),=C'VPH'    VPH OPTION                                   
         BNE   OPT84                                                            
         MVC   NDVPHOPT,22(R3)                                                  
         CLI   NDVPHOPT,C'C'           =C(OMPUTE)                               
         BE    OPTNEXT                                                          
         CLI   NDVPHOPT,C'P'           =P(ROGRAM)                               
         BE    OPTNEXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
OPT84    CLC   12(3,R3),=C'PRE'    DEMO PRECISSION OPTION                       
         BNE   OPT86                                                            
         MVI   NDPREOPT,C'Y'                                                    
         CLC   22(3,R3),=CL3'CAB'                                               
         BNE   OPTERR                                                           
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT86    CLC   12(4,R3),=C'BCOM'   USE THIS COMMENT CODE                        
         BNE   OPT88                                                            
         MVC   NDCMDEF2,22(R3)                                                  
         CLI   NDCMDEF2+2,C'*'                                                  
         BE    OPTERR                                                           
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT88    CLC   12(3,R3),=C'TOP'   RANK LIMITED TO NNN                           
         BNE   OPT92                                                            
         TM    3(R3),X'80'         TEST NUMERIC                                 
         BNO   OPTERR                                                           
         MVC   NDRNKMAX,8(R3)                                                   
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT92    CLC   12(5,R3),=C'STRIP'  OPTION TO STRIP                              
         BNE   OPT94                                                            
         OI    NDDOWNL,GLDLSTRP                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT94    CLC   12(7,R3),=C'NOTRUNC'    NO TRUNCATION                            
         BNE   OPT96                                                            
         OI    NDDOWNL,GLDLNOTR                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT96    CLC   12(7,R3),=C'ALLALPH'    ALL ALPHA                                
         BNE   OPT98                                                            
         OI    NDDOWNL,GLDLALPH                                                 
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT98    CLC   12(8,R3),=C'DOWNTAPE'    TAPE                                    
         BNE   OPT100                                                           
         OI    NDDOWNL,GLDLACTV+GLDLNOHD+GLDLNOTR+GLDLALPH+GLDLSTRP             
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT100   DS    0H                                                               
         CLC   12(3,R3),=C'ACC'         ACCOUNTING PROD FILTER                  
         BNE   OPT110                                                           
         L     RE,ANETWS1                                                       
         USING CLTHDR,RE                                                        
         LA    RE,CLIST                                                         
         LA    RF,220                                                           
         CLC   0(3,RE),22(R3)                                                   
         BE    OPT105                                                           
         LA    RE,4(RE)                                                         
         BCT   RF,*-14                                                          
         B     OPTERR                                                           
OPT105   MVC   NDACCPRD,3(RE)          SET PRD CODE FILT FOR NIACCGEN           
         B     OPTNEXT                                                          
         DROP  RE                                                               
         SPACE 1                                                                
OPT110   DS    0H                                                               
         CLC   12(3,R3),=C'CML'    COMMERCIAL CLASS FILTER                      
         BNE   OPT120                                                           
         MVC   NDCMLCLS,22(R3)                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT120   DS    0H                                                               
         CLC   12(4,R3),=C'UPLD'   UPLOAD FILTER                                
         BNE   OPT130                                                           
         CLI   22(R3),C'O'         UPLOAD ONLY                                  
         BNE   OPT122                                                           
         OI    NBUPLD,X'01'                                                     
         B     OPTNEXT                                                          
OPT122   CLI   22(R3),C'N'         EXCLUDE UPLOADED DATA                        
         BNE   OPT125                                                           
         OI    NBUPLD,X'02'                                                     
         B     OPTNEXT                                                          
OPT125   CLI   22(R3),C'X'         ONLY PROGRAMS WITH NUMERIC IN 3/4            
         BNE   OPT130                                                           
         OI    NBUPLD,X'04'                                                     
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT130   DS   0H                                                                
* BHRDATE FILTER FOR BILLING EXCLUDES ALL UNITS THAT DO NOT                     
* MATCH FILTER                                                                  
* BHRDATEI FILTER FOR BILLING INCLUDES ALL UNIT INFO BUT EXCLUDES               
* ALL BILLING INFO THAT DOES NOT MEET FILTER                                    
         CLC   12(8,R3),=C'BHRDATEI'                                            
         BNE   *+12                                                             
         OI    NBVARIND,X'01'                                                   
         B     OPT133                                                           
*                                                                               
         CLC  12(7,R3),=C'BHRDATE'   DATE FILTER FOR BILL HEADER READ           
         BE    OPT133                                                           
         CLC  12(7,R3),=C'BHPDATE'    POST DATE                                 
         BE    OPT133                                                           
         CLC  12(7,R3),=C'BHIDATE'    INVOICE DATE                              
         BE    OPT133                                                           
         CLC  12(7,R3),=C'BHEDATE'    EDI DATE FILTER                           
         BE    OPT133                                                           
         CLC  12(7,R3),=C'BHDDATE'    DUE DATE                                  
         BNE   OPT140                                                           
OPT133   GOTO1 DATVAL,DMCB,(0,22(R3)),WORK                                      
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    OPTDERR                                                          
         MVC   WORK+6(6),WORK    ASSUME END = START                             
         LA    R1,22(R1,R3)                                                     
         CLI   0(R1),C' '                                                       
         BE    OPTNEXT                                                          
         CLI   0(R1),C'-'          CHECK DELIMITER                              
         BNE   OPTDERR                                                          
         LA    R1,1(R1)                                                         
         ST    R1,DMCB                                                          
         GOTO1 DATVAL,DMCB,,WORK+6                                              
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BZ    OPTDERR                                                          
         CLC   WORK+6(6),WORK    CHECK END V START                              
         BL    OPTEBFRS                                                         
         CLC   12(7,R3),=C'BHRDATE'         IF RUN DATE FILTER                  
         BNE   OPTNEXT                                                          
         GOTO1 DATCON,DMCB,WORK,(2,NBBILSTR)                                    
         GOTO1 DATCON,DMCB,WORK+6,(2,NBBILEND)                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT140   DS    0H                                                               
         CLC   12(6,R3),=C'BHTYPE'        BILL HEAD READ                        
         BNE   OPT150                                                           
         CLC   22(3,R3),=C'AOR'                                                 
         BE    OPTNEXT                                                          
         CLC   22(4,R3),=C'-AOR'                                                
         BE    OPTNEXT                                                          
         CLI   22(R3),C'M'                                                      
         BE    OPTNEXT                                                          
         CLC   22(2,R3),=C'-M'                                                  
         BE    OPTNEXT                                                          
         CLI   22(R3),C'B'                                                      
         BE    OPTNEXT                                                          
         BNE   OPTERR                                                           
         SPACE 1                                                                
OPT150   DS    0H                                                               
         CLC   12(8,R3),=C'NOREQDET' NO REQUEST PAGE ON DOWNLOAD                
         BNE   OPT152                                                           
         TM    NDDOWNL,X'80'       IS DOWN SET                                  
         BO    OPT151              YES/ERROR                                    
         OI    GENSTAT2,NOREQDET                                                
         B     OPTNEXT                                                          
OPT151   MVC   HALF,=H'665'        CAN'T HAVE DOWN AND NOREQDET                 
         B     ERR22                                                            
*                                                                               
OPT152   CLI   12(R3),C'S'         SPACING                                      
         BNE   OPT160                                                           
         MVC   NDSPAOPT,11(R3)                                                  
         CLI   NDSPAOPT,0                                                       
         BE    OPTERR                                                           
         CLI   NDSPAOPT,3                                                       
         BH    OPTERR                                                           
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT160   CLC   12(5,R3),=C'PRIOR'  OPTION FOR PRIOR INFORMATION                 
         BNE   OPT170                                                           
         MVI   NDPRIOR,X'FF'       SET PRIOR SWITCH                             
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT170   CLC   12(5,R3),=C'AFTER'  OPTION FOR AFTER INFORMATION                 
         BNE   OPT180                                                           
         OI    NDAFTER,X'FF'       SET AFTER SWITCH                             
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT180   CLC   12(6,R3),=C'RNDVPH'  ROUND VPHS                                  
         BNE   OPT190                                                           
         MVI   NDRNDVPH,C'Y'        SET ROUND SWITCH                            
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT190   CLC   12(5,R3),=C'FLOAT'   FLOAT=- OPTION                              
         BNE   OPT200                                                           
         CLI   22(R3),C'-'                                                      
         BNE   OPTERR                                                           
         OI    NDLOCAL,NDMINUS                                                  
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT200   CLC   12(8,R3),=C'CCYYMMDD'      CENTURY OPTION                        
         BE    OPT202                                                           
         CLC   12(4,R3),=C'CYMD'                                                
         BNE   OPT205                                                           
OPT202   OI    NDLOCAL,NDCCYY                                                   
         B     OPTNEXT                                                          
*                                                                               
OPT205   CLC   =C'YYMMDD',12(R3)          NUMERIC DATE OPTION                   
         BE    *+10                                                             
         CLC   =C'YMD',12(R3)                                                   
         BNE   *+12                                                             
         OI    NDLOCAL,NDYYMMDD                                                 
         B     OPTNEXT                                                          
*                                                                               
         CLC   =C'MM/DD/YY',12(R3)          NUMERIC DATE OPTION                 
         BNE   *+12                                                             
         OI    NDLOCAL,NDMMDDYY                                                 
         B     OPTNEXT                                                          
*                                                                               
OPT210   CLC   12(4,R3),=C'POST'           POSTING TYPE FILTER                  
         BNE   OPT220                                                           
         CLI   22(R3),C'-'         NEGATIVE FILTER                              
         BNE   OPT212                                                           
         BAS   RE,OPT214                                                        
         MVC   NDPOSTYP,23(R3)                                                  
         NI    NDPOSTYP,X'40'                                                   
         B     OPTNEXT                                                          
OPT212   LA    RF,22(R3)           POSITIVE FILTER                              
         BAS   RE,OPT214                                                        
         MVC   NDPOSTYP,22(R3)                                                  
         B     OPTNEXT                                                          
*                                                                               
OPT214   LA    R1,POSTLST          CKECK AGAINST VALID POSTING TYPES            
OPT215   CLC   0(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    OPTERR                                                           
         B     OPT215                                                           
POSTLST  DC    C'NSCOD'                                                         
*                                                                               
OPT220   DS    0H                                                               
         CLC   12(5,R3),=C'PRIMP'  INCREASE PRECISION OF IMPRESSIONS            
         BNE   OPT230                                                           
         OI    NBINDS,X'40'                                                     
         B     OPTNEXT                                                          
*                                                                               
OPT230   DS    0H                                                               
         CLC   12(3,R3),=C'PFLT'   PROGRAM FILTER (PUP ONLY)                    
         BNE   OPT240                                                           
         CLI   1(R3),0                                                          
         BE    OPTERR                                                           
         CLI   1(R3),4                                                          
         BH    OPTERR                                                           
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDPROGFT(0),22(R3)                                               
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT240   CLC   12(5,R3),=C'TODAY'  FILTER ON TODAYS ACTIVITY                    
         BNE   OPT250                                                           
         LA    R5,NBACTSTR                                                      
         GOTO1 DATCON,DMCB,(5,WORK),(2,0(R5))                                   
         MVC   2(2,R5),0(R5)                                                    
         B     OPTNEXT                                                          
         SPACE 1                                                                
OPT250   CLC   =C'DOWNTOTT',12(R3)  DOWNLOAD TOTALS AND TEXT                    
         BNE   OPT260                                                           
         OI    NDDOWNL,GLDLTOTS                                                 
         OI    NDDWNL2,GLDLTTXT                                                 
         B     OPTNEXT                                                          
OPT260   CLC   =C'DOWNTOT',12(R3)  DOWNLOAD TOTALS                              
         BNE   OPT270                                                           
         OI    NDDOWNL,GLDLTOTS                                                 
         B     OPTNEXT                                                          
*                                                                               
OPT270   CLC   =C'ALLOC',12(R3)    ALLOCATED UNITS                              
         BNE   OPT280                                                           
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         MVI   NBSELFLT,6          UNALLOCATED ONLY                             
         B     OPTNEXT                                                          
         OI    NBACCFLT,X'02'      ALLOCATED ONLY                               
         B     OPTNEXT                                                          
*                                                                               
OPT280   DS    0H                                                               
         CLC    =C'NOG',12(R3)     DON'T APPLY DEMO/PKG GUARANTEE               
         BNE    OPT290                                                          
         MVI    NBDEMRAW,C'Y'                                                   
         B     OPTNEXT                                                          
*                                                                               
OPT290   DS    0H                                                               
*                                                                               
         CLC   =C'COMM',12(R3)      COMMERCIAL FILTER                           
         BNE   OPT295                                                           
         MVC   NDCMLFLT,22(R3)                                                  
         B     OPTNEXT                                                          
*                                                                               
OPT295   DS    0H                                                               
         CLC   =C'GHIST',12(R3)      GOAL HISTORY RECS ONLY                     
         BNE   OPT300                                                           
         OI    NDLOCAL,NDGHIST                                                  
         MVI   NDSEPOPT,C'Y'       AND SET TO PRINT ALL REC                     
         B     OPTNEXT                                                          
*                                                                               
*                                                                               
OPT300   DS    0H                                                               
         CLC   =C'RFP',12(R3)      RFPDATES?                                    
         BNE   OPT310                                                           
         OI    NBVARIND,X'80'                                                   
         B     OPTNEXT                                                          
*                                                                               
OPT310   DS    0H                                                               
         CLC   =C'GQ',12(R3)           GOAL EQUIVALENCING?                      
         BE    OPT312                                                           
         CLC   =C'GOALEQU',12(R3)      GOAL EQUIVALENCING?                      
         BNE   OPT320                                                           
OPT312   OI    NBINDS,X'04'        YES                                          
         B     OPTNEXT                                                          
*                                                                               
OPT320   DS    0H                                                               
         CLC   =C'OVR',12(R3)      DEMO OVERRIDE FILTERS?                       
         BNE   OPT325                                                           
         CLI   22(R3),C'B'         BOTH ACT AND EST                             
         BNE   *+12                                                             
         OI    NBINDS,X'03'                                                     
         B     OPTNEXT                                                          
         CLI   22(R3),C'E'         EST ONLY                                     
         BNE   *+12                                                             
         OI    NBINDS,X'02'                                                     
         B     OPTNEXT                                                          
         CLI   22(R3),C'A'         ACT ONLY                                     
         BNE   OPT325                                                           
         OI    NBINDS,X'01'                                                     
         B     OPTNEXT                                                          
*                                                                               
OPT325   DS    0H                                                               
         CLC   =C'0DOLS',12(R3)    0 DOLLAR OPTION?                             
         BNE   OPT330                                                           
         CLI   22(R3),C'Y'                                                      
         BNE   *+12                                                             
         OI    NBSBKEND,X'10'      ONLY ZERO DOLLARS                            
         B     OPTNEXT                                                          
         OI    NBSBKEND,X'40'      SKIP ZERO DOLLARS                            
         B     OPTNEXT                                                          
*                                                                               
OPT330   DS    0H                                                               
         CLC   =C'PQIX',12(R3)      PQIX TEST?                                  
         BNE   OPT332                                                           
         MVI   NDPQIX,C'Y'                                                      
         B     OPTNEXT                                                          
*                                                                               
OPT332   DS    0H                                                               
         CLC   =C'MSRPRD',12(R3)     MASTER PROD FOR UDEF RECS                  
         BNE   OPT334                                                           
         OI    NBSBKEND,X'80'                                                   
         B     OPTNEXT                                                          
*                                                                               
*                                                                               
OPT334   DS    0H                 POSSIBLE 4 ENTRIES                            
*                                 INV=NNNNNN,NNNNNN-NNNNNN,                     
*                                     -NNNNNN,NNNNNN-                           
         CLC   =C'INV',12(R3)     INVOICE FILTER                                
         BNE   OPT336                                                           
         CLI   1(R3),6             CHECK OBVIOUS ERROR                          
         BL    OPTERR                                                           
         CLI   1(R3),13                                                         
         BH    OPTERR                                                           
         ICM   R1,15,NBINVFLT      IS IT AROUND?                                
         BZ    OPTNEXT             NO                                           
         CLI   1(R3),6             ONLY ONE INVOICE INPUT?                      
         BNE   OPT334A                                                          
         MVC   29(6,R3),22(R3)     YES / SET IT AT END ALSO                     
         B     OPT334C                                                          
OPT334A  CLI   1(R3),13            START AND END INVOICES INPUT?                
         BE    OPT334C                                                          
         CLI   22(R3),C'-'          -NNNNNN?                                    
         BNE   OPT334B                                                          
         MVI   0(R1),C'*'          YES                                          
         MVC   6(6,R1),23(R3)                                                   
         B     OPT334X                                                          
OPT334B  CLI   28(R3),C'-'         NNNNNN-?                                     
         BNE   OPTERR                                                           
         MVC   0(6,R1),22(R3)                                                   
         MVI   6(R1),C'*'                                                       
         B     OPT334X                                                          
OPT334C  MVC   0(6,R1),22(R3)                                                   
         MVC   6(6,R1),29(R3)                                                   
OPT334X  B     OPTNEXT                                                          
*                                                                               
*                                                                               
OPT336   DS    0H                                                               
         CLC   =C'ISOD',12(R3)     ISODATE?                                     
         BNE   OPT338                                                           
         OI    NDLOCAL,NDISODAT                                                 
         B     OPTNEXT                                                          
                                                                                
OPT338   DS    0H                                                               
*                                                                               
         CLC   =C'MRUN',12(R3)                                                  
         BNE   OPT340                                                           
         OI    NBVARIND,X'02'      SET MRUN FILTER                              
         B     OPTNEXT                                                          
OPT340   DS    0H                                                               
         CLC   =C'NRCPM',12(R3)                                                 
         BNE   OPT342                                                           
         OI    NBVARIND,X'08'      DON'T ROUND CPM                              
         B     OPTNEXT                                                          
OPT342   DS    0H                                                               
         CLC   =C'EXPDEM6',12(R3)   FULL EXPANDED DEMO VALUES?                  
         BNE   OPT344                                                           
         OI    NBSBKEND,NBEXPDM6   DON'T TRUNCATE VALUE                         
         B     OPTNEXT                                                          
OPT344   DS    0H                                                               
         CLC   =C'EXPDEM',12(R3)   EXPANDED DEMO VALUES?                        
         BNE   OPT346                                                           
         OI    NBSBKEND,NBEXPDEM   EXPANDED DEMO VALUES                         
         B     OPTNEXT                                                          
OPT346   DS    0H                                                               
         CLC   =C'NOESTACT',12(R3)  DON'T DO EST AS ACT DEMOS                   
         BNE   OPT348                                                           
         OI    NBINDS2,NBNOEACT                                                 
         B     OPTNEXT                                                          
OPT348   DS    0H                                                               
         CLC   =C'PAYPEND',12(R3)   PAYPENDING OPTION                           
         BNE   OPT350                                                           
         OI    NBPPDOPT,NBPPDONQ    TURN ON OPTION                              
         B     OPTNEXT                                                          
*                                                                               
OPT350   DS    0H                                                               
*                                                                               
OPTERR   MVI   ERROR,INVALID                                                    
         NETGO NVCURSOR                                                         
         SPACE 1                                                                
OPTNEXT  LA    R3,62(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R4,OPT00                                                         
         B     XIT2                                                             
         SPACE 1                                                                
OPTDERR  MVI   ERROR,INVDATE                                                    
         NETGO NVCURSOR                                                         
         SPACE 1                                                                
OPTEBFRS MVI   ERROR,INVEBFRS                                                   
         NETGO NVCURSOR                                                         
*                                                                               
XIT2     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*              VALIDATE A DEFINITION EXPRESSION                                 
         SPACE 3                                                                
*              INPUT               R1=A(DEFINITION LIST) (R4 LOCAL)             
*                                  R3=A(SCANNER BLOCK)                          
         SPACE 1                                                                
VALDEFS  NTR1                                                                   
         LR    R4,R1               R4=A(DEFINITION LIST)                        
         CLC   22(4,R3),=C'NOCP'   SPECIAL ...=NOCPP EXPRESSION                 
         BNE   VDEF1                                                            
         MVI   1(R4),0             TURN OFF CPP/CPM                             
         B     XIT2                                                             
         SPACE 1                                                                
VDEF1    XC    0(8,R4),0(R4)       CLEAR THIS                                   
         ZIC   R1,1(R3)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    OPTERR                                                           
         LA    R1,22(R1,R3)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R3,22(R3)           (BUMP TO EXPRESSION)                         
         LA    R0,8                (MAX 8 TERMS)                                
         MVI   BYTE,0                                                           
         SPACE 1                                                                
VDEF2    LA    RF,2                2 BYTE EXPRESSIONS                           
         MVI   0(R4),X'02'         RAW CPP/CPM                                  
         CLC   0(2,R3),=C'RC'                                                   
         BE    VDEFRAW                                                          
         MVI   0(R4),X'E2'         EQUIVALENT CPP/CPM                           
         CLC   0(2,R3),=C'EC'                                                   
         BE    VDEFEQU                                                          
         MVI   0(R4),X'03'         POST INDEX                                   
         CLC   0(2,R3),=C'PX'                                                   
         BE    VDEFNXT                                                          
         MVI   0(R4),X'04'         EQUIVALENT INDEX                             
         CLC   0(2,R3),=C'EX'                                                   
         BE    VDEFEQU                                                          
         LA    RF,1                1 BYTE EXPRESSIONS                           
         MVI   0(R4),X'01'         RAW DATA                                     
         CLI   0(R3),C'R'                                                       
         BE    VDEFRAW                                                          
         MVI   0(R4),X'E1'         EQUIVALENT DATA                              
         CLI   0(R3),C'E'                                                       
         BE    VDEFEQU                                                          
         MVI   0(R4),X'05'         SPACE                                        
         CLI   0(R3),C'S'                                                       
         BE    VDEFNXT                                                          
         B     OPTERR                                                           
         SPACE 1                                                                
VDEFEQU  OI    BYTE,X'80'                                                       
*****    OI    NDANYDEM,EQU                                                     
         B     VDEFNXT                                                          
         SPACE 1                                                                
VDEFRAW  OI    BYTE,X'40'                                                       
****     OI    NDANYDEM,RAW                                                     
         SPACE 1                                                                
VDEFNXT  LA    R4,1(R4)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),C'/'          NEED A /                                     
         BNE   OPTERR                                                           
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         BE    VDEFLST                                                          
         BCT   R0,VDEF2                                                         
         B     OPTERR                                                           
         SPACE 1                                                                
VDEFLST  CLI   BYTE,X'C0'          IF BOTH RAW AND EQUIV REQUESTED              
**       BNE   XIT                 ASSUME THEY NEED BOTH                        
         OI    NDANYDEM,X'20'      NOTE OVERRIDE EXPRESSION                     
         B     XIT2                                                             
         EJECT                                                                  
*              VALIDATE A STACK EXPRESSION                                      
         SPACE 3                                                                
*              INPUT               R3=A(SCANNER BLOCK)                          
         SPACE 1                                                                
VALSTACK NTR1                                                                   
         ZIC   R1,1(R3)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    OPTERR                                                           
         LA    R1,22(R1,R3)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R3,22(R3)           (BUMP TO EXPRESSION)                         
         LA    R4,NDSTADEF                                                      
         LA    R0,8                (MAX 8 TERMS)                                
         SPACE 1                                                                
VSTK2    LA    RF,4                CHECK FOR 'DIFF' VERSIONS                    
         LA    R1,31                                                            
         CLC   0(4,R3),=C'DIFF'    DIFF=31                                      
         BE    VSTK6                                                            
         LA    RF,3                                                             
         CLC   0(4,R3),=C'DIF/'                                                 
         BE    VSTK6                                                            
         LA    RF,1                                                             
         CLC   0(2,R3),=C'D/'                                                   
         BE    VSTK6                                                            
         CLC   0(2,R3),=C'GT'      PREFIX OF GT=GRAND TOTAL ONLY                
         BNE   VSTK2A                                                           
         OI    0(R4),X'10'                                                      
         LA    R3,2(R3)                                                         
         B     VSTK4                                                            
VSTK2A   CLI   0(R3),C'D'          PREFIX OF D=DETAIL ONLY                      
         BNE   *+12                                                             
         OI    0(R4),X'80'                                                      
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'T'          PREFIX OF T=TOTAL ONLY                       
         BNE   *+12                                                             
         OI    0(R4),X'40'                                                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
VSTK4    LA    RF,5                5 BYTE EXPRESSIONS                           
         LA    R1,10                                                            
         CLC   0(5,R3),=C'SPACE'   SPACE=10                                     
         BE    VSTK6                                                            
         LA    R1,32                                                            
         CLC   0(5,R3),=C'INDEX'   INDEX=32                                     
         BE    VSTK6                                                            
         SPACE 1                                                                
         LA    RF,4                4 BYTE EXPRESSIONS                           
         LA    R1,6                                                             
         CLC   0(4,R3),=C'COST'    COST=6                                       
         BE    VSTK6                                                            
         LA    R1,7                                                             
         CLC   0(4,R3),=C'UNIT'    UNIT=7                                       
         BE    VSTK6                                                            
         LA    R1,23                                                            
         CLC   0(4,R3),=C'GOAL'    GOAL=23                                      
         BE    VSTK5                                                            
         LA    R1,34                                                            
         CLC   0(4,R3),=C'BDGT'    PUP BUDGET =34                               
         BE    VSTK6                                                            
         LA    R1,35                                                            
         CLC   0(4,R3),=C'CPMG'    PUP PLAN CPM GUA. =35                        
         BE    VSTK6                                                            
         LA    R1,24                                                            
         CLC   0(4,R3),=C'ECPP'    EST CPP/CPM =24                              
         BE    VSTK6                                                            
         CLC   0(4,R3),=C'ECPM'                                                 
         BE    VSTK6                                                            
         LA    R1,25                                                            
         CLC   0(4,R3),=C'ACPP'    ACT CPP/CPM =25                              
         BE    VSTK6                                                            
         CLC   0(4,R3),=C'ACPM'                                                 
         BE    VSTK6                                                            
         LA    R1,26                                                            
         CLC   0(4,R3),=C'GCPP'    GOAL CPP =26                                 
         BE    VSTK5                                                            
         LA    R1,21                                                            
         CLC   0(4,R3),=C'EEST'    EQU EST =21                                  
         BE    VSTK4Q                                                           
         LA    R1,22                                                            
         CLC   0(4,R3),=C'EACT'    EQU ACT =22                                  
         BE    VSTK4Q                                                           
         LA    R1,24                                                            
         CLC   0(4,R3),=C'EECP'    EQU EST CPM/CPP                              
         BE    VSTK4Q                                                           
         LA    R1,25                                                            
         CLC   0(4,R3),=C'EACP'    EQU ACT CPM/CPP                              
         BE    VSTK4Q                                                           
         LA    R1,27                                                            
         CLC   0(4,R3),=C'REST'    RAW EST =27                                  
         BE    VSTK4Q                                                           
         LA    R1,28                                                            
         CLC   0(4,R3),=C'RACT'    RAW ACT =28                                  
         BE    VSTK4Q                                                           
         LA    R1,29                                                            
         CLC   0(4,R3),=C'RECP'    RAW EST CPM/CPP                              
         BE    VSTK4Q                                                           
         LA    R1,30                                                            
         CLC   0(4,R3),=C'RACP'    RAW ACT CPM/CPP                              
         BE    VSTK4Q                                                           
         LA    R1,31                                                            
         CLC   0(4,R3),=C'DIFF'    DIFF = 31                                    
         BE    VSTK6                                                            
         SPACE 1                                                                
         LA    RF,3                3 BYTE EXPRESSIONS                           
         LA    R1,1                                                             
         CLC   0(3,R3),=C'IMP'     IMP=1                                        
         BE    VSTK6                                                            
         LA    R1,2                                                             
         CLC   0(3,R3),=C'GRP'     GRP=2                                        
         BE    VSTK6                                                            
         LA    R1,3                                                             
         CLC   0(3,R3),=C'CPM'     CPM=3                                        
         BE    VSTK6                                                            
         LA    R1,4                                                             
         CLC   0(3,R3),=C'CPP'     CPP=4                                        
         BE    VSTK6                                                            
         LA    R1,5                                                             
         CLC   0(3,R3),=C'VPH'     VPH=5                                        
         BE    VSTK6                                                            
         LA    R1,6                                                             
         CLC   0(3,R3),=C'NET'     NET=6                                        
         BNE   SKIPNET                                                          
         MVI   NDA$OPT,13          SET ROUTINE NUMBER FOR NENETACC              
         BE    VSTK6                                                            
SKIPNET  LA    R1,8                                                             
         CLC   0(3,R3),=C'CPU'     CPU=8                                        
         BE    VSTK6                                                            
         LA    R1,9                                                             
         CLC   0(3,R3),=C'RPU'     RPU=9                                        
         BE    VSTK6                                                            
         LA    R1,11                                                            
         CLC   0(3,R3),=C'IPU'     IPU=11                                       
         BE    VSTK6                                                            
         LA    R1,21                                                            
         CLC   0(3,R3),=C'EST'     EST=21                                       
         BE    VSTK6                                                            
         LA    R1,22                                                            
         CLC   0(3,R3),=C'ACT'     ACT=22                                       
         BE    VSTK6                                                            
         LA    R1,31                                                            
         CLC   0(3,R3),=C'DIF'     DIF=31                                       
         BE    VSTK6                                                            
         LA    R1,33                                                            
         CLC   0(3,R3),=C'CPX'     CPX=33                                       
         BE    VSTK6                                                            
         SPACE 1                                                                
         LA    RF,2                2 BYTE EXPRESSIONS                           
         LA    R1,24                                                            
         CLC   0(2,R3),=C'EC'      EST CPP/CPM =24                              
         BE    VSTK6                                                            
         LA    R1,25                                                            
         CLC   0(2,R3),=C'AC'      ACT CPP/CPM =25                              
         BE    VSTK6                                                            
         LA    R1,26                                                            
         CLC   0(2,R3),=C'GC'      GOAL CPP/CPM =26                             
         BE    VSTK5                                                            
         LA    R1,32                                                            
         CLC   0(2,R3),=C'IX'      IX=INDEX=32                                  
         BE    VSTK6                                                            
         LA    R1,10                                                            
         CLC   0(2,R3),=C'SP'      SP=SPACE=10                                  
         BE    VSTK6                                                            
         SPACE 1                                                                
         LA    RF,1                1 BYTE EXPRESSIONS                           
         LA    R1,1                                                             
         CLI   0(R3),C'I'          I=IMP=1                                      
         BE    VSTK6                                                            
         LA    R1,2                                                             
         CLI   0(R3),C'G'          G=GRP=2                                      
         BE    VSTK6                                                            
         LA    R1,5                                                             
         CLI   0(R3),C'V'          V=VPH=5                                      
         BE    VSTK6                                                            
         LA    R1,6                                                             
         CLI   0(R3),C'$'          $=COST=6                                     
         BE    VSTK6                                                            
         LA    R1,7                                                             
         CLI   0(R3),C'U'          U=UNITS=7                                    
         BE    VSTK6                                                            
         LA    R1,10                                                            
         CLI   0(R3),C'S'          S=SPACE=10                                   
         BE    VSTK6                                                            
         LA    R1,21                                                            
         CLI   0(R3),C'E'          E=EST=21                                     
         BE    VSTK6                                                            
         LA    R1,22                                                            
         CLI   0(R3),C'A'          A=ACT=22                                     
         BE    VSTK6                                                            
         LA    R1,31                                                            
         CLI   0(R3),C'D'          D=DIFF=31                                    
         BE    VSTK6                                                            
         B     OPTERR                                                           
         SPACE 1                                                                
VSTK4Q   OI    NDANYDEM,X'20'      (NOTE - RAW AND EQUIV NEEDED)                
         OI    NDANYDEM,RAW+EQU                                                 
         B     VSTK6                                                            
         SPACE 1                                                                
VSTK5    OI    NDCOLIND,X'80'      (NOTE THAT WE WILL NEED GOALS)               
         B     VSTK6                                                            
         SPACE 1                                                                
VSTK6    STC   R1,1(R4)            SAVE EXPRESSION NUMBER                       
         LA    R4,2(R4)                                                         
         AR    R3,RF                                                            
         CLI   0(R3),C'/'          NEED A /                                     
         BNE   OPTERR                                                           
         LA    R3,1(R3)                                                         
         CLI   0(R3),C' '                                                       
         BE    XIT2                                                             
         BCT   R0,VSTK2                                                         
         B     OPTERR                                                           
         EJECT                                                                  
*              ROUTINE TO VALIDATE ACCOUNTING STACK                             
         SPACE 3                                                                
VALAST   NTR1                                                                   
         ZIC   R1,1(R3)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    BADCOLX                                                          
         LA    R1,22(R1,R3)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R3,22(R3)           (BUMP TO EXPRESSION)                         
         LA    R4,NDASTDEF         ADDRESS ACC STACK DEFINITION                 
         XC    NDASTDEF,NDASTDEF                                                
         LA    R0,8                (MAX 8 TERMS)                                
         SPACE 1                                                                
VALAST2  LR    R1,R3               GET L'THIS EXPRESSION                        
         SPACE 1                                                                
VALAST4  CLI   0(R1),C'/'                                                       
         BE    VALAST6                                                          
         LA    R1,1(R1)                                                         
         B     VALAST4                                                          
         SPACE 1                                                                
VALAST6  SR    R1,R3                                                            
         LTR   R1,R1                                                            
         BZ    OPTERR                                                           
         CH    R1,=H'8'                                                         
         BH    OPTERR                                                           
         MVC   DUB,SPACES                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R3)                                                     
         LA    R3,2(R3,R1)         (R3 TO NEXT EXPRESSION)                      
         BAS   RE,LUPAST                                                        
         CLI   1(R4),0                                                          
         BNE   VALASTX                                                          
         MVI   0(R4),X'40'                                                      
         CLI   DUB,C'T'                                                         
         BE    VALAST8                                                          
         MVI   0(R4),X'80'                                                      
         CLI   DUB,C'D'                                                         
         BE    VALAST8                                                          
         CLC   DUB(4),=C'OPCT'     ONLY PRINT PCT/SKIP PREVIOUS DATA            
         BNE   OPTERR                                                           
         MVI   0(R4),0             RESET STATUS                                 
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         MVI   0(RE),X'01'         SET PREVIOUS DATA = NO PRINT                 
         B     VALAST8                                                          
         SPACE 1                                                                
VALAST8  MVC   DUB(L'DUB-1),DUB+1  STRIP OFF THE D OR T                         
         BAS   RE,LUPAST                                                        
         CLI   1(R4),0                                                          
         BE    OPTERR                                                           
         SPACE 1                                                                
VALASTX  LA    R4,2(R4)                                                         
         CLI   0(R3),X'41'                                                      
         BL    XIT2                                                             
         BCT   R0,VALAST2                                                       
         B     OPTERR                                                           
         EJECT                                                                  
*              LOOK UP ACCOUNTING STACK EXPRESSION                              
         SPACE 3                                                                
*              INPUT               DUB HAS POSSIBLE EXPRESSION                  
*              OUTPUT              RETURN NUMBER AT 1(R4)                       
         SPACE 1                                                                
LUPAST   NTR1                                                                   
         L     R1,=A(ASEXPTAB)     LOOK UP EXPRESSION TABLE FIRST               
         A     R1,RELOO                                                         
         SPACE 1                                                                
LUPAST2  CLC   0(7,R1),DUB                                                      
         BE    LUPAST4                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   LUPAST2                                                          
         B     XIT2                PXZ/RESTRICTED TO EXPRESSION TABLE           
         SPACE 1                                                                
         LA    R1,DUB                                                           
         BAS   RE,LUPACC           NOW LOOK FOR AN ACCOUNTING ENTRY             
         MVC   1(1,R4),NDSFTARG    RETURN SOFT ARG IF ANY GOOD                  
         B     XIT2                                                             
         SPACE 1                                                                
LUPAST4  MVC   1(1,R4),7(R1)       RETURN EXPRESSION NUMBER                     
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO LOOK UP FOR ACCGEN ARGUMENT NUMBER                    
         SPACE 3                                                                
LUPACC   NTR1                                                                   
*                                  R1=A(ENTRY NAME)                             
         MVI   NDSFTARG,0          PASS ARG BACK IN NDSFTARG                    
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),0(R1)                                                  
         LA    R1,WORK+15                                                       
         LA    R0,8                                                             
         SPACE 1                                                                
LUPACC2  STC   R0,WORK+5           GET L'FIELD                                  
         CLI   0(R1),C' '                                                       
         BH    LUPACC4                                                          
         BCTR  R1,0                                                             
         BCT   R0,LUPACC2                                                       
         B     XIT2                                                             
         SPACE 1                                                                
LUPACC4  OC    NDDRONE,NDDRONE                                                  
         BNZ   LUPACC6                                                          
         GOTO1 CALLOV,DMCB,0,X'D9000A39'  LOAD T00A39 (DRONE)                   
         MVC   NDDRONE,DMCB                                                     
         SPACE 1                                                                
LUPACC6  MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRENTRY                                                 
         MVC   DRDICT(8),=C'NETWRITE'                                           
         L     R1,NBAIO                                                         
         ST    R1,DRNETIO                                                       
         MVC   DRCOMFAC,NBACOM                                                  
         LA    R2,WORK                                                          
         ST    R2,DRNETFLD                                                      
         LA    R2,DRGEN                                                         
         GOTO1 NDDRONE,DMCB,(R2)                                                
         CLI   DRERROR,0                                                        
         BNE   XIT2                                                             
         L     R4,DRNETIO                                                       
         LA    R4,42(R4)                                                        
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEITD,R4                                                         
         CLC   DEITYPE,=C'M+'      CHECK DATA TYPE                              
         BNE   XIT2                                                             
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEIAD,R4                                                         
         MVC   NDSFTARG,DEIARGS       SET SOFT DOLLAR ARG                       
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO PICK OFF ACCOUNTING ARGS FOR STACK                    
         SPACE 3                                                                
VVALACC  DS    0H                                                               
         ZIC   R1,1(R3)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    BADCOLX                                                          
         LA    R1,22(R1,R3)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R3,22(R3)           (BUMP TO EXPRESSION)                         
         LA    R4,DRARGSI          ADDRESS INPUT ARGS                           
         MVI   DRNARGSI,16                                                      
         LA    R0,8                (MAX 8 TERMS)                                
         SPACE 1                                                                
VALACC2  LR    R1,R3               GET L'THIS EXPRESSION                        
         SPACE 1                                                                
VALACC4  CLI   0(R1),C'/'                                                       
         BE    VALACC6                                                          
         LA    R1,1(R1)                                                         
         B     VALACC4                                                          
         SPACE 1                                                                
VALACC6  SR    R1,R3                                                            
         LTR   R1,R1                                                            
         BZ    BADCOLX                                                          
         CH    R1,=H'8'                                                         
         BH    BADCOLX                                                          
         XC    DUB,DUB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(R3)                                                     
         LA    R3,2(R3,R1)         (R3 TO NEXT EXPRESSION)                      
         LA    R1,DUB                                                           
         BAS   RE,LUPACC                                                        
         MVC   0(1,R4),NDSFTARG                                                 
         CLI   NDSFTARG,0                                                       
         BE    BADCOLX                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R3),C' '                                                       
         BH    VALACC2                                                          
         B     XIT2                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* USER DEFINITION RECORDS                                                       
                                                                                
VALUDEF  DS    0H                                                               
         CLC   NBSELCLI,=C'ALL'    IF CLIENT = XXX                              
         BE    VUDEF0                                                           
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   VUDEF0                                                           
         BAS   RE,GTCLIENT         FILL UDEFD WITH CLIENT DATA                  
                                                                                
VUDEF0   DS    0H                                                               
         CLI   1(R4),2             AT LEAST ONE UDEF EXPRESSION                 
         BL    VUDEFNE                                                          
         BE    *+12                                                             
         CLI   NDMYPOSO,0          NO MORE THAN ONE FOR HEADS AND MIDS          
         BNE   VUDEFNE                                                          
         CLI   1(R4),11            ELSE MAX 4 UDEF EXPRESSIONS                  
         BH    VUDEFNE                                                          
         ZIC   R0,1(R4)                                                         
         LA    R1,DRARGSI          SET ARGUMENTS TO INPUT ROUTINE               
         XC    DRARGSI,DRARGSI                                                  
         SR    R2,R2               R2=LENGTH OF ARGS TO DRIVER ROUTINES         
         LA    R4,22(R4)           R4=A(UDEF EXPRESSION)                        
         SR    RE,RE               RE=WIDTH OF DRIVER INPUT FIELD               
         MVI   BYTE,0              BYTE=WIDTH OF COLUMN                         
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALUDEFT            ONLINE UDEF VALIDATION                       
         L     RF,NDUDEFD          OFFLINE/ADDRESS OF UDEF DATA                 
         USING SBLOCK,RF                                                        
*                                                                               
VUDEF2   CH    R0,=H'2'            EACH EXPRESSION IS 2 CHARACTERS              
         BL    VUDEFNE                                                          
         LA    R3,UDEFTAB          VALIDATE THEM                                
*                                                                               
VUDEF3   CLI   0(R3),0                                                          
         BE    VUDEFNE                                                          
         CLC   0(2,R4),0(R3)                                                    
         BE    *+12                                                             
         LA    R3,L'UDEFTAB(R3)                                                 
         B     VUDEF3                                                           
         MVC   0(1,R1),7(R3)       UDEF TYPE CODE                               
         OC    SBEUDEF,8(R3)       SET UDEF EXTRACT FIELD                       
         SR    R5,R5                                                            
         ICM   R5,3,4(R3)                                                       
         LA    R5,SBLOCK(R5)                                                    
         MVC   2(1,R1),0(R5)       MOVE UDEF DATA TYPE TO ARGS                  
         CLI   0(R5),C'D'          TEST DATA TYPE = DATE                        
         BNE   *+8                                                              
         LA    RE,2(RE)            YES-ALLOW 2 MORE FOR COMPRESSED DATE         
         SR    R8,R8                                                            
         CLC   =C'ALL',NBSELCLI    TEST ALL CLIENT REQUEST                      
         BNE   *+12                                                             
         IC    R8,6(R3)            YES-USE MAX LENGTH                           
         B     VUDEF4                                                           
                                                                                
         SR    R5,R5               NO-PICK UP LENGTH FOR CLIENT                 
         ICM   R5,3,2(R3)                                                       
         LA    R5,SBLOCK(R5)                                                    
         IC    R8,0(R5)                                                         
         C     R8,=F'20'           IF < 20 , SET TO 20                          
         BH    VUDEF4                                                           
         C     R8,=F'0'            UNLESS 0 (NO UDEF FOR CATEGORY)              
         BE    VUDEF4                                                           
         LA    R8,20                                                            
         SR    R5,R5               CHECK AGAINST MAX LANGTH                     
         IC    R5,6(R3)                                                         
         C     R5,=F'20'           IF MAX < 20                                  
         BNL   VUDEF4                                                           
         LR    R8,R5               USE THAT                                     
*                                                                               
VUDEF4   STC   R8,1(R1)            WIDTH GOES INTO ARGS                         
         LA    R2,3(R2)            TOTAL L'ARGUMENTS SO FAR                     
         AR    RE,R8               TOTAL L'DRIVER INPUT FIELD SO FAR            
         CLM   R8,1,BYTE           COMPARE TO COLUMN WIDTH SO FAR               
         BNH   *+8                                                              
         STC   R8,BYTE             LOW-UPDATE COLUMN WIDTH                      
*                                                                               
         CLI   NDMYPOSO,0          IF HEAD OR MID, DONE                         
         BNE   VUDEF6                                                           
         SH    R0,=H'2'            TEST ANY MORE EXPRESSIONS                    
         BNP   VUDEF6                                                           
         CLI   2(R4),C'/'          YES-MUST BE SEPARATED BY /                   
         BNE   VUDEFNE                                                          
         LA    R1,3(R1)            VALIDATE NEXT                                
         LA    R4,3(R4)                                                         
         BCT   R0,VUDEF2                                                        
*                                                                               
VUDEF6   MVC   DRARGSO,DRARGSI     O/P ARGS = I/P ARGS                          
         STC   R2,DRNARGSI         LENGTH OF ARGUMENT LISTS                     
         STC   R2,DRNARGSO                                                      
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,DRLENI           LENGTH FOR DRIVER INPUT ROUTINE              
         CLI   BYTE,0              TEST MAX WIDTH SET                           
         BNE   *+8                                                              
         MVI   BYTE,8              NO-DEFAULT TO 8                              
         MVC   DRLENO,BYTE         LENGTH FOR DRIVER OUTPUT ROUTINE             
         LA    R1,DRH1ARGS+1                                                    
         LA    RE,DRARGSI                                                       
         SR    R0,R0                                                            
*                                                                               
VUDEF7   CLI   0(RE),0                                                          
         BE    VUDEF8                                                           
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         A     R0,=F'1'                                                         
         LA    RE,3(RE)                                                         
         B     VUDEF7                                                           
*                                                                               
VUDEF8   STC   R0,DRH1ARGS         N'UDEF EXPRESSIONS                           
         A     R0,=F'1'                                                         
         STC   R0,DRH1NARG         LENGTH OF HEADING ROUTINE ARGS               
         OI    DRH1OPTS,DRHALGNL   LEFT ALIGN HEADINGS                          
*                                                                               
VUDEFEQ  CR    RB,RB                                                            
         B     VUDEFX                                                           
*                                                                               
VUDEFNE  CR    RB,R6                                                            
         B     VUDEFX                                                           
*                                                                               
VUDEFX   XIT1                                                                   
         SPACE 2                                                                
VALUDEFT DS    0H                                                               
*                                  R0 HAS LENGTH OF UDEF INPUT                  
         LA    R3,4                MAX POSSIBLE EXPRESSIONS                     
VDF3     LA    RE,4                MAX ENTRIES IN UDEFTAB                       
         LA    R2,UDEFTAB                                                       
         CH    R0,=H'2'            EACH UDEF IS 2 CHARACTERS                    
         BL    VUDEFNE                                                          
VDF5     CLC   0(2,R4),0(R2)       IS IT A MATCH                                
         BE    VDF10                                                            
         LA    R2,L'UDEFTAB(R2)                                                 
         BCT   RE,VDF5                                                          
         B     VUDEFNE                                                          
VDF10    SH    R0,=H'2'            ANY MORE EXPRESSIONS                         
         C     R0,=F'0'                                                         
         BE    VDFTX                                                            
         CLI   2(R4),C'/'          MUST BE SEPARATED BY /                       
         BNE   VUDEFNE                                                          
         BCTR  R0,0                                                             
         LA    R4,3(R4)            VALIDATE NEXT                                
         BCT   R3,VDF3                                                          
VDFTX    B     VUDEFEQ             OK                                           
         SPACE 2                                                                
         DS    0H                                                               
UDEFTAB  DS    0CL9                                                             
         DC    CL2'P1',AL2(SBUP1LEN-SBLOCK),AL2(SBUP1TYP-SBLOCK)                
         DC    AL1(L'SBUP1FLD),X'01',AL1(SBEUPRD1)                              
         DC    CL2'P2',AL2(SBUP2LEN-SBLOCK),AL2(SBUP2TYP-SBLOCK)                
         DC    AL1(L'SBUP2FLD),X'02',AL1(SBEUPRD2)                              
         DC    CL2'E1',AL2(SBUE1LEN-SBLOCK),AL2(SBUE1TYP-SBLOCK)                
         DC    AL1(L'SBUE1FLD),X'03',AL1(SBEUEST1)                              
         DC    CL2'E2',AL2(SBUE2LEN-SBLOCK),AL2(SBUE2TYP-SBLOCK)                
         DC    AL1(L'SBUE2FLD),X'04',AL1(SBEUEST2)                              
         DC    X'00'                                                            
         DROP  RF                                                               
         EJECT                                                                  
* - GET CLIENT USER DEFINITION DATA                                             
GTCLIENT NTR1                                                                   
         USING CLTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   CKEYAM(3),NBACTAM   READ CLIENT RECORD                           
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOT FILE                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE CLIENT RECORD                        
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         L     R1,NDUDEFD          GET USER DEF DESCRIPTIONS                    
         USING SBLOCK,R1                                                        
         MVC   SBUP1DES,CPU1       PRODUCT DESCRIPTION                          
         MVC   SBUP1TYP,CPU1TYPE   LENGTH                                       
         MVC   SBUP1LEN,CPU1LEN    TYPE                                         
         MVC   SBUP2DES,CPU2       DESCRIPTION                                  
         MVC   SBUP2TYP,CPU2TYPE   LENGTH                                       
         MVC   SBUP2LEN,CPU2LEN    TYPE                                         
         MVC   SBUE1DES,CEU1       ESTIMATE DESCRIPTION                         
         MVC   SBUE1TYP,CEU1TYPE   LENGTH                                       
         MVC   SBUE1LEN,CEU1LEN    TYPE                                         
         MVC   SBUE2DES,CEU2       DESCRIPTION                                  
         MVC   SBUE2TYP,CEU2TYPE   LENGTH                                       
         MVC   SBUE2LEN,CEU2LEN    TYPE                                         
         DROP  R1                                                               
         SPACE 1                                                                
* - RESET TO READ UNIT FILE                                                     
RESUNTF  XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
* THANK YOU EVAN JORDAN *                                                       
*          DATA SET SPWRIGEN   AT LEVEL 085 AS OF 10/03/00                      
* VALIDATE UCOM EXPRESSION                                                      
* INPUT  : R4=A(SCANNER BLOCK FOR UCOM EXPRESSION).                             
*          EXPRESSION IS OF THE FORM UCOM=AA/BB/CC/DD, WHERE AA, ETC            
*          ARE UCOM EXPRESSIONS. VALID ONES ARE P1 - P4 AND E1 - E4.            
* OUTPUT : DRIVER INPUT ROUTINE ARGS ARE 2 BYTES FOR EACH EXPRESSION:           
*          FORMAT IS +0(1) - UCOM TYPE CODE (1=P1,2=P2,5=E1,6=E2)               
*                    +1(1) - INPUT LENGTH                                       
*          DRIVER OUTPUT ROUTINE ARGS ARE THE SAME AS INPUT ARGS.               
*          DRIVER HEADING ROUTINE ARGS ARE: +0(1) - N'UCOM EXPRESSIONS          
*                                           +1(1) - 1ST UCOM TYPE CODE          
*                                           +2(1) - 2ND UCOM TYPE CODE          
*                                           ETC.                                
*          SBEUCOM IS SET TO EXTRACT PRODUCT AND/OR ESTIMATE FIELDS.            
*                                                                               
         SPACE 1                                                                
VALUCOM  DS    0H                                                               
                                                                                
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   VUCOM0                                                           
         CLC   NBSELCLI,=C'ALL'    AND NOT FOR ALL CLIENTS                      
         BE    VUCOM0                                                           
*                                  GET CLT LEVEL UCOM                           
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING DDUCOMD,RF                                                       
         MVC   UCACOMF,ACOMFACS                                                 
         MVI   UCSYS,C'N'                                                       
         MVC   UCSAM,NBACTAM                                                    
         MVC   UCSCLT,NBACTCLI                                                  
         MVI   UCOPT,UCOTTL        GET TITLES ONLY                              
*                                                                               
         GOTO1 =V(DDUCOM),ELEM                                                  
         LA    RF,ELEM                                                          
         CLI   UCERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    UCDATA,UCDNOCLT     NO CLT LEVEL UCOM REC                        
         BNZ   VUCOM0                                                           
*                                                                               
         L     RE,NDAUCOM                                                       
         USING UCOMD,RE                                                         
*                                                                               
         MVC   UCDP1LEN(4),UCPMXLNS  GET PRD UCOM LENGTHS                       
         L     R1,UCPTTLS                                                       
         MVC   UCDP1DES(80),0(R1)    AND PRD UCOM TITLES                        
         MVC   UCDP1TYP(4),UCPEDITS  TYPE OF DATA  D=DATE                       
*                                                                               
         MVC   UCDE1LEN(4),UCEMXLNS  GET EST UCOM LENGTHS                       
         L     R1,UCETTLS                                                       
         MVC   UCDE1DES(80),0(R1)    AND EST UCOM TITLES                        
         MVC   UCDE1TYP(4),UCEEDITS  TYPE OF DATA  D=DATE                       
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
VUCOM0   EQU   *                                                                
*                                                                               
         CLI   1(R4),2             AT LEAST ONE UCOM EXPRESSION                 
         BL    VUCOMNE                                                          
         BE    *+12                                                             
         CLI   NDMYPOSO,0            NO MORE THAN ONE FOR HEADS/MIDS            
         BNE   VUCOMNE                                                          
         CLI   1(R4),11            ELSE MAX 4 UCOM EXPRESSIONS                  
         BH    VUCOMNE                                                          
         ZIC   RF,1(R4)                                                         
         LA    R1,DRARGSI          SET ARGUMENTS TO INPUT ROUTINE               
         XC    DRARGSI,DRARGSI                                                  
         SR    R2,R2               R2=LENGTH OF ARGS TO DRIVER ROUTINES         
         LA    R4,22(R4)           R4=A(UCOM EXPRESSION)                        
         SR    RE,RE               RE=WIDTH OF DRIVER INPUT FIELD               
         MVI   BYTE,0              BYTE=WIDTH OF COLUMN                         
*                                                                               
VUCOM2   CH    RF,=H'2'            EACH EXPRESSION IS 2 CHARACTERS              
         BL    VUCOMNE                                                          
         LA    R3,UCOMTAB          VALIDATE THEM                                
*                                                                               
VUCOM3   CLI   0(R3),0                                                          
         BE    VUCOMNE                                                          
         CLC   0(2,R4),0(R3)                                                    
         BE    *+12                                                             
         LA    R3,L'UCOMTAB(R3)                                                 
         B     VUCOM3                                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUCOM5              ONLY VALIDATE ONLINE                         
*                                                                               
         MVC   0(1,R1),4(R3)       UCOM CODE P1,P2 ETC                          
         SR    R5,R5               PICK UP EDIT TYPE FROM CLIENT                
         ICM   R5,3,5(R3)                                                       
         A     R5,NDAUCOM                                                       
         IC    R0,0(R5)                                                         
         STC   R0,2(R1)            SET INTO ARGS LIST                           
*                                                                               
         LA    R0,32                                                            
         CLC   NBSELCLI,=C'ALL'    TEST ALL CLIENT REQUEST                      
         BE    VUCOM4                                                           
         SR    R5,R5               NO-PICK UP MAX LENGTH FROM CLIENT            
         ICM   R5,3,2(R3)                                                       
         A     R5,NDAUCOM                                                       
         IC    R0,0(R5)                                                         
*                                                                               
VUCOM4   STC   R0,1(R1)            WIDTH GOES INTO ARGS                         
         LA    R2,3(R2)            TOTAL L'ARGUMENTS SO FAR                     
         AR    RE,R0               TOTAL L'DRIVER INPUT FIELD SO FAR            
         CLM   R0,1,BYTE           COMPARE TO COLUMN WIDTH SO FAR               
         BNH   *+8                                                              
         STC   R0,BYTE             LOW-UPDATE COLUMN WIDTH                      
*                                                                               
VUCOM5   CLI   NDMYPOSO,0            IF HEAD OR MID, DONE                       
         BNE   VUCOM6                                                           
         AHI   RF,-2               TEST ANY MORE EXPRESSIONS                    
         BNP   VUCOM6                                                           
         CLI   2(R4),C'/'          YES-MUST BE SEPARATED BY /                   
         BNE   VUCOMNE                                                          
         LA    R1,3(R1)            VALIDATE NEXT                                
         LA    R4,3(R4)                                                         
         BCT   RF,VUCOM2                                                        
*                                                                               
VUCOM6   MVC   DRARGSO,DRARGSI     O/P ARGS = I/P ARGS                          
         STC   R2,DRNARGSI         LENGTH OF ARGUMENT LISTS                     
         STC   R2,DRNARGSO                                                      
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         STC   RE,DRLENI           LENGTH FOR DRIVER INPUT ROUTINE              
         CLI   BYTE,0              TEST MAX WIDTH SET                           
         BNE   *+8                                                              
         MVI   BYTE,8              NO-DEFAULT TO 8                              
         MVC   DRLENO,BYTE         LENGTH FOR DRIVER OUTPUT ROUTINE             
         LA    R1,DRH1ARGS+1                                                    
         LA    RE,DRARGSI                                                       
         SR    RF,RF                                                            
*                                                                               
VUCOM7   CLI   0(RE),0                                                          
         BE    VUCOM8                                                           
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)            BUMP DRH1ARGS                                
         LA    RE,3(RE)            BUMP DRARGSI                                 
         B     VUCOM7                                                           
*                                                                               
VUCOM8   STC   RF,DRH1ARGS         N'UCOM EXPRESSIONS                           
         LA    RF,1(RF)                                                         
         STC   RF,DRH1NARG         LENGTH OF HEADING ROUTINE ARGS               
         OI    DRH1OPTS,DRHALGNL   LEFT ALIGN HEADINGS                          
*                                                                               
         MVC   WORK(16),DRARGSI    TO SEE WHAT IVE GOT                          
*                                                                               
**       XC    FILENAME,FILENAME   RESET TO READ UNITFILE                       
**       NETGO NVSETUNT,DMCB                                                    
**       MVI   NBFUNCT,NBFRDHI                                                  
*                                                                               
         CR    RB,RB                                                            
         B     *+6                                                              
VUCOMNE  LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
         DS    0H                                                               
UCOMTAB  DS    0CL7                                                             
         DC    CL2'P1',AL2(UCDP1LEN-UCOMD),X'01',AL2(UCDP1TYP-UCOMD)            
         DC    CL2'P2',AL2(UCDP2LEN-UCOMD),X'02',AL2(UCDP2TYP-UCOMD)            
         DC    CL2'P3',AL2(UCDP3LEN-UCOMD),X'03',AL2(UCDP3TYP-UCOMD)            
         DC    CL2'P4',AL2(UCDP4LEN-UCOMD),X'04',AL2(UCDP4TYP-UCOMD)            
         DC    CL2'E1',AL2(UCDE1LEN-UCOMD),X'05',AL2(UCDE1TYP-UCOMD)            
         DC    CL2'E2',AL2(UCDE2LEN-UCOMD),X'06',AL2(UCDE1TYP-UCOMD)            
         DC    CL2'E3',AL2(UCDE3LEN-UCOMD),X'07',AL2(UCDE1TYP-UCOMD)            
         DC    CL2'E4',AL2(UCDE4LEN-UCOMD),X'08',AL2(UCDE1TYP-UCOMD)            
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
BADCOLX  MVC   HALF,=H'656'                                                     
         MVI   BYTE,0                                                           
         B     MYCURSR                                                          
         SPACE 1                                                                
MYCURSR  MVI   ERROR,X'FE'                                                      
         GOTO1 NDCURSOR                                                         
*                                                                               
         SPACE                                                                  
GETEL2   AH    (R4),DATADISP                                                    
*                                                                               
FIRSTEL2 CLI   0(R4),0                                                          
         BNE   *+10                                                             
         CLI   0((R4)),1                                                        
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0((R4))                                                   
         BCR   8,RE                                                             
*                                                                               
NEXTEL2  SR    RF,RF                                                            
         IC    RF,1((R4))                                                       
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1((R4)),1                                                        
         BR    RE                                                               
         AR    ((R4)),RF                                                        
         B     FIRSTEL2                                                         
*                                                                               
RELOO    DS    F                                                                
*                                                                               
* - 2 BYTE ERROR MESSAGES                                                       
ERR22    LA    R3,WORK                                                          
         USING GBLOCK,R3                                                        
         CLI   GMSGTYPE,C'I'       ..INFORMATION MESSAGE                        
         BE    *+10                                                             
         XC    GBLOCK,GBLOCK       ..NO/CLEAR BLOCK(DEFAULT=ERRORMSG)           
         MVC   GERROR,HALF         SET ERROR MSG CODE                           
         CLI   BYTE,X'FF'          .SOFT ERROR MESSAGE                          
         BNE   ERR22X                                                           
         LA    R1,ELEM             .YES/PASS ADDRESS OF SUPPLEMENT MSG          
         STCM  R1,7,GASUBST                                                     
         MVI   BYTE,0                                                           
ERR22X   GOTO1 NDERR                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         OI    6(R2),X'40'         POSITION CURSOR                              
         GOTO1 ERREX               SYSTEM MESSAGE                               
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEASTTAB                                                       
*              MACRO EXPANSION TABLES                                           
         SPACE 3                                                                
MACTABLE DS    0D                                                               
MACENTRY DS    CL20                                                             
         DC    CL8'MCEARH  ',A(MCERH),A(MCARH),X'00000000'                      
         DC    CL8'MCEARH1 ',A(MCERH),A(MCAR1),X'00000000'                      
         DC    CL8'MCEARH2 ',A(MCERH),A(MCAR2),X'00000000'                      
         DC    CL8'MCEARH3 ',A(MCERH),A(MCAR3),X'00000000'                      
         DC    CL8'MCEARH4 ',A(MCERH),A(MCAR4),X'00000000'                      
         DC    CL8'MCEARH5 ',A(MCERH),A(MCAR5),X'00000000'                      
         DC    CL8'MCEARH6 ',A(MCERH),A(MCAR6),X'00000000'                      
         DC    CL8'MCEARH7 ',A(MCERH),A(MCAR7),X'00000000'                      
         DC    CL8'MCEARH8 ',A(MCERH),A(MCAR8),X'00000000'                      
         SPACE 1                                                                
         DC    CL8'MCEAIH  ',A(MCEIH),A(MCAIH),X'00000000'                      
         DC    CL8'MCEAIH1 ',A(MCEIH),A(MCAI1),X'00000000'                      
         DC    CL8'MCEAIH2 ',A(MCEIH),A(MCAI2),X'00000000'                      
         DC    CL8'MCEAIH3 ',A(MCEIH),A(MCAI3),X'00000000'                      
         DC    CL8'MCEAIH4 ',A(MCEIH),A(MCAI4),X'00000000'                      
         DC    CL8'MCEAIH5 ',A(MCEIH),A(MCAI5),X'00000000'                      
         DC    CL8'MCEAIH6 ',A(MCEIH),A(MCAI6),X'00000000'                      
         DC    CL8'MCEAIH7 ',A(MCEIH),A(MCAI7),X'00000000'                      
         DC    CL8'MCEAIH8 ',A(MCEIH),A(MCAI8),X'00000000'                      
         SPACE 1                                                                
         DC    CL8'MCEAR1  ',A(MCER1),A(MCAR1),X'00000000'                      
         DC    CL8'MCEAR2  ',A(MCER1),A(MCAR2),X'00000000'                      
         DC    CL8'MCEAR3  ',A(MCER1),A(MCAR3),X'00000000'                      
         DC    CL8'MCEAR4  ',A(MCER1),A(MCAR4),X'00000000'                      
         DC    CL8'MCEAR5  ',A(MCER1),A(MCAR5),X'00000000'                      
         DC    CL8'MCEAR6  ',A(MCER1),A(MCAR6),X'00000000'                      
         DC    CL8'MCEAR7  ',A(MCER1),A(MCAR7),X'00000000'                      
         DC    CL8'MCEAR8  ',A(MCER1),A(MCAR8),X'00000000'                      
         SPACE 1                                                                
         DC    CL8'MCEAI1  ',A(MCEI1),A(MCAI1),X'00000000'                      
         DC    CL8'MCEAI2  ',A(MCEI1),A(MCAI2),X'00000000'                      
         DC    CL8'MCEAI3  ',A(MCEI1),A(MCAI3),X'00000000'                      
         DC    CL8'MCEAI4  ',A(MCEI1),A(MCAI4),X'00000000'                      
         DC    CL8'MCEAI5  ',A(MCEI1),A(MCAI5),X'00000000'                      
         DC    CL8'MCEAI6  ',A(MCEI1),A(MCAI6),X'00000000'                      
         DC    CL8'MCEAI7  ',A(MCEI1),A(MCAI7),X'00000000'                      
         DC    CL8'MCEAI8  ',A(MCEI1),A(MCAI8),X'00000000'                      
         SPACE 1                                                                
         DC    CL8'MCRH1   ',A(MCRH),A(MCR1),X'00000000'                        
         DC    CL8'MCRH2   ',A(MCRH),A(MCR2),X'00000000'                        
         DC    CL8'MCRH3   ',A(MCRH),A(MCR3),X'00000000'                        
         DC    CL8'MCRH4   ',A(MCRH),A(MCR4),X'00000000'                        
         DC    CL8'MCRH5   ',A(MCRH),A(MCR5),X'00000000'                        
         DC    CL8'MCRH6   ',A(MCRH),A(MCR6),X'00000000'                        
         DC    CL8'MCRH7   ',A(MCRH),A(MCR7),X'00000000'                        
         DC    CL8'MCRH8   ',A(MCRH),A(MCR8),X'00000000'                        
         SPACE 1                                                                
         DC    CL8'MCIH1   ',A(MCIH),A(MCI1),X'00000000'                        
         DC    CL8'MCIH2   ',A(MCIH),A(MCI2),X'00000000'                        
         DC    CL8'MCIH3   ',A(MCIH),A(MCI3),X'00000000'                        
         DC    CL8'MCIH4   ',A(MCIH),A(MCI4),X'00000000'                        
         DC    CL8'MCIH5   ',A(MCIH),A(MCI5),X'00000000'                        
         DC    CL8'MCIH6   ',A(MCIH),A(MCI6),X'00000000'                        
         DC    CL8'MCIH7   ',A(MCIH),A(MCI7),X'00000000'                        
         DC    CL8'MCIH8   ',A(MCIH),A(MCI8),X'00000000'                        
         SPACE 1                                                                
         DC    CL8'MCR2    ',A(MCR1),A(MCR2),X'00000000'                        
         DC    CL8'MCR3    ',A(MCR1),A(MCR3),X'00000000'                        
         DC    CL8'MCR4    ',A(MCR1),A(MCR4),X'00000000'                        
         DC    CL8'MCR5    ',A(MCR1),A(MCR5),X'00000000'                        
         DC    CL8'MCR6    ',A(MCR1),A(MCR6),X'00000000'                        
         DC    CL8'MCR7    ',A(MCR1),A(MCR7),X'00000000'                        
         DC    CL8'MCR8    ',A(MCR1),A(MCR8),X'00000000'                        
         SPACE 1                                                                
         DC    CL8'MCI2    ',A(MCI1),A(MCI2),X'00000000'                        
         DC    CL8'MCI3    ',A(MCI1),A(MCI3),X'00000000'                        
         DC    CL8'MCI4    ',A(MCI1),A(MCI4),X'00000000'                        
         DC    CL8'MCI5    ',A(MCI1),A(MCI5),X'00000000'                        
         DC    CL8'MCI6    ',A(MCI1),A(MCI6),X'00000000'                        
         DC    CL8'MCI7    ',A(MCI1),A(MCI7),X'00000000'                        
         DC    CL8'MCI8    ',A(MCI1),A(MCI8),X'00000000'                        
         SPACE 1                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
MCERH    DC    CL4'ERH '                                                        
MCARH    DC    CL4'ARH '                                                        
MCER1    DC    CL4'ER1 '                                                        
MCAR1    DC    CL4'AR1 '                                                        
MCER2    DC    CL4'ER2 '                                                        
MCAR2    DC    CL4'AR2 '                                                        
MCER3    DC    CL4'ER3 '                                                        
MCAR3    DC    CL4'AR3 '                                                        
MCER4    DC    CL4'ER4 '                                                        
MCAR4    DC    CL4'AR4 '                                                        
MCER5    DC    CL4'ER5 '                                                        
MCAR5    DC    CL4'AR5 '                                                        
MCER6    DC    CL4'ER6 '                                                        
MCAR6    DC    CL4'AR6 '                                                        
MCER7    DC    CL4'ER7 '                                                        
MCAR7    DC    CL4'AR7 '                                                        
MCER8    DC    CL4'ER8 '                                                        
MCAR8    DC    CL4'AR8 '                                                        
MCER9    DC    CL4'ER9 '                                                        
MCAR9    DC    CL4'AR9 '                                                        
         SPACE 1                                                                
MCEIH    DC    CL4'EIH '                                                        
MCAIH    DC    CL4'AIH '                                                        
MCEI1    DC    CL4'EI1 '                                                        
MCAI1    DC    CL4'AI1 '                                                        
MCEI2    DC    CL4'EI2 '                                                        
MCAI2    DC    CL4'AI2 '                                                        
MCEI3    DC    CL4'EI3 '                                                        
MCAI3    DC    CL4'AI3 '                                                        
MCEI4    DC    CL4'EI4 '                                                        
MCAI4    DC    CL4'AI4 '                                                        
MCEI5    DC    CL4'EI5 '                                                        
MCAI5    DC    CL4'AI5 '                                                        
MCEI6    DC    CL4'EI6 '                                                        
MCAI6    DC    CL4'AI6 '                                                        
MCEI7    DC    CL4'EI7 '                                                        
MCAI7    DC    CL4'AI7 '                                                        
MCEI8    DC    CL4'EI8 '                                                        
MCAI8    DC    CL4'AI8 '                                                        
MCEI9    DC    CL4'EI9 '                                                        
MCAI9    DC    CL4'AI9 '                                                        
         SPACE 1                                                                
MCRH     DC    CL4'RH  '                                                        
MCR1     DC    CL4'R1  '                                                        
MCR2     DC    CL4'R2  '                                                        
MCR3     DC    CL4'R3  '                                                        
MCR4     DC    CL4'R4  '                                                        
MCR5     DC    CL4'R5  '                                                        
MCR6     DC    CL4'R6  '                                                        
MCR7     DC    CL4'R7  '                                                        
MCR8     DC    CL4'R8  '                                                        
         SPACE 1                                                                
MCIH     DC    CL4'IH  '                                                        
MCI1     DC    CL4'I1  '                                                        
MCI2     DC    CL4'I2  '                                                        
MCI3     DC    CL4'I3  '                                                        
MCI4     DC    CL4'I4  '                                                        
MCI5     DC    CL4'I5  '                                                        
MCI6     DC    CL4'I6  '                                                        
MCI7     DC    CL4'I7  '                                                        
MCI8     DC    CL4'I8  '                                                        
         EJECT                                                                  
         SPACE 1                                                                
*        INCLUDE NEGENINCLS                                                     
*        INCLUDE NEDATELSTD                                                     
*        INCLUDE SPGENCLT                                                       
*        INCLUDE CTGENDIC                                                       
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE FATIOB                                                         
*        INCLUDE NEWRIFFD                                                       
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NEDATELSTD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE NEGENPLAN                                                      
       ++INCLUDE CTGENDIC                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE NEWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NEWRIUDEFD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE NEGBLOCKD                                                      
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE NETPQIND                                                       
       ++INCLUDE DDUCOMD                                                        
       ++INCLUDE NEUCOMD                                                        
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETDEMOT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001NEWRI99   08/20/02'                                      
         END                                                                    
