*          DATA SET ACGETFORM  AT LEVEL 082 AS OF 07/30/20                      
*PHASE GETFRMA                                                                  
*                                                                               
**********************************************************************          
*                                                                    *          
* PARAMETERS ARE AS FOLLOWS                                          *          
*                                                                    *          
* P1  BYTE  0     X'01' - FORMAT INFO                                *          
*     BYTE 1-3    A(BLOCK)                                           *          
*                                                                    *          
* P2  BYTE 0      X'00'                                              *          
*     BYTE 1-3    A(COMFACS)                                         *          
*                                                                    *          
* P3  BYTE 0      X'00'                                              *          
*     BYTE 1-3    A(SYSTEM SPECIFIC OVERLAY)                         *          
*                                                                    *          
**********************************************************************          
**********************************************************************          
* MNAS 03/06/2014 LVL=056 : DSFTK-41 - ADD ENTRY TO CUSNOTAB         *          
**********************************************************************          
         TITLE 'GETFORM - ACCOUNT FORMAT MODULE'                                
GETFORM  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*GETFORM                                             
         USING WORKD,RC                                                         
         USING ACWORKD,RA          (PASSED FROM SOURCE BOOK)                    
*                                                                               
         MVC   PARMS(PARMSLNQ),0(R1)                                            
*DSFTK-150                                                                      
         MVI   RUNSTAT,0                                                        
*DSFTK-150                                                                      
*                                                                               
         USING FORMD,R2                                                         
         SR    R2,R2                                                            
         ICM   R2,7,PBLOCK                                                      
         USING COMFACSD,R1                                                      
         ICM   R1,15,PCOMFACS                                                   
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VMASTC,CMASTC                                                    
         MVC   VDATCON,CDATCON                                                  
*DSFTK-150                                                                      
         MVC   VADDAY,CADDAY                                                    
*        TM    FFRMRST,AGYSPCL                                                  
*        BZ    *+8                                                              
*        OI    RUNSTAT,RUNSPCL     JPM SPECIAL ROUTINE                          
*DSFTK-150                                                                      
         DROP  R1                                                               
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         CLI   PREC,PFRM           ARE WE READING FORMAT INFO?                  
         BNE   GETF10                                                           
*                                                                               
         BRAS  RE,FRMREAD                                                       
         CLI   FFRMFRM,EOF                                                      
         BNE   *+8                 USE TABLE IF FORMAT REC NOT FOUND            
*                                                                               
         BRAS  RE,FRMRD                                                         
*                                                                               
         USING UTLD,R1                                                          
         USING MASTD,RE                                                         
GETF10   ICM   RE,15,VMASTC                                                     
         L     R1,MCUTL            R1=A(UTL)                                    
         MVC   TSYS,SVTSYS         RESTORE PHYSICAL SYSTEM NUMBER               
*DSFTK-150                                                                      
         CLI   MCRECOVR,C'W'       IS THIS A SOON RUN?                          
         BNE   *+8                                                              
         OI    RUNSTAT,RUNSN                                                    
*DSFTK-150                                                                      
         DROP  R1,RE                                                            
*                                                                               
         CLI   PREC,PRUNIT         ARE WE FORMATING OUTPUT LINE?                
         BNE   *+8                                                              
         BRAS  RE,RUNIT                                                         
*                                                                               
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INIT                                                                          
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   XSPACES,X'40'       INIT THE FIELD WITH A SPACE                  
         LA    RE,XSPACES                                                       
         LHI   RF,L'XSPACES                                                     
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES        FILL XSPACES WITH SPACES                     
         MVCL  RE,R0                                                            
*                                                                               
         MVI   ZERO,C'0'           FILL WITH CHARACTER ZEROES                   
         MVC   ZERO+1(L'ZERO-1),ZERO                                            
*                                                                               
         USING UTLD,R1                                                          
         USING MASTD,RE                                                         
         ICM   RE,15,VMASTC                                                     
         L     R1,MCUTL             R1=A(UTL)                                   
         MVC   SVTSYS,TSYS          SAVE SE # OF CALLING PROGRAM                
         MVI   TSYS,X'0A'           CONTROL                                     
         J     EXIT                                                             
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ FORMAT INFO                                                              
***********************************************************************         
         SPACE 1                                                                
FRMRD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,FRTNAREA         RE=A(OUTPUT RETURN AREA)                     
         LHI   RF,FRTNLNQ          RF=(LENGTH OF OUTPUT RETURN AREA)            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR RETURN AREA                            
*                                                                               
         USING AGYTABD,RE                                                       
         L     RE,=A(AGYTAB)                                                    
FRMR01   CLI   0(RE),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   AGYTAGY,FORMCPY     MATCH ON COMPANY                             
         BNE   *+14                                                             
         CLC   AGYTFRMC,FORMFRM    MATCH ON FORMAT CODE                         
         BE    *+12                                                             
         AHI   RE,AGYTLNQ                                                       
         B     FRMR01                                                           
*                                                                               
         MVC   FFRMRLN,AGYTRLN     RECORD LENGTH                                
         MVC   FFRMBSZ,AGYTBSZ     BLOCK SIZE                                   
*        MVC   FFRMNME,AGYTFRMN    FORMAT NAME                                  
         MVC   FFRMRST,AGYTSTAT    RECORD STATUS                                
         MVC   SVADDR3,AGYTFRMR    ADDRESS OF ROUTINE                           
*DSFTK-150                                                                      
*        TM    FFRMRST,AGYSPCL                                                  
*        BZ    *+8                                                              
*        OI    RUNSTAT,RUNSPCL     JPM SPECIAL ROUTINE                          
*DSFTK-150                                                                      
         DROP  RE                                                               
*                                                                               
         USING FRMTABD,R4                                                       
         L     R4,SVADDR3                                                       
         LA    R3,FFRMFRM          FORMAT RECORD OUTPUT BLOCK                   
         LA    RF,FFRMFLD          SET FRMFIELD OUTPUT BLOCK                    
         STCM  RF,15,SVADDR2                                                    
         LA    R6,FRMRMAX          RECORD MAXIMUM                               
FRMR05   ICM   R1,15,FRMRENT                                                    
         STCM  R1,15,SVADDR        POINT TO 1ST FIELD ADDRESS                   
FRMR10   CLI   0(R4),EOF           END OF RECORD TABLE                          
         BNE   *+12                                                             
         MVI   0(R3),EOF                                                        
         B     FRMRX                                                            
         MVC   0(FRMLNQ,R3),0(R4)                                               
         ICM   RF,15,SVADDR2       SET FRMFIELD OUTPUT BLOCK                    
         STCM  RF,15,FRMRENT-FRMREC(R3)                                         
*                                                                               
         USING FLDENTD,RE                                                       
FRMR20   ICM   RE,15,SVADDR                                                     
         ICM   RF,15,SVADDR2                                                    
         LA    R0,FRMFMAX          FIELD MAXIMUM                                
FRMR30   CLI   0(RE),EOF           END OF FIELD TABLE                           
         BNE   *+12                                                             
         MVI   0(RF),EOF           SET END OF FILE                              
         B     FRMR40                                                           
         MVC   0(FLDLNQ,RF),0(RE)                                               
         LA    RE,FLDLNQ(RE)                                                    
         LA    RF,FLDLNQ(RF)                                                    
         BCT   R0,FRMR30                                                        
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
FRMR40   AHI   RF,1                BUMP PAST EOF                                
         STCM  RF,15,SVADDR2                                                    
         LA    R4,FRMLNQ(R4)                                                    
         LA    R3,FRMLNQ(R3)                                                    
         BCT   R6,FRMR05                                                        
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
FRMRX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ FORMAT INFO                                                              
***********************************************************************         
         SPACE 1                                                                
         USING BANKRECD,IO                                                      
FRMREAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,FFRMFRM          SET START OF FFRMFRM AS                      
         MVI   0(RE),EOF           SET END OF FILE                              
         ST    RE,ACURREC          SET CURRENT ADDRESS OF RECS                  
         LA    RE,FFRMFLD          SET START OF FFRMFLD AS                      
         MVI   0(RE),EOF           SET END OF FILE                              
         MVI   0(RE),0             INDICATE FIRST FLDREC                        
         ST    RE,ACURENT          SET CURRENT ADDRESS OF FIELDS                
*                                                                               
         XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ    C'B' BANK RECORD                             
         MVI   BANKSUB,BANKSFQ     C'F' FORMAT RECORD                           
         MVC   BANKCDE,FORMBCDE    BANK CODE                                    
         MVC   BANKFORM,FORMFRM    FORMAT CODE                                  
*                                                                               
         MVC   KEYACC,IO                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYACC,IO                    
         CLC   KEYACC(BANKREC-BANKRECD),IO                                      
         BNE   FRX                 EXIT                                         
         B     FR025                                                            
FR010    GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYACC,IO                    
FR020    CLC   KEYACC(BANKREC-BANKRECD),IO                                      
         BNE   FRX                 EXIT WHEN DONE                               
*                                                                               
FR025    GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',BANKDA,IO,DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO+BANFIRST                                                   
FR030    CLI   0(R4),0                                                          
         BE    FR010                                                            
         CLI   0(R4),NAMELQ        NAME ELEMENT                                 
         BE    FR050                                                            
         CLI   0(R4),BFIELQ        FORMAT INFO ELEMENT                          
         BE    FR060                                                            
         CLI   0(R4),BFRELQ        FORMAT RECORD ELEMENT                        
         BE    FR070                                                            
         CLI   0(R4),BFLELQ        FORMAT FIELD ELEMENT                         
         BE    FR080                                                            
FR040    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     FR030                                                            
*                                                                               
         USING NAMELD,R4                                                        
FR050    SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     FR040                                                            
         MVC   FFRMNME(0),NAMEREC  FORMAT NAME                                  
         DROP  R4                                                               
*                                                                               
         USING BFIELD,R4                                                        
FR060    SR    R1,R1                                                            
         MVC   FFRMRLN,BFIRLEN     RECORD LENGTH                                
         MVC   FFRMBSZ,BFIBSZE     BLOCK SIZE                                   
         MVC   FFRMRST,BFISTAT     FORMAT STATUS                                
         B     FR040                                                            
         DROP  R4                                                               
*                                                                               
         USING FRMTABD,R3                                                       
         USING BFRELD,R4                                                        
*R070    L     R3,ACURREC          CURRENT ADDRESS OF RECORD RECORD             
FR070    L     R3,ACURENT          CURRENT ADDRESS OF FIELD RECORD              
         CLI   0(R3),0             IS THIS THE FIRST ONE?                       
         BE    *+12                                                             
         AHI   R3,1                                                             
         ST    R3,ACURENT                                                       
         L     R3,ACURREC          CURRENT ADDRESS OF RECORD RECORD             
         MVC   FRMRECN,BANKREC     RECORD NUMBER                                
         MVC   FRMSTATS,BFRRSTAT                                                
         L     RF,ACURENT          CURRENT ADDRESS OF FIELDS                    
         STCM  RF,15,FRMRENT                                                    
         LA    R3,FRMLNQ(R3)                                                    
         MVI   0(R3),EOF                                                        
         ST    R3,ACURREC                                                       
         B     FR040                                                            
         DROP  R3,R4                                                            
*                                                                               
         USING FLDENTD,R3                                                       
         USING BFLELD,R4                                                        
FR080    L     R3,ACURENT          CURRENT FIELD POSITION                       
         MVI   BYTE,0                                                           
         MVC   FLDNO,BFLFLD#       FIELD # (KEYWORD)                            
         MVC   FLDDSP,BFLDSP       DISPLACEMENT TO FIELD                        
         MVC   FLDLEN,BFLLEN       LENGTH OF FIELD                              
         MVC   FLDSTATS,BFLSTAT    FIELD STATUS BYTES                           
         MVC   FLDFRM,BFLFRM       DATE FORMAT                                  
         MVC   FLDTYPE,BFLFTYP     FIELD TYPE FOR DEPENDENT FIELDS              
         MVC   FLDOVR,BFLOVR       DATE OVERRIDE FIELD                          
         MVC   FLDSRCE,SPACES                                                   
         SR    R1,R1                                                            
         CLI   BFLLN,BFLLNQ        DO WE HAVE ANY SOURCE?                       
         BNH   FR090                                                            
         IC    R1,BFLLN                                                         
         SHI   R1,BFLLNQ+1                                                      
         MVC   FLDSRCE(0),BFLSRCE                                               
         EX    R1,*-6                                                           
*                                                                               
FR090    LA    R3,FLDLNQ(R3)                                                    
         ST    R3,ACURENT                                                       
         MVI   0(R3),EOF                                                        
         B     FR040                                                            
*                                                                               
FRX      J     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
FLDATAB  DS    0C                  FIELD ADDRESS TABLE                          
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(0),AL4(0)                                                    
         DC    AL1(EOF)                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  R2                                                               
**********************************************************************          
* PUT ALL FILED TO TAPE WORK AREA BASED ON FORMAT TABLE              *          
**********************************************************************          
         SPACE 1                                                                
RUNIT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING ACBANKD,R2                                                       
         SR    R2,R2                                                            
         ICM   R2,7,PBLOCK                                                      
*                                                                               
         MVI   ACBRCNT,0                                                        
*                                                                               
         L     R3,ACBOUTR          R3=A(OUTPUT RECORD)                          
         ST    R3,SVADDR2          SAVE OFF ORIGINAL STARTING ADDRESS           
         LR    RE,R3               CLEAR OUTPUT TAPE REC TO SPACES              
         LH    RF,ACBOUTRL           MAXIMUM 1000 BYTES                         
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,IO                POINT TO TEMPORARY OUTPUT AREA              
         LHI   RF,IOLNQ             MAXIMUM 2000 BYTES                          
         MVCL  RE,R0                                                            
         XC    TMPDISP,TMPDISP      INITIALIZE DISPLACEMENT                     
         XC    FLDDELIM,FLDDELIM    INITIALIZE FIELD DELIMETER                  
         XC    FLDSURRC,FLDSURRC    FIELD SURROUNDING CHARACTER FOR VLF         
*                                                                               
         USING FRMTABD,R7                                                       
         L     R7,ACBFRMT                                                       
RUNIT10  CLI   0(R7),EOF                                                        
         BE    RUNITX                                                           
         NI    ACBFLAG,X'FF'-ACBSKP                                             
         NI    FLAG,X'FF'-SKPOVRD                                               
         CLC   FRMRECN,ACBRMODE                                                 
         BNE   RUNIT60                                                          
         TM    FRMSTAT,FRMFRDEP    IS THIS FOR FOREIGN DEPENDENT                
         BNO   RUNIT20                                                          
         LA    R1,BO               SET IT AS BRANCH ON ONES                     
         TM    FRMSTAT2,FRMFRGN    IS THIS FOR FOREIGN AGENCIES ONLY            
         BNO   *+8                                                              
         LA    R1,BNO              ELSE ITS BRANCH ON NOT ONES                  
         TM    ACBFLAG,ACBFRGN                                                  
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNIT60                                                        
*                                                                               
RUNIT20  TM    FRMSTAT2,FRMUSO     IS THIS FOR US ONLY?                         
         BNO   RUNIT70                                                          
         LA    R1,BO               SET IT BRANCH ON ONES                        
         TM    FRMSTAT2,FRMFRGN    IS THIS FOR FOREIGN AGENCIES ONLY            
         BNO   *+8                                                              
         LA    R1,BNO              ELSE ITS BRANCH ON NOT ONES                  
         TM    ACBFLAG,ACBFRGN                                                  
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNIT60                                                        
         B     RUNIT70                                                          
*                                                                               
RUNIT30  TM    ACBFLAG,ACBSKP      NOTHING TO OUTPUT                            
         BNO   RUNIT40                                                          
         TM    FRMSTAT2,FRMFDEP    IS IT DEPENDENT?                             
         BNO   RUNIT60                                                          
         TM    FLAG,SKPOVRD        OVERRIDING THIS SKIP?                        
         BNO   RUNIT60                                                          
RUNIT40  DS    0H                                                               
*                                                                               
         TM    FRMSTAT2,FRMBVLF    NEED VARIABLE LENGTH FIELDS ?                
         BNO   RUNIT50                                                          
         XC    TMPDISP,TMPDISP                                                  
         L     R3,ACBOUTR          R3=A(OUTPUT RECORD)                          
         LR    RE,R3               CLEAR OUTPUT TAPE REC TO SPACES              
         LH    RF,ACBOUTRL         MAXIMUM 1000 BYTES                           
         LHI   R1,IOLNQ                                                         
         ICM   R1,8,XSPACES                                                     
*                                                                               
         LA    R0,IO                POINT TO TEMPORARY OUTPUT AREA              
         MVCL  RE,R0                AND MOVE TO ACTUAL OUTPUT AREA              
*                                                                               
RUNIT50  SR    R1,R1                                                            
         IC    R1,ACBRCNT          ADD TO RECORD COUNT                          
         AHI   R1,1                                                             
         STC   R1,ACBRCNT          STORE UPDATED RECORD COUNT                   
*                                                                               
*DSFTK-150                                                                      
         TM    ACBRSTAT,AGYSPCL    NEED VARIABLE LENGTH FIELDS ?                
         BNO   *+12                                                             
         LH    RF,=H'350'                                                       
         B     RUNIT55                                                          
*DSFTK-150                                                                      
         LH    RF,ACBRECLN                                                      
         TM    ACBRSTAT,ACBVRLN    IS THIS A VARIABLE RECORD LENGTH?            
         BNO   *+12                  IF NOT-USE ACBRECLN                        
         ICM   RF,3,0(R3)            ELSE USE FIRST 2 BYTES OF RECORD           
         AHI   RF,1                BUMP PAST LAST BYTE                          
RUNIT55  AR    R3,RF                                                            
         LR    RE,R3                                                            
         L     RF,SVADDR2                                                       
         SR    RE,RF                                                            
         CHI   RE,FORMLNQ                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         AH    RF,ACBOUTRL         GET TO END OF MAX AREA                       
         CR    R3,RF               MAKE SURE IO AREA SUPPLIED IS BIG            
         BL    *+6                 ENOUGH                                       
         DC    H'0'                                                             
*                                                                               
RUNIT60  AHI   R7,FRMLNQ                                                        
         B     RUNIT10                                                          
*                                                                               
RUNIT70  DS    0H                                                               
         TM    FRMSTAT2,FRMBVLF    BUILDING VARIABLE LENGTH FIELDS ?            
         BNO   RUNIT80                                                          
         TM    FRMSTAT2,FRMDCOMA   IS COMMA A SEPARATOR                         
         BO    *+6                                                              
         DC    H'0'                NEED A FLD SEPARATOR IF VAR LEN FLD          
         MVI   FLDDELIM,COMMA                                                   
         TM    FRMSTAT2,FRMSCQUO   FLD SURROUNDING CHARACTER                    
         BNO   *+8                                                              
         MVI   FLDSURRC,QUOTES                                                  
*                                                                               
RUNIT80  SR    R5,R5                                                            
         ICM   R5,15,FRMRENT       FORMAT RECORD ENTRY                          
*                                                                               
         USING FLDENTD,R5                                                       
RUNIT90  CLI   0(R5),EOF           DONE                                         
         BE    RUNIT30                                                          
         CLI   FLDNO,BK#ACC#       ACCOUNT                                      
         BE    RUNIT120                                                         
         CLI   FLDNO,BK#TRN#       TRANSIT NUMBER                               
         BE    RUNIT120                                                         
         CLI   FLDNO,BK#CHK#       CHECK NUMBER                                 
         BE    RUNIT130                                                         
         CLI   FLDNO,BK#NETA       AMOUNT (NET)                                 
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#GRSA       AMOUNT (GROSS)                               
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#CSDA       AMOUNT (CASH DISCOUNT)                       
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#CHKA       AMOUNT (CHECK)                               
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#BTOT       AMOUNT (BATCH TOTAL)                         
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#BTOTA      AMOUNT (TOTAL OF ALL BATCHES)                
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#NETTA      AMOUNT (TOTAL NET)                           
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#GRSTA      AMOUNT (TOTAL GROSS)                         
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#CSDTA      AMOUNT (TOTAL CASH DISC)                     
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#VOIDA      AMOUNT (VOID)                                
         BE    RUNIT140                                                         
         CLI   FLDNO,BK#RECCN      COUNT (RECORD)                               
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#TRNCN      COUNT (TRNSACTION)                           
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#VDRCN      COUNT (VOID)                                 
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#HDRCN      COUNT (HEADER RECORD)                        
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#CHKCN      COUNT (ITEMS/CHECK)                          
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#BCNT       COUNT (ONE BATCH)                            
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#BCNTA      COUNT (ALL BATCHES)                          
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#CHKS       COUNT (CHECKS ONLY)                          
         BE    RUNIT150                                                         
*DSFTK-135                                                                      
         CLI   FLDNO,BK#RECT       COUNT (TOTAL RECORDS)                        
         BE    RUNIT150                                                         
*DSFTK-135                                                                      
         CLI   FLDNO,BK#RECTL      COUNT (TOTAL RECORDS LITE)                   
         BE    RUNIT150                                                         
         CLI   FLDNO,BK#TRNDT      DATE (TRANSACTION)                           
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#ACTDT      DATE (ACTIVITY)                              
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#CHKDT      DATE (CHECK)                                 
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#INVDT      DATE (INVOICE)                               
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#PERDT      DATE (PERIOD)                                
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#TODDT      DATE (TODAY'S DATE)                          
         BE    RUNIT160                                                         
         CLI   FLDNO,BK#VAR        VARIABLE WITH SOURCE                         
         BE    RUNIT170                                                         
         CLI   FLDNO,BK#PAYNM      NAME (PAYEE)                                 
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#CPYNM      NAME (COMPANY)                               
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#PCTNM      NAME (PAYEE CONTACT NAME)                    
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#BNKNM      NAME (BANK NAME)                             
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#CLINM      NAME (CLIENT NAME)                           
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#MEDNM      NAME (MEDIA NAME)                            
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#CTRNM      NAME (COUNTRY NAME)                          
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#ICTRN      IDI NAME (COUNTRY NAME)                      
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#PRDNM      NAME (PRODUCT NAME)                          
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#PUBNM      NAME (PUB NAME)                              
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#IDINM      NAME (USER-ID NAME)                          
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#JOBNM      NAME (JOB NAME)                              
         BE    RUNIT180                                                         
         CLI   FLDNO,BK#RTE#       ROUTING NUMBER                               
         BE    RUNIT190                                                         
         CLI   FLDNO,BK#CUS#       CUSTOMER NUMBER                              
         BE    RUNIT200                                                         
         CLI   FLDNO,BK#EST#       ESTIMATE NUMBER                              
         BE    RUNIT210                                                         
         CLI   FLDNO,BK#CHKTY      CHECK NUMBER TYPE                            
         BE    RUNIT220                                                         
         CLI   FLDNO,BK#ADDR       ADDRESS LINES                                
         BE    RUNIT230                                                         
         CLI   FLDNO,BK#IADDR      IDI ADDRESS LINES                            
         BE    RUNIT230                                                         
         CLI   FLDNO,BK#ACTCD      DDS ACCOUNT CODE                             
         BE    RUNIT240                                                         
         CLI   FLDNO,BK#CTY        CITY                                         
         BE    RUNIT250                                                         
         CLI   FLDNO,BK#ICTY       IDI CITY                                     
         BE    RUNIT250                                                         
         CLI   FLDNO,BK#ST         STATE                                        
         BE    RUNIT260                                                         
         CLI   FLDNO,BK#IST        IDI STATE                                    
         BE    RUNIT260                                                         
         CLI   FLDNO,BK#ZIP        ZIP                                          
         BE    RUNIT270                                                         
         CLI   FLDNO,BK#IZIP       IDI ZIP                                      
         BE    RUNIT270                                                         
         CLI   FLDNO,BK#FAX#       FAX NUMBER                                   
         BE    RUNIT280                                                         
         CLI   FLDNO,BK#EML        EMAIL                                        
         BE    RUNIT290                                                         
         CLI   FLDNO,BK#INV#       INVOICE NUMBER                               
         BE    RUNIT300                                                         
         CLI   FLDNO,BK#INVDS      INVOICE DESCRIPTION                          
         BE    RUNIT310                                                         
         CLI   FLDNO,BK#PTYP       PAY TYPE                                     
         BE    RUNIT320                                                         
         CLI   FLDNO,BK#ENTID      ENTITY ID                                    
         BE    RUNIT330                                                         
         CLI   FLDNO,BK#FSEQ#      SEQUENCE # (FILE)                            
         BE    RUNIT340                                                         
         CLI   FLDNO,BK#BSEQ#      SEQUENCE # (BATCH)                           
         BE    RUNIT340                                                         
         CLI   FLDNO,BK#TSEQ#      SEQUENCE # (TRANSMISSION)                    
         BE    RUNIT340                                                         
         CLI   FLDNO,BK#RSEQ#      SEQUENCE # (RANDOM)                          
         BE    RUNIT340                                                         
         CLI   FLDNO,BK#STY#       STYLE NUMBER                                 
         BE    RUNIT350                                                         
         CLI   FLDNO,BK#MCDE       MAIL HANDLING CODE                           
         BE    RUNIT360                                                         
         CLI   FLDNO,BK#TIME       CURRENT TIME                                 
         BE    RUNIT370                                                         
         CLI   FLDNO,BK#CTRY       COUNTRY                                      
         BE    RUNIT380                                                         
         CLI   FLDNO,BK#ICTRY      IDI COUNTRY                                  
         BE    RUNIT380                                                         
         CLI   FLDNO,BK#PERD       DATA (PERIOD DATE)                           
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#PUBIN      DATA (PUB INDICATOR)                         
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#NMEIN      DATA (NAME INDICATOR)                        
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#REQDT      DATA (REQUEST DATA FIELD 1)                  
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#REQD2      DATA (REQUEST DATA FIELD 2)                  
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#BLK1       DATA (BLOCK DATA 1)                          
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#BLK2       DATA (BLOCK DATA 2)                          
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#LOGO       DATA (LOGO CODE)                             
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#RADC       DATA (RETURN ADDRESS CODE)                   
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#SIG        DATA (SIGNATURE CODE)                        
         BE    RUNIT390                                                         
         CLI   FLDNO,BK#RLEN       RECORD LENGTH                                
         BE    RUNIT400                                                         
         CLI   FLDNO,BK#TVEN       TRUE VENDOR                                  
         BE    RUNIT410                                                         
         CLI   FLDNO,BK#OVRF       OVERFLOW                                     
         BE    RUNIT420                                                         
         CLI   FLDNO,BK#CLI        CLIENT CODE                                  
         BE    RUNIT430                                                         
         CLI   FLDNO,BK#PROD       PRODUCT CODE                                 
         BE    RUNIT440                                                         
         CLI   FLDNO,BK#JOB        JOB CODE                                     
         BE    RUNIT450                                                         
         CLI   FLDNO,BK#RMTDV      REMITTANCE DELIVERY (FAX OR EMAIL)           
         BE    RUNIT460                                                         
         CLI   FLDNO,BK#PHN        VENDOR PHONE NUMBER                          
         BE    RUNIT470                                                         
         CLI   FLDNO,BK#TREF       TRUE REFERENCE                               
         BE    RUNIT480                                                         
         CLI   FLDNO,BK#CKDIG      CHECK DIGIT                                  
         BE    RUNIT490                                                         
         CLI   FLDNO,BK#ENTR#      ENTRY HASH                                   
         BE    RUNIT500                                                         
         CLI   FLDNO,BK#FILL       BLOCK FILLER                                 
         BE    RUNIT510                                                         
         CLI   FLDNO,BK#FLIDM      FILE ID MODIFIER                             
         BE    RUNIT520                                                         
         CLI   FLDNO,BK#BLKC       BLOCK COUNT                                  
         BE    RUNIT530                                                         
         CLI   FLDNO,BK#GEN#       GENERATION NUMBER                            
         BE    RUNIT540                                                         
*                                                                               
         CLI   FLDNO,BK#IDICD       IDI USER ID CODE                            
         BE    RUNIT550                                                         
*                                                                               
         CLI   FLDNO,BK#REPVN       REP/VENDOR SOURCE                           
         BE    RUNIT560                                                         
*                                                                               
         CLI   FLDNO,BK#MOS        MONTH OF SERVICE                             
         BE    RUNIT160                                                         
*                                                                               
         CLI   FLDNO,BK#CNTR       TRUE CONTRA ACCOUNT                          
         BE    RUNIT570                                                         
*                                                                               
*DSFTK-150                                                                      
         CLI   FLDNO,BK#PCTY       PCARD PAYMENT TYPE                           
         BE    RUNIT580                                                         
*                                                                               
*DSFTK-150                                                                      
         CLI   FLDNO,BK#OFFCT      TRANSACTION OFFICE                           
         BE    RUNIT590                                                         
*                                                                               
         CLI   FLDNO,BK#SWIFT      SWIFT CODE                                   
         BE    RUNIT600                                                         
*                                                                               
         CLI   FLDNO,BK#IACC#      INTL BANK ACCOUNT NUMBER                     
         BE    RUNIT120                                                         
*                                                                               
         CLI   FLDNO,BK#IBKNM      INTL BANK NAME                               
         BE    RUNIT180                                                         
*                                                                               
         CLI   FLDNO,BK#IBKAD      INTL BANK ADDRESS                            
         BE    RUNIT230                                                         
*                                                                               
         CLI   FLDNO,BK#IBCTY      INTL BANK CITY                               
         BE    RUNIT250                                                         
*                                                                               
         CLI   FLDNO,BK#IBST       INTL BANK STATE                              
         BE    RUNIT260                                                         
*                                                                               
         CLI   FLDNO,BK#IBZIP      INTL BANK ZIP CODE                           
         BE    RUNIT270                                                         
*                                                                               
         CLI   FLDNO,BK#IBCTR      INTL BANK COUNTRY                            
         BE    RUNIT380                                                         
*                                                                               
         CLI   FLDNO,BK#IBCTN      INTL BANK COUNTRY NAME                       
         BE    RUNIT180                                                         
*                                                                               
         CLI   FLDNO,BK#REPV2       REP/VENDOR SOURCE EXPANDED                  
         BE    RUNIT610                                                         
         CLI   FLDNO,BK#REPV3       REP/VENDOR SOURCE EXPANDED                  
         BE    RUNIT620                                                         
*                                                                               
*SPEC-40627                                                                     
         CLI   FLDNO,BK#CKDLV      CHECK DELIVERY FLAG                          
         BE    RUNIT630                                                         
*                                                                               
         CLI   FLDNO,BK#TRNCT      TRANSACTION COUNT (NOT DTL COUNT)            
         BE    RUNIT640                                                         
*                                                                               
         CLI   FLDNO,BK#CTSTZ      CITY/STATE/ZIP SQUASHED                      
         BE    RUNIT650                                                         
*                                                                               
*SPEC-40627                                                                     
*MN SPEC-46328                                                                  
         CLI   FLDNO,BK#SCFF1      SC FREEFORM TEXT FIELD 1                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF2      SC FREEFORM TEXT FIELD 2                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF3      SC FREEFORM TEXT FIELD 3                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF4      SC FREEFORM TEXT FIELD 4                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF5      SC FREEFORM TEXT FIELD 5                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF6      SC FREEFORM TEXT FIELD 6                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF7      SC FREEFORM TEXT FIELD 7                     
         BE    RUNIT660                                                         
         CLI   FLDNO,BK#SCFF8      SC FREEFORM TEXT FIELD 8                     
         BE    RUNIT660                                                         
*                                                                               
*MN SPEC-46328                                                                  
RUNIT100 DS    0H                                                               
         TM    ACBRSTAT,FFRCSV     IS THIS A CSV FILE                           
         BZ    RUNIT105            STRIP OUT COMMAS IN TEXT FIELDS              
         CLI   FLDLEN,1            SKIP FIELD SEPARATOR COMMAS                  
         BNH   RUNIT105                                                         
                                                                                
         LR    R4,R3               START OF CURRENT FIELD IN RECORD             
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    R4,R1                                                            
         SR    R1,R1                                                            
         ICM   R1,1,FLDLEN         LENGTH OF CURRENT FIELD                      
                                                                                
RUNIT103 CLI   0(R4),C','          REPLACE COMMA WITH SPACE                     
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R1,RUNIT103                                                      
                                                                                
RUNIT105 TM    FRMSTAT2,FRMBVLF    BUILDING VARIABLE LENGTH FIELDS ?            
         BNO   RUNIT110                                                         
         BRAS  RE,BUILDVLF         BUILD OUTPUT LINE WITH VAR LEN FLDS          
*                                                                               
RUNIT110 AHI   R5,FLDLNQ                                                        
         B     RUNIT90                                                          
*                                                                               
RUNIT120 BRAS  RE,RUNBACC          BANK ACCOUNT/TRANSIT NUMBER                  
         B     RUNIT100                                                         
*                                                                               
RUNIT130 BRAS  RE,RUNCHK#          CHECK NUMBER                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT140 BRAS  RE,RUNAMNT          AMOUNT FIELD                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT150 BRAS  RE,RUNCNT           COUNT                                        
         B     RUNIT100                                                         
*                                                                               
RUNIT160 BRAS  RE,RUNDTE           DATE                                         
         B     RUNIT100                                                         
*                                                                               
RUNIT170 BRAS  RE,RUNVAR           VARIABLE DATA                                
         B     RUNIT100                                                         
*                                                                               
RUNIT180 BRAS  RE,RUNNAME          NAME                                         
         B     RUNIT100                                                         
*                                                                               
RUNIT190 BRAS  RE,RUNRTE#          ROUTING NUMBER                               
         B     RUNIT100                                                         
*                                                                               
RUNIT200 BRAS  RE,RUNCUS#          CUSTOMER NUMBER                              
         B     RUNIT100                                                         
*                                                                               
RUNIT210 BRAS  RE,RUNEST#          ESTIMATE NUMBER                              
         B     RUNIT100                                                         
*                                                                               
RUNIT220 BRAS  RE,RUNCHKT          CHECK NUMBER TYPE                            
         B     RUNIT100                                                         
*                                                                               
RUNIT230 BRAS  RE,RUNADDR          ADDRESS LINES                                
         B     RUNIT100                                                         
*                                                                               
RUNIT240 BRAS  RE,RUNPACC          PAYEE DDS ACCOUNT CODE                       
         B     RUNIT100                                                         
*                                                                               
RUNIT250 BRAS  RE,RUNPCTY          PAYEE CITY                                   
         B     RUNIT100                                                         
*                                                                               
RUNIT260 BRAS  RE,RUNPST           PAYEE STATE                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT270 BRAS  RE,RUNPZIP          PAYEE ZIP                                    
         B     RUNIT100                                                         
*                                                                               
RUNIT280 BRAS  RE,RUNPFAX          PAYEE FAX NUMBER                             
         B     RUNIT100                                                         
*                                                                               
RUNIT290 BRAS  RE,RUNPEML          PAYEE EMAIL                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT300 BRAS  RE,RUNINV#          INVOICE NUMBER                               
         B     RUNIT100                                                         
*                                                                               
RUNIT310 BRAS  RE,RUNINVD          INVOICE DESCRIPTION (NARRATIVE)              
         B     RUNIT100                                                         
*                                                                               
RUNIT320 BRAS  RE,RUNPAYT          CHECK PAYMENT TYPE                           
         B     RUNIT100                                                         
*                                                                               
RUNIT330 BRAS  RE,RUNENTID         ENTITY ID                                    
         B     RUNIT100                                                         
*                                                                               
RUNIT340 BRAS  RE,RUNSEQ#          SEQUENCE NUMBER                              
         B     RUNIT100                                                         
*                                                                               
RUNIT350 BRAS  RE,RUNSTY#          STYLE NUMBER                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT360 BRAS  RE,RUNMAIL          MAIL HANDLING CODE                           
         B     RUNIT100                                                         
*                                                                               
RUNIT370 BRAS  RE,RUNCTIME         CURRENT TIME                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT380 BRAS  RE,RUNCTRY          COUNTRY CODE                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT390 BRAS  RE,RUNDATA          DATA (GENERIC INFO)                          
         B     RUNIT100                                                         
*                                                                               
RUNIT400 BRAS  RE,RUNRLEN          RECORD LENGTH                                
         B     RUNIT100                                                         
*                                                                               
RUNIT410 BRAS  RE,RUNTVEN          TRUE VENDOR                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT420 BRAS  RE,RUNOVRF          OVERFLOW FIELD                               
         B     RUNIT100                                                         
*                                                                               
RUNIT430 BRAS  RE,RUNCLI           CLIENT CODE                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT440 BRAS  RE,RUNPROD          PRODUCT CODE                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT450 BRAS  RE,RUNJOB           JOB CODE                                     
         B     RUNIT100                                                         
*                                                                               
RUNIT460 BRAS  RE,RUNRDV           REMITTANCE DELIVERY                          
         B     RUNIT100                                                         
*                                                                               
RUNIT470 BRAS  RE,RUNPHN           VENDOR PHONE NUMBER                          
         B     RUNIT100                                                         
*                                                                               
RUNIT480 BRAS  RE,RUNTREF          TRUE REFERENCE                               
         B     RUNIT100                                                         
*                                                                               
RUNIT490 BRAS  RE,RUNCDIG          CHECK DIGIT                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT500 BRAS  RE,RUNENT#          ENTRY HASH                                   
         B     RUNIT100                                                         
*                                                                               
RUNIT510 BRAS  RE,RUNBFLL          BLOCK FILLER                                 
         B     RUNIT100                                                         
*                                                                               
RUNIT520 BRAS  RE,RUNIDM           FILE ID MODIFIER                             
         B     RUNIT100                                                         
*                                                                               
RUNIT530 BRAS  RE,RUNBLKC          BLOCK COUNT                                  
         B     RUNIT100                                                         
*                                                                               
RUNIT540 BRAS  RE,RUNGEN#          GENERATION NUMBER                            
         B     RUNIT100                                                         
*                                                                               
RUNIT550 BRAS  RE,RUNIDCD          IDI USER ID CODE                             
         B     RUNIT100                                                         
*                                                                               
RUNIT560 BRAS  RE,RUNREPCD         REP/VENDOR SOURCE                            
         B     RUNIT100                                                         
*                                                                               
RUNIT570 BRAS  RE,RUNCNTR          CONTRA ACCOUNT                               
         B     RUNIT100                                                         
*                                                                               
*DSFTK-150                                                                      
RUNIT580 BRAS  RE,RUNPCRD          PCARD PAYEMNT TYPE                           
         B     RUNIT100                                                         
*                                                                               
*DSFTK-150                                                                      
RUNIT590 BRAS  RE,RUNTROF          TRANSACTION OFFICE                           
         B     RUNIT100                                                         
*                                                                               
RUNIT600 BRAS  RE,RUNSWFT          SWIFT CODE                                   
         B     RUNIT100                                                         
*                                                                               
RUNIT610 BRAS  RE,RUNREPV2         REP/VENDOR SOURCE                            
         B     RUNIT100                                                         
*                                                                               
RUNIT620 BRAS  RE,RUNREPV3         REP/VENDOR SOURCE                            
         B     RUNIT100                                                         
*                                                                               
*SPEC-40627                                                                     
RUNIT630 BRAS  RE,RUNCKDLV         CHECK DELIVERY FLAG                          
         B     RUNIT100                                                         
*                                                                               
*UNIT640 BRAS  RE,RUNCNT           TRANSACTION COUNT (NOT DTL COUNT)            
RUNIT640 BRAS  RE,RUNAMNT          TRANSACTION COUNT (NOT DTL COUNT)            
         B     RUNIT100                                                         
*                                                                               
RUNIT650 BRAS  RE,RUNCSZ           CITY/STATE/ZIP SQUASHED                      
         B     RUNIT100                                                         
*                                                                               
*SPEC-40627                                                                     
*MN SPEC-46328                                                                  
RUNIT660 BRAS  RE,RUNSCFFT         SC FREEFORM TEXT FIELDS 1-8                  
         B     RUNIT100                                                         
*MN SPEC-46328                                                                  
RUNITX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* BANK ACCOUNT/TRANSIT NUMBER                                        *          
**********************************************************************          
         SPACE 1                                                                
RUNBACC  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,ACBDBNK#                                                      
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RE,ACBSBNK#                                                      
         LR    R1,RE               SAVE ADDRESS OF INPUT                        
         SR    R0,R0                                                            
         IC    R0,FLDIDSP          GET DISPLACEMENT IN INPUT                    
         AR    RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,R1               RESTORE INPUT START                          
         AHI   RE,L'ACBDBNK#-1     FIND OUT LENGTH OF DATA                      
         LA    R1,L'ACBDBNK#                                                    
         CLI   0(RE),X'40'                                                      
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
         SR    R1,R0                                                            
         STCM  R1,3,HALF           SAVE OFF LENGTH OF DATA                      
*                                                                               
         CLI   FLDNO,BK#IACC#      INTERNATIONAL BANK ACC NUMBER                
         BNE   RUNBA05                                                          
         LA    RE,ACBINTB#                                                      
         LR    R1,RE               SAVE ADDRESS OF INPUT                        
         SR    R0,R0                                                            
         IC    R0,FLDIDSP          GET DISPLACEMENT IN INPUT                    
         AR    RE,R0                                                            
         ST    RE,FULL                                                          
         LR    RE,R1               RESTORE INPUT START                          
         AHI   RE,L'ACBINTB#-1     FIND OUT LENGTH OF DATA                      
         LA    R1,L'ACBINTB#                                                    
         CLI   0(RE),X'40'                                                      
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R1,*-12                                                          
         SR    R1,R0                                                            
         STCM  R1,3,HALF           SAVE OFF LENGTH OF DATA                      
*                                                                               
RUNBA05  LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         LR    RF,R1                                                            
         SH    RF,HALF                                                          
         BP    RUNBA10                                                          
*                                                                               
         LR    RE,R3               ACCOUNT                                      
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         L     RF,FULL             BANK ACCOUNT NUMBER                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       ACCOUNT NUMBER                               
         B     RUNBAX                                                           
*                                                                               
RUNBA10  CH    R1,HALF                                                          
         BNH   *+12                                                             
         LH    R1,HALF             LENGTH OF SOURCE FIELD IS MAX LENGTH         
         AHI   R1,-1                                                            
         TM    FLDSTAT,FLFT        IS THE FIELD LEFT JUSTIFIED                  
         BNO   RUNBA20                                                          
         L     R6,FULL                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R6)       ACCOUNT NUMBER                               
         LA    RE,1(R1,RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES     ASSUME SPACE PADDED                          
         TM    FLDSTAT,FSPCPAD                                                  
         BO    RUNBAX                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ZERO        ELSE IT MUST BE ZEROES                       
         B     RUNBAX                                                           
*                                                                               
RUNBA20  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES     ASSUME SPACE PADDED                          
         TM    FLDSTAT,FSPCPAD                                                  
         BO    RUNBA30                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ZERO        ELSE IT MUST BE ZEROES                       
RUNBA30  LA    RE,1(RF,RE)                                                      
         L     RF,FULL                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       BANK ACCOUNT                                 
*                                                                               
RUNBAX   J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK NUMBER                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNCHK#  NTR1  BASE=*,LABEL=*                                                   
         TM    FLDSTAT,FVDEP       IS THIS VOID DEPENDENT?                      
         BNO   RUNC#10                                                          
         LA    R1,BNH              SET IT AS BRANCH ON NOT HIGH                 
         TM    FLDSTAT,FVOID       IS THIS A VOID VARIABLE?                     
         BNO   *+8                                                              
         LA    R1,BH               ELSE ITS BRANCH ON HIGH                      
         CP    ACBNET,=P'0'                                                     
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNC#X                                                         
*                                                                               
RUNC#10  LA    RE,ACBCHECK+L'ACBCHECK-1  FIND OUT LENGTH OF DATA                
         LA    R0,L'ACBCHECK                                                    
         CLI   0(RE),X'40'                                                      
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R0,*-12                                                          
         STCM  R0,3,HALF           SAVE OFF LENGTH OF DATA                      
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         ST    RE,FULL             SAVE FOR PREFIX IF NECESSARY                 
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
*        AHI   R1,-1                                                            
         LR    RF,R1                                                            
         SH    RF,HALF                                                          
         BNM   *+6                                                              
         DC    H'0'                LENGTH MUST BE BAD                           
         CH    R1,HALF                                                          
*        BNH   *+12                                                             
         BNH   *+8                                                              
         LH    R1,HALF             LENGTH OF SOURCE FIELD IS MAX LENGTH         
         AHI   R1,-1                                                            
         LTR   RF,RF               DOES THE CHECK # LENGTH=MAX LENGTH?          
         BZ    RUNC#32             YES                                          
         AHI   RF,-1                                                            
         TM    FLDSTAT,FLFT        IS THE FIELD LEFT JUSTIFIED                  
         BNO   RUNC#20                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBCHECK    CHECK NUMBER                                 
         LA    RE,1(R1,RE)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES     ASSUME SPACE PADDED                          
         TM    FLDSTAT,FSPCPAD                                                  
         BO    RUNC#40                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ZERO        ELSE IT MUST BE ZEROES                       
         B     RUNC#40                                                          
*                                                                               
RUNC#20  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES     ASSUME SPACE PADDED                          
         TM    FLDSTAT,FSPCPAD                                                  
         BO    RUNC#30                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ZERO        ELSE IT MUST BE ZEROES                       
RUNC#30  LA    RE,1(RF,RE)                                                      
RUNC#32  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBCHECK    CHECK NUMBER                                 
RUNC#40  TM    FLDSTAT,FPREFIX     ARE WE USING PREFIXES?                       
         BNO   RUNC#X                                                           
*                                                                               
         USING PRFXTBD,RF                                                       
         L     RF,=A(PRFXTAB)                                                   
RUNC#50  CLC   PRFALPHA,XSPACES                                                 
         BNE   *+16                                                             
         TM    FLDSTAT,FPRFXDFT    DO WE WANT DEFAULT?                          
         BNO   RUNC#X                                                           
         B     RUNC#80                                                          
         CLC   ACBALPHA,PRFALPHA                                                
         BNE   RUNC#70                                                          
         LA    RE,ACBCSHAC                                                      
         LA    R1,L'ACBCSHAC                                                    
         TM    PRFOPT,PRFLEDG      MATCH ON LEDGER?                             
         BNO   RUNC#60                                                          
         LA    RE,ACBCUL           LEDGER FROM CONTRA                           
         LA    R1,L'ACBCUL                                                      
         TM    PRFOPT,PRFACCM      MATCH ON LEDGER FROM ACCT INSTEAD?           
         BZ    *+12                                                             
         LA    RE,ACBVUL           LEDGER FROM ACCOUNT                          
         LA    R1,L'ACBVUL                                                      
RUNC#60  AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),PRFDATA                                                  
         BE    RUNC#80                                                          
RUNC#70  AHI   RF,PRFTBLNQ                                                      
         B     RUNC#50                                                          
*                                                                               
RUNC#80  SR    R0,R0                                                            
         IC    R0,PRFDSP                                                        
         SR    R1,R1                                                            
         IC    R1,PRFLN                                                         
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         L     RE,FULL                                                          
         AR    RE,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),PRFPRF                                                   
*                                                                               
RUNC#X   J     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK NUMBER TYPE                                                  *          
**********************************************************************          
         SPACE 1                                                                
RUNCHKT  NTR1  BASE=*,LABEL=*                                                   
         CLC   ACBCHKTY,XSPACES    ANY CHECK NUMBER TYPE?                       
         BNH   RUNCTX                                                           
*                                                                               
         TM    FLDSTAT,FVDEP       IS THIS VOID DEPENDENT?                      
         BNO   RUNCT10                                                          
         LA    R1,BNH              SET IT AS BRANCH ON NOT HIGH                 
         TM    FLDSTAT,FVOID       IS THIS A VOID VARIABLE?                     
         BNO   *+8                                                              
         LA    R1,BH               ELSE ITS BRANCH ON HIGH                      
         CP    ACBNET,=P'0'                                                     
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNCTX                                                         
*                                                                               
RUNCT10  LA    RE,ACBCHECK+L'ACBCHECK-1  FIND OUT LENGTH OF DATA                
         LA    R0,L'ACBCHECK                                                    
         CLI   0(RE),X'40'                                                      
         BH    *+12                                                             
         AHI   RE,-1                                                            
         BCT   R0,*-12                                                          
         STCM  R0,3,HALF           SAVE OFF LENGTH OF DATA                      
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         ST    RE,FULL             SAVE FOR PREFIX IF NECESSARY                 
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         LR    RF,R1                                                            
         SH    RF,HALF                                                          
         BP    *+6                 THE LENGTH IS BAD ....                       
         DC    H'0'                OR MAYBE THE CODING IS BAD                   
         CH    R1,HALF                                                          
         BNH   *+12                                                             
         LH    R1,HALF             LENGTH OF SOURCE FIELD IS MAX LENGTH         
         AHI   R1,-1                                                            
         TM    FLDSTAT,FLFT        IS THE FIELD LEFT JUSTIFIED                  
         BO    *+8                                                              
         LA    RE,1(RF,RE)                                                      
         MVC   0(1,RE),ACBCHKTY    CHECK NUMBER TYPE                            
*                                                                               
RUNCTX   J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* AMOUNT FIELD                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNAMNT  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3               AMOUNT FIELD                                 
         SR    R1,R1               (DETAIL AMNT/TRAILER AMNT)                   
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
*                                                                               
         LA    RF,ACBNET                                                        
         LA    R1,L'ACBNET                                                      
         CLI   FLDNO,BK#NETA       AMOUNT (NET)                                 
         BE    RUNAM10                                                          
         LA    RF,ACBGROSS                                                      
         LA    R1,L'ACBGROSS                                                    
         CLI   FLDNO,BK#GRSA       AMOUNT (GROSS)                               
         BE    RUNAM10                                                          
         LA    RF,ACBCD                                                         
         LA    R1,L'ACBCD                                                       
         CLI   FLDNO,BK#CSDA       AMOUNT (CASH DISCOUNT)                       
         BE    RUNAM10                                                          
         LA    RF,ACBCKAMT                                                      
         LA    R1,L'ACBCKAMT                                                    
         CLI   FLDNO,BK#CHKA       AMOUNT (CHECK)                               
         BE    RUNAM10                                                          
         LA    RF,ACBTNET                                                       
         LA    R1,L'ACBTNET                                                     
         CLI   FLDNO,BK#NETTA      AMOUNT (TOTAL NET)                           
         BE    RUNAM10                                                          
         LA    RF,ACBTGRS                                                       
         LA    R1,L'ACBTGRS                                                     
         CLI   FLDNO,BK#GRSTA      AMOUNT (TOTAL GROSS)                         
         BE    RUNAM10                                                          
         LA    RF,ACBTCSD                                                       
         LA    R1,L'ACBTCSD                                                     
         CLI   FLDNO,BK#CSDTA      AMOUNT (TOTAL CASH DISC)                     
         BE    RUNAM10                                                          
         LA    RF,ACBVAMT                                                       
         LA    R1,L'ACBVAMT                                                     
         CLI   FLDNO,BK#VOIDA      AMOUNT (VOID)                                
         BE    RUNAM10                                                          
         LA    RF,ACBBTOT                                                       
         LA    R1,L'ACBBTOT                                                     
         CLI   FLDNO,BK#BTOT       AMOUNT (BATCH TOTAL)                         
         BE    RUNAM10                                                          
         LA    RF,ACBBTOTA                                                      
         LA    R1,L'ACBBTOTA                                                    
         CLI   FLDNO,BK#BTOTA      AMOUNT (TOTAL OF ALL BATCHES)                
         BE    RUNAM10                                                          
*SPEC-40627                                                                     
         LA    RF,ACBTRNCT         TRANSACTION COUNT (NOT DTL COUNT)            
         LA    R1,L'ACBTRNCT                                                    
         CLI   FLDNO,BK#TRNCT      HEADER AND TRAILER                           
         BE    RUNAM10                                                          
*SPEC-40627                                                                     
         DC    H'0'                                                             
*                                                                               
RUNAM10  AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         ZAP   DUB,0(0,RF)                                                      
         CP    DUB,=P'0'           CHECK FOR ZERO AMOUNTS                       
         BNZ   RUNAM20                                                          
         TM    FLDSTAT2,FZERO      EXCLUDE $0.00?                               
         BNO   RUNAM20                                                          
         OI    ACBFLAG,ACBSKP      SKIP THIS ENTRY                              
         B     RUNAMX                                                           
RUNAM20  MVC   WRKAMT,=X'404040404040402020202020202020202020'                  
         MVI   WRKAMT+16,X'21'                                                  
         TM    FLDSTAT2,F2DEC      INCLUDE 2 DECIMAL PLACES?                    
         BNO   *+10                                                             
         MVC   WRKAMT,=X'4040404040402020202020202020214B2020'                  
         TM    FLDSTAT,FSPCPAD                                                  
         BO    *+8                                                              
         MVI   WRKAMT,C'0'                                                      
         TM    FLDSTAT3,FCOMMAS    DO THEY WANT COMMAS?                         
         BNO   RUNAM25                                                          
         MVC   WRKAMT+1(L'WRKAMT-1),WRKCOM                                      
         TM    FLDSTAT2,F2DEC      INCLUDE 2 DECIMAL PLACES?                    
         BNO   *+10                                                             
         MVC   WRKAMT+1(L'WRKAMT-1),WRKDEC                                      
RUNAM25  ED    WRKAMT,DUB+2                                                     
*                                                                               
         TM    FLDSTAT2,FMINUS     DO THEY WANT TRAILING MINUS                  
         BNO   RUNAM30                                                          
         CP    DUB,=P'0'                                                        
         BNL   RUNAM30                                                          
         MVC   WRKAMT(L'WRKAMT-1),WRKAMT+1                                      
         MVI   WRKAMT+L'WRKAMT-1,X'60'                                          
*                                                                               
RUNAM30  TM    FLDSTAT,FLFT        LEFT JUSTIFY THE AMOUNT?                     
         BZ    RUNAM70                                                          
         LA    R1,L'WRKAMT                                                      
         LA    RF,WRKAMT(R1)       POINT TO END OF AMOUNT                       
RUNAM35  AHI   RF,-1               BUMP BACK ONE                                
         CLI   0(RF),C' '                                                       
         BNH   RUNAM40                                                          
         BCT   R1,RUNAM35                                                       
*                                                                               
RUNAM40  LA    RF,WRKAMT(R1)       BUMP UP TO 1ST DIGIT IN WORK                 
         LA    R0,L'WRKAMT                                                      
         SR    R0,R1               LENGTH FOR MOVE                              
         LR    R1,R0               DO THIS B/B CAN'T USE R0 FOR EXMVC           
         BCTR  R1,0                                                             
         TM    FLDSTAT2,FFLOAT     IF THEY WANT A LEADING MINUS AND THE         
         BNO   RUNAM45             AMOUNT IS NEGATIVE START FILLING IN          
         CP    DUB,=P'0'           THE AMOUNT IN THE 2ND BYTE OF OUTPUT         
         BNL   RUNAM45             FIELD.                                       
         EXMVC R1,1(RE),0(RF)                                                   
         B     RUNAM75                                                          
RUNAM45  EXMVC R1,0(RE),0(RF)                                                   
         B     RUNAM75                                                          
*                                                                               
RUNAM70  LHI   R1,L'WRKAMT         DISPLACEMENT TO END OF WORK FLD              
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         SR    R1,RF                                                            
         LA    RF,WRKAMT(R1)                                                    
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
RUNAM75  TM    FLDSTAT2,FFLOAT     DO THEY WANT LEADING MINUS                   
         BNO   RUNAMX                                                           
         CP    DUB,=P'0'                                                        
*SPEC-28147                                                                     
*        BNL   RUNAMX                                                           
         BNL   RUNAM90                                                          
*SPEC-28147                                                                     
         AHI   R1,1                RESTORE ORIGINAL LENGTH FROM ABOVE           
         TM    FLDSTAT,FSPCPAD                                                  
         BZ    RUNAM86                                                          
RUNAM80  CLI   0(RE),C'.'          DID WE HIT A DECIMAL?                        
         BE    RUNAM82                                                          
         CLI   0(RE),C' '                                                       
         BH    RUNAM84                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,RUNAM80                                                       
         B     RUNAMX                                                           
*                                                                               
RUNAM82  DS    0H                                                               
*        AHI   RE,-1               BACK UP 1 BYTE *CAUSING BUG*                 
*        CL#0389136N               POSSIBLY WHEN LEFT JUSTIFYING                
*MN BUG FOUND IN TESTING SPEC-40627                                             
         TM    FLDSTAT,FLFT        LEFT JUSTIFY THE AMOUNT?                     
         BNZ   RUNAM83                                                          
         AHI   RE,-1               BACK UP 1 BYTE                               
         MVI   0(RE),C'0'              AND PUT 0 BEFORE DECIMAL                 
         AHI   RE,-1               BACK UP 1 BYTE                               
         MVI   0(RE),C'-'              AND PUT FLOATING MINUS                   
         B     RUNAM87                                                          
RUNAM83  DS    0H                                                               
*MN BUG FOUND IN TESTING SPEC-40627                                             
         MVC   3(1,RE),2(RE)                                                    
         MVC   2(1,RE),1(RE)                                                    
         MVC   1(1,RE),0(RE)                                                    
         MVI   0(RE),C'0'              AND PUT 0 BEFORE DECIMAL                 
RUNAM84  AHI   RE,-1               BACK UP 1 BYTE                               
RUNAM86  MVI   0(RE),C'-'              AND PUT FLOATING MINUS                   
*SPEC-28147                                                                     
RUNAM87  TM    FLDFRM,FLDFRMNM                                                  
         BZ    RUNAMX                                                           
         AHI   RE,-1                                                            
         MVI   0(RE),C'0'                                                       
         B     RUNAMX                                                           
*                                                                               
RUNAM90  TM    FLDFRM,FLDFRMNM                                                  
         BZ    RUNAMX                                                           
RUNAM91  CLI   0(RE),C' '                                                       
         BNE   RUNAM92                                                          
         LA    RE,1(RE)                                                         
         B     RUNAM91                                                          
RUNAM92  AHI   RE,-1                                                            
         MVI   0(RE),C'0'                                                       
*SPEC-28147                                                                     
*                                                                               
RUNAMX   J     EXIT                                                             
*                                                                               
WRKAMT   DS    CL18                WORK AMOUNT FIELD                            
WRKCOM   DC    XL17'40404020206B2020206B2020206B202020'                         
WRKDEC   DC    XL17'4040402020206B2020206B2020214B2020'                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* COUNT                                                              *          
**********************************************************************          
         SPACE 1                                                                
RUNCNT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RUNCNTX                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
*                                                                               
         LA    RF,ACBFULC                                                       
         CLI   FLDNO,BK#RECCN      COUNT (RECORD)                               
         BE    RUNCNT10                                                         
         LA    RF,ACBFULC2                                                      
         CLI   FLDNO,BK#TRNCN      COUNT (TRNSACTION)                           
         BE    RUNCNT10                                                         
         LA    RF,ACBFULC3                                                      
         CLI   FLDNO,BK#VDRCN      COUNT (VOID)                                 
         BE    RUNCNT10                                                         
         CLI   FLDNO,BK#CHKCN      COUNT (ITEMS/CHECK) (ACBFULC3)               
         BE    RUNCNT10                                                         
         LA    RF,ACBFULC4                                                      
         CLI   FLDNO,BK#HDRCN      COUNT (HEADER RECORD)                        
         BE    RUNCNT10                                                         
         LA    RF,ACBBCNT                                                       
         CLI   FLDNO,BK#BCNT       COUNT (BATCH COUNT FOR 1 BATCH)              
         BE    RUNCNT10                                                         
         LA    RF,ACBBCNTA                                                      
         CLI   FLDNO,BK#BCNTA      COUNT (BATCH COUNT FOR ALL BATCHES)          
         BE    RUNCNT10                                                         
         LA    RF,ACBFULC5                                                      
         CLI   FLDNO,BK#CHKS       COUNT (CHECKS ONLY)                          
         BE    RUNCNT10                                                         
*DSFTK-135                                                                      
         LA    RF,ACBRECT                                                       
         CLI   FLDNO,BK#RECT       TOTAL RECORDS                                
         BE    RUNCNT10                                                         
*DSFTK-135                                                                      
         LA    RF,ACBRECTL         TOTAL RECORDS MINUS SELECT                   
         CLI   FLDNO,BK#RECTL      HEADER AND TRAILER                           
         BE    RUNCNT10                                                         
*SPEC-40627                                                                     
         LA    RF,ACBTRNCT         TRANSACTION COUNT (NOT DTL COUNT)            
         CLI   FLDNO,BK#TRNCT      HEADER AND TRAILER                           
         BE    RUNCNT10                                                         
*SPEC-40627                                                                     
         DC    H'0'                                                             
*                                                                               
RUNCNT10 ZAP   DUB,0(L'ACBFULC,RF)                                              
*                                                                               
         MVC   WORK(17),=X'4040404040402020202020202020202020'                  
         MVI   WORK+15,X'21'                                                    
         MVI   WORK,C'0'                                                        
         ED    WORK(17),DUB+2                                                   
         LHI   R1,17               DISPLACEMENT TO END OF WORK FLD              
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         SR    R1,RF                                                            
         LA    RF,WORK(R1)                                                      
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
RUNCNTX  J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* DATE (TRANSACTION/ACTIVITY/INVOICE/TODAY)                          *          
**********************************************************************          
         SPACE 1                                                                
RUNDTE   NTR1  BASE=*,LABEL=*                                                   
*        CLI   FRMRECN,ACBRHDR     MAKE SURE NOT HEADER                         
*        BNH   RUNDT40                                                          
*                                                                               
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RUNDTEX                                                          
*                                                                               
         TM    FLDSTAT,FVDEP       IS THIS VOID DEPENDENT?                      
         BNO   RUNDT10                                                          
         LA    R1,BNH              SET IT AS BRANCH ON NOT HIGH                 
         TM    FLDSTAT,FVOID       IS THIS A VOID VARIABLE?                     
         BNO   *+8                                                              
         LA    R1,BH               ELSE ITS BRANCH ON HIGH                      
         CP    ACBNET,=P'0'                                                     
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNDTEX                                                        
RUNDT10  LA    R0,1                DMCB PARM 1 (BYTE 0)                         
         CLI   FLDNO,BK#TODDT      TODAY'S DATE                                 
         BE    RUNDT40                                                          
         LA    RE,ACBTDTE                                                       
         CLI   FLDNO,BK#TRNDT      DATE (TRANSACTION)                           
         BE    RUNDT20                                                          
         LA    RE,ACBADTE                                                       
         CLI   FLDNO,BK#ACTDT      DATE (ACTIVITY)                              
         BE    RUNDT20                                                          
         LA    RE,ACBCDTE                                                       
         CLI   FLDNO,BK#CHKDT      DATE (CHECK)                                 
         BE    RUNDT20                                                          
         LA    RE,ACBIDTE                                                       
         CLI   FLDNO,BK#INVDT      DATE (INVOICE)                               
         BE    RUNDT20                                                          
         LA    RE,ACBPDTE                                                       
         CLI   FLDNO,BK#PERDT      DATE (PERIOD)                                
         BE    RUNDT20                                                          
         LA    RE,THREE                                                         
         MVC   THREE(2),ACBMOS                                                  
         MVI   THREE+2,X'01'                                                    
         CLI   FLDNO,BK#MOS        DATE (MONTH OF SERVICE)                      
         BE    RUNDT20                                                          
         DC    H'0'                                                             
*                                                                               
         USING OVRTABD,RF                                                       
RUNDT20  L     RF,=A(OVRTAB)                                                    
RUNDT30  CLI   0(RF),EOF                                                        
         BE    RUNDT50                                                          
         CLC   ACBALPHA,OVRALPHA                                                
         BE    *+12                                                             
         AHI   RF,OVRLNQ                                                        
         B     RUNDT30                                                          
*                                                                               
         CP    ACBNET,=P'0'                                                     
         BH    RUNDT50                                                          
         ICM   R1,3,OVRDSP                                                      
         LA    RE,0(R1,R2)                                                      
         B     RUNDT50                                                          
         DROP  RF                                                               
*                                                                               
RUNDT40  DS    0H                  USE TODAY'S DATE FOR HEADER                  
         LA    R0,5                DMCB PARM 1 (BYTE 0)                         
         SR    RE,RE               DMCB PARM 1 (BYTES 1-3)                      
*                                                                               
RUNDT50  DS    0H                                                               
*DSFTK-150                                                                      
         XC    HALF2,HALF2         MUST ALWAYS PERFORM THESE                    
         MVC   HALF2(1),FLDOVR     INSTRUCTIONS                                 
*                                                                               
         CLI   FLDOVR,X'FF'        NEED TO ADD DAYS                             
         BNE   RUNDT52                                                          
*                                                                               
         CLI   FLDSRCE+5,C'S'      SKIP ADDING DAYS ON SOON RUN                 
         BNE   *+12                                                             
         TM    RUNSTAT,RUNSN       IS THIS A SOON RUN?                          
         BO    RUNDT51                                                          
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 VDATCON,DMCB,((R0),(RE)),(0,WORK),0                              
*                                                                               
         MVC   FULL,FLDSRCE+1      NUMBER OF DAYS TO ADD                        
         L     R2,FULL                                                          
         GOTO1 VADDAY,DMCB,WORK,WORK+10,(R2)                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+10),(1,WORK+20),0                           
*                                                                               
         LA    R0,1                DMCB PARM 1 (BYTE 0)                         
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE(3),WORK+20   YMD PACKED                                   
         LA    RE,DOUBLE           NEW DATCON SOURCE YMD PACKED                 
RUNDT51  MVC   FLDOVR,FLDSRCE      RESET TO ORIGINAL DATE OVERRIDE              
*                                                                               
RUNDT52  DS    0H                                                               
*DSFTK-150                                                                      
*                                                                               
         MVC   BYTE,FLDTYPE                                                     
         NI    BYTE,X'37'          ANY SPECIAL OVERRIDE                         
         BZ    RUNDT55                                                          
         AHI   R0,X'30'            DMCB PARM 1 (BYTE 0) DATE MODIFIER           
         NI    BYTE,X'FF'-X'30'    TURN OFF X'30' DATE MODIFIER                 
RUNDT55  LLC   R2,BYTE             DMCB PARM 3 (BYTE 0)                         
         CHI   R2,5                VALID ENTRIES 1,2,3,4,5                      
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDFRM                                                        
         MVC   WORK,SPACES                                                      
         GOTO1 VDATCON,DMCB,((R0),(RE)),((RF),WORK),((R2),0)                    
         LR    RE,R3               CHECK DATE                                   
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK        DATE                                         
*                                                                               
         CLI   FLDOVR,0                                                         
         BE    RUNDTEX                                                          
         CLI   FLDTYPE,FDCPY       IS THIS COMPANY DEPENDENT?                   
         BE    RUNDTEX               DON'T ALLOW ANY OVERRIDES                  
         CLI   FLDOVR,1            MMDDYY FORMAT                                
         BNE   RUNDT60                                                          
         MVC   0(4,RE),WORK+2                                                   
         MVC   4(2,RE),WORK                                                     
         B     RUNDTEX                                                          
RUNDT60  CLI   FLDOVR,2            MMDDYYYY FORMAT                              
         BNE   RUNDT70                                                          
         MVC   0(4,RE),WORK+4                                                   
         MVC   4(4,RE),WORK                                                     
         B     RUNDTEX                                                          
RUNDT70  CLI   FLDOVR,4            DDMMYY FORMAT                                
         BNE   RUNDT80                                                          
         MVC   0(2,RE),WORK+4                                                   
         MVC   2(2,RE),WORK+2                                                   
         MVC   4(2,RE),WORK                                                     
         B     RUNDTEX                                                          
RUNDT80  CLI   FLDOVR,5            MM/DD/YY FORMAT                              
         BNE   RUNDT90                                                          
         MVC   0(2,RE),WORK+2                                                   
         MVI   2(RE),C'/'                                                       
         MVC   3(2,RE),WORK+4                                                   
         MVI   5(RE),C'/'                                                       
         MVC   6(2,RE),WORK                                                     
         B     RUNDTEX                                                          
RUNDT90  CLI   FLDOVR,6            JULIAN FORMAT                                
         BNE   RUNDT100                                                         
         EDIT  (P3,WORK+1),(5,(RE)),FLOAT=0                                     
         B     RUNDTEX                                                          
RUNDT100 CLI   FLDOVR,7            MM/DD/YYYY FORMAT                            
         BNE   RUNDT110                                                         
         MVC   0(2,RE),WORK+4                                                   
         MVI   2(RE),C'/'                                                       
         MVC   3(2,RE),WORK+6                                                   
         MVI   5(RE),C'/'                                                       
         MVC   6(4,RE),WORK                                                     
         B     RUNDTEX                                                          
*                                                                               
RUNDT110 CLI   FLDOVR,8            YYYY/MM/DD FORMAT                            
         BNE   RUNDT120                                                         
         MVC   0(4,RE),WORK                                                     
         MVI   4(RE),C'/'                                                       
         MVC   5(2,RE),WORK+5                                                   
         MVI   7(RE),C'/'                                                       
         MVC   8(2,RE),WORK+8                                                   
         B     RUNDTEX                                                          
*                                                                               
RUNDT120 CLI   FLDOVR,9            JULIAN FORMAT MAT                            
         BNE   RUNDTEX                                                          
         EDIT  (P3,WORK+1),(6,(RE)),FLOAT=0                                     
         B     RUNDTEX                                                          
*                                                                               
RUNDTEX  DS    0H                                                               
*DSFTK-150                                                                      
         MVC   FLDOVR,HALF2        RESTORE ORIGINAL VALUE                       
*DSFTK-150                                                                      
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* VARIABLE DATA                                                      *          
**********************************************************************          
         SPACE 1                                                                
RUNVAR   NTR1  BASE=*,LABEL=*                                                   
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RUNVARX                                                          
*                                                                               
         TM    FLDSTAT,FVDEP       IS THIS A VOID DEPENDENT VARIABLE?           
         BNO   RUNVA20                                                          
         LA    R1,BH                                                            
         TM    FLDSTAT2,FNETNPOS   IS IT ONLY A VOID IF NET IS NOT > 0          
         BO    RUNVA10                                                          
         LA    R1,BNE                                                           
         TM    FLDSTAT3,FNETZERO   IS IT ONLY A VOID IF NET=0                   
         BO    RUNVA10                                                          
         LA    R1,BNL                                                           
RUNVA10  CP    ACBNET,=P'0'                                                     
         EX    R1,*+8                                                           
         B     *+4                                                              
         BC    0,RUNVARX                                                        
RUNVA20  TM    FLDSTAT,FCTRY       IS THIS COUNTRY DEPENDENT?                   
         BNO   RUNVA40                                                          
         TM    FLDSTAT2,FCAN       ONLY FOR CANADA?                             
         BNO   RUNVA30                                                          
         TM    FLDSTAT3,FCADC      ONLY FOR CANADIAN CURRENCY?                  
         BNO   RUNVA25                                                          
         TM    ACBFLAG,ACBCADC     IS THIS CANADIAN CURRENCY?                   
         BNO   RUNVARX               NO - SKIP                                  
         B     RUNVA40                                                          
RUNVA25  TM    ACBFLAG,ACB820      DON'T DO THIS CODE FOR POSPAY                
         BO    RUNVA25A                                                         
         TM    ACBFLAG,ACBEFT      ACBCAN MAY NOT BE SET WHEN INVALID           
         BZ    RUNVA27             ADDRESS.                                     
         TM    ACBRSTAT,ACBINVML   IS ADDRESS INVALID?                          
         BO    RUNVA30                                                          
RUNVA25A LA    RF,ACBDCTRY         POINT TO DESTINATION COUNTRY                 
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RF,ACBCTRY          POINT TO SOURCE COUNTRY                      
         CLC   =C'CA',0(RF)                                                     
         BNE   RUNVARX                                                          
         B     RUNVA30                                                          
RUNVA27  TM    ACBFLAG,ACBCAN      IS THIS CANADIAN?                            
         BNO   RUNVARX               NO - SKIP                                  
*                                                                               
RUNVA30  DS    0H                  NO OTHER COUNTRIES CURRENTLY                 
*                                                                               
RUNVA40  LR    RE,R3               VARIABLE DATA                                
         LA    RF,FLDSRCE          SOURCE DATA                                  
         ICM   R1,1,FLD#CPY        IS THIS NTRY FOR SPECIFIC COMPANIES?         
         BZ    *+10                                                             
         MHI   R1,2                MULIPLY BY TWO FOR ALPHA CODE                
         AR    RF,R1               BUMP PASSED ALPHA CODES FOR SOURCE           
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
RUNVARX  J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* NAME (PAYEE/COMPANY/PAYEE CONTACT/BANK)                            *          
**********************************************************************          
         SPACE 1                                                                
RUNNAME  NTR1  BASE=*,LABEL=*                                                   
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RUNNAMEX                                                         
*                                                                               
* CHECK FOR OVERFLOW                                                            
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         ST    RE,FULL                                                          
*                                                                               
         LA    R1,L'ACBPAYEE-1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES     ALWAYS CLEAR FIELD TO MAX LEN                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         CHI   RF,L'ACBPAYEE                                                    
         BNH   *+8                                                              
         LHI   RF,L'ACBPAYEE                                                    
         STC   RF,BYTE                                                          
*                                                                               
         GOTOR CHKOVR,DMCB,(L'ACBPAYEE,ACBPAYEE),((RF),FULL)                    
         BNE   RUNNAMEX                                                         
*                                                                               
         L     RE,FULL                                                          
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
*                                                                               
         LA    RF,ACBPAYEE                                                      
         CLI   FLDNO,BK#PAYNM      NAME (PAYEE)                                 
         BE    RUNNAM10                                                         
         LA    RF,ACBCPYNM                                                      
         CLI   FLDNO,BK#CPYNM      NAME (COMPANY)                               
         BE    RUNNAM10                                                         
         LA    RF,ACBVCNME                                                      
         CLI   FLDNO,BK#PCTNM      NAME (PAYEE CONTACT NAME)                    
         BE    RUNNAM10                                                         
         LA    RF,ACBBNKNM                                                      
         CLI   FLDNO,BK#BNKNM      NAME (BANK NAME)                             
         BE    RUNNAM10                                                         
         LA    RF,ACBCLINM                                                      
         CLI   FLDNO,BK#CLINM      NAME (CLIENT NAME)                           
         BE    RUNNAM10                                                         
         LA    RF,ACBMEDNM                                                      
         CLI   FLDNO,BK#MEDNM      NAME (MEDIA NAME)                            
         BE    RUNNAM10                                                         
         LA    RF,ACBDCTRN         PAYEE COUNTRY NAME                           
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RF,ACBCTRN          PAYER COUNTRY NAME                           
         CLI   FLDNO,BK#CTRNM      NAME (COUNTRY NAME)                          
         BE    RUNNAM10                                                         
         LA    RF,ACBIDCRN         IDI DESTINATION COUNTRY NAME                 
         CLI   FLDNO,BK#ICTRN      IDI NAME (COUNTRY NAME)                      
         BE    RUNNAM10                                                         
         LA    RF,ACBPRDNM                                                      
         CLI   FLDNO,BK#PRDNM      NAME (PRUDUCT NAME)                          
         BE    RUNNAM10                                                         
         LA    RF,ACBPUBNM                                                      
         CLI   FLDNO,BK#PUBNM      NAME (PUB NAME)                              
         BE    RUNNAM10                                                         
         LA    RF,ACBIDIDN         IDI DESTINATION NAME                         
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RF,ACBORIGN         IDI ORIGIN NAME                              
         CLI   FLDNO,BK#IDINM      NAME (IDI NAME)                              
         BE    RUNNAM10                                                         
         LA    RF,ACBJOBN                                                       
         CLI   FLDNO,BK#JOBNM      NAME (JOB NAME)                              
         BE    RUNNAM10                                                         
*                                                                               
         LA    RF,ACBINTNM                                                      
         CLI   FLDNO,BK#IBKNM      INTL BANK NAME                               
         BE    RUNNAM10                                                         
         LA    R4,ACBIBCTN                                                      
         CLI   FLDNO,BK#IBCTN      INTL BANK COUNTRY NAME                       
         BE    RUNNAM10                                                         
*                                                                               
         DC    H'0'                                                             
*                                                                               
RUNNAM10 AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
RUNNAMEX J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTING NUMBER                                                     *          
**********************************************************************          
         SPACE 1                                                                
RUNRTE#  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBDRTE#                                                      
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBSRTE#                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBSRTE#                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBSRTE#                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNRTE#X J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SWIFT CODE                                                         *          
**********************************************************************          
         SPACE 1                                                                
RUNSWFT  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBSWIFT                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBSWIFT                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBSWIFT                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNSWFTX J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CUSTOMER NUMBER                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING CUSNOTBD,R1                                                      
RUNCUS#  NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(CUSNOTAB)                                                  
RUNCU10  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    RUNCU30                                                          
         CLC   ACBALPHA,CUSALPHA   MATCH ON AGENCY ID                           
         BNE   RUNCU20                                                          
         CLI   2(R1),X'FF'                                                      
         BE    RUNCU30                                                          
         CLC   ACBCSHAC,CUSACCT    MATCH ON SC ACCOUNT                          
         BE    RUNCU30                                                          
RUNCU20  AHI   R1,CUSTBLNQ                                                      
         B     RUNCU10                                                          
*                                                                               
RUNCU30  MVC   WORK(10),CUSNO      CUSTOMER NUMBER                              
         DROP  R1                                                               
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'WORK                                                        
         BNH   *+8                                                              
         LHI   R1,L'WORK                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
*                                                                               
RUNCUS#X J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ESTIMATE NUMBER                                                    *          
**********************************************************************          
         SPACE 1                                                                
RUNEST#  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         EDIT  ACBEST#,(5,WORK),FILL=0                                          
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,5                                                             
         BNH   *+8                                                              
         LA    R1,5                                                             
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* ADDRESS LINES                                                      *          
**********************************************************************          
         SPACE 1                                                                
RUNADDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,ACBDADR          POINT TO DESTINATION ADDRESS                 
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBADDR          POINT TO SOURCE ADDRESS                      
         LA    RE,L'ACBDADR        SET LENGTH                                   
         CLI   FLDNO,BK#ADDR       ADDRESS LINES                                
         BE    RUNADD05                                                         
         LA    R4,ACBIDADR         POINT TO IDI DESTINATION ADDRESS             
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBIADR          POINT TO IDI ORIGIN ADDRESS                  
         LA    RE,L'ACBIADR        SET LENGTH                                   
         CLI   FLDNO,BK#IADDR      IDI ADDRESS LINES                            
         BE    RUNADD05                                                         
*                                                                               
         LA    R4,ACBINTAD         INTL BANK ADDRESS                            
         LA    RE,L'ACBINTAD       SET LENGTH                                   
         CLI   FLDNO,BK#IBKAD      INTL BANK ADDRESS LINES                      
         BE    RUNADD05                                                         
*                                                                               
RUNADD05 GOTOR CHKDEP,DMCB,((RE),(R4))      CHECK FOR DEPENDENCIES              
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
*                                                                               
         TM    FLDSTAT3,FINVADR    ARE WE MARKING INVALID ADDRESSES             
         BNO   RUNADD10                                                         
*        TM    ACBRSTAT,ACBINVML   IS ADDRESS INVALID?                          
*        BNO   RUNADD10            ** TOOK THIS OUT B/C IT WASN'T               
*        LA    RF,INVADDR             BEING USED.                               
*        B     RUNADD20                                                         
*                                                                               
RUNADD10 LA    RF,ACBDADR                                                       
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RF,ACBADDR                                                       
         LA    R0,L'ACBADDR                                                     
         CLI   FLDNO,BK#ADDR       ADDRESS LINES                                
         BE    RUNADD15                                                         
         LA    RF,ACBIDADR         POINT TO IDI DESTINATION ADDRESS             
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    RF,ACBIADR          POINT TO IDI ORIGIN ADDRESS                  
         LA    R0,L'ACBIADR        SET LENGTH                                   
         CLI   FLDNO,BK#IADDR      IDI ADDRESS LINES                            
         BE    RUNADD15                                                         
*                                                                               
         LA    RF,ACBINTAD         INTL BANK ADDRESS                            
         LA    R0,L'ACBINTAD       SET LENGTH                                   
         CLI   FLDNO,BK#IBKAD      INTL BANK ADDRESS LINES                      
         BE    RUNADD15                                                         
*                                                                               
RUNADD15 LR    R6,R0               SAVE FIELD LENGTH FOR LATER COMPARE          
         SR    R1,R1                                                            
         ICM   R1,1,FLDLIN#                                                     
         BZ    RUNADD20                                                         
         AHI   R1,-1               ADJUST LINE NUMBER FOR DISPLACEMENT          
         BNP   RUNADD20                                                         
         STH   R1,HALF                                                          
         MH    R0,HALF                                                          
         AR    RF,R0                                                            
*                                                                               
RUNADD20 SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CR    R1,R6               COMPARE TO INDIVIDUAL FIELD                  
         BNH   *+6                                                              
         LR    R1,R6                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       ADDRESS LINE 1-5                             
         J     EXIT                                                             
*                                                                               
INVADDR  DC    CL30'*INVALID ADDRESS*'                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PAYEE DDS ACCOUNT CODE                                             *          
**********************************************************************          
         SPACE 1                                                                
RUNPACC  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBVACC                                                     
         BNH   RNPAC10                                                          
         CHI   R1,L'ACBVULA                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBVULA                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBVULA                                                  
         J     EXIT                                                             
*                                                                               
RNPAC10  AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBVACC                                                  
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PAYEE CITY                                                         *          
**********************************************************************          
         SPACE 1                                                                
RUNPCTY  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBDCTY          POINT TO DESTINATION CITY                    
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBCTY           POINT TO SOURCE CITY                         
         CLI   FLDNO,BK#CTY        REGULAR CITY                                 
         BE    *+8                                                              
         LA    R4,ACBIDCTY         POINT TO IDI DESTINATION CITY                
*                                                                               
         CLI   FLDNO,BK#IBCTY                                                   
         BNE   *+8                                                              
         LA    R4,ACBIBCTY         POINT TO INTL BANK CITY                      
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBDCTY,(R4))      CHECK FOR DEPENDENCIES         
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBCTY                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBCTY                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PAYEE STATE                                                        *          
**********************************************************************          
         SPACE 1                                                                
RUNPST   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBDST           POINT TO DESTINATION STATE                   
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBST            POINT TO SOURCE STATE                        
         CLI   FLDNO,BK#ST         REGULAR STATE                                
         BE    *+8                                                              
         LA    R4,ACBIDST          POINT TO IDI DESTINATION STATE               
*                                                                               
         CLI   FLDNO,BK#IBST                                                    
         BNE   *+8                                                              
         LA    R4,ACBIBST          POINT TO INTL BANK STATE                     
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBDST,(R4))       CHECK FOR DEPENDENCIES         
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBST                                                       
         BNH   *+8                                                              
         LHI   R1,L'ACBST                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PAYEE ZIP                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNPZIP  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBDZIP          POINT TO DESTINATION ZIP                     
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBZIP           POINT TO SOURCE ZIP                          
         CLI   FLDNO,BK#ZIP        REGULAR ZIP                                  
         BE    *+8                                                              
         LA    R4,ACBIDZIP         POINT TO IDI DESTINATION ZIP                 
*                                                                               
         CLI   FLDNO,BK#IBZIP                                                   
         BNE   *+8                                                              
         LA    R4,ACBIBZIP         POINT TO INTL BANK ZIP CODE                  
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBDZIP,(R4))      CHECK FOR DEPENDENCIES         
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBZIP                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBZIP                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PAYEE FAX NUMBER                                                   *          
**********************************************************************          
         SPACE 1                                                                
RUNPFAX  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBVFAX#                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBVFAX#                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBVFAX#                                                 
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PAYEE EMAIL                                                        *          
**********************************************************************          
         SPACE 1                                                                
RUNPEML  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBVEML                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBVEML                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBVEML                                                  
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* INVOICE NUMBER                                                     *          
**********************************************************************          
         SPACE 1                                                                
RUNINV#  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBINV#                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBINV#                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBINV#                                                  
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* TRUE REFERENCE NUMBER                                              *          
**********************************************************************          
         SPACE 1                                                                
RUNTREF  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CLC   ACBTREF,SPACES       IF NO TRUE REFERENCE USE INV#               
         BNH   RUNTRF10                                                         
         CHI   R1,L'ACBTREF                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBTREF                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBTREF                                                  
         J     EXIT                                                             
*                                                                               
RUNTRF10 CHI   R1,L'ACBINV#                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBINV#                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBINV#                                                  
         J     EXIT                                                             
**********************************************************************          
* CHECK DIGIT                                                        *          
* LAST DIGIT OF THE ROUTING NUMBER                                              
**********************************************************************          
         SPACE 1                                                                
RUNCDIG  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBDRTE#+8                                               
         J     EXIT                                                             
**********************************************************************          
* ENTRY HASH                                                         *          
* THE SUM OF ALL THE DESINATION ROUTING #'S                                     
**********************************************************************          
         SPACE 1                                                                
RUNENT#  NTR1  BASE=*,LABEL=*                                                   
         EDIT  (P6,ACBENTH),(10,WORK),FILL=0                                    
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         J     EXIT                                                             
**********************************************************************          
* BLOCK FILLER                                                       *          
* TAKE THE FILLER CHARACTER DEFINED IN THE FORMAT AND WRITE IT OUT              
* FOR THE LENGTH SPECIFIED EG. 999999999'S                                      
* MAY BE CALLED MULTIPLE TIMES TO PUT OUT RECORD TO FILL UP A BLOCK             
* FOR THE GHG94 FORMATS THE BLOCK COUNT IS 10 SO NEED TO HAVE 10                
* RECORDS IN EACH BLOCK                                                         
**********************************************************************          
         SPACE 1                                                                
RUNBFLL  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         LA    RF,FLDSRCE          FILL CHARACTER                               
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         MVC   0(1,RE),0(RF)       MOVE IN FILLER CHAR TO 1ST POSITION          
         SR    R1,R1                                                            
         IC    R1,FLDLEN           TAKE THE LENGTH AND SUBTRACT 2               
         AHI   R1,-2               ONE FOR EXECUTED MOVE AND ONE FOR            
         EX    R1,*+8              1ST POSITION WHICH IS ALREADY DONE           
         B     *+10                                                             
         MVC   1(0,RE),0(RE)                                                    
         J     EXIT                                                             
**********************************************************************          
* FILE ID MODIFIER                                                   *          
* USE THE LAST TWO DIGITS OF THE GENERATION NUMBER OF THE DATASET TO            
* DETERMINE WHICH ONE BYTE FILE ID MODIFIER TO USE                              
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNIDM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,=A(FILIDTB)                                                   
RNIDM05  LA    R1,FILGEN#                                                       
         LA    R0,5                 EACH ENTRY HAS 5 TWO DIGIT GEN #'S          
         CLI   0(RF),X'FF'                                                      
         BE    RNIDMX                                                           
RNIDM10  CLC   0(2,R1),ACBFLIDM+2   COMPARE THE LAST TWO DIGITS                 
         BE    RNIDM15                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,RNIDM10                                                       
         LA    RF,FILIDLNQ(RF)                                                  
         B     RNIDM05                                                          
*                                                                               
RNIDM15  LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FILMOD                                                   
*                                                                               
RNIDMX   J     EXIT                                                             
         DROP  RF                                                               
**********************************************************************          
* BLOCK COUNT                                                        *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNBLKC  NTR1  BASE=*,LABEL=*                                                   
         EDIT  (P4,ACBBLCNT),(6,WORK),FILL=0                                    
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
         J     EXIT                                                             
**********************************************************************          
* GENERATION NUMBER                                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNGEN#  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBFLIDM                                                 
         J     EXIT                                                             
**********************************************************************          
* IDI USER ID CODE                                                   *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNIDCD  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBIDICD                                                 
         J     EXIT                                                             
*                                                                               
**********************************************************************          
* REP/STATION/PUB FOR VENDOR CODE TO CONNECT                         *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNREPCD NTR1  BASE=*,LABEL=*                                                   
*DSFTK-82,DSFTK-131                                                             
         CLI   FLDOVR,0               NO OVERRIDE                               
         BNH   RREP20                                                           
         CLI   FLDOVR,1               DEFAULT VALUE OVERRIDE                    
         BNE   RREP20                 (DEFINED IN FORMAT WITH #VAR)             
         CLC   ACBREPVN,SPACES        IF NO VALUE PRESENT USE DEFAULT           
         BNH   RREPXIT                DEFINED IN ACFRMTAB                       
*DSFTK-82,DSFTK-131                                                             
RREP20   LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBREPVN                                                 
RREPXIT  J     EXIT                                                             
**********************************************************************          
* REP/STATION/PUB FOR VENDOR CODE SPEC-9755                          *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNREPV2 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RR2PXIT                                                          
*                                                                               
         CLI   FLDOVR,0               NO OVERRIDE                               
         BNH   RR2P20                                                           
         CLI   FLDOVR,1               DEFAULT VALUE OVERRIDE                    
         BNE   RR2P20                 (DEFINED IN FORMAT WITH #VAR)             
         CLC   ACBREPVN,SPACES        IF NO VALUE PRESENT USE DEFAULT           
         BNH   RR2PXIT                DEFINED IN ACFRMTAB                       
*                                                                               
RR2P20   LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBREPV2                                                 
RR2PXIT  J     EXIT                                                             
**********************************************************************          
* REP/STATION/PUB FOR VENDOR CODE SPEC-9755                          *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNREPV3 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RR3PXIT                                                          
*                                                                               
         CLI   FLDOVR,0               NO OVERRIDE                               
         BNH   RR3P20                                                           
         CLI   FLDOVR,1               DEFAULT VALUE OVERRIDE                    
         BNE   RR3P20                 (DEFINED IN FORMAT WITH #VAR)             
         CLC   ACBREPVN,SPACES        IF NO VALUE PRESENT USE DEFAULT           
         BNH   RR3PXIT                DEFINED IN ACFRMTAB                       
*                                                                               
RR3P20   LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBREPV3                                                 
RR3PXIT  J     EXIT                                                             
**********************************************************************          
* CONTRA ACCOUNT                                                     *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNCNTR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,ACBCNTR                                                       
         GOTOR CHKDEP,DMCB,(L'ACBCNTR,(R4))     CHECK FOR DEPENDENCIES          
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBCNTR                                                  
         J     EXIT                                                             
*DSFTK-150                                                                      
**********************************************************************          
* PCARD PAYMENT TYPE                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNPCRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBPCTY                                                  
         J     EXIT                                                             
*DSFTK-150                                                                      
**********************************************************************          
* TRANSACTION OFFICE                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING FILIDTBD,RF                                                      
RUNTROF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBOFFCT                                                 
         J     EXIT                                                             
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* INVOICE DESCRIPTION (NARRATIVE)                                    *          
**********************************************************************          
         SPACE 1                                                                
RUNINVD  NTR1  BASE=*,LABEL=*                                                   
         GOTOR CHKDEP,DMCB,(L'ACBINVDS,ACBINVDS) CHECK FOR DEPENDENCIES         
         JNE   EXIT                                                             
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBINVDS                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBINVDS                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBINVDS                                                 
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CHECK PAYMENT TYPE                                                 *          
**********************************************************************          
         SPACE 1                                                                
RUNPAYT  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBPTYP                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBPTYP                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBPTYP                                                  
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* ENTITY ID                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNENTID NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBENTID                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBENTID                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBENTID                                                 
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* SEQUENCE NUMBER                                                    *          
**********************************************************************          
         SPACE 1                                                                
RUNSEQ#  NTR1  BASE=*,LABEL=*                                                   
         CLI   FLDNO,BK#TSEQ#      SEQUENCE # (TRANSMISSION)                    
         BNE   RUNSQ10                                                          
         TM    ACBFLAG,ACBSKP      ARE WE SKIPPING THIS ENTRY?                  
         BO    RUNSQ10             THEN DON'T INCRMEMENT                        
         PACK  DUB,ACBTSEQ#                                                     
         AP    DUB,=P'1'                                                        
         EDIT  (P4,DUB+4),ACBTSEQ#,FILL=0                                       
*                                                                               
RUNSQ10  LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         SR    R0,R0                                                            
         CHI   R1,L'ACBFSEQ#                                                    
         BNH   *+12                                                             
         LHI   R1,L'ACBFSEQ#                                                    
         B     RUNSQ20                                                          
         LA    R0,L'ACBFSEQ#                                                    
         SR    R0,R1               GET DISPLACEMENT                             
*                                                                               
RUNSQ20  LA    RF,ACBFSEQ#                                                      
         CLI   FLDNO,BK#FSEQ#      SEQUENCE # (FILE)                            
         BE    RUNSQ30                                                          
         LA    RF,ACBBSEQ#                                                      
         CLI   FLDNO,BK#BSEQ#      SEQUENCE # (BATCH)                           
         BE    RUNSQ30                                                          
         LA    RF,ACBTSEQ#                                                      
         CLI   FLDNO,BK#TSEQ#      SEQUENCE # (TRANSMISSION)                    
         BE    RUNSQ30                                                          
         LA    RF,ACBRSEQ#                                                      
         CLI   FLDNO,BK#RSEQ#      SEQUENCE # (RANDOM)                          
         BE    RUNSQ30                                                          
         DC    H'0'                                                             
*                                                                               
RUNSQ30  AR    RF,R0               ADD DISPLACEMENT                             
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* STYLE NUMBER                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNSTY#  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBSTYL#                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBSTYL#                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBSTYL#                                                 
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* MAIL HANDLING CODE                                                 *          
**********************************************************************          
         SPACE 1                                                                
RUNMAIL  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBMLCDE                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBMLCDE                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACBMLCDE                                                 
         J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CURRENT TIME                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNCTIME NTR1  BASE=*,LABEL=*                                                   
         OC    ACBTIMEP,ACBTIMEP                                                
         BZ    RUNC10                                                           
         CLC   ACBTIMEP,SPACES                                                  
         BNE   RUNC20                                                           
RUNC10   TIME  DEC                                                              
         ST    R0,DUB                                                           
         MVC   ACBTIMEP,DUB                                                     
                                                                                
*        TIME  DEC,TIMEOUT,LINKAGE=SYSTEM                                       
RUNC20   UNPK  WORK(15),ACBTIMEP                                                
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,15                                                            
         BNH   *+8                                                              
         LHI   R1,15                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),WORK                                                     
*                                                                               
         CLI   FLDOVR,0                                                         
         BE    RUNCTMX                                                          
         CLI   FLDOVR,1            HH:MM:SS                                     
         BNE   RUNCTMX                                                          
         MVC   0(2,RE),WORK                                                     
         MVI   2(RE),C':'                                                       
         MVC   3(2,RE),WORK+2                                                   
         MVI   5(RE),C':'                                                       
         MVC   6(2,RE),WORK+4                                                   
*                                                                               
RUNCTMX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* COUNTRY CODE                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNCTRY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,ACBDCTRY         POINT TO DESTINATION COUNTRY                 
         TM    FLDSTAT,FDEST       IS THIS FOR THE DESTINATION?                 
         BO    *+8                                                              
         LA    R4,ACBCTRY          POINT TO SOURCE COUNTRY                      
         CLI   FLDNO,BK#CTRY       REGULAR COUNTRY                              
         BE    *+8                                                              
         LA    R4,ACBIDCRY         POINT TO IDI COUNTRY                         
*                                                                               
         CLI   FLDNO,BK#IBCTR                                                   
         BNE   *+8                                                              
         LA    R4,ACBIBCTR         POINT TO INTL BANK COUNTRY                   
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBCTRY                                                     
         BNH   *+8                                                              
         LHI   R1,L'ACBCTRY                                                     
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
         XIT1                                                                   
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* DATA (GENERIC INFO)                                                *          
**********************************************************************          
         SPACE 1                                                                
RUNDATA  NTR1  BASE=*,LABEL=*                                                   
*SPEC-28147                                                                     
         CLI   FLDNO,BK#BLK2       DATA (GENERIC BLOCK DATA)                    
         BNE   RUNDAT05                                                         
         LA    R4,ACBBLK2                                                       
         GOTOR CHKDEP,DMCB,(L'ACBBLK2,(R4))                                     
         BNE   RUNDATX                                                          
         B     RUNDAT08                                                         
*SPEC-28147                                                                     
RUNDAT05 GOTOR CHKDEP,0            CHECK FOR DEPENDENCIES                       
         BNE   RUNDATX                                                          
*                                                                               
RUNDAT08 LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
*                                                                               
         LA    RF,ACBPERDT                                                      
         LA    R1,L'ACBPERDT                                                    
         CLI   FLDNO,BK#PERD       DATA (PERIOD DATE)                           
         BE    RUNDAT10                                                         
         LA    RF,ACBPUBIN                                                      
         LA    R1,L'ACBPUBIN                                                    
         CLI   FLDNO,BK#PUBIN      DATA (PUB INDICATOR)                         
         BE    RUNDAT10                                                         
         LA    RF,ACBNMEIN                                                      
         LA    R1,L'ACBNMEIN                                                    
         CLI   FLDNO,BK#NMEIN      DATA (NAME INDICATOR)                        
         BE    RUNDAT10                                                         
         LA    RF,ACBREQDT                                                      
         LA    R1,L'ACBREQDT                                                    
         CLI   FLDNO,BK#REQDT      DATA (REQUEST DATA FIELD 1)                  
         BE    RUNDAT10                                                         
         LA    RF,ACBREQD2                                                      
         LA    R1,L'ACBREQD2                                                    
         CLI   FLDNO,BK#REQD2      DATA (REQUEST DATA FIELD 2)                  
         BE    RUNDAT10                                                         
         LA    RF,ACBBLK1                                                       
         LA    R1,L'ACBBLK1                                                     
         CLI   FLDNO,BK#BLK1       DATA (GENERIC BLOCK DATA)                    
         BE    RUNDAT10                                                         
         LA    RF,ACBBLK2                                                       
         LA    R1,L'ACBBLK2                                                     
         CLI   FLDNO,BK#BLK2       DATA (GENERIC BLOCK DATA)                    
         BE    RUNDAT10                                                         
         LA    RF,ACBLOGO                                                       
         LA    R1,L'ACBLOGO                                                     
         CLI   FLDNO,BK#LOGO       DATA (LOGO CODE)                             
         BE    RUNDAT10                                                         
         LA    RF,ACBRADC                                                       
         LA    R1,L'ACBRADC                                                     
         CLI   FLDNO,BK#RADC       DATA (RETURN ADDR CODE)                      
         BE    RUNDAT10                                                         
         LA    RF,ACBSIG                                                        
         LA    R1,L'ACBSIG                                                      
         CLI   FLDNO,BK#SIG        DATA (SIGNATURE CODE)                        
         BE    RUNDAT10                                                         
         DC    H'0'                                                             
*                                                                               
RUNDAT10 AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
RUNDATX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* VARIABLE RECORD LENGTH                                             *          
**********************************************************************          
         SPACE 1                                                                
RUNRLEN  NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACBOUTR                                                       
         LH    R1,ACBOUTRL                                                      
         AHI   R1,-1                                                            
         AR    RE,R1                                                            
RUNL10   CR    R3,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHAR                   
         BH    RUNL20                                                           
         BCT   RE,RUNL10                                                        
         DC    H'0'                                                             
*                                                                               
RUNL20   SR    RE,R3                                                            
         AHI   RE,1                                                             
         STH   RE,HALF                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'HALF                                                        
         BNH   *+8                                                              
         LHI   R1,L'HALF                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),HALF                                                     
         XIT1                                                                   
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* TRUE VENDOR                                                        *          
**********************************************************************          
         SPACE 1                                                                
RUNTVEN  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBTVENC                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBTVENC                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBTVENC                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNTVENX XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* OVERFLOW FIELD                                                     *          
**********************************************************************          
         SPACE 1                                                                
RUNOVRF  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBOVRFL         POINT TO OVERFLOW FIELD                      
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBOVRFL,(R4))    CHECK FOR DEPENDENCIES          
         JNE   RUNOVX                                                           
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES                                                  
         CHI   R1,L'ACBOVRFL                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBOVRFL-1                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
RUNOVX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CLIENT CODE                                                        *          
**********************************************************************          
         SPACE 1                                                                
RUNCLI   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBCLI                                                        
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBCLI                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBCLI                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNCLIX  XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* PRODUCT CODE                                                       *          
**********************************************************************          
         SPACE 1                                                                
RUNPROD  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBPRO                                                        
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBPRO,(R4))      CHECK FOR DEPENDENCIES          
         BNE   RUNPROX                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBPRO                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBPRO                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNPROX  XIT1                                                                   
*                                                                               
**********************************************************************          
* JOB CODE                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNJOB   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBJOB                                                        
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBJOB,(R4))      CHECK FOR DEPENDENCIES          
         BNE   RUNJOBX                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBJOB                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBJOB                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNJOBX  XIT1                                                                   
*                                                                               
**********************************************************************          
* REMITTANCE DELIVERY                                                *          
**********************************************************************          
         SPACE 1                                                                
RUNRDV   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBRMTDV                                                      
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBRMTDV,(R4))    CHECK FOR DEPENDENCIES          
         BNE   RUNRDVX                                                          
*                                                                               
         CLI   ACBRMTDV,C'F'         FAX?                                       
         BNE   *+12                                                             
         BRAS  RE,RUNPFAX           GO ADD FAX NUMBER                           
         B     RUNRDVX                                                          
         BRAS  RE,RUNPEML           MUST BE EMAIL                               
*                                                                               
RUNRDVX  XIT1                                                                   
*                                                                               
**********************************************************************          
* VENDOR PHONE NUMBER                                                *          
**********************************************************************          
         SPACE 1                                                                
RUNPHN   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBPHN                                                        
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBPHN,(R4))      CHECK FOR DEPENDENCIES          
         BNE   RUNPHNX                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBPHN                                                      
         BNH   *+8                                                              
         LHI   R1,L'ACBPHN                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNPHNX  XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
*SPEC-40627                                                                     
**********************************************************************          
* CHECK DELIVER FLAG  SPEC-40627                                     *          
**********************************************************************          
         SPACE 1                                                                
RUNCKDLV NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBCKDLV                                                      
*                                                                               
         GOTOR CHKDEP,DMCB,(L'ACBCKDLV,(R4))    CHECK FOR DEPENDENCIES          
         BNE   RUNCDLX                                                          
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBCKDLV                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBCKDLV                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNCDLX  XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CITY STATE ZIP SQUASHED WITH SEPARATING COMMAS SPEC-40627          *          
**********************************************************************          
         SPACE 1                                                                
RUNCSZ   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBDCTY                                                       
*        GOTOR CHKDEP,DMCB,(L'ACBCKDLV,(R4))    CHECK FOR DEPENDENCIES          
*        BNE   RUNCDLX                                                          
*                                                                               
         MVC   WORK,SPACES                 CLEAR WORK AREA                      
         MVC   WORK(L'ACBDCTY),ACBDCTY     MOVE IN CITY                         
                                                                                
         LA    R4,L'ACBDCTY                FIND LAST CHAR IN CITY               
         LA    R1,WORK                                                          
         LA    R1,L'ACBDCTY(R1)                                                 
RUNCSZ2  CLI   0(R1),C' '                                                       
         BH    RUNCSZ5                                                          
         BCTR  R1,0                                                             
         BCT   R4,RUNCSZ2                                                       
                                                                                
RUNCSZ5  LA    R1,1(R1)                    MOVE IN COMMA AFTER CITY             
         MVI   0(R1),C','                                                       
         MVC   WORK+L'ACBDCTY+2(L'ACBDST),ACBDST  MOVE IN STATE                 
         MVC   WORK+L'ACBDCTY+2+L'ACBDST+1(L'ACBDZIP),ACBDZIP                   
         CLC   ACBDZIP+6(4),SPACES         MOVE IN ZIP                          
         BE    RUNCSZ8                                                          
         MVI   WORK+L'ACBDCTY+2+L'ACBDST+6,C'-'  RESTORE - IN 10 DIGIT          
         MVC   WORK+L'ACBDCTY+2+L'ACBDST+7(4),ACBDZIP+5                         
*                                                                               
RUNCSZ8  GOTO1 ADSQUASH,DMCB,WORK,64       REMOVE EXCESS SPACES                 
*                                                                               
         LA    R4,WORK                                                          
         LR    RE,R3                       CALCULATE LENGTH                     
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
                                                                                
         CHI   R1,L'ACBDCTY+L'ACBDST+L'ACBDZIP                                  
         BNH   *+8                                                              
         LHI   R1,L'ACBDCTY+L'ACBDST+L'ACBDZIP                                  
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)          CITY, ST ZIP TO FILE LINE                 
*                                                                               
RUNCSZX  XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
*SPEC-40627                                                                     
*MN SPEC-46328                                                                  
**********************************************************************          
* SC FREEFORM TEXT    SPEC-46328                                     *          
**********************************************************************          
         SPACE 1                                                                
RUNSCFFT NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ACBSCFF1                                                      
         CLI   FLDNO,BK#SCFF1      SC FREEFORM TEXT FIELD 1                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF2                                                      
         CLI   FLDNO,BK#SCFF2      SC FREEFORM TEXT FIELD 2                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF3                                                      
         CLI   FLDNO,BK#SCFF3      SC FREEFORM TEXT FIELD 3                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF4                                                      
         CLI   FLDNO,BK#SCFF4      SC FREEFORM TEXT FIELD 4                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF5                                                      
         CLI   FLDNO,BK#SCFF5      SC FREEFORM TEXT FIELD 5                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF6                                                      
         CLI   FLDNO,BK#SCFF6      SC FREEFORM TEXT FIELD 6                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF7                                                      
         CLI   FLDNO,BK#SCFF7      SC FREEFORM TEXT FIELD 7                     
         BE    RUNSCF10                                                         
         LA    R4,ACBSCFF8                                                      
         CLI   FLDNO,BK#SCFF8      SC FREEFORM TEXT FIELD 8                     
         BE    RUNSCF10                                                         
*                                                                               
RUNSCF10 GOTOR CHKDEP,DMCB,(L'ACBSCFF1,(R4))    CHECK FOR DEPENDENCIES          
         BNE   RUNSCFFX                                                         
*                                                                               
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         CHI   R1,L'ACBSCFF1                                                    
         BNH   *+8                                                              
         LHI   R1,L'ACBSCFF1                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)                                                    
*                                                                               
RUNSCFFX XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
*MN SPEC-46328                                                                  
**********************************************************************          
* CHECK DEPENDENCIES FOR FIELD DEPENDENT FIELDS                      *          
*       P1 - FIELD (FOR FIELDS THAT ARE SELF DEPENDENT) OR 0         *          
**********************************************************************          
         SPACE 1                                                                
CHKDEP   NTR1  BASE=*,LABEL=*                                                   
         TM    FLDSTAT2,FFLDDEP    IS THIS A FIELD DEPENDENT VARIABLE?          
         JNO   CHKDXYES                                                         
*                                                                               
* SELF DEPENDENT?                                                               
*                                                                               
         CLI   FLDTYPE,FDITSLF     IS THE FIELD DEPENDENT ON ITSELF?            
         JNE   CHKD10                                                           
         MVC   CDPARMS(CDPRMLNQ),0(R1)                                          
         SR    R1,R1                                                            
         ICM   R1,1,CDPFLDL        GET FIELD LENGTH                             
         JZ    CHKDXNO                                                          
         SR    RF,RF                                                            
         ICM   RF,7,CDPFLD         FIELD                                        
         JZ    CHKDXNO                                                          
         CLI   FLDNO,BK#ADDR       IS THIS AN ADDRESS?                          
         BNE   CHKD05                                                           
         SR    RE,RE                                                            
         ICM   RE,1,FLDLIN#        ANY ADDR LINE # INDICATED?                   
         BZ    CHKD05                                                           
         AHI   RE,-1               ADJUST LINE NUMBER FOR DISPLACEMENT          
         BNP   CHKD05                                                           
         STH   RE,HALF                                                          
         LR    R0,R1               COPY LENGTH TO R0                            
         MH    R0,HALF                                                          
         AR    RF,R0                                                            
*                                                                               
CHKD05   AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,RF),XSPACES                                                  
         JNH   CHKDXNO                                                          
         CLI   FLDNO,BK#ADDR       IF THIS IS AN ADDRESS SET SKIP               
         BNE   CHKDXYES            OVERRIDE BIT SINCE EITHER ADDR LINE          
         OI    FLAG,SKPOVRD        3 OR 4 NEED TO EXIST TO PUT OUT THE          
         J     CHKDXYES            RECORD.                                      
*                                                                               
* COMPANY DEPENDENT?                                                            
*                                                                               
CHKD10   CLI   FLDTYPE,FDCPY       IS THE FIELD DEPENDENT ON A COMPANY?         
         JNE   CHKD20                                                           
         LA    RF,FLDSRCE          FIRST COMPANY                                
         SR    R1,R1                                                            
         ICM   R1,1,FLD#CPY        NUMBER OF COMPANIES                          
         JZ    CHKDXNO                                                          
CHKD15   CLC   ACBALPHA,0(RF)      MATCH ON COMPANY                             
         JE    CHKDXYES                                                         
         LA    RF,2(RF)                                                         
         JCT   R1,CHKD15                                                        
         J     CHKDXNO                                                          
*                                                                               
* LEDGER DEPENDENT?                                                             
*                                                                               
CHKD20   CLI   FLDTYPE,FDLDGR      IS THIS LEDGER DEPENDENT?                    
         JNE   CHKD30                                                           
         LA    R1,ACBLDG           ASSUME ITS THE ACCOUNT LEDGER                
         CLC   QPROG,=C'55'        FOR CHECKS IT'S THE ACCOUNT LEDGER           
         JE    CHKD22                                                           
         LA    R1,ACBCUL+1         CHECK CONTRA LEDGER                          
         CLC   QPROG,=C'57'        FOR POSPAY IT'S THE CONTRA LEDGER            
         JE    CHKD22                                                           
*                                                                               
CHKD22   CLC   0(1,R1),FLDSRCE     MATCH ON SOURCE                              
         JE    CHKDXYES                                                         
         J     CHKDXNO                                                          
*                                                                               
*                                                                               
CHKD30   DS    0H                                                               
*                                                                               
* OTHER FIELD DEPENDENT?   (THIS ALWAYS STAY LAST)                              
*                                                                               
         USING TYPTABD,RF                                                       
CHKD99   L     RF,=A(FLDTAB)                                                    
CHKD99A  CLI   0(RF),EOF                                                        
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   FLDTYPE,TYP#        MATCH ON DATE TYPE                           
         JE    *+12                                                             
         AHI   RF,TYPLNQ                                                        
         J     CHKD99A                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TYPLN                                                         
         AHI   R1,-1                                                            
         SR    RE,RE                                                            
         ICM   RE,3,TYPDSP                                                      
         LA    R4,0(RE,R2)         ADD DISP TO BASE                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R4),XSPACES                                                  
         JH    CHKDXYES                                                         
*                                                                               
CHKDXNO  TM    FRMSTAT2,FRMFDEP    IS THIS RECORD DEPENDENT ON THIS FLD         
         JNO   CHKDXNX                                                          
         OI    ACBFLAG,ACBSKP      SKIP THIS ENTRY                              
*        J     EXITN                                                            
*                                                                               
CHKDXNX  SR    R1,R1                                                            
         CR    RC,R1                                                            
         B     CHKDXX                                                           
CHKDXYES CR    RC,RC                                                            
CHKDXX   XIT1                                                                   
*                                                                               
CDPARMS  DS    0C                  PARMS PASSED BY CALL                         
CDPFLDL  DS    AL1                 FIELD LENGTH                                 
CDPFLD   DS    AL3                 FIELD                                        
CDPRMLNQ EQU   *-CDPARMS                                                        
         DROP  RF                                                               
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* CHECK IF FIELD CURRENT NEEDS TO GO TO AN OVERFLOW FIELD            *          
**********************************************************************          
         SPACE 1                                                                
CHKOVR   NTR1  BASE=*,LABEL=*                                                   
         TM    FLDSTAT3,FOVRFL     IS THIS A FIELD OVERFLOWABLE?                
         BNO   CHKOXYES                                                         
*                                                                               
         MVC   COPARMS(COPRMLNQ),0(R1)                                          
         MVC   ACBOVRFL,XSPACES                                                 
         SR    R4,R4                                                            
         ICM   R4,1,COPSFLDL       GET SENDING FIELD LENGTH                     
         BZ    CHKOXNO                                                          
         SR    R6,R6                                                            
         ICM   R6,1,COPRFLDL       GET RECEIVING FIELD LENGTH                   
         BZ    CHKOXNO                                                          
         CR    R4,R6               IS FIELD > THAN RECEIVING FIELD              
         BNH   CHKOXYES                                                         
         SR    RF,RF                                                            
         ICM   RF,7,COPSFLD                                                     
         GOTO1 CHOPPER,DMCB,((R4),(RF)),((R6),IO),2                             
         L     R0,8(R1)            NUMBER OF LINES USED                         
         CHI   R0,1                                                             
         BNH   CHKOXYES                                                         
*                                                                               
         LA    RF,IO                                                            
         SR    RE,RE                                                            
         ICM   RE,7,COPSFLD                                                     
         SR    R1,R1                                                            
         IC    R1,COPSFLDL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),XSPACES                                                  
         LR    R1,R6                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
         LA    RF,0(R6,RF)         BUMP TO NEXT ENTRY IN CHOPPER BLK            
         LR    R1,R6                                                            
         CHI   R1,L'ACBOVRFL                                                    
         BNH   *+8                                                              
         LA    R1,L'ACBOVRFL                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBOVRFL(0),0(RF)                                                
         B     CHKOXYES                                                         
*                                                                               
CHKOXNO  SR    R1,R1                                                            
         CR    RC,R1                                                            
         B     CHKOXX                                                           
*                                                                               
CHKOXYES CR    RC,RC                                                            
CHKOXX   XIT1                                                                   
*                                                                               
COPARMS  DS    0C                  PARMS PASSED BY CALL                         
COPSFLDL DS    AL1                 SENDING FIELD LENGTH                         
COPSFLD  DS    AL3                 SENDING FIELD                                
COPRFLDL DS    AL1                 RECIEVING FIELD LENGTH                       
COPRFLD  DS    AL4                 RECEIVING FIELD                              
COPRMLNQ EQU   *-COPARMS                                                        
*                                                                               
DELIML   DC    C'DELIMITERLIST=<, ',X'FF'                                       
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* BUILD IO TEMPORARY OUTPUT AREA WITH VARIABLE LENGTH FIELDS         *          
*       SEPARATED BY DELIMETER FROM 0(R3) ACTUAL OUTPUT AREA         *          
* INPUT                                                                         
* R3=A(ACTUAL OUTPUT AREA)                                           *          
* R7=A(CURRENT FORMAT HDR,DTL,TRL ) FRMTABD                          *          
* R5=A(FIELD ENTRY) FLDENTD                                          *          
**********************************************************************          
         SPACE 1                                                                
BUILDVLF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,TMPDISP        CURRENT IO DISPLACEMENT                      
         LA    R4,IO(R6)           START OF FIELD TO MOVE DATA TO               
*                                                                               
         LR    RE,R3               POINT RE TO ACTUAL OUTPUT AREA               
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         SR    R2,R2                                                            
         ICM   R2,3,FLDDSP                                                      
         AR    RE,R2               POINT TO START OF FIELD                      
         TM    FLDSTAT3,FSNDASIS   ADJUST FLD FOR SPACES ?                      
         BO    BLDVLF30            NO                                           
*                                                                               
         LA    RF,0(R1,RE)         POINT TO END OF FIELD                        
         BCTR  RF,0                POINT TO END OF FIELD-1                      
BLDVLF10 CLI   0(RF),C' '          GET RID OF LEADING SPACES                    
         BNE   BLDVLF30            CALCULATE VARIABLE LENGTH                    
         BCTR  RF,0                DECREMENT POINTER TO FIELD                   
         BCT   R1,BLDVLF10         R1=LNGTH OF FLD TO MOVE TO IO                
         B     BLDVLF50                                                         
*                                                                               
BLDVLF30 DS    0H                                                               
         NI    FLAG,X'FF'-FLGDFID  DELIMETER FOUND IN DATA                      
         LLC   RF,FLDDELIM         FIELD SEPARATOR                              
         XC    TRTAB,TRTAB         BUILD TRANSLATE TABLE ON THE FLY             
         LA    RF,TRTAB(RF)                                                     
         MVI   0(RF),X'FF'                                                      
         BCTR  R1,0                SUBTRACT 1 FOR EX TRT AND EX MVC             
         LR    RF,R1               SAVE OFF LENGTH TRT CHANGES R1               
         EX    R1,*+8                                                           
         B     *+10                                                             
         TRT   0(0,RE),TRTAB                                                    
         BZ    BLDVLF40            NO DELIMETER IN DATA FOUND                   
         LR    R1,RF               RESTORE LENGTH                               
         OI    FLAG,FLGDFID        DELIMETER FOUND IN DATA                      
*                                                                               
         MVC   0(1,R4),FLDSURRC    MOVE IN FIELD SURROUNDING CHARA              
         LA    R4,1(R4)            BUMP TEMP OUTPUT AREA BY 1                   
         AHI   R6,1                BUMP DISPLACEMENT BY 1                       
BLDVLF40 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RE)       MOVE DATA FROM ACTUAL TO TEMP                
         LA    R1,1(R1)            RESET R1 TO ACTUAL MOVE LENGTH               
         AR    R4,R1               BUMP TEMP OUTPUT AREA BY DATA LENGTH         
         AR    R6,R1               BUMP DIPLACEMENT                             
*                                                                               
         TM    FLAG,FLGDFID        IS DELIM IN DATA ?                           
         BNO   BLDVLF50                                                         
         MVC   0(1,R4),FLDSURRC                                                 
         LA    R4,1(R4)            BUMP CURRENT POSITION IN IO                  
         AHI   R6,1                BUMP DISPLACEMENT                            
*                                                                               
BLDVLF50 TM    FLDSTAT3,FNODELIM   DO WE NOT WANT SEPARATOR ?                   
         BO    *+14                NO                                           
         MVC   0(1,R4),FLDDELIM    MOVE THE FIELD SEPARATOR                     
         AHI   R6,1                                                             
         STCM  R6,3,TMPDISP        SAVE CURRENT DISPLACEMENT                    
BLDVLFX  CR    RC,RC                                                            
         XIT1                                                                   
*                                                                               
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
**********************************************************************          
* FIELD TABLES                                                       *          
*    POS 1   - FIELD NUMBER                                          *          
*    POS 2-3 - FIELD DISPLACEMENT                                    *          
*    POS 4-5 - FIELD OVERRIDE                                        *          
*    POS 4/5 OVRRD POS 4   - LENGTH OF SOURCE DATA                   *          
*            OVRRD POS 5   - EMPTY                                   *          
**********************************************************************          
         SPACE 1                                                                
OVRTAB   DS    0C                                                               
         DC    CL2'1M',AL2(ACBADTE-ACBANKD)                                     
         DC    AL1(EOF)                                                         
         SPACE 1                                                                
FLDTAB   DS    0C                                                               
         DC    AL1(FDCLINM),AL2(ACBCLINM-ACBANKD),AL1(L'ACBCLINM,0)             
         DC    AL1(FDPRDNM),AL2(ACBPRDNM-ACBANKD),AL1(L'ACBCLINM,0)             
         DC    AL1(FDINVDS),AL2(ACBINVDS-ACBANKD),AL1(L'ACBINVDS,0)             
         DC    AL1(FDEDBLK),AL2(ACBBLK1-ACBANKD),AL1(L'ACBBLK1,0)               
         DC    AL1(FDTEL#),AL2(ACBPHN-ACBANKD),AL1(L'ACBPHN,0)                  
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* SUPPORT TABLE                                                      *          
*       CUSNOTAB-CUSTOMER NUMBER TABLE                               *          
*       DODSTAB-OVERRIDES FOR BANKS WHO REFUSE TO PAY                *          
*       CLASSEX-CLASS EXCPT WHERE BANKS DON'T WANT 8 CHAR CLASS      *          
*       FRMTAB-KEY WORD DISPL TABLE(KEYED OFF X'20' IN AC57TAB       *          
*       LOCTAB-TABLE OF ACCNUM/USRIDS KEYED OFF FRMRENT IN FRMTAB    *          
**********************************************************************          
         SPACE 1                                                                
CUSNOTAB DS    0CL24                                                            
         DC    CL2'CT',CL12'C10SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C20SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C40SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C70SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C80SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C90SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'U10SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C50SBO',CL10'9024131730'                            
         DC    CL2'CT',CL12'U50SBO',CL10'9024131730'                            
         DC    CL2'CT',CL12'U90SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C75SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C77SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C45SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'U45SBO',CL10'8000258860'                            
         DC    CL2'CT',CL12'C26SBO',CL10'8000258860'                            
         DC    CL2'CT',12X'FF',CL10'25896'                                      
*                                                                               
         DC    CL2'CY',CL12'Q2000',CL10'8000256740'                             
         DC    CL2'CY',CL12'Q2001',CL10'8000256740'                             
         DC    CL2'CY',CL12'T2000',CL10'8000256740'                             
         DC    CL2'CY',CL12'T2001',CL10'8000256740'                             
         DC    CL2'CY',CL12'V2000',CL10'8000256740'                             
         DC    CL2'CY',CL12'V2001',CL10'8000256740'                             
         DC    CL2'CY',CL12'V5000',CL10'8000256740'                             
         DC    CL2'CY',CL12'J2000',CL10'8000256740'                             
         DC    CL2'CY',CL12'J2001',CL10'8000256740'                             
         DC    CL2'CY',12X'FF',CL10'25674'                                      
*                                                                               
         DC    CL2'DN',CL12'A2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'A2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'D2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'D2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'F2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'F2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'J2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'J2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'O2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'O2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'OH2000',CL10'8000258960'                            
         DC    CL2'DN',CL12'P2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'P2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'R2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'R2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'U2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'S2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'F2001',CL10'8000258960'                             
         DC    CL2'DN',CL12'Q2000',CL10'4769681400'                             
         DC    CL2'DN',CL12'M2000',CL10'4769681400'                             
         DC    CL2'DN',CL12'W2000',CL10'8000258960'                             
         DC    CL2'DN',CL12'W2001',CL10'8000258960'                             
         DC    CL2'DN',12X'FF',CL10'21636'                                      
*                                                                               
         DC    CL2'G9',CL12'W2000',CL10'7185219010'                             
         DC    CL2'G9',CL12'W2000A',CL10'7185219010'                            
         DC    CL2'G9',CL12'W2000P',CL10'7185219010'                            
         DC    CL2'G9',CL12'W2001',CL10'7185219010'                             
         DC    CL2'G9',CL12'W2001A',CL10'7185219010'                            
         DC    CL2'G9',12X'FF',CL10'21901'                                      
*                                                                               
         DC    CL2'MI',CL12'B2000',CL10'8000216360'                             
         DC    CL2'MI',CL12'B2001',CL10'8000216360'                             
         DC    CL2'MI',CL12'J2000',CL10'8000216360'                             
         DC    CL2'MI',CL12'J2001',CL10'8000216360'                             
         DC    CL2'MI',CL12'M2000',CL10'8000216360'                             
         DC    CL2'MI',CL12'N2000',CL10'9000118140'                             
         DC    CL2'MI',CL12'R2000',CL10'8000216360'                             
         DC    CL2'MI',12X'FF',CL10'21636'                                      
*                                                                               
         DC    CL2'OU',CL12'OA2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OA2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OB2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OB2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OD2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OD2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OE2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OE2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OF2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OF2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OG2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OG2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OH2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OH2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OI2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OI2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OJ2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OJ2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OP2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OP2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OR2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OR2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OT2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OT2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OU2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OU2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OX2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OX2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OY2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OY2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OW2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OW2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'ON2000',CL10'11814'                                 
         DC    CL2'OU',CL12'ON2001',CL10'11814'                                 
         DC    CL2'OU',CL12'OS2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OS2001',CL10'8000259000'                            
         DC    CL2'OU',CL12'OZ2000',CL10'8000259000'                            
         DC    CL2'OU',CL12'OZ2001',CL10'8000259000'                            
*MN SPEC-35463                                                                  
*        DC    CL2'OU',CL12'C10SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'U10SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C20SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C40SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C50SBO',CL10'9024131730'                            
*        DC    CL2'OU',CL12'U50SBO',CL10'9024131730'                            
*        DC    CL2'OU',CL12'C70SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C75SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C77SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C80SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C90SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'U90SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C10SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'U10SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C45SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'U45SBO',CL10'8000258860'                            
*        DC    CL2'OU',CL12'C26SBO',CL10'8000258860'                            
*MN SPEC-35463                                                                  
*MN SPEC-48447                                                                  
         DC    CL2'OU',CL12'OWD200',CL10'8000258860'                            
*MN SPEC-48447                                                                  
         DC    CL2'OU',12X'FF',CL10'21636'                                      
*                                                                               
         DC    CL2'PY',CL12'N2000',CL10'21636'                                  
         DC    CL2'PY',CL12'N2001',CL10'21636'                                  
         DC    CL2'PY',CL12'Z2000',CL10'21636'                                  
         DC    CL2'PY',CL12'OTIANCN',CL10'9000118140'                           
         DC    CL2'PY',CL12'OTIAPCN',CL10'9000118140'                           
         DC    CL2'PY',CL12'OTIARCN',CL10'9000118140'                           
         DC    CL2'PY',CL12'OTIANUS',CL10'9000118140'                           
         DC    CL2'PY',CL12'OTIAPUS',CL10'9000118140'                           
         DC    CL2'PY',CL12'OTIARUS',CL10'9000118140'                           
         DC    CL2'PY',12X'FF',CL10'11814'                                      
*                                                                               
         DC    2X'FF',12X'FF',CL10'21636'     GENERIC DEFAULT VALUE             
*                                                                               
PRFXTAB  DS    0C                                                               
         DC    C'BD',C'BA006       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BA010       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BA011       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BA012       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BA013       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BA014       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BC011       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BC012       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BC013       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BC014       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BC015       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BD001       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BD002       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BD003       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BF001       ',X'000004',CL4'2101'                        
         DC    C'BD',C'BF002       ',X'000004',CL4'2101'                        
         DC    C'BD',C'BW009       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BW011       ',X'000004',CL4'2101'                        
         DC    C'BD',C'BW012       ',X'000004',CL4'2101'                        
         DC    C'BD',C'BW013       ',X'000004',CL4'2101'                        
         DC    C'BD',C'BW014       ',X'000004',CL4'0000'                        
         DC    C'BD',C'BW002A      ',X'000004',CL4'2101'                        
         DC    C'BD',C'BW010A      ',X'000004',CL4'2102'                        
         DC    C'BD',C'BW005A      ',X'000004',CL4'2103'                        
         DC    C'B$',C'SP          ',X'800202',CL4'56'                          
         DC    C'B$',C'SS          ',X'800202',CL4'53'                          
         DC    C'B$',C'SU          ',X'800202',CL4'51'                          
         DC    C'H7',C'SP          ',X'C00004',CL4'4000'                        
         DC    C'H7',C'SS          ',X'C00004',CL4'3000'                        
         DC    C'H7',C'SU          ',X'C00004',CL4'5000'                        
         DC    C'H7',C'SV          ',X'C00004',CL4'1000'                        
         DC    C'H7',C'SW          ',X'C00004',CL4'2000'                        
         DC    C'H7',C'SX          ',X'C00004',CL4'7000'                        
         DC    C'H7',C'SY          ',X'C00004',CL4'6000'                        
         DC    C'H7',C'SQ          ',X'C00004',CL4'4500'                        
         DC    C'H7',C'ST          ',X'C00004',CL4'5500'                        
         DC    C'JG',C'ZD8731006327',X'000004',CL4'1000'                        
         DC    C'JW',C'B001        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B002        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B003        ',X'000301',CL4'7'                           
         DC    C'JW',C'B007        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B010        ',X'000301',CL4'7'                           
         DC    C'JW',C'B012        ',X'000301',CL4'7'                           
         DC    C'JW',C'B014        ',X'000301',CL4'7'                           
         DC    C'JW',C'B015        ',X'000301',CL4'4'                           
         DC    C'JW',C'B016        ',X'000301',CL4'2'                           
         DC    C'JW',C'B017        ',X'000301',CL4'3'                           
         DC    C'JW',C'B018        ',X'000301',CL4'4'                           
         DC    C'JW',C'B019        ',X'000301',CL4'2'                           
         DC    C'JW',C'B020        ',X'000301',CL4'3'                           
         DC    C'JW',C'B021        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B022        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B023        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B024        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B025        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B026        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B027        ',X'000004',CL4'0000'                        
         DC    C'JW',C'B028        ',X'000004',CL4'0000'                        
         DC    C'M2',C'SP          ',X'C00004',CL4'4000'                        
         DC    C'M2',C'SS          ',X'C00004',CL4'3000'                        
         DC    C'M2',C'SU          ',X'C00004',CL4'5000'                        
         DC    C'M2',C'SV          ',X'C00004',CL4'1000'                        
         DC    C'M2',C'SX          ',X'C00004',CL4'7000'                        
         DC    C'M2',C'SW          ',X'C00004',CL4'2000'                        
         DC    C'M2',C'SY          ',X'C00004',CL4'6000'                        
         DC    C'NE',C'CD006       ',X'000004',CL4'0000'                        
         DC    C'UB',C'CFFU2BOS    ',X'000004',CL4'0000'                        
         DC    C'UB',C'MMFU20766   ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU1D      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU1V      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2D      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2H      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2P      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2T      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2U      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2V      ',X'000004',CL4'0000'                        
         DC    C'UB',C'01FU2W      ',X'000004',CL4'0000'                        
         DC    C'WW',C'B001W       ',X'000004',CL4'3000'                        
         DC    C'WW',C'B002W       ',X'000004',CL4'4000'                        
         DC    C'WW',C'B003W       ',X'000004',CL4'5000'                        
         DC    C'WW',C'B004W       ',X'000004',CL4'1000'                        
         DC    C'WW',C'B006W       ',X'000004',CL4'7000'                        
         DC    C'WW',C'B001        ',X'000004',CL4'2000'                        
         DC    C'WW',C'B002        ',X'000004',CL4'3000'                        
         DC    C'WW',C'B003        ',X'000004',CL4'2000'                        
         DC    C'WW',C'B004        ',X'000004',CL4'4000'                        
         DC    C'YE',C'CD469098509A',X'000004',CL4'1000'                        
         DC    C'YE',C'CD469098509B',X'000004',CL4'2000'                        
         DC    C'YE',C'CD469098509C',X'000004',CL4'3000'                        
         DC    C'YE',C'CD469098509D',X'000004',CL4'4000'                        
         DC    C'YE',C'CD469098509E',X'000004',CL4'5000'                        
         DC    C'YF',C'ZDBM87396328',X'000004',CL4'1000'                        
         DC    C'YF',C'ZDCW87336327',X'000004',CL4'1000'                        
         DC    C'YN',C'SW          ',X'C00004',CL4'6000'                        
         DC    C'YN',C'B001C       ',X'000004',CL4'5000'                        
         DC    C'YN',C'B001D       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B001E       ',X'000004',CL4'5000'                        
         DC    C'YN',C'B001EN      ',X'000004',CL4'5000'                        
         DC    C'YN',C'B001F       ',X'000004',CL4'5000'                        
         DC    C'YN',C'B001G       ',X'000004',CL4'5000'                        
         DC    C'YN',C'B001M       ',X'000004',CL4'5100'                        
         DC    C'YN',C'B001O       ',X'000004',CL4'5300'                        
         DC    C'YN',C'B001W       ',X'000004',CL4'5200'                        
         DC    C'YN',C'B002C       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B002D       ',X'000004',CL4'2000'                        
         DC    C'YN',C'B002E       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B002EN      ',X'000004',CL4'3000'                        
         DC    C'YN',C'B002F       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B002G       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B002M       ',X'000004',CL4'3100'                        
         DC    C'YN',C'B002O       ',X'000004',CL4'3300'                        
         DC    C'YN',C'B002W       ',X'000004',CL4'3200'                        
         DC    C'YN',C'B003C       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003D       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003E       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003EN      ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003F       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003G       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B003M       ',X'000004',CL4'4100'                        
         DC    C'YN',C'B003O       ',X'000004',CL4'4300'                        
         DC    C'YN',C'B003W       ',X'000004',CL4'4200'                        
         DC    C'YN',C'B004C       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004D       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004E       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004EN      ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004F       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004G       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004K       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B004M       ',X'000004',CL4'1100'                        
         DC    C'YN',C'B004N       ',X'000004',CL4'9000'                        
         DC    C'YN',C'B004O       ',X'000004',CL4'1300'                        
         DC    C'YN',C'B004W       ',X'000004',CL4'1200'                        
         DC    C'YN',C'B005        ',X'000004',CL4'1000'                        
         DC    C'YN',C'B005A       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B005B       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B005C       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B005F       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B007F       ',X'000004',CL4'3000'                        
         DC    C'YN',C'B008F       ',X'000004',CL4'4000'                        
         DC    C'YN',C'B006F       ',X'000004',CL4'7000'                        
         DC    C'YN',C'B006G       ',X'000004',CL4'7000'                        
         DC    C'YN',C'B006M       ',X'000004',CL4'7001'                        
         DC    C'YN',C'B006N       ',X'000004',CL4'9000'                        
         DC    C'YN',C'B006O       ',X'000004',CL4'7300'                        
         DC    C'YN',C'B006SSY     ',X'000004',CL4'1000'                        
         DC    C'YN',C'B006W       ',X'000004',CL4'7200'                        
         DC    C'YN',C'B007A       ',X'000004',CL4'5200'                        
         DC    C'YN',C'B007B       ',X'000004',CL4'3200'                        
         DC    C'YN',C'B007C       ',X'000004',CL4'4200'                        
         DC    C'YN',C'B007D       ',X'000004',CL4'1200'                        
         DC    C'YN',C'B007E       ',X'000004',CL4'7200'                        
         DC    C'YN',C'B004Z       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B009F       ',X'000004',CL4'1000'                        
         DC    C'YN',C'B009FSY     ',X'000004',CL4'1000'                        
         DC    C'YN',C'B009P       ',X'000004',CL4'9000'                        
         DC    C'YN',C'BBOAMECV    ',X'000004',CL4'1000'                        
         DC    C'YN',C'BBOAMECS    ',X'000004',CL4'3000'                        
         DC    C'YN',C'BBOAMECP    ',X'000004',CL4'4000'                        
         DC    C'YN',C'BBOAMECU    ',X'000004',CL4'5000'                        
         DC    C'YN',C'BBOAMECY    ',X'000004',CL4'7000'                        
         DC    C'YN',C'BBOAOUTV    ',X'000004',CL4'1300'                        
         DC    C'YN',C'BBOAOUTP    ',X'000004',CL4'4300'                        
         DC    C'YN',C'BBOAOUTS    ',X'000004',CL4'3300'                        
         DC    C'YN',C'BBOAOUTU    ',X'000004',CL4'5300'                        
         DC    C'YN',C'BBOAOUTY    ',X'000004',CL4'7300'                        
         DC    C'YP',C'DW6301469080',X'000004',CL4'1000'                        
         DC    C'YP',C'DW6301469085',X'000004',CL4'2000'                        
         DC    C'YP',C'DW6301469088',X'000004',CL4'1000'                        
         DC    C'YP',C'DW6301469089',X'000004',CL4'2000'                        
         DC    C'YP',C'DW6301469105',X'000004',CL4'3000'                        
         DC    C'  ',C'            ',X'000004',CL4'PRE0'                        
*                                                                               
***********************************************************************         
* TABLE TO DETERMINE FILE ID MODIFIER CHARACTER BASED ON GENERATION             
* NUMBER OF THE DATASET BEING CREATED (USED BY ACH EFT GHG94X FORMATS)          
***********************************************************************         
         SPACE 1                                                                
FILIDTB  DS    0C                                                               
         DC    CL10'0121416181',C'A'                                            
         DC    CL10'0222426282',C'B'                                            
         DC    CL10'0323436383',C'C'                                            
         DC    CL10'0424446484',C'D'                                            
         DC    CL10'0525456585',C'E'                                            
         DC    CL10'0626466686',C'F'                                            
         DC    CL10'0727476787',C'G'                                            
         DC    CL10'0828486888',C'H'                                            
         DC    CL10'0929496989',C'I'                                            
         DC    CL10'1030507090',C'J'                                            
         DC    CL10'1131517191',C'K'                                            
         DC    CL10'1232527292',C'L'                                            
         DC    CL10'1333537393',C'M'                                            
         DC    CL10'1434547494',C'N'                                            
         DC    CL10'1535557595',C'O'                                            
         DC    CL10'1636567696',C'P'                                            
         DC    CL10'1737577797',C'Q'                                            
         DC    CL10'1838587898',C'R'                                            
         DC    CL10'1939697999',C'S'                                            
         DC    CL10'2040608000',C'T'                                            
         DC    X'FF'                                                            
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* FORMAT TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
* FRMTAB                                                                        
       ++INCLUDE ACFRMTAB                                                       
         EJECT                                                                  
***********************************************************************         
* STORAGE DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
PARMS    DS    0A                                                               
PREC     DS    XL1                                                              
PFRM     EQU   X'01'                                                            
PRUNIT   EQU   X'02'                                                            
PBLOCK   DS    AL3                 USER BLOCK                                   
PCOMFACS DS    AL4                                                              
PSYSOVR  DS    AL4                 SYSTEM SPECIFIC OVERLAY                      
PARMSLNQ EQU   *-PARMS                                                          
*                                                                               
VDATAMGR DS    A                                                                
VMASTC   DS    A                                                                
VDATCON  DS    A                                                                
*DSFTK-150                                                                      
VADDAY   DS    A                                                                
*DSFTK-150                                                                      
*                                                                               
ACURREC  DS    A                   CURRENT ADDRESS OF RECORD ENTRIES            
ACURENT  DS    A                   CURRENT ADDRESS OF FIELD ENTRIES             
*                                                                               
SVADDR   DS    AL4                 SAVED AREA FOR ADDRESS                       
SVADDR2  DS    AL4                 SAVED AREA FOR ADDRESS                       
SVADDR3  DS    AL4                 SAVED AREA FOR ADDRESS                       
SVDISP   DS    X                   SAVED AREA FOR DISPLACMENT TO BNAME          
*                                                                               
*DSFTK-150                                                                      
RUNSTAT  DS    XL1                 RUN STATUS -                                 
RUNSN    EQU   X'80'               SOON RUN                                     
RUNSPCL  EQU   X'40'               JPM SPECIAL ROUTINE                          
*DSFTK-150                                                                      
ZERO     DS    CL50                CHARACTER ZEROS                              
*                                                                               
TIMEOUT  DS    PL8                 OUTPUT EXPRESSION FOR TIME MACRO             
FLAG     DS    XL1                                                              
SKPOVRD  EQU   X'80'               SKIP OVERRIDE                                
FLGDFID  EQU   X'40'               DELIMETER IN DATA                            
*                                                                               
XSPACES  DS    CL200                                                            
*                                                                               
SVTSYS   DS    XL(L'TSYS)                                                       
*                                                                               
KEYACC   DS    CL42                                                             
*                                                                               
         DS    0D                                                               
IO       DS    2000C                                                            
IOLNQ    EQU   *-IO                                                             
*                                                                               
BH       EQU   X'20'               BRANCH ON HIGH                               
BNH      EQU   X'D0'               BRANCH ON NOT HIGH                           
BO       EQU   X'10'               BRANCH ON ONES                               
BNO      EQU   X'E0'               BRANCH ON NOT ONES                           
BE       EQU   X'80'               BRANCH ON EQUAL                              
BNE      EQU   X'70'               BRANCH ON NOT EQUAL                          
BL       EQU   X'40'               BRANCH ON LOW                                
BNL      EQU   X'B0'               BRANCH ON NOT LOW                            
*                                                                               
TRTAB    DS    XL256               TRANSLATE TABLE                              
FLDDELIM DS    XL1                 VARIABLE LENGTH FIELD DELIMETER              
COMMA    EQU   C','                COMMA                                        
FLDSURRC DS    XL1                 FIELD SURROUNDING CHARACTER                  
QUOTES   EQU   C'"'                QUOTATION MARK                               
*                                                                               
TMPDISP  DS    XL2                 CURRENT DISPLACEMENT IN IO                   
*                                                                               
WORKX    EQU   *                                                                
         SPACE 1                                                                
*********************************************************************           
* EQUATES                                                           *           
*********************************************************************           
         SPACE 1                                                                
EOF      EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FIELD ADDRESS TABLE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLDATBD  DSECT                                                                  
FLDAREC# DS    XL1                 FIELD NUMBER                                 
FLDALOC  DS    AL4                 ADDRESS OF FIELD ENTRY TABLE                 
FLDALNQ  EQU   *-FLDATBD                                                        
         EJECT                                                                  
*********************************************************************           
* OVERRIDE TABLE DSECT                                              *           
*********************************************************************           
         SPACE 1                                                                
OVRTABD  DSECT                                                                  
OVRALPHA DS    CL2                 ALPHA ID FOR OVERRIDES                       
OVRDSP   DS    XL2                 DISPLACEMENT TO FIELD                        
OVRLNQ   EQU   *-OVRTABD                                                        
         EJECT                                                                  
*********************************************************************           
* TYPE TAB DSECT                                                    *           
*      - SEQUENCE # TABLE                                           *           
*      - DATE TABLE                                                 *           
*      - NAME TABLE                                                 *           
*      - AMOUNT TABLE                                               *           
*      - COUNT TABLE                                                *           
*      - FIELD TABLE                                                *           
*      - DATA TABLE                                                 *           
*********************************************************************           
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYP#     DS    XL1                 FIELD NUMBER                                 
TYPDSP   DS    XL2                 ADDRESS OF FIELD                             
TYPALPHA DS    0CL2                ALPHA ID FOR OVERRIDES                       
TYPLN    DS    XL1                 LENGTH OF SOURCE                             
         DS    XL1                                                              
TYPLNQ   EQU   *-TYPTABD                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER PREFIX TABLE                                        *          
**********************************************************************          
PRFXTBD  DSECT                                                                  
PRFALPHA DS    CL2                 COMPANY ALPHA ID                             
PRFDATA  DS    CL12                BANK ACCOUNT OR LEDGER                       
PRFOPT   DS    XL1                 MATCHING ON? (ACCOUNT IS DEFAULT)            
PRFLEDG  EQU   X'80'                 MATCH ON LEDGER                            
PRFACCM  EQU   X'40'                 MATCH ON LDG FROM ACCT(NOT CONTRA)         
PRFDSP   DS    XL1                 DISPLACEMENT TO PREFIX                       
PRFLN    DS    XL1                 LENGTH OF PREFIX                             
PRFPRF   DS    CL4                 PREFIX                                       
PRFTBLNQ EQU   *-PRFXTBD                                                        
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER CUSTOMER NUMBER TABLE                               *          
**********************************************************************          
         SPACE 1                                                                
CUSNOTBD DSECT                                                                  
CUSALPHA DS    CL2                 COMPANY ALPHA ID                             
CUSACCT  DS    CL12                SC ACCOUNT                                   
CUSNO    DS    CL10                CUSTOMER NUMBER                              
CUSTBLNQ EQU   *-CUSNOTBD                                                       
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER FILE ID MODIFIER TABLE                              *          
**********************************************************************          
         SPACE 1                                                                
FILIDTBD DSECT                                                                  
FILGEN#  DS    CL10             LIST OF LAST TWO DIGITS OF GEN#                 
FILMOD   DS    CL1              FILE ID MODIFIER TO USE IN THE OUTPUT           
FILIDLNQ EQU   *-FILIDTBD                                                       
         EJECT                                                                  
***********************************************************************         
* GETFORM DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE ACGETFORMD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE GEGENFILE                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE GEGENBNK                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE FAUTL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082ACGETFORM 07/30/20'                                      
         END                                                                    
