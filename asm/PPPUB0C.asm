*          DATA SET PPPUB0C    AT LEVEL 020 AS OF 09/18/08                      
*PHASE T4060CA                                                                  
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* SMYE  11/05  TWO-CHARACTER OFFICE CHANGES                                     
*                                                                               
* SMYE 08/17/05 VALIDATE FAX NUMBER (ADRFAX) AS NUMERIC AND                     
*                FORMAT AS NNN-NNN-NNNN WHEN 10-CHAR LENGTH                     
*                                                                               
* SMYE  03/05  USE IOAREA FOR PUB ADDRESS REC AND PUBIO FOR PUB REC AND         
*                DISPLAY DATA FROM PUBREC IF ADDING NEW PAY ADDRESS AND         
*                INPUT SCREEN IS BLANK                                          
*                                                                               
* KWAN  09/00  ADD 3 MORE CLIENT LIST LINES (TOTAL OF 5 NOW)                    
*                                                                               
* BPLA  09/00  ADD ANOTHER CLIENT LIST LINE                                     
*                                                                               
* BPLA  09/00  FIX END OF OTHER CLIENT LIST CHECKING                            
*                                                                               
* SMYE  12/99   ENLARGED I/O AREA CLEARED FOR LARGER, ONE-SIZE RECORD           
*                                                                               
* KWAN  07/99   REMOVED ELEM LENGTH CHECKING                                    
*                                                                               
* SMYE   3/99   ADDED E-MAIL ADDRESS FIELD                                      
*                                                                               
* BPLA   1/97   REQUIRE NAME IF ESWITCH IS 1                                    
*               (ONLY FOR PAYING ADDRESS RECORDS)                               
*                                                                               
* SMYE  12/96   CHANGED TO MAINTAIN PUBAREC ADDRESS RECORDS                     
*               INSTEAD OF PUBREC ADDRESS ELEMENTS                              
*                                                                               
* SMYE  12/96   ACTIVATED OTHER ADDRESSES DISPLAY FOR ALL AGENCIES              
*                                                                               
* BPLA  3/96    DISPLAY OF OTHER ADDRESSES FOR SJR ONLY                         
*               ALSO DELETE OLD ELEM (IT MAY BE TOO SMALL)                      
*               NEW ELEMENT ALWAYS BUILT IN ELEAREA NOW                         
*                                                                               
* BPLA  3/96    DISPLAY ADDRESSES ALREADY SET UP                                
*                                                                               
* SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
* BPLA 11/1/91  ALLOW FOR SHIPPING ADDRESS (ADDR,SHIP)                          
*                                                                               
* SWON 12/28/89 ADD FAX NUMBER FIELD                                L01         
*                                                                               
         TITLE 'T4060C  PUBFILE MAINT   ADDRESS SCREEN'                         
         PRINT NOGEN                                                            
T4060C   CSECT                                                                  
         NMOD1 0,T4060C,R8         NOTE R8 AS 2ND BASE                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
*SMY*    LA    R9,PUBIO                                                         
         LA    R9,IOAREA                                                        
         USING PUBARECD,R9                                                      
*                                                                               
         MVI   ADDCHSW,0                                                        
*                                                                               
         MVI   LTLIND,0                                                         
         MVI   ELCOD,X'0B'                                                      
         CLC   BCODE,=C'SHI'    SHIPPING                                        
         BE    STARTIO                                                          
         MVI   ELCOD,X'08'                                                      
         CLC   BCODE,=C'PAY'                                                    
         BE    STARTIO                                                          
         MVI   ELCOD,X'09'                                                      
         CLC   BCODE,=C'TRA'                                                    
         BE    STARTIO                                                          
         MVI   ELCOD,X'0A'                                                      
         CLC   BCODE,=C'CON'                                                    
         BE   *+6                                                               
         DC    H'0'                     INVALID SCREEN                          
*                                                                               
STARTIO  DS    0H                                                               
         XCEFL PUBAREC,300              CLEAR PUB RECORD AREA                   
         XC    KEY,KEY                                                          
         XC    PUBADDR,PUBADDR          CLEAR PUB ADDRESS                       
*                                                                               
         MVC   PUBAKMED,BMED            MEDIA                                   
         MVC   PUBAKPUB(6),BPUB         PUB                                     
         MVC   PUBAKAGY,AGYALPHA        AGENCY                                  
         MVI   PUBAKCOD,X'82'           RECORD CODE (PUB ADDRESS)               
         MVC   PUBAKTYP,ELCOD           ADDRESS TYPE                            
         MVC   PUBAKCLT,BCLT            CLIENT/OFFICE/DEFAULT                   
         MVC   KEY(25),PUBAKEY                                                  
*                                                                               
         MVC   SVINBTS,DMINBTS     SAVE DMINBTS                                 
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGHPUB                                                       
         MVC   DMINBTS,SVINBTS     RESTORE DMINBTS                              
*                                                                               
         CLC   KEY(25),KEYSAVE     PUB ADDRESS RECORD FOUND ?                   
         BE    STARTGET            YES - GET THE RECORD                         
         CLI   BACT,1              NO - IS IT ADD ?                             
         BE    CLISCRN             YES                                          
         B     CKIND               NO                                           
*                                                                               
STARTGET BAS   RE,GETPUBA                                                       
         MVC   PUBADDR,KEY+27      SAVE ADDRESS OF FOUND RECORD                 
         OI    LTLIND,X'10'        IF REC EXISTS, ADDR ELEMENT EXISTS           
*                                                                               
CKIND    DS    0H                                                               
         CLI   BACT,1              ADD                                          
         BNE   CLISCRN                                                          
         TM    LTLIND,X'10'                                                     
         BZ    CLISCRN                                                          
         CLI   KEY+25,X'FF'        FLAGGED FOR DELETION ?                       
         BE    CLISCRN             YES - OK TO EDIT                             
CKINDERR LA    R3,COMBERR                                                       
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
CLISCRN  CLI   BYTE2,1                                                          
         BE    FORMATP                                                          
*                                                                               
* CLIENT SCREEN IN TWA SO EDIT IT  (UNLESS ACT=DISP)                            
*                                                                               
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
EDITREPS DS    0H                                                               
EDITR    LA    R2,ADRNAMEH                                                      
         MVI   ESWITCH,0                                                        
         XC    PUBAONAM(249),PUBAONAM     CLEAR ELEMENT FROM NAME "ON"          
         CLI   5(R2),0                                                          
         BE    EDITR2                                                           
         BAS   RE,MOVE                                                          
         MVC   PUBAONAM,WORK                                                    
         OI    ESWITCH,1                                                        
*                                                                               
EDITR2   LA    R2,ADRLIN1H                                                      
         XC    PUBAOLN1,PUBAOLN1                                                
         CLI   5(R2),0                                                          
         BE    EDITR4                                                           
         BAS   RE,MOVE                                                          
         MVC   PUBAOLN1,WORK                                                    
         OI    ESWITCH,1                                                        
*                                                                               
EDITR4   LA    R2,ADRLIN2H                                                      
         XC    PUBAOLN2,PUBAOLN2                                                
         CLI   5(R2),0                                                          
         BE    EDITR6                                                           
         BAS   RE,MOVE                                                          
         MVC   PUBAOLN2,WORK                                                    
         OI    ESWITCH,1                                                        
*                                                                               
EDITR6   LA    R2,ADRATTNH                                                      
         XC    PUBAOATN,PUBAOATN                                                
         CLI   5(R2),0                                                          
         BE    EDITR8                                                           
         BAS   RE,MOVE                                                          
         MVC   PUBAOATN,WORK                                                    
         OI    ESWITCH,1                                                        
*                                                                               
EDITR8   LA    R2,ADRTELEH                                                      
         XC    PUBAOTEL,PUBAOTEL                                                
         CLI   5(R2),0                                                          
         BE    EDITR10                                                          
         BAS   RE,MOVE                                                          
         MVC   PUBAOTEL,WORK                                                    
         OI    ESWITCH,1                                                        
*                                                                               
EDITR10  LA    R2,ADRFAXH                                                       
         XC    PUBAOFAX,PUBAOFAX                                                
         CLI   5(R2),0                                                          
         BE    EDITR14                                                          
         LA    R3,2        INVALID INPUT FIELD                                  
         CLC   ADRFAX(3),=C'MB='                                                
         BNE   EDITR11                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(3,SCANBLK)                                       
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         CLI   SCANBLK+1,X'08'     MUST BE 8 DIGITS                             
         BNE   ERROR                                                            
         TM    SCANBLK+3,X'80'     VALID NUMERIC                                
         BNO   ERROR                                                            
         CLC   SCANBLK+22(2),=C'62'                                             
         BNE   ERROR                                                            
         B     EDITR13                                                          
*                                                                               
EDITR11  DS    0H                                                               
         CLC   ADRFAX(3),=C'FX='                                                
         BE    EDITR13                                                          
*                                  NOT MB= OR FX=                               
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         ZIC   R0,ADRFAXH+5        GET LENGTH OF INPUT                          
         XC    WORK,WORK           INIT WORKAREA                                
         LA    RF,ADRFAX           POINT TO INPUT                               
         LA    RE,WORK                                                          
*                                                                               
LHVFAXLP DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    LHVFAXCN                                                         
         CLI   0(RF),C'9'                                                       
         BH    LHVFAXCN                                                         
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
LHVFAXCN DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,LHVFAXLP                                                      
*                                                                               
LHVFAXDN DS    0H                                                               
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
*                                                                               
         CHI   RE,10               MUST BE AT LEAST 10 DIGITS                   
         BL    ERROR                                                            
*                                                                               
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PUBAOFAX(0),WORK    SAVE FAX NUMBER FOR POSSIBLE FORMAT          
*                                                                               
         MVC   ADRFAX,PUBAOFAX       AND FOR NON-FORMATTING                     
*                                                                               
*        RE-DISPLAY AND SAVE AS FORMATTED NUMBER IF 10 DIGITS                   
*                                                                               
         AHI   RE,1                ADD BACK EXECUTED MOVE REDUCTION             
         CHI   RE,10                                                            
         BNE   LHVFAXXX            NOT 10 DIGITS                                
*                                                                               
         XC    ADRFAX,ADRFAX       CLEAR FIELD                                  
*                                                                               
         MVC   ADRFAX(3),PUBAOFAX  AREA CODE                                    
         MVI   ADRFAX+3,C'-'                                                    
         MVC   ADRFAX+4(3),PUBAOFAX+3  EXCHANGE                                 
         MVI   ADRFAX+7,C'-'                                                    
         MVC   ADRFAX+8(4),PUBAOFAX+6  NUMBER                                   
*                                                                               
         LA    RE,12               SAVE NEW LENGTH                              
*                                                                               
LHVFAXXX DS    0H                                                               
         STC   RE,ADRFAXH+5        SET LENGTH FOR MOVE BELOW                    
*                                                                               
EDITR13  DS    0H                                                               
         BAS   RE,MOVE                                              L02         
         OI    ADRFAXH+6,X'80'     XMIT                                         
         MVC   PUBAOFAX,WORK                                        L02         
         OI    ESWITCH,1                                            L02         
*                                                                               
EDITR14  LA    R2,ADREADDH         E-MAIL ADDRESS (60 BYTES)                    
         XC    PUBAOEAD,PUBAOEAD                                                
         CLI   5(R2),0                                                          
         BE    EDITRX                                                           
*                                  TEST FOR INVALID E-MAIL CHARACTERS           
         ZIC   R1,5(R2)            INPUT DATA LENGTH                            
         LA    RE,8(R2)            POINT TO INPUT DATA                          
         LA    R3,2                INVALID INPUT FIELD                          
EDITR18  CLI   0(RE),C' '          BLANK ?                                      
         BNH   ERROR               YES - INVALID                                
*                                                                               
* TEST FOR MORE (WHEN KNOWN) INVALID E-MAIL CHARACTERS                          
*                                                                               
         LA    RE,1(RE)            BUMP TO NEXT INPUT PSITION                   
         BCT   R1,EDITR18          TEST NEXT                                    
*                                                                               
* NO KNOWN INVALID CHARACTERS                                                   
*                                                                               
         MVC   WORK60,SPACES                                                    
         ZIC   R1,5(R2)            INPUT DATA LENGTH                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK60(0),8(R2)     EXECUTED MOVE OF E-MAIL ADDRESS              
         MVC   PUBAOEAD,WORK60                                                  
         OI    ESWITCH,1                                                        
*                                                                               
EDITRX   DS    0H                                                               
         CLI   ESWITCH,1           SEE IF ANY ENTRY                             
         BNE   UPDATE2                                                          
         CLI   ADRNAMEH+5,0        SEE IF NAME INPUT                            
         BNE   UPDATE2                                                          
         CLI   ELCOD,X'08'         SEE IF DOING PAYING ADDRESS                  
         BNE   UPDATE2                                                          
*                                                                               
         LA    R3,1                FIELD MISSING                                
         LA    R2,ADRNAMEH         NAME REQUIRED FOR PAYING ADDRESS             
         B     ERROR                                                            
*                                                                               
UPDATE2  DS    0H                  FINISH RECORD FIELDS                         
*                                                                               
         MVC   PUBAOVEL(1),ELCOD                                                
         MVI   PUBAOVEL+1,X'FE'    LENGTH                                       
         MVC   PUBAOFF(3),BCLT                                                  
         ZIC   RE,PUBAOVEL+1       LENGTH OF ELEMENT                            
         LA    RE,33(RE)           ADD HEADER LENGTH                            
         STH   RE,HALF             SAVE RECORD LENGTH                           
*                                                                               
* PUBADDR = 0'S IF NEW REC TO BE ADDED                                          
* IF ESWITCH=0 DO NOTHING (NO DATA ENTERED)                                     
* PUBADDR NE 0'S IF REC EXISTS                                                  
* IF ESWITCH=0 FLAG FOR DELETE (DMWRT) ELSE REWRITE RECORD                      
*                                                                               
UPDATE4  DS    0H                                                               
         OC    PUBADDR,PUBADDR     IS IT A NEW ADDRESS RECORD ?                 
         BZ    ADDIT               YES                                          
*                                  NO - RECORD EXISTS                           
         CLI   ESWITCH,0           ANYTHING ENTERED ?                           
         BNE   WRITEIT             YES                                          
         MVC   KEY+27(4),PUBADDR   NO - MUST BE DELETE                          
         MVC   PUBAREC+25(2),HALF  RECORD LENGTH - MAY HAVE CHANGED             
         BAS   RE,PUTPUBA          REWRITE "EMPTY" RECORD                       
         MVI   KEY+25,X'FF'                                                     
         BAS   RE,WRITEPUB         FLAG DIR FOR DELETE                          
         B     UPDATOVR            FINISH WORK                                  
*                                                                               
WRITEIT  DS    0H                                                               
         MVC   KEY+27(4),PUBADDR                                                
         MVC   PUBAREC+25(2),HALF  RECORD LENGTH - MAY HAVE CHANGED             
         BAS   RE,PUTPUBA                                                       
         CLI   KEY+25,X'FF'        FLAGGED FOR DELETION ?                       
         BNE   UPDATOVR            NO                                           
         MVI   KEY+25,0            TURN OFF                                     
         BAS   RE,WRITEPUB         REWRITE DIR                                  
         B     UPDATOVR            FINISH WORK                                  
*                                                                               
ADDIT    DS    0H                                                               
         CLI   ESWITCH,0           ANYTHING ENTERED ?                           
*SMY*    BE    DONE                NO                                           
         BNE   ADDIT2              YES                                          
         BAS   RE,USEPUB           DISPLAY PUBREC ADDRESS INFO                  
         LA    R2,ADRNAMEH                                                      
         B     EXIT                                                             
ADDIT2   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBAREC                                                  
         MVC   PUBAREC+25(2),HALF  RECORD LENGTH                                
         BAS   RE,ADDPUBA                                                       
*                                                                               
UPDATOVR DS    0H                                                               
         MVI   ADDCHSW,1                                                        
         B     PUTVEN                                                           
         EJECT                                                                  
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'0C'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   FMT5                                                             
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDITREPS                                                         
*                                                                               
FMT2     LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90406FC'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'0C'                                                    
*                                                                               
FMT5     DS    0H                                                               
         TM    LTLIND,X'10'                                                     
         BNZ   PUTFLDS                                                          
         FOUT  ADRNAMEH,SPACES,30                                               
         FOUT  ADRLIN1H,SPACES,30                                               
         FOUT  ADRLIN2H,SPACES,30                                               
         FOUT  ADRATTNH,SPACES,20                                               
         FOUT  ADRTELEH,SPACES,12                                               
         FOUT  ADRFAXH,SPACES,12                                    L02         
         FOUT  ADREADDH,SPACES,60                                               
         B     PUTVEN                                                           
*                                                                               
PUTFLDS  TM    LTLIND,X'10'                                                     
         BNO   PUTVEN                                                           
         LA    R4,PUBAREC+33                                                    
PUTFLD1  CLC   0(1,R4),ELCOD                                                    
         BNE   NEXT1                                                            
         CLC   2(3,R4),BCLT                                                     
         BNE   NEXT1                                                            
         FOUT  ADRNAMEH,PUBAONAM,30                                             
         FOUT  ADRLIN1H,PUBAOLN1,30                                             
         FOUT  ADRLIN2H,PUBAOLN2,30                                             
         FOUT  ADRATTNH,PUBAOATN,20                                             
         FOUT  ADRTELEH,PUBAOTEL,12                                             
         FOUT  ADRFAXH,SPACES,12       CLEAR                                    
         FOUT  ADREADDH,SPACES,60      CLEAR                                    
*                                                                               
         OC    PUBAOFAX,PUBAOFAX      IS THERE A FAX NUMBER?                    
         BZ    PUTFLD5                NO                                        
         FOUT  ADRFAXH,PUBAOFAX,12                                              
*                                                                               
PUTFLD5  OC    PUBAOEAD,PUBAOEAD      SEE IF HAS E-MAIL ADDRESS                 
         BZ    PUTVEN                 NO                                        
         FOUT  ADREADDH,PUBAOEAD,60                                             
*                                                                               
PUTVEN   DS    0H                                                               
         FOUT  ADRLSTAH,SPACES,20                                               
         FOUT  ADRLIS1H,SPACES,80                                               
         FOUT  ADRLIS2H,SPACES,80                                               
         FOUT  ADRLIS3H,SPACES,80                                               
         FOUT  ADRLIS4H,SPACES,80                                               
         FOUT  ADRLIS5H,SPACES,80                                               
         MVI   LISTSW,0               CLEAR OTHER ADDRESS FOUND SW              
*                                                                               
         LA    R6,ADRLIS1                                                       
         LA    R5,ADRLIS1                                                       
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   PUBAKMED,BMED            MEDIA                                   
         MVC   PUBAKPUB(6),BPUB         PUB                                     
         MVC   PUBAKAGY,AGYALPHA        AGENCY                                  
         MVI   PUBAKCOD,X'82'           RECORD CODE (PUB ADDRESS)               
         MVC   PUBAKTYP,ELCOD           ADDRESS TYPE                            
         MVC   KEY(11),PUBAKEY                                                  
*                                                                               
         BAS   RE,HIGHPUB                                                       
         B     PUTV2B                                                           
PUTV2    BAS   RE,SEQPUB                                                        
PUTV2B   CLC   KEY(11),KEYSAVE     PUB ADDRESS RECORD OF THIS TYPE ?            
         BNE   PUTVX5              NO - CHECK IF ANY USED                       
         CLC   11(3,R4),BCLT                                                    
         BE    PUTV2               SKIP ONE I JUST DISPLAYED                    
         MVI   LISTSW,1            SET OTHER ADDRESS FOUND SW                   
         MVC   0(2,R5),11(R4)                                                   
         CLI   13(R4),C' '         SEE IF 3 CHARACTERS                          
         BNH   PUTV4                                                            
         MVC   0(3,R5),11(R4)                                                   
         CLC   11(3,R4),=X'FFFFFF'                                              
         BNE   *+10                                                             
         MVC   0(3,R5),=C'ALL'                                                  
         LA    R5,3(R5)                                                         
         B     PUTV6                                                            
*                                                                               
PUTV4    CLI   0(R5),X'FF'                                                      
         BNE   PUTV5                                                            
         MVI   0(R5),C'*'          MUST BE OFFICE                               
         MVC   BYTE,1(R5)          SEE IF HAVE 2-CHARACTER                      
         BAS   RE,OFCCHG             PRINTABLE OFFICE                           
*****    BE    *+6                                                              
*****    DC    H'0'                SHOULD NEVER HAPPEN                          
         BNE   PUTV5               OFFICER DID NOT FIND OFFICE                  
*                                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         CLI   OFCOFC2+1,C' '      2-CHARACTER OFFICE ?                         
         BNH   PUTV5               NO                                           
         MVC   1(2,R5),OFCOFC2     REPLACE 1-CHR "INTERNAL" OFFICE              
         LA    R5,3(R5)                                                         
         B     PUTV6                                                            
         DROP  R1                                                               
*                                                                               
PUTV5    LA    R5,2(R5)                                                         
*                                                                               
PUTV6    LA    RE,76(R6)           NEED AT LEAST 4 MORE CHARS                   
         CR    R5,RE               END OF SCREEN FIELD                          
         BNL   PUTVX               STOP IF HIGH OR EQUAL                        
*                                                                               
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         B     PUTV2                                                            
*                                                                               
PUTVX    DS    0H                                                               
         LA    R6,ADRLIS2          SET TO 2ND LIST FIELD                        
         CR    R5,R6               SEE IF ALREADY ON SECOND LINE                
         BH    *+12                CHECK NEXT LINE                              
         LA    R5,ADRLIS2          CONTINUE DISPLAY                             
         B     PUTV2                                                            
*                                                                               
         LA    R6,ADRLIS3          SET TO 3ND LIST FIELD                        
         CR    R5,R6               SEE IF ALREADY ON SECOND LINE                
         BH    *+12                CHECK NEXT LINE                              
         LA    R5,ADRLIS3          CONTINUE DISPLAY                             
         B     PUTV2                                                            
*                                                                               
         LA    R6,ADRLIS4          SET TO 4ND LIST FIELD                        
         CR    R5,R6               SEE IF ALREADY ON SECOND LINE                
         BH    *+12                CHECK NEXT LINE                              
         LA    R5,ADRLIS4          CONTINUE DISPLAY                             
         B     PUTV2                                                            
*                                                                               
         LA    R6,ADRLIS5          SET TO 5ND LIST FIELD                        
         CR    R5,R6               SEE IF ALREADY ON SECOND LINE                
         BH    PUTVX5              STOP DISPLAYING (NO MORE ROOM)               
         LA    R5,ADRLIS5          CONTINUE DISPLAY                             
         B     PUTV2                                                            
*                                                                               
PUTVX5   DS    0H                                                               
         MVC   KEY(25),PUBAKEY     RESTORE DIR READ                             
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGHPUB                                                       
         NI    DMINBTS,X'FF'-X'08'     RESET DMINBTS                            
*                                                                               
         CLI   LISTSW,0     SEE IF ANY OTHER ADDRESSES FOUND                    
         BNE   PUTVXX                                                           
         B     CKSRDS       IF NONE,LEAVE FIELDS BLANK                          
*                                                                               
PUTVXX   FOUT  ADRLSTAH,=C'ADDRESSES EXIST FOR:',20                             
         BCTR  R5,0           BACK-UP R5                                        
         CLI   0(R5),C','     SEE IF IT IS A COMMA                              
         BNE   *+8                                                              
         MVI   0(R5),C' '     BLANK-OUT LAST COMMA                              
*                                                                               
CKSRDS   DS    0H                                                               
         CLI   ADDCHSW,1      SEE IF ELEM ADDED OR CHANGED                      
         BE    DONE                                                             
*                                                                               
         CLC   PUBAKAGY(2),AGYALPHA                                             
         BNE   PROTECT                                                          
*                                                                               
         BAS   RE,USEPUB           SEE IF PUBREC DATA S/B OUTPUT                
*                                                                               
         LA    R2,ADRNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PROTECT  DS    0H                                                               
         OI    ADRNAMEH+1,X'20'                                                 
         OI    ADRLIN1H+1,X'20'                                                 
         OI    ADRLIN2H+1,X'20'                                                 
         OI    ADRATTNH+1,X'20'                                                 
         OI    ADRTELEH+1,X'20'                                                 
         OI    ADRFAXH+1,X'20'                                                  
         OI    ADREADDH+1,X'20'                                                 
*                                                                               
         MVI   SAVSCRN,0                                                        
*                                                                               
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,ADRNAMEH                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
VNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLC   ELCOD,0(R4)                                                      
         BCR   8,RE                                                             
         CLI   0(R4),0                                                          
         BNE   *-18                                                             
VNEXTELX LTR   R4,R4                                                            
         BR    RE                                                               
*                                                                               
LTLIND   DS    CL1                                                              
ELCOD    DS    CL1                                                              
ASWITCH  DS    CL1                                                              
ESWITCH  DS    CL1                                                              
LISTSW   DS    CL1                                                              
ADDCHSW  DS    CL1                                                              
SVINBTS  DS    CL1                                                              
*                                                                               
FLDINV   EQU   2                                                                
COMBERR  EQU   112                                                              
REPERR   EQU   122                                                              
*                                                                               
ELCNT    DC    H'0'                                                             
VIRGERR  DC    H'0'                                                             
SPACES   DC    80C' '                                                           
DMWORK1  DS    12D                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    CL32                                                             
SCANBLK  DS    3CL32                                                            
WORK60   DS    CL60                FOR HANDLING E-MAIL ADDRESS                  
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
NEXT1    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    PUTVEN                                                           
         B     PUTFLD1                                                          
         SPACE 3                                                                
*             COMMUNICATION WITH DATA MANAGER (PUBFILE ADDRESS RECORDS)         
         SPACE 3                                                                
GETPUBA  MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILEA                                                         
         SPACE 2                                                                
PUTPUBA  MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILEA                                                         
         SPACE 2                                                                
ADDPUBA  MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILEA                                                         
         SPACE 2                                                                
PUBFILEA NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         SPACE 3                                                                
       ++INCLUDE PUGENEROL                                                      
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        ON A PAY ADDRESS ADD ACTION FILL IN SCREEN WITH DATA         *         
*           FROM THE PUBREC IF NO NAME IS FOUND ON SCREEN             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
USEPUB   NTR1                                                                   
*                                                                               
         CLC   BCODE,=C'PAY'       PAY ADDRESS ?                                
         BNE   USEPUBXT            NO - DO NOTHING                              
         CLI   BACT,1              ADD RECORD ?                                 
         BNE   USEPUBXT            NO - DO NOTHING                              
*SMY*    OC    PUBADDR,PUBADDR     DO WE HAVE AN ADDRESS RECORD ?               
*SMY*    BNZ   USEPUBXT            YES - DO NOTHING                             
         CLI   ADRNAME,C' '        NAME ENTERED ?                               
         BH    USEPUBXT            YES - DO NOTHING                             
*                                                                               
         LA    R4,KEY              GET THE PUBREC                               
         USING PUBREC,R4                                                        
         XC    KEY,KEY                                                          
         MVC   PUBKMED,BMED            MEDIA                                    
         MVC   PUBKPUB(6),BPUB         PUB                                      
         MVC   PUBKAGY,AGYALPHA        AGENCY                                   
         MVI   PUBKCOD,X'81'           RECORD CODE (PUB RECORD)                 
*                                                                               
*SMY*    MVC   SVINBTS,DMINBTS     SAVE DMINBTS                                 
*SMY*    OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         BAS   RE,HIGHPUB                                                       
*SMY*    MVC   DMINBTS,SVINBTS     RESTORE DMINBTS                              
*                                                                               
         CLC   KEY(25),KEYSAVE     PUB RECORD FOUND ?                           
         BE    *+6                 YES - GET IT                                 
         DC    H'0'                SHOULD NEVER HAPPEN                          
         DROP  R4                                                               
*                                                                               
         BAS   RE,GETPUB                                                        
*                                                                               
         LA    R4,PUBIO                                                         
         USING PUBREC,R4                                                        
         FOUT  ADRNAMEH,PUBNAME,20                                              
         OI    ADRNAMEH+1,X'01'       SET MODIFIED                              
         FOUT  ADRLIN1H,PUBLINE1,30                                             
         FOUT  ADRLIN2H,PUBLINE2,30                                             
*                                                                               
         LA    R4,PUBREC+33                                                     
         MVI   ELCOD,X'11'            PUBSADEL (SUPPLEMENTAL ADDRESS)           
         BAS   RE,VNEXTEL                                                       
         BNE   USEP20                                                           
         USING PUBSADD,R4                                                       
         FOUT  ADRATTNH,PUBATTN,20                                              
         FOUT  ADRTELEH,PUBTEL,12                                               
         FOUT  ADRFAXH,PUBSFAXN,12                                              
         FOUT  ADREADDH,SPACES,60                                               
         DROP  R4                                                               
*                                                                               
USEP20   DS    0H                                                               
         LA    R4,PUBIO                                                         
         USING PUBREC,R4                                                        
         LA    R4,PUBREC+33                                                     
         MVI   ELCOD,X'70'            PUBWEBEL (WEBSITE ELEMENT)                
         BAS   RE,VNEXTEL                                                       
         BNE   USEP30                                                           
         USING PUBWEBD,R4                                                       
*                                                                               
         ZIC   R5,1(R4)                                                         
         AHI   R5,-2               FOR OVERHEAD                                 
         LTR   R5,R5               ZERO LENGTH ?                                
         BZ    USEP30                                                           
         FOUT  ADREADDH,PUBWEBS,(R5)                                            
*                                                                               
USEP30   DS    0H                  RESTORE NOT NEEDED - EXITING AFTER           
*                                    THIS ROUTINE (SEE BAS  RE,USEPUB)          
*                                                                               
USEPUBX  DS    0H                                                               
         LA    R5,L'SPCLMSG                                                     
         FOUT  PBLMSGH,SPCLMSG,(R5)                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
USEPUBXT DS    0H                                                               
         XIT1                                                                   
*                                                                               
SPCLMSG  DC    C'** PUB DATA SHOWN - CHANGE AND/OR HIT ENTER TO ADD RECX        
               ORD'                                                             
*                                                                               
         SPACE 2                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  CONVERT 1-CHR "INTERNAL REPRESENTATION" OFFICE TO 2-CHR EQUIVALENT *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 2                                                                
OFCCHG   NTR1                                                                   
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,BYTE                                                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,(C'2',WORK),(0,ACOMFACS)                               
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
*******************************************************                         
       ++INCLUDE PUGENOLD                                                       
         ORG   IOAREA                                                           
         DS    CL500                                                            
         DS    D                                                                
ASYSPARS DS    A                                                                
         DS    A                                                                
         DS    CL50                SPARE WORK AREA                              
*                                                                               
         DS    CL30                                                             
PUBIO    DS    4000C                                                            
         ORG   PUBIO                                                            
*                                                                               
PUBARECD DSECT                                                                  
       ++INCLUDE PPUBAREC                                                       
         EJECT                                                                  
*                                                                               
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PUBSADD  DSECT                                                                  
       ++INCLUDE PUBSADEL                                                       
PUBWEBD  DSECT                                                                  
       ++INCLUDE PUBWEBEL                                                       
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBFCD          ADDRESS SCREEN                               
*                                                                               
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPPUB0C   09/18/08'                                      
         END                                                                    
