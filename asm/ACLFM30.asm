*          DATA SET ACLFM30    AT LEVEL 130 AS OF 05/01/02                      
*PHASE T60330A,+0                                                               
         TITLE 'CLIENT STATEMENT - DEFINITIONS'                                 
T60330   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,60330,R8,R7,RR=R6,CLEAR=YES                            
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
*                                                                               
         ST    R6,PRELO                                                         
         L     R1,=A(DEFTAB)                                                    
         A     R1,PRELO                                                         
         ST    R1,ADEFTAB                                                       
*                                                                               
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         MVC   SCANNER,CSCANNER    A(SCANNER)                                   
*                                                                               
         LA    R2,HDLCODEH         CODE MUST BE INPUT                           
         NI    LOGHEADH+1,X'F7'    UNHIGHLIGHT IT                               
         GOTO1 ANY                                                              
*                                                                               
         CLI   CSACTN,0            FIRST TIME                                   
         BNE   RS03                                                             
         BAS   RE,LEDGSTUC                                                      
         MVI   CSACTN,HDL          SET HEADLINE DATA                            
         B     BLDKEY              AND VALIDATE KEY                             
*                                                                               
RS03     TM    CSACTN,HLP          IF LAST ACTION WAS  "HELP"                   
         BNO   RS05                                                             
         CLI   PFKEY,PFRTN         ONLY VALID PF KEY IS RETURN                  
         BNE   INVLDHP             INVALID INPUT FOR HELP                       
         NI    CSACTN,X'FF'-HLP    TURN-OFF THE HELP                            
         BAS   RE,RSCRN            RESTORE SAVED SCREEN                         
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(20),=CL20'RECORD DISPLAYED'                              
         L     R2,RTNCURSE         SET CURSOR TO SAME FIELD                     
         AR    R2,RA                                                            
         B     EXIT                                                             
         SPACE 1                                                                
RS05     CLI   PFKEY,0             DID THEY USE PFKEY                           
         BE    BLDKEY              NO,  OK TO PROCESS MODES                     
         EJECT                                                                  
*                                                                               
*              VALIDATE PF KEYS                                                 
*                                                                               
VALPF    LA    R1,PFTAB            FIND CODE FOR LAST ACTION                    
         L     RF,ACURSOR                                                       
         SR    RF,RA                                                            
         ST    RF,RTNCURSE         SAVE CURRENT CURSOR ADDRESS                  
*                                                                               
VALPF02  CLC   CSACTN,0(R1)                                                     
         BE    VALPF04                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   VALPF02                                                          
         DC    H'0'                INVALID CSACTN CODE                          
*                                                                               
VALPF04  SR    R2,R2                                                            
         ICM   R2,7,1(R1)          R2=DISPLACEMENT TO TABLE OF                  
         AR    R2,RB                  KEY/ACTION                                
*                                                                               
VALPF06  CLC   0(1,R2),PFKEY       MATCH PFKEY TO TABLE                         
         BE    VALPF08                                                          
         LA    R2,5(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   VALPF06                                                          
         B     INVLDPF             INVALID PF KEY                               
*                                                                               
VALPF08  MVC   SVPFKEY,PFKEY       SAVE PFKEY LAST ENTERED                      
         CLI   1(R2),0             SEE IF WE CHANGED SCREENS                    
         BE    VALPF10             NO, SO KEEP SAME ACTION                      
         BAS   RE,SSCRN            SAVE THE CURRENT CCREEN                      
         MVC   CSACTN,1(R2)        NEW ACTION                                   
         BAS   RE,GSCRN            GET NEW SCREEN                               
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
VALPF10  MVI   PFKEY,0             CLEAR KEY FOR NEXT TIME                      
         SR    RF,RF               RF TO A(DISPLAY ROUTINE)                     
         ICM   RF,7,2(R2)                                                       
         AR    RF,RB                                                            
         BR    RF                                                               
         EJECT                                                                  
*              BUILD LEDGER INFORMATION INTO DATATABLE                          
         SPACE 2                                                                
LEDGSTUC NTR1                                                                   
         USING ACKEYD,R4                                                        
         LA    R4,IO2                                                           
         MVC   ACKEYACC(ACLENGTH-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),COMPANY                                              
         MVC   ACKEYACC+1(2),=C'SR'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         MVI   ERROR,LDGINVAL                                                   
         B     LDGSTR99                                                         
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,ACHRELQ      HIERARCHY ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     LDGSTR99                                                         
*                                                                               
         USING ACHEIRD,R6                                                       
         MVC   LEVA,ACHRLEVA                                                    
         MVC   LEVANAME,ACHRDESA                                                
         MVC   LEVB,ACHRLEVB                                                    
         MVC   LEVBNAME,ACHRDESB                                                
         MVC   LEVC,ACHRLEVC                                                    
         MVC   LEVCNAME,ACHRDESC                                                
         MVC   LEVD,ACHRLEVD                                                    
         MVC   LEVDNAME,ACHRDESD                                                
LDGSTR99 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 2                                                                
BLDKEY   CLI   MODE,BUILDKEY                                                    
         BNE   BLDHDL                                                           
*                                                                               
         LA    R2,HDLCODEH                                                      
         USING ACKEYD,R4                                                        
         LA    R4,KEY              BUILD KEY FOR C.S. REOCRD                    
         MVC   KEY,SPACES                                                       
         MVI   ACCSTYPE,ACCSEQU                                                 
         MVI   ACCSSREC,ACCSSEQU                                                
         MVC   ACCSCMP,COMPANY                                                  
         MVC   ACCSCDE,HDLCODE                                                  
         OC    ACCSCDE,SPACES                                                   
*                                                                               
         TM    HDLCODEH+4,X'20'    CODE NOT CHANGED                             
         BO    EXIT                                                             
*                                                                               
         MVI   ANYKEY,C'Y'         KEY CHANGE                                   
         OI    HDLCODEH+4,X'20'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              BUILD HEADLINE ELEMENTS                                          
*                                                                               
BLDHDL   DS    0H                                                               
         CLI   MODE,BUILDREC                                                    
         BNE   DSPHDL                                                           
         LA    R2,HDLNMEH                                                       
         CLI   HDLNMEH+5,0                                                      
         BE    BLDHD01             NAME HAS NOT BEEN INPUT                      
         GOTO1 NAMIN               GET FORMAT NAME                              
*                                                                               
BLDHD01  TM    CSACTN,HDL          BUILDING HEADLINE                            
         BNO   BLDROW                                                           
*                                                                               
         LA    R4,IO2              REMOVE HEADLINE ELEMENTS                     
         LA    R5,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
         USING RSHD,R5                                                          
BLDH03   CLI   0(R5),0             END OF RECORD                                
         BE    BLDH10                                                           
         CLI   RSHEL,RSHELQ        IS IT A HEADLINE ELEMENT                     
         BNE   *+8                                                              
         MVI   RSHEL,X'FF'         MARK FOR DELETE                              
         IC    R1,RSHLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     BLDH03                                                           
*                                                                               
BLDH10   GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         LA    R3,HEADFLD          LIST OF FIELD HEADERS                        
         SPACE 1                                                                
BLDH12   XR    R2,R2                                                            
         ICM   R2,7,0(R3)                                                       
         AR    R2,RA               R2=CURRENT FIELD HEADER                      
         CLI   5(R2),0                                                          
         BE    BLDH35              NO INPUT                                     
         XC    ELEMENT,ELEMENT                                                  
         LA    R5,ELEMENT                                                       
         MVI   RSHEL,RSHELQ        ELEMENT CODE                                 
         MVC   RSHTYPE,3(R3)       TYPE OF DATA                                 
         MVC   RSHSEQ,4(R3)        SEQUENCE                                     
         SPACE 1                                                                
         XR    R1,R1                                                            
         IC    R1,5(R2)            INPUT DATA LENGTH                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSHDATA(0),8(R2)    INPUT DATA TO ELEMENT                        
         LA    R1,RSHLENQ+1(R1)    ADD STANDARD LENGTH TO DATA LENGTH           
         STC   R1,RSHLEN           ELEMENT LENGTH                               
         CLI   8(R2),C'&&'         DATA DEFINITION                              
         BE    BLDH20                                                           
         OI    RSHFRM,RSHFREE      FREE FORM                                    
         B     BLDH30                                                           
         SPACE 1                                                                
BLDH20   OI    RSHFRM,RSHDEF       DEFINITION                                   
         MVI   ERROR,INVALID       INVALID INPUT                                
         IC    R1,5(R2)            INPUT DATA LENGTH                            
         SH    R1,=H'2'            LENGTH FOR EX INSTR.                         
         CH    R1,=H'1'            CODE MUST BE AT LEAST 2                      
         BL    EXIT                                                             
         CH    R1,=H'3'            NOT MORE THAN 4                              
         BH    EXIT                                                             
*                                                                               
         USING DEFTD,R6                                                         
         L     R6,ADEFTAB          DEFINITION TABLE                             
         MVC   FINDDEF,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FINDDEF(0),RSHDATA+1                                             
         CLC   FINDDEF(3),=CL3'BLK'       HARD CODE FOR SPECIAL CASE            
         BNE   BLDH23                     CONTINUE AS NORMAL                    
         CLI   FINDDEF+3,C'0'             IS THE NUMBER ZERO                    
         BE    INVLDNU                    YES, SO ERROR ONLY ALLOW 1-9          
         TM    FINDDEF+3,X'F0'            IS IT A NUMBER CHARACTER 1-9?         
         BO    BLDH30                     IT OK, ADD THIS ELEMENT               
         MVI   ERROR,INVALID                                                    
         B     INVLDNU                                                          
*                                                                               
BLDH23   EQU   *                                                                
         CLC   FINDDEF,DEFCDE                 DOES CODE MATCH?                  
         BNE   BLDH26                                                           
         TM    DEFIND,HDL          IS IT VALID FOR HEADLINES                    
         BZ    BLDH26                                                           
         B     BLDH30              VALID, GET NEXT SCREEN LINE                  
*                                                                               
BLDH26   SR    RF,RF                                                            
         IC    RF,DEFLEN           LENGTH OF THIS ENTRY                         
         AR    R6,RF               R6 TO NEXT ENTRY                             
         CLI   0(R6),X'FF'                                                      
         BNE   BLDH23                                                           
         B     INVLDHT            ERROR - END OF DEFINITIONS - NO MATCH         
*                                                                               
BLDH30   GOTO1 ADDANEL                                                          
BLDH35   LA    R3,5(R3)           R3 TO NEXT HEADFLD ENTRY                      
         CLI   0(R3),X'FF'                                                      
         BNE   BLDH12                                                           
         MVI   ERROR,X'FF'        END OF SCREEN ENTRIES  - NO ERRORS            
         LA    R2,HDLCODEH                                                      
         CLI   LOGACT,C'N'         IF ACTION IS NEW                             
         BNE   EXIT                                                             
         MVC   LOGACT(5),=C'AMEND' CHANGE IT TO AMEND FOR NEXT TIME             
         OI    LOGACTH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
*              BUILD ROW ELEMENTS                                               
         SPACE 1                                                                
BLDROW   TM    CSACTN,ROW          BUILDING HEADLINE                            
         BNO   BLDCOL                                                           
*                                                                               
         LA    R4,IO2              REMOVE OLD ROW ELEMENTS                      
         LA    R5,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
         USING RSRD,R5                                                          
BLDR03   CLI   0(R5),0             END OF RECORD                                
         BE    BLDR10                                                           
         CLI   RSREL,RSRELQ        IS IT A ROW ELEMENT                          
         BNE   *+8                                                              
         MVI   RSREL,X'FF'                                                      
         IC    R1,RSRLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     BLDR03                                                           
*                                                                               
BLDR10   GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         LA    R3,ROWFRSTH         R3 TO ROW LINE                               
         USING ROWD,R3                                                          
         LA    R0,1                ROW NUMBER                                   
         MVI   FLAG1,0                                                          
*                                                                               
BLDR12   LA    R2,ROWDATAH         ROW DATA                                     
         CLI   ROWDATAH+5,0                                                     
         BNE   BLDR20                                                           
         MVI   ERROR,INVALID       ANY INPUT AFTER THE FIRST BLANK              
         LA    RF,ROWTABH          DATA FIELD IS INVALID                        
         SR    R1,R1                                                            
*                                                                               
BLDR14   IC    R1,0(R2)                                                         
         AR    R2,R1               R2 TO NEXT FIELD HEADER                      
         CR    R2,RF                                                            
         BNL   BLDR50              END OF SCREEN, NO ERRORS                     
         CLI   5(R2),0                                                          
         BNE   EXIT                ERROR, INVALID INPUT                         
         B     BLDR14                                                           
*                                                                               
BLDR20   XC    ELEMENT,ELEMENT                                                  
         LA    R5,ELEMENT                                                       
         MVI   FNDX,1                                                           
         MVI   RSREL,RSRELQ        ELEMENT CODE                                 
         MVI   RSRLEN,RSRLENQ      LENGTH                                       
         STC   R0,RSRSEQ           SEQUENCE                                     
*                                                                               
         MVC   RSRDATA,ROWDATA     INPUT DATA TO ELEMENT                        
         OC    RSRDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,ROWDATAH,(4,BLOCK)                                  
*                                                                               
         MVC   INPRMS,DMCB+4                                                    
         LA    R4,BLOCK                                                         
         CLI   0(R4),1             CODE MUST BE AT LEAST 2                      
         BL    EXIT                                                             
         CLI   0(R4),4             NOT MORE THAN 4                              
         BH    EXIT                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(R4)            GET LENGTH OF FIRST PARAM                    
         BCTR  R1,0                LENGTH FOR EX INSTR.                         
         USING DEFTD,R6                                                         
         L     R6,ADEFTAB          DEFINITION TABLE                             
         MVC   FINDDEF,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FINDDEF(0),12(R4)                                                
*                                                                               
BLDR23   EQU   *                                                                
         CLC   FINDDEF,DEFCDE      DOES CODE MATCH?                             
         BNE   BLDR26                                                           
         TM    DEFIND,ROW          IS IT VALID FOR ROW                          
         BO    BLDR30              VALID ROW DATA                               
*                                                                               
BLDR26   SR    RF,RF                                                            
         IC    RF,DEFLEN           LENGTH OF THIS ENTRY                         
         AR    R6,RF               R6 TO NEXT ENTRY                             
         CLI   0(R6),X'FF'                                                      
         BNE   BLDR23                                                           
         B     INVLDRT             ERROR-END OF DEFINITIONS - NO MATCH          
*                                                                               
BLDR30   EQU   *                                                                
         LA    R2,ROWDATAH         COLUMN DATA                                  
         IC    R1,INPRMS           INPUT PARAMETERS                             
         SR    RF,RF                                                            
         IC    RF,DEFRPMX          MAX. THAT S/B INPUT                          
         SR    R1,RF                                                            
         BNP   *+8                                                              
         B     INVLDP2             INVALID NUMBER OF PARAMETERS                 
*                                                                               
         SR    R1,R1               CLEAR FOR USE IN VALIDATION                  
         MVI   ERROR,X'FF'                                                      
         BAS   RE,VALPARM          VALIDATION ROUTINE FOR PARAMETERS            
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                                                             
         OC    DEFRVAL,DEFRVAL     SPECIAL VALIDATION ROUTINE                   
         BZ    BLDR33                                                           
         SR    RF,RF                                                            
         ICM   RF,7,DEFRVAL                                                     
         AR    RF,RB               A(VALIDATION ROUTINE)                        
         BASR  RE,RF                                                            
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                ERROR IN INPUT PARAMETERS                    
*                                                                               
BLDR33   EQU   *                                                                
         OC    RSROPT,FLAGBIT1     TURN ON CODE/NAME/BOTH                       
         LA    R2,ROWTYPEH         TYPE (L,C,R)-HEADLINE, M-MIDELINE            
         MVI   ERROR,INVALID                                                    
         MVI   FNDX,1                                                           
         MVC   RSRTYPE,ROWTYPE                                                  
         CLI   ROWTYPEH+5,0                                                     
         BE    BLDR36                                                           
         CLI   ROWTYPE,RSRMID      IS IT A MIDLINE?                             
         BNE   BLDR34                                                           
         MVI   FLAG1,1             SO FLAG FOR NO MORE HEADS ALLOWED            
         B     BLDR36                                                           
*                                                                               
BLDR34   EQU   *                                                                
         CLI   ROWTYPE,RSRLHEAD    PRINT AS LEFT HEAD?                          
         BE    *+8                                                              
         CLI   ROWTYPE,RSRCHEAD    PRINT AS CENTER HEAD?                        
         BE    *+8                                                              
         CLI   ROWTYPE,RSRRHEAD    PRINT AS RIGHT HEAD?                         
         BNE   BLDR35                                                           
         CLI   FLAG1,0             HAVE WE REACHED A MIDLINE YET?               
         BNE   INVLDTY             YES SO HEADLINE IS ILLEGAL                   
         OI    RSROPT,RSRPAGE      PAGE ON HEAD TYPE                            
         B     BLDR36                                                           
*                                                                               
BLDR35   EQU   *                                                                
         CLI   ROWTYPE,C' '                                                     
         BNE   EXIT                                                             
*                                                                               
BLDR36   LA    R2,ROWTOTLH         TOTAL Y OR S / OR N                          
         MVI   ERROR,INVALID                                                    
         CLI   ROWTOTLH+5,0                                                     
         BE    BLDR40                                                           
         CLI   ROWTOTL,C'N'                                                     
         BE    BLDR40                                                           
         OI    RSROPT,RSRTOT       MUST WANT TOTALS                             
         CLI   ROWTOTL,C'Y'                                                     
         BE    BLDR40                                                           
         OI    RSROPT,RSRTOTSP                                                  
         CLI   ROWTOTL,C'S'        TOTAL BUT ON SEPERATE PAGE                   
         BNE   EXIT                                                             
*                                                                               
BLDR40   GOTO1 ADDANEL                                                          
         LA    R3,ROWLNQ(R3)       R3=NEXT ROW LINE                             
BLDR41   AH    R0,=H'1'            INCREMENT SEQUENCE NUMBER                    
         LA    RF,ROWTABH                                                       
         CR    R3,RF                                                            
         BL    BLDR12                                                           
         SPACE 1                                                                
BLDR50   MVI   ERROR,X'FF'        END OF SCREEN ENTRIES  - NO ERRORS            
         LA    R2,HDLCODEH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                BUILD COLUMN ELEMENTS                                          
         SPACE 1                                                                
BLDCOL   TM    CSACTN,COL          BUILDING COLUMN ELEMENTS                     
         BNO   BLDPRF                                                           
*                                                                               
         LA    R4,IO2              REMOVE OLD COLUMN LEMENTS                    
         LA    R5,ACRECORD                                                      
         SR    R1,R1                                                            
         XC    COLARRY,COLARRY     TO STORE SORT SEQUENCE                       
         MVI   COLARRY,C'*'                                                     
*                                                                               
         USING RSCD,R5                                                          
BLDC03   CLI   0(R5),0             END OF RECORD                                
         BE    BLDC10                                                           
         CLI   RSCEL,RSCELQ        IS IT A COLUMN ELEMENT                       
         BNE   *+8                                                              
         MVI   RSCEL,X'FF'                                                      
         IC    R1,RSCLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     BLDC03                                                           
*                                                                               
BLDC10   GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         LA    R0,1                COLUMN ROW NUMBER                            
         MVI   TWIDTH,1            CONTAINS TOTAL WIDTH                         
         LA    R3,COLFRSTH         R3 TO COLUMN DATA                            
         CLI   SVPFKEY,PFDEL       IF DELETE OR                                 
         BE    BLDC11                                                           
         CLI   SVPFKEY,PFINS       INSERT WAS PRESSED                           
         BNE   BLDC12                                                           
BLDC11   L     RF,RTNCURSE         WAS IT IN A COLUMN LINE?                     
         AR    RF,RA                                                            
         CR    RF,R3                                                            
         BL    INVLDPF                                                          
         USING COLD,R3                                                          
*                                                                               
BLDC12   LA    R2,COLDATAH         COLUMN DATA                                  
         CLI   COLDATAH+5,0                                                     
         BNE   BLDC20                                                           
         MVI   ERROR,INVALID       ANY INPUT AFTER THE FIRST BLANK              
         LA    RF,COLLNQ(R3)       DATA FIELD IS INVALID                        
         SR    R1,R1                                                            
*                                                                               
BLDC14   IC    R1,0(R2)                                                         
         AR    R2,R1               R2 TO NEXT FIELD HEADER                      
         CR    R2,RF                                                            
         BNL   BLDC45              END OF SCREEN, NO ERRORS                     
         CLI   5(R2),0                                                          
         BNE   EXIT                ERROR, INVALID INPUT                         
         B     BLDC14                                                           
*                                                                               
BLDC20   EQU   *                                                                
         CLI   SVPFKEY,PFDEL                                                    
         BNE   BLDC22                                                           
         L     RF,RTNCURSE                                                      
         LR    R6,R3                                                            
         AR    RF,RA                                                            
         CR    RF,R6                                                            
         BL    BLDC22                                                           
         LA    R6,COLLNQ(R6)                                                    
         CR    RF,R6                                                            
         BH    BLDC22                                                           
         TWAXC (R3),(R6),PROT=N                                                 
         MVI   SVPFKEY,0                                                        
         B     BLDC45                                                           
BLDC22   XC    ELEMENT,ELEMENT                                                  
         LA    R5,ELEMENT                                                       
         MVI   FNDX,1              INITIALIZE TO FIRST PARAMETER                
         MVI   RSCEL,RSCELQ        ELEMENT CODE                                 
         STC   R0,RSCSEQ           SEQUENCE                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
*              CHANGE DELIMITERS IN SCANNER JUST TO COMMAS                      
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),C',=,:'                              
         SR    R1,R1                                                            
         IC    R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    EXIT                INVALID INPUT                                
         STC   R1,INPRMS           NUMBER OF INPUT PARAMETERS                   
         LA    RF,BLOCK                                                         
         CLI   1(RF),0                                                          
         BNE   EXIT                                                             
         IC    R1,0(RF)                                                         
         BCTR  R1,0                LENGTH FOR EX INSTR.                         
         CH    R1,=H'0'            CODE MUST BE AT LEAST 1                      
         BL    EXIT                                                             
         CH    R1,=H'3'            NOT MORE THAN 4                              
         BH    EXIT                                                             
*                                                                               
         MVC   RSCDATA,COLDATA     INPUT DATA TO ELEMENT                        
         OC    RSCDATA,SPACES                                                   
*                                                                               
         USING DEFTD,R6                                                         
         L     R6,ADEFTAB          DEFINITION TABLE                             
         MVC   FINDDEF,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FINDDEF(0),RSCDATA                                               
*                                                                               
BLDC23   EQU   *                                                                
         CLC   FINDDEF,DEFCDE      DOES CODE MATCH?                             
         BNE   BLDC26                                                           
         TM    DEFIND,COL          IS IT VALID FOR COLUMN                       
         BO    BLDC30              VALID COLUMN DATA                            
*                                                                               
BLDC26   SR    RF,RF                                                            
         IC    RF,DEFLEN           LENGTH OF THIS ENTRY                         
         AR    R6,RF               R6 TO NEXT ENTRY                             
         CLI   0(R6),X'FF'                                                      
         BNE   BLDC23                                                           
         B     INVLDCT             ERROR-END OF DEFINITIONS - NO MATCH          
*                                                                               
BLDC30   EQU   *                                                                
         LA    R2,COLSORTH                                                      
         CLI   COLSORTH+5,0       IS THERE INPUT?                               
         BE    BLDC31             NO, SO IT IS OK BY DEFAULT                    
         MVI   ERROR,NOTNUMRC     IS IT NUMERIC                                 
         TM    COLSORTH+4,X'08'                                                 
         BZ    EXIT               NO GET OUT                                    
         TM    DEFIND,ROW         CAN WE SORT ON THIS COLUMN?                   
         BZ    INVLDS1            NO, INVALID SORT COLUMN                       
*                                                                               
BLDC30B  EQU   *                                                                
         SR    R1,R1                                                            
         IC    R1,COLSORTH+5      CONVERT SORT NUBMER TO BINARY                 
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,COLSORT(0)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=Y(MAXCOLS)      CAN'T BE HIGHER THAN NUMBER OF COLS          
         BH    INVLDS2             SORT NUMBER TO HIGH                          
         LA    RF,COLARRY(R1)      POINT TO COLARRY OF SORTNUM                  
         CLI   0(RF),C'*'          IF NOT ALREADY MARKED                        
         BE    INVLDS3             DUPLICATE SORT NUMBER                        
         MVI   0(RF),C'*'          MARK IT WITH A '*'                           
         STC   R1,RSCSORTN         MOVE NUMBER INTO ELEMENT                     
*                                                                               
BLDC31   EQU   *                                                                
         LA    R2,COLDATAH         COLUMN DATA                                  
         IC    R1,INPRMS           INPUT PARAMETERS                             
         SR    RF,RF                                                            
         IC    RF,DEFCPMX          MAX. THAT S/B INPUT                          
         SR    R1,RF                                                            
         BNP   BLDC32                                                           
         MVC   FNDX,INPRMS         TOO MANY PARAMS                              
         B     INVLDP2             INVALID NUMBER OF PARAMETERS                 
*                                                                               
BLDC32   SR    R1,R1               CLEAR FOR USE IN VALIDATION                  
         MVI   ERROR,X'FF'                                                      
         BAS   RE,VALPARM          VALIDATION ROUTINE FOR PARAMETERS            
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                                                             
         MVI   PCTCOL,C'N'         SET FLAG OFF AS DEFAULT                      
         OC    DEFCVAL,DEFCVAL     SPECIAL VALIDATION ROUTINE                   
         BZ    BLDC33                                                           
         SR    RF,RF                                                            
         ICM   RF,7,DEFCVAL                                                     
         AR    RF,RB               A(VALIDATION ROUTINE)                        
         BASR  RE,RF                                                            
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                ERROR IN INPUT PARAMETERS                    
*                                                                               
BLDC33   EQU   *                                                                
         OC    RSCOPT,DEFCST       MOVE IN COLUMN STATUS                        
         CLI   PCTCOL,C'Y'         WAS THIS A PERCENT COL?                      
         BE    *+8                                                              
         NI    RSCOPT,X'FF'-RSCPCT NO, SO TURN OFF FLAG FOR PERCENT             
         OC    RSCOPT2,FLAGBIT1    IF FLAGS SET IN VALIDATION ROUTINE           
         OC    RSCOPT,FLAGBIT2     IF TYPE SET IN VALIDATION ROUTINE            
         OC    RSCDTEFG,FLAGBIT3   IF DATES SET IN VALIDATION ROUTINE           
*                                                                               
         LA    R2,COLPARAH                                                      
         TM    RSCOPT,ACUM         IS IT AN ACCUMULATED COL?                    
         BNZ   BLDC33A                                                          
         CLI   COLPARAH+5,0                                                     
         BE    BLDC33C                                                          
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
BLDC33A  TM    RSCOPT,CALC         IS IT A CALCULATED COL?                      
         BZ    BLDC33B                                                          
         CLI   COLPARAH+5,0                                                     
         BE    BLDC33C                                                          
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
BLDC33B  LA    R2,COLPARAH                                                      
         OI    RSCOPT,BYBILL                                                    
         BAS   RE,VALDATE                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                ERROR IN INPUT PARAMETERS                    
*                                                                               
BLDC33C  CLI   COLHIDE,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSCOPT,HIDE                                                      
*                                                                               
         LA    R2,COLWIDEH         WIDTH                                        
         CLI   COLWIDEH+5,0                                                     
         BNE   BLDC33D                                                          
         MVI   RSCWDTH,36                                                       
         CLI   RSCSEQ,1                                                         
         BE    BLDC34                                                           
         TM    RSCOPT,DNAME+DCODE                                               
         BO    BLDC34                                                           
         MVC   RSCWDTH,DEFWTH      MOVE IN DEFAULT WIDTH                        
         B     BLDC34                                                           
*                                                                               
BLDC33D  MVI   ERROR,NOTNUMRC                                                   
         TM    COLWIDEH+4,X'08'    MUST BE NUMERIC                              
         BNO   EXIT                                                             
         SR    R1,R1                                                            
         IC    R1,COLWIDEH+5       GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,COLWIDE(0)      PACK IT                                      
         CVB   R1,DUB                                                           
         STC   R1,RSCWDTH          SAVE WIDTH IN ELEMENT                        
*                                                                               
BLDC34   TM    RSCOPT,HIDE                                                      
         BNZ   BLDC36                                                           
         SR    R1,R1                                                            
         IC    R1,RSCWDTH                                                       
         SR    R4,R4                                                            
         IC    R4,TWIDTH                                                        
         LA    R4,1(R1,R4)         ADD UP TOTAL WIDTH                           
         STC   R4,TWIDTH                                                        
*                                                                               
BLDC36   EQU   *                                                                
         LA    R2,COLTOTLH         TOTAL Y OR N                                 
         MVI   ERROR,INVALID                                                    
         CLI   COLTOTLH+5,0                                                     
         BE    BLDC38                                                           
         TM    DEFCST,ACUM         CAN'T TOTAL ON COL                           
         BNZ   EXIT                                                             
         CLI   COLTOTL,C'N'                                                     
         BE    BLDC38                                                           
         CLI   COLTOTL,C'Y'                                                     
         BNE   EXIT                                                             
         OI    RSCOPT2,RSRTOT                                                   
*                                                                               
BLDC38   MVC   RSCHDL1,COLHED1     COLUMN HEADS 1                               
         OC    RSCHDL1,SPACES                                                   
         MVC   RSCHDL2,COLHED2     AND 2                                        
         OC    RSCHDL2,SPACES                                                   
         MVI   RSCLEN,RSCLNQ2      LENGTH WITH 2 HEADINGS                       
         CLC   RSCHDL2,SPACES                                                   
         BNE   BLDC40                                                           
         MVI   RSCLEN,RSCLNQ1      LENGTH WITH 1 HEADING                        
         CLC   RSCHDL1,SPACES                                                   
         BNE   BLDC40                                                           
         MVI   RSCLEN,RSCLNQ       LENGTH WITH NO HEADINGS                      
         SPACE 1                                                                
BLDC40   GOTO1 ADDANEL                                                          
         AH    R0,=H'1'            INCREMENT SEQUENCE NUMBER                    
BLDC45   LA    R3,COLLNQ(R3)       R3=NEXT COLUMN LINE                          
         LA    RF,COLTABH                                                       
         CR    R3,RF                                                            
         BL    BLDC12                                                           
         SPACE 1                                                                
BLDC50   EQU   *                                                                
         LA    RF,COLARRY                                                       
         LA    R1,MAXCOLS                                                       
         LA    R3,COLFRSTH         ADDR TO PLACE CURSIR FOR ERROR               
         LA    R2,COLSORTH                                                      
         SPACE 1                                                                
BLDC52   EQU   *                                                                
         CLC   0(1,RF),1(RF)                                                    
         BE    BLDC54                                                           
         CLI   1(RF),C'*'                                                       
         BE    INVLDS4             INVALID SORT SEQ.                            
         SPACE 1                                                                
BLDC54   EQU   *                                                                
         LA    RF,1(RF)                                                         
         BCT   R1,BLDC52                                                        
         SPACE 1                                                                
         OI    COLTWDTH+6,X'80'    TRANSMIT                                     
         SR    R4,R4                                                            
         IC    R4,TWIDTH           GET TOTAL WIDTH FOR REPORT                   
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  COLTWDT,DUB         UNPACK TO SCREEN                             
         CLI   COLTWDT,C'0'                                                     
         BNE   *+8                                                              
         MVI   COLTWDT,C' '                                                     
         CLC   COLTWDT+1(2),=C'00'                                              
         BNE   DSPCOL                                                           
         MVI   COLTWDT+1,C' '                                                   
         B     DSPCOL                                                           
         EJECT                                                                  
BLDPRF   TM    CSACTN,PRF          BUILDING PROFILE OPTION FOR FORMAT           
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO2              REMOVE PROFILE FORMAT ELEMENT                
         LA    R5,ACRECORD                                                      
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         XC    COLARRY2,COLARRY2   CLEAR OUT ARRAY INFO ON COL OPTINS           
         MVI   TOTROWS,0                                                        
*                                                                               
         USING RSPD,R5                                                          
BLDP03   EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    BLDP20              NO ELEMENT FOUND SO BUILD ONE                
*                                                                               
         USING RSRD,R5                                                          
         CLI   RSREL,RSRELQ        IS AT ROW ELEMENT                            
         BNE   *+10                                                             
         MVC   TOTROWS,RSRSEQ       GET LAST SEQ NUMBER                         
*                                                                               
         USING RSCD,R5                                                          
         CLI   RSCEL,RSCELQ        IS AT COL ELEMENT                            
         BNE   BLDP06                                                           
         IC    R1,RSCSEQ                                                        
         LA    RF,COLARRY2(R1)     POINT INTO ARRAY,COL NUM IS INDEX            
         OC    0(1,RF),RSCOPT      SAVE COLUMN OPTIONS                          
*                                                                               
BLDP06   EQU   *                                                                
         USING RSPD,R5                                                          
         CLI   RSPEL,RSPELQ        IS IT A PROFILE FORMAT ELEMENT?              
         BE    BLDP10                                                           
         IC    R1,RSPLEN                                                        
         AR    R5,R1                                                            
         B     BLDP03              GET NEXT                                     
*                                                                               
BLDP10   EQU   *                                                                
         MVI   RSPEL,X'FF'         TO DELETE ELEMENT                            
         GOTO1 REMANEL,DMCB,(X'FF',0)                                           
*                                                                               
BLDP20   EQU   *                                                                
         LA    R5,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   RSPEL,RSPELQ        ELEMENT CODE                                 
         MVI   RSPLEN,RSPLENQ      ELEMENT LENGTH                               
*                                                                               
         LA    R2,PRFLFJTH                                                      
         CLI   PRFLFJT,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPPOPT,RSPLFJT     LEFT JUSTIFY REPORT                          
*                                                                               
         LA    R2,PRFBOXH                                                       
         CLI   PRFBOX,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSPPOPT,RSPBOX      TURN ON BOXES                                
*                                                                               
         LA    R2,PRFIACTH                                                      
         CLI   PRFIACT,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPPOPT,RSPIACT     TURN ON INACTIVE ACCT. PRINT                 
*                                                                               
         LA    R2,PRFRTOTH                                                      
         CLI   PRFRTOT,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPPOPT,RSPRTOT     TURN ON REDUNDANT TOTAL OPT.                 
*                                                                               
         LA    R2,PRFRDATH                                                      
         CLI   PRFRDAT,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPPOPT,RSPRDAT     TURN ON REDUNDANT DATA IN COL OPT            
*                                                                               
         LA    R2,PRFPCMAH                                                      
         CLI   PRFPCMA,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPEDOPT,RSPEDCMA   TURN ON TO PRINT COMMAS                      
*                                                                               
         LA    R2,PRFPZAMH                                                      
         CLI   PRFPZAM,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPEDOPT,RSPEDZRO   TURN ON TO PRINT ZERO AMOUNTS                
*                                                                               
         LA    R2,PRFPZTOH                                                      
         CLI   PRFPZTO,C'Y'                                                     
         BNE   *+8                                                              
         OI    RSPPOPT,RSPZEROT    TURN ON TO PRINT ZERO TOTALS                 
*                                                                               
         LA    R2,PRFPMINH                                                      
         CLI   PRFPMIN,C'T'                                                     
         BNE   *+12                                                             
         OI    RSPEDOPT,RSPEDTRL   TRAILING MINUS ON                            
         B     BLDP25                                                           
         CLI   PRFPMIN,C'B'                                                     
         BNE   *+12                                                             
         OI    RSPEDOPT,RSPEDBKT   MINUS AS BRACKETS                            
         B     BLDP25                                                           
         CLI   PRFPMIN,C'L'                                                     
         BNE   *+12                                                             
         OI    RSPEDOPT,RSPEDLED   LEADING MINUS                                
         B     BLDP25                                                           
         OI    RSPEDOPT,RSPEDCR    MINUS AS CR                                  
         CLI   PRFPMIN,C'C'                                                     
         BE    BLDP25                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
BLDP25   LA    R2,PRFPCTSH                                                      
         MVC   RSPPCTS,PRFPCTS                                                  
         CLI   PRFPCTS,C'0'                                                     
         BE    BLDP30                                                           
         CLI   PRFPCTS,C'1'                                                     
         BE    BLDP30                                                           
         CLI   PRFPCTS,C'2'                                                     
         BE    BLDP30                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
BLDP30   EQU   *                                                                
         LA    R2,PRFRNDH                                                       
         MVC   RSPRND,PRFRND                                                    
         CLI   PRFRND,C'P'         ROUND IN PENNIES (NO ROUNDING)?              
         BE    BLDP40                                                           
         CLI   PRFRND,C'D'         ROUND IN DOLLARS?                            
         BE    BLDP40                                                           
         CLI   PRFRND,C'T'         ROUND IN THOUSANDS?                          
         BE    BLDP40                                                           
         CLI   PRFRND,C'M'         ROUND IN MILLIONS?                           
         BE    BLDP40                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
BLDP40   EQU   *                                                                
*        LA    R2,PRFUNTH          UNIT/LEDGER                                  
*        CLI   PRFUNTH+5,1         ANY UNIT INPUT?                              
*        BE    *+8                                                              
*        B     BLDP41                                                           
*        MVI   ERROR,INVALID       NO, INVALID INPUT                            
*        B     EXIT                                                             
*                                                                               
*LDP41   MVC   RSPUNT(2),PRFUNT                                                 
         MVC   RSPLIST,PRFLIST     GET LIST                                     
*                                                                               
         LA    R2,PRFRKONH         RANKING ON WHAT ROW LEVEL TOTAL              
         CLI   PRFRKONH+5,0        WAS THERE INPUT?                             
         BE    BLDP50                                                           
*                                                                               
         MVC   TEMPCHAR,PRFRKON+1                                               
         TM    TEMPCHAR,X'F0'      IS IT A NUMBER?                              
         BO    *+12                YES                                          
         MVI   ERROR,INVALID       NO, INVALID INPUT                            
         B     EXIT                                                             
*                                                                               
         NI    TEMPCHAR,X'0F'      CONVERT TO NUMBER                            
         SR    R1,R1               SAVE NUMBER IN R1                            
         IC    R1,TEMPCHAR                                                      
*                                                                               
         CLI   PRFRKON,C'R'        RANK ON ROW#                                 
         BE    BLDP45                                                           
         CLI   PRFRKON,C'C'        RANK ON COL#                                 
         BE    BLDP46                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
*                                                                               
BLDP45   EQU   *                                                                
         CLC   TEMPCHAR,TOTROWS    RANK UP TO N ROWS                            
         BH    INVLDRW                                                          
         B     BLDP48                                                           
*                                                                               
BLDP46   EQU   *                                                                
         LA    RF,COLARRY2(R1)     POINT INTO ARRAY FOR COL                     
         TM    0(RF),ACUM                                                       
         BNZ   INVLDCL             INVALID COLUMN TO RANK                       
*                                                                               
BLDP48   EQU   *                                                                
         MVC   RSPRKON,PRFRKON                                                  
         STC   R1,RSPRKON+1                                                     
*                                                                               
         LA    R2,PRFRKCLH         RANKING ON WHAT COLUMN                       
         MVI   ERROR,MISSING                                                    
         CLI   PRFRKCLH+5,0        WAS THERE INPUT?                             
         BE    EXIT                                                             
         MVI   ERROR,NOTNUMRC                                                   
         TM    PRFRKCLH+4,X'08'    IS IT NUMERIC?                               
         BZ    EXIT                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PRFRKCLH+5       LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PRFRKCL(0)                                                   
         CVB   R1,DUB                                                           
*                                                                               
         STC   R1,RSPRKCL                                                       
         LA    RF,COLARRY2(R1)     POINT TO COLUMN AND TEST SAVED FLAGS         
         TM    0(RF),ACUM                                                       
         BZ    INVLDCL                                                          
*                                                                               
         LA    R2,PRFRKORH         RANK ORDER (ACCENDING/DECENDING)             
         CLI   PRFRKOR,C'A'                                                     
         BE    BLDP49                                                           
         CLI   PRFRKOR,C'D'                                                     
         BE    BLDP49                                                           
         MVI   ERROR,INVALID                                                    
         B     EXIT                                                             
BLDP49   EQU   *                                                                
         MVC   RSPRKOR,PRFRKOR                                                  
*                                                                               
BLDP50   EQU   *                                                                
         GOTO1 ADDANEL                                                          
         MVI   ERROR,X'FF'                                                      
         LA    R2,HDLCODEH                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*              DISPLAY HEADLINE DATA                                            
         SPACE 2                                                                
DSPHDL   CLI   MODE,DSPLYREC                                                    
         BNE   XIT                                                              
         TWAXC HDLNMEH                                                          
         LA    R4,IO               HEADLINE ELEMENTS                            
         MVC   HDLCODE,ACCSCDE     CODE TO SCREEN                               
         OI    HDLCODEH+4,X'20'                                                 
         OI    HDLCODEH+6,X'80'                                                 
         LA    R2,HDLNMH                                                        
         GOTO1 NAMOUT              FORMAT NAME                                  
         SPACE 1                                                                
         TM    CSACTN,HDL          DISPLAY HEADLINE                             
         BNO   DSPROW                                                           
         SPACE 1                                                                
         LA    R5,ACRECORD                                                      
         SPACE 1                                                                
         USING RSHD,R5                                                          
DSPH03   CLI   0(R5),0             END OF RECORD                                
         BE    DSPH30                                                           
         CLI   RSHEL,RSHELQ        IS IT A HEADLINE ELEMENT                     
         BE    DSPH10                                                           
         SPACE 1                                                                
DSPH04   SR    R1,R1                                                            
         IC    R1,RSHLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     DSPH03                                                           
         SPACE 1                                                                
DSPH10   LA    R3,HEADFLD          LIST OF FIELD HEADERS                        
         SPACE 1                                                                
DSPH12   CLC   RSHTYPE,3(R3)       MATCH TYPE OF DATA                           
         BNE   DSPH15                                                           
         CLC   RSHSEQ,4(R3)        AND SEQUENCE                                 
         BNE   DSPH15                                                           
         XR    R2,R2                                                            
         ICM   R2,7,0(R3)                                                       
         AR    R2,RA               R2=CURRENT FIELD HEADER                      
         SR    R1,R1                                                            
         IC    R1,RSHLEN                                                        
         LA    R0,RSHLENQ                                                       
         AH    R0,=H'1'                                                         
         SR    R1,R0               R1=DATA LENGTH FOR EX INSTR.                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RSHDATA     DATA SCREEN                                  
         B     DSPH04                                                           
         SPACE 1                                                                
DSPH15   LA    R3,5(R3)            R3=NEXT HEADFLD ENTRY                        
         CLI   0(R3),X'FF'                                                      
         BNE   DSPH12                                                           
         DC    H'0'                BAD ELEMENT                                  
         SPACE 1                                                                
DSPH30   MVI   ERROR,X'FE'         END OF SCREEN ENTRIES  - NO ERRORS           
         OI    LOGHEADH+1,X'08'    HIGHLIGHT IT                                 
         MVC   LOGHEAD(14),=CL14'HEADER SCREEN'                                 
         LA    R2,HDLCODEH                                                      
         CLI   LOGACT,C'A'                                                      
         BNE   EXIT                                                             
         MVC   LOGHEAD+14(14),=CL14'-ENTER CHANGES'                             
         LA    R2,HDLTITLH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY ROW DATA                                                 
*                                                                               
DSPROW   TM    CSACTN,ROW          DISPLAY ROW DATA                             
         BNO   DSPCOL                                                           
*                                                                               
         LA    R4,IO               ROW ELEMENTS                                 
         LA    R5,ACRECORD                                                      
         USING ROWD,R3                                                          
         LA    R3,ROWFRSTH         R3 TO FIRST ROW DATA                         
*                                                                               
         USING RSRD,R5                                                          
DSPR03   CLI   0(R5),0             END OF RECORD                                
         BE    DSPR30                                                           
         CLI   RSREL,RSRELQ        IS IT ROW DATA                               
         BE    DSPR10                                                           
*                                                                               
DSPR04   SR    R1,R1                                                            
         IC    R1,RSRLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     DSPR03                                                           
*                                                                               
DSPR10   MVC   ROWDATA(L'RSRDATA),RSRDATA             DATA TO SCREEN            
         MVC   ROWTYPE,RSRTYPE     (R,C,L)-HEADLINE, M-MIDLINE,BLANK            
         MVI   ROWTOTL,C'N'                                                     
         TM    RSROPT,RSRTOT                                                    
         BZ    DSPR20                                                           
         MVI   ROWTOTL,C'Y'        TOTAL OPTION                                 
         TM    RSROPT,RSRTOTSP                                                  
         BZ    DSPR20                                                           
         MVI   ROWTOTL,C'S'        TOTAL OPTION ON SEPERATE PAGE                
*                                                                               
DSPR20   LA    R3,ROWLNQ(R3)       LOOP TO NEXT ELEMENT                         
         B     DSPR04                                                           
*                                                                               
DSPR30   MVI   ERROR,X'FE'        END OF SCREEN ENTRIES  - NO ERRORS            
         OI    LOGHEADH+1,X'08'    HIGHLIGHT IT                                 
         MVC   LOGHEAD(12),=CL12'ROWS SCREEN'                                   
         LA    R2,HDLCODEH                                                      
         CLI   LOGACT,C'A'                                                      
         BNE   EXIT                                                             
         MVC   LOGHEAD+12(14),=CL14'-ENTER CHANGES'                             
         LA    R3,ROWFRSTH                                                      
         LA    R2,ROWDATAH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY COLUMN ELEMENTS                                          
         SPACE 1                                                                
DSPCOL   TM    CSACTN,COL          COLUMN ELEMENTS                              
         BNO   DSPPRF                                                           
*                                                                               
         LA    R4,IO2              GET COLUMN ELEMENTS                          
         CLI   LOGACT,C'I'                                                      
         BNE   *+12                                                             
         LA    R4,IO                                                            
         MVI   SVPFKEY,0                                                        
         LA    R5,ACRECORD                                                      
         LA    R3,COLFRSTH         R3 TO COLUMN DATA                            
         SR    R0,R0                                                            
         LA    R0,1                TOTAL WIDTH                                  
*                                                                               
         USING COLD,R3                                                          
         USING RSCD,R5                                                          
DSPC03   CLI   0(R5),0             END OF RECORD                                
         BE    DSPC50                                                           
         CLI   RSCEL,RSCELQ        IS IT A COLUMN ELEMENT                       
         BE    DSPC12                                                           
*                                                                               
DSPC04   SR    R1,R1                                                            
         IC    R1,RSCLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     DSPC03                                                           
*                                                                               
DSPC12   CLI   LOGACT,C'A'                                                      
         BNE   DSPC20                                                           
         CLI   SVPFKEY,PFINS       INSERT COLUMN                                
         BNE   DSPC20                                                           
         L     RF,RTNCURSE                                                      
         LR    R6,R3                                                            
         AR    RF,RA                                                            
         CR    RF,R6                                                            
         BL    DSPC20                                                           
         LA    R6,COLLNQ(R6)                                                    
         CR    RF,R6                                                            
         BH    DSPC20                                                           
         TWAXC (R3),(R6),PROT=N                                                 
         MVI   SVPFKEY,0                                                        
         LR    R3,R6                                                            
*                                                                               
DSPC20   SR    R1,R1                                                            
         MVC   COLDATA(L'RSCDATA),RSCDATA        COLUMN DATA                    
         CLI   RSCPARA,C' '                                                     
         BL    *+14                                                             
         MVC   COLPARA(L'RSCPARA),RSCPARA                                       
         B     DSPC35                                                           
         MVC   WORK(16),SPACES                                                  
         SR    R1,R1                                                            
         TM    RSCDTEFG,X'FF'-RSCTODTE                                          
         BZ    DSPC30                                                           
         CLC   RSCSTDT,=XL2'8000'                                               
         BNE   DSPC21                                                           
         MVC   WORK(5),=CL5'PRIOR'                                              
         B     DSPC27                                                           
DSPC21   TM    RSCSTDT,X'80'                                                    
         BZ    *+6                                                              
         BCTR  R1,0                                                             
DSPC22   ICM   R1,3,RSCSTDT                                                     
         CVD   R1,DUB                                                           
         MVC   WORK(7),=XL7'40202020202060'                                     
         LA    R1,WORK+5                                                        
         EDMK  WORK(7),DUB+5                                                    
         CLI   WORK+5,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+5,C'0'                                                      
         BCTR  R1,0                                                             
         MVC   0(1,R1),WORK+6                                                   
         MVI   WORK+6,C' '                                                      
*                                                                               
         TM    RSCSTDT,X'80'                                                    
         BNZ   DSPC25                                                           
         TM    RSCDTEFG,RSCDAY                                                  
         BNZ   DSPC25                                                           
         OC    RSCENDT,RSCENDT                                                  
         BZ    DSPC30                                                           
DSPC25   SR    R1,R1                                                            
         CLC   RSCENDT,=XL2'8000'                                               
         BNE   DSPC26                                                           
         MVI   WORK+7,C','                                                      
         MVC   WORK+8(5),=CL5'AFTER'                                            
         B     DSPC30                                                           
DSPC26   TM    RSCDTEFG,RSCDAY                                                  
         BZ    DSPC30                                                           
DSPC27   MVI   WORK+7,C','                                                      
         TM    RSCENDT,X'80'                                                    
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         ICM   R1,3,RSCENDT                                                     
         CVD   R1,DUB                                                           
         MVC   WORK+8(7),=XL7'40202020202060'                                   
         LA    R1,WORK+13                                                       
         EDMK  WORK+8(7),DUB+5                                                  
         CLI   WORK+13,C' '                                                     
         BNE   *+8                                                              
         MVI   WORK+13,C'0'                                                     
         BCTR  R1,0                                                             
         MVC   0(1,R1),WORK+14                                                  
         MVI   WORK+14,C' '                                                     
DSPC30   SR    R1,R1                                                            
         LA    RE,15                                                            
         LA    RF,WORK                                                          
DSPC32   CLI   0(RF),X'40'                                                      
         BNH   DSPC33                                                           
         AH    R1,=H'01'                                                        
         AH    RF,=H'01'                                                        
         BCT   RE,DSPC32                                                        
         B     DSPC34                                                           
DSPC33   EXMVC RE,0(RF),1(RF)                                                   
         BCT   RE,DSPC32                                                        
DSPC34   LA    R1,L'COLPARA-1                                                   
         EXMVC R1,COLPARA,WORK                                                  
*                                                                               
DSPC35   SR    R1,R1                                                            
         IC    R1,RSCWDTH          GET WIDTH                                    
         TM    RSCOPT,HIDE                                                      
         BNZ   DSPC36                                                           
         LA    R1,1(R1)                                                         
         AR    R0,R1               TOTAL UP WIDTH                               
         BCTR  R1,R0                                                            
DSPC36   CLI   RSCWDTH,0                                                        
         BE    DSPC37                                                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  COLWIDE,DUB         UNPACK TO SCREEN                             
         CLI   COLWIDE,C'0'        SUPPRESS LEADING ZERO                        
         BNE   DSPC37                                                           
         MVC   COLWIDE(1),COLWIDE+1                                             
         MVI   COLWIDE+1,C' '                                                   
*                                                                               
DSPC37   EQU   *                                                                
         MVI   COLTOTL,C' '        DEFAULT                                      
         TM    RSCOPT,ACUM         IS IT AN ACCUMULATED FIELD?                  
         BNZ   DSPC38              YES SO GET OUT                               
         MVI   COLTOTL,C'N'        SET OPTION NO                                
         TM    RSCOPT2,RSRTOT      IS IT ON?                                    
         BZ    *+8                                                              
         MVI   COLTOTL,C'Y'        TOTAL OPTION ON                              
*                                                                               
DSPC38   EQU   *                                                                
         ZIC   R1,RSCSORTN                                                      
         CLI   RSCSORTN,0                                                       
         BE    DSPC39                                                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  COLSORT,DUB         UNPACK TO SCREEN                             
         CLI   COLSORT,C'0'        SUPPRESS LEADING ZERO                        
         BNE   DSPC39                                                           
         MVC   COLSORT(1),COLSORT+1                                             
         MVI   COLSORT+1,C' '                                                   
         SPACE 1                                                                
DSPC39   XC    COLHED1,COLHED1     CLEAR OUT                                    
         XC    COLHED2,COLHED2     CLEAR OUT                                    
         CLI   RSCLEN,RSCLNQ       LENGTH WITH NO HEADINGS                      
         BE    DSPC40                                                           
         MVC   COLHED1,RSCHDL1                                                  
         CLI   RSCLEN,RSCLNQ1      LENGTH WITH 1 HEADING                        
         BE    DSPC40                                                           
         MVC   COLHED2,RSCHDL2                                                  
         SPACE 1                                                                
DSPC40   EQU   *                                                                
         MVI   COLHIDE,C'N'                                                     
         TM    RSCOPT,HIDE         DO WE WANT A HIDDEN COLUMN?                  
         BZ    *+8                                                              
         MVI   COLHIDE,C'Y'                                                     
*                                                                               
         SR    R1,R1                                                            
         LR    RE,R3                                                            
         LA    RF,COLLNQ(RE)                                                    
DSPC43   OI    6(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CR    RE,RF                                                            
         BL    DSPC43                                                           
*                                                                               
DSPC45   LA    R3,COLLNQ(R3)      R3=NEXT COLUMN LINE                           
         LA    RF,COLTABH                                                       
         CR    R3,RF                                                            
         BNL   DSPC50                                                           
         B     DSPC04                                                           
*                                                                               
DSPC50   EQU   *                                                                
         CVD   R0,DUB                                                           
         CP    DUB,=P'164'                                                      
         BH    INVLDWD                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  COLTWDT,DUB            UNPACK TO SCREEN                          
         CLI   COLTWDT,C'0'                                                     
         BNE   *+8                                                              
         MVI   COLTWDT,C' '                                                     
         CLC   COLTWDT+1(2),=C'00'                                              
         BNE   *+8                                                              
         MVI   COLTWDT+1,C' '                                                   
         SPACE 1                                                                
DSPC60   MVI   ERROR,X'FE'         END OF SCREEN ENTRIES  - NO ERRORS           
         OI    LOGHEADH+1,X'08'    HIGHLIGHT IT                                 
         MVC   LOGHEAD(15),=CL15'COLUMNS SCREEN'                                
         LA    R2,HDLCODEH                                                      
         CLI   LOGACT,C'I'                                                      
         BE    EXIT                                                             
         MVI   ERROR,X'FF'                                                      
         TWAXC (R3),COLTABH,PROT=N                                              
         B     EXIT                                                             
         EJECT                                                                  
DSPPRF   TM    CSACTN,PRF          DISPLAY PROFILE OPTION                       
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO               PROFILE FORMAT ELEMENTS                      
         LA    R5,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
         MVI   PRFPCMA,C'Y'        PRINT COMMAS                                 
         MVI   PRFPZAM,C'N'        ZERO AMOUNTS                                 
         MVI   PRFPZTO,C'N'        ZERO TOTALS                                  
         MVI   PRFPMIN,C'T'        TRAILING MINUS SIGN                          
         USING RSPD,R5                                                          
DSPP03   EQU   *                                                                
         CLI   0(R5),0                                                          
         BNE   DSPP04              NO ELEMENT TO DISPLAY                        
         MVI   PRFLFJT,C'N'        DEFAUTLS, LEFT JUSTIFY?                      
         MVI   PRFBOX,C'Y'         BOXES?                                       
         MVI   PRFIACT,C'N'        SHOW INACTIVE ACCTS.?                        
         MVI   PRFRTOT,C'N'        PRINT REDUNDANT TOTALS?                      
         MVI   PRFRDAT,C'N'        PRINT REDUNDANT DATA?                        
         MVI   PRFPCTS,C'2'        TWO DECIMALS PLACES FOR PERCENTS             
         MVI   PRFRND,C'P'         PRINT TO THE PENNY                           
         MVI   PRFRKOR,C'D'        RANK ORDER                                   
         B     DSPP50                                                           
DSPP04   EQU   *                                                                
         CLI   RSPEL,RSPELQ        IS IT PROFILE ELEMENT?                       
         BE    DSPP12                                                           
*                                                                               
DSPP05   EQU   *                                                                
         IC    R1,RSPLEN           GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         B     DSPP03                                                           
*                                                                               
DSPP12   EQU   *                                                                
         MVI   PRFLFJT,C'Y'        PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPLFJT     DO WE WANT TO LEFT JUSTIFY?                  
         BO    *+8                 YES                                          
         MVI   PRFLFJT,C'N'        NO                                           
*                                                                               
         MVI   PRFBOX,C'Y'         PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPBOX      DO WE WANT BOXES?                            
         BO    *+8                 YES                                          
         MVI   PRFBOX,C'N'         NO                                           
*                                                                               
         MVI   PRFIACT,C'Y'        PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPIACT     DO WE WANT INACTIVE ACCOUNTS?                
         BO    *+8                 YES                                          
         MVI   PRFIACT,C'N'        NO                                           
*                                                                               
         MVI   PRFRTOT,C'Y'        PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPRTOT     DO WE WANT REDUNDANT TOTALS?                 
         BO    *+8                 YES                                          
         MVI   PRFRTOT,C'N'        NO                                           
*                                                                               
         MVI   PRFRDAT,C'Y'        PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPRDAT     DO WE WANT REDUNDANT DATA IN COLUMN?         
         BO    *+8                 YES                                          
         MVI   PRFRDAT,C'N'        NO                                           
*                                                                               
         CLI   RSPEDOPT,0          ARE THERE ANY EDIT SETTINGS ON?              
         BE    DSPP20              NO, SO SHOW DEFAULTS                         
*                                                                               
         MVI   PRFPCMA,C'Y'        PRINT OPTION FLAG                            
         TM    RSPEDOPT,RSPEDCMA   DO WE WANT COMMAS?                           
         BO    *+8                 YES                                          
         MVI   PRFPCMA,C'N'        NO                                           
*                                                                               
         MVI   PRFPZAM,C'Y'        PRINT OPTION FLAG                            
         TM    RSPEDOPT,RSPEDZRO   PRINT ZERO AS NONBLANK                       
         BO    *+8                 YES                                          
         MVI   PRFPZAM,C'N'        NO                                           
*                                                                               
         MVI   PRFPZTO,C'Y'        PRINT OPTION FLAG                            
         TM    RSPPOPT,RSPZEROT    DO WE WANT ZERO TOTALS TO PRINT?             
         BO    *+8                 YES                                          
         MVI   PRFPZTO,C'N'        NO                                           
*                                                                               
         MVI   PRFPMIN,C'T'        PRINT OPTION FLAG                            
         TM    RSPEDOPT,RSPEDTRL   TRAILING MINUS SIGN?                         
         BO    DSPP20              YES                                          
         MVI   PRFPMIN,C'L'        PRINT OPTION FLAG                            
         TM    RSPEDOPT,RSPEDLED   LEADING MINUS SIGN?                          
         BO    DSPP20              YES                                          
         MVI   PRFPMIN,C'B'                                                     
         TM    RSPEDOPT,RSPEDBKT   BRACKET MINUS SIGN?                          
         BO    DSPP20              YES                                          
         MVI   PRFPMIN,C'C'        NO                                           
*                                                                               
DSPP20   MVC   PRFPCTS,RSPPCTS     0/1/2 DECIMAL PLACES ON PERCENTS             
         MVC   PRFRND,RSPRND       ROUNDING OPTION P/D/T/M                      
*        MVC   PRFUNT(2),RSPUNT    UNIT/LEDGER                                  
         MVC   PRFLIST,RSPLIST     BILLING SOURCE LIST                          
*                                                                               
         CLI   RSPRKON,0           ARE WE RANKING?                              
         BZ    DSPP50              NO                                           
         MVC   PRFRKON,RSPRKON                                                  
         OI    PRFRKON+1,X'F0'     CHANGE BACK TO NUMBER                        
*                                                                               
         IC    R1,RSPRKCL                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRFRKCL,DUB         UNPACK TO SCREEN                             
*                                                                               
         MVC   PRFRKOR,RSPRKOR     ORDER OF RANKING                             
DSPP50   EQU   *                                                                
         MVI   ERROR,X'FE'                                                      
         OI    LOGHEADH+1,X'08'    HIGHLIGHT IT                                 
         MVC   LOGHEAD(15),=CL15'PROFILE SCREEN'                                
         LA    R2,HDLCODEH                                                      
         CLI   LOGACT,C'A'                                                      
         BNE   EXIT                                                             
         MVC   LOGHEAD+15(14),=CL14'-ENTER CHANGES'                             
         LA    R2,PRFLFJTH                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY THE HELP SCREENS                                         
         SPACE 1                                                                
DSPHLP   DS    0H                                                               
         MVC   BYTE,CSACTN                                                      
         NI    BYTE,X'FF'-HLP                                                   
         MVI   SIDE,C'L'           FIRST FILL UP THE LEFT SIDE                  
         LA    R2,HLP08H           START AT LINE 8                              
         MVI   LINES,16            NUMBER OF LINES REMAINING                    
         USING DEFTD,R6                                                         
         L     R6,ADEFTAB          LOOK THRU DEFINITION TABLE                   
*                                                                               
DSPHLP03 SR    R1,R1                                                            
         IC    R1,BYTE             R1=ACTION BITS                               
         SR    R4,R4               SET TO ZEROS                                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    DEFIND,0            IS IT VALID FOR THIS ACTION                  
         BZ    DSPHLP20            NO, SKIP THIS ENTRY                          
         CLC   LINES,DEFNUM        ARE THERE ENOUGH LINES                       
         BNL   DSPHLP08                                                         
         CLI   SIDE,C'R'           ALREADY ON THE RIGHT                         
         BE    DSPHLP30            SKIP THE REST                                
         MVI   SIDE,C'R'           NOW DO THE RIGHT SIDE                        
         LA    R2,HLP08H           START AT LINE 8                              
         MVI   LINES,16            NUMBER OF LINES REMAINING                    
*                                                                               
DSPHLP08 EQU   *                                                                
         LA    R5,8(R2)            R5 TO DATA LINE                              
         CLI   SIDE,C'L'           AM I STILL ON THE LEFT                       
         BE    *+8                                                              
         LA    R5,40(R5)           NOW ON RIGHT                                 
*                                                                               
         CLI   DEFCDE,C'A'         CHECKING ACCOUNT LEVS "A*C" OF "A*N"         
         BNE   DSPHLP10            NOT AN ACCOUNT LEVEL, JUMP OUT               
*                                                                               
         MVC   ACCTYPE,=CL8'CODE(  )'                                           
         CLI   DEFCDE+2,C'C'                                                    
         BE    DSPHLP09            OK SO FAR                                    
         MVC   ACCTYPE,=CL8'NAME(36)'                                           
         CLI   DEFCDE+2,C'N'                                                    
         BNE   DSPHLP10            NOT AN ACCOUNT LEVEL, JUMP OUT               
*                                                                               
DSPHLP09 EQU   *                                                                
         MVC   LEVNUM,DEFCDE+1     GET LEVEL NUMBER FORM CODE                   
         TM    LEVNUM,X'F0'        CHARACTER NUMBER = X'F0'-X'F9'?              
         BNO   DSPHLP10            NOT AN ACCOUNT LEVEL, JUMP OUT               
*                                                                               
         NI    LEVNUM,X'0F'        YES CONVERT TO BINARY NUMBER                 
         IC    R4,LEVNUM                                                        
         BCTR  R4,0                                                             
         MH    R4,=Y(L'LEVA+L'LEVANAME)                                         
         LA    R4,LEVA(R4)                                                      
         CLI   0(R4),0             NOT A LEGAL LEVEL IF = 0                     
         BE    DSPHLP20            SKIP THIS REQUEST                            
*                                                                               
DSPHLP10 EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,LINES            LINES REMAINING                              
         SR    R3,R3                                                            
         IC    R3,DEFNUM           NUMBER OF DESCRIPTION LINES FOR THIS         
         SR    R0,R3                                                            
         STC   R0,LINES            LINES REMAINING AFTER THIS                   
*                                                                               
         MVC   0(4,R5),DEFCDE      CODE                                         
         MVI   4(R5),C'='                                                       
         LA    RE,DEFDSC           TABLE DISCRIPTION FIELD                      
*                                                                               
         LTR   R4,R4               AN ACCOUNT LEVEL?      R4 = ADDR.            
         BZ    DSPHLP17                                                         
         MVC   SQUISH,SPACES                                                    
         MVC   SQUISH(L'LEVANAME),1(R4)      NAME OF LEVEL                      
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         CLI   DEFCDE+2,C'N'                                                    
         BE    DSPHLP16            IF NAME JUST DISPLAY AS IS                   
*                                                                               
         EDIT  (R1),(2,LEVLEN)     CONVERT LEVEL LENGTH TO EBCDIC               
         CLI   LEVLEN,C' '         IS IT A ONE OR TWO DIGETS?                   
         BNE   DSPHLP16                                                         
         MVC   ACCTYPE+5(2),ACCTYPE+6                                           
         MVI   ACCTYPE+7,C' '                                                   
*                                                                               
DSPHLP16 LA    R1,L'LEVANAME                                                    
         LA    R4,SQUISH(R1)                                                    
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-12                                                          
         MVC   2(L'ACCTYPE,R4),ACCTYPE                                          
         LA    RE,SQUISH                                                        
*                                                                               
DSPHLP17 MVC   5(28,R5),0(RE)      DESCRIPTION                                  
         LA    RE,28(RE)                                                        
         LA    R2,L'HLP23H+L'HLP23(R2)       R2=START OF NEXT LINE              
         LA    R5,L'HLP23H+L'HLP23(R5)       R5=NEXT DESCRIPTION AREA           
         BCT   R3,DSPHLP17                                                      
*                                                                               
DSPHLP20 SR    RF,RF                                                            
         IC    RF,DEFLEN           LENGTH OF THIS ENTRY                         
         AR    R6,RF               R6 TO NEXT ENTRY                             
         CLI   0(R6),X'FF'                                                      
         BNE   DSPHLP03                                                         
*                                                                               
DSPHLP30 MVI   ERROR,X'FE'                                                      
         OI    LOGHEADH+1,X'08'    HIGHLIGHT IT                                 
         MVC   LOGHEAD(14),=C'HELP DISPLAYED'                                   
         LA    R2,HDLCODEH                                                      
         B     EXIT                                                             
         EJECT                                                                  
         USING DEFTD,R6                                                         
VALPARM  NTR1                                                                   
         XC    USEDTYPE,USEDTYPE   CLEAR TO NONE USED                           
         XC    FLAGBIT1,FLAGBIT1   TURN OFF ALL BITS                            
         XC    FLAGBIT2,FLAGBIT2   TURN OFF ALL BITS                            
         XC    FLAGBIT3,FLAGBIT3   TURN OFF ALL BITS                            
         MVI   MYFNDX,1            SET TO ONE                                   
         OC    FLAGBIT1,DEFIND                                                  
         NI    FLAGBIT1,DCODE+DNAME                                             
         LA    R4,BLOCK                                                         
         CLI   12(R4),C'='         SPECIAL CASE                                 
         BE    VALPRM90            GET OUT IT IS A CALC COMP.                   
         CLC   12(2,R4),=CL2'UF'   SPECIAL CASE                                 
         BE    VALPRM90            GET OUT IT IS A USER FIELD                   
         SR    R1,R1                                                            
         CLI   INPRMS,1                                                         
         BE    VALPRM90                                                         
*                                                                               
VALPRM10 EQU   *                                                                
         LA    R4,32(R4)           BUMP UP TO NEXT PARAM IN BLOCK               
         IC    R1,MYFNDX                                                        
         AH    R1,=H'1'                                                         
         STC   R1,MYFNDX                                                        
         STC   R1,FNDX                                                          
         LA    R0,NPRMTYPE                                                      
         LA    R5,PRMTBL                                                        
         CLI   0(R4),4                                                          
         BNH   VALPRM15                                                         
         MVI   ERROR,INVALID                                                    
         B     VALPRM90                                                         
*                                                                               
VALPRM15 EQU   *                                                                
         IC    R1,0(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
         USING PRMD,R5                                                          
VALPRM20 EQU   *                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),PRMCODE                                                 
         BE    VALPRM30                                                         
*                                                                               
VALPRM25 EQU   *                                                                
         LA    R5,PRMNEXTQ(R5)                                                  
         BCT   R0,VALPRM20                                                      
         MVI   ERROR,INVALID                                                    
         B     VALPRM90                                                         
*                                                                               
VALPRM30 EQU   *                                                                
         LA    RE,DEFCPRMS                                                      
         TM    CSACTN,COL                                                       
         BO    *+8                                                              
         LA    RE,DEFRPRMS                                                      
         SR    RF,RF                                                            
         IC    RF,PRMTYPE                                                       
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(RE),0                                                          
         BZ    VALPRM25            LOOP BACK TO CHECK REST OF PARA LIST         
*                                                                               
VALPRM40 EQU   *                                                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    USEDTYPE,0                                                       
         BZ    VALPRM50                                                         
         MVI   ERROR,INVALID                                                    
         B     VALPRM90                                                         
*                                                                               
VALPRM50 EQU   *                                                                
         OC    PRMCHECK,PRMCHECK                                                
         BZ    VALPRM80                                                         
         MVI   ERROR,X'FF'                                                      
         ICM   RF,7,PRMCHECK                                                    
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                                                             
*                                                                               
VALPRM80 EQU   *                                                                
         OC    USEDTYPE,PRMTYPE                                                 
         IC    R1,INPRMS                                                        
         BCTR  R1,0                                                             
         STC   R1,INPRMS                                                        
         CLI   INPRMS,1                                                         
         BNE   VALPRM10                                                         
*                                                                               
VALPRM90 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        R4 = PARAMETER IN BLOCK                                                
*        R6 = DEFTAB ENTRY                                                      
*                                                                               
         USING DEFTD,R6                                                         
VALTYPE  NTR1                                                                   
         CLI   12(R4),C'C'         WAS IT CODE                                  
         BNE   VALTY10                                                          
         OI    FLAGBIT1,DCODE      APPENED IT WITH CODE                         
         NI    FLAGBIT1,X'FF'-DNAME     TURN OFF CODE                           
         TM    DEFIND,DCODE                                                     
         BO    VALTY90                                                          
         MVI   ERROR,INVALID                                                    
         B     VALTY90                                                          
*                                                                               
VALTY10  EQU   *                                                                
         CLI   12(R4),C'N'              WAS IT NAME                             
         BNE   VALTY90                                                          
         OI    FLAGBIT1,DNAME           TURN ON NAME                            
         NI    FLAGBIT1,X'FF'-DCODE     TURN OFF CODE                           
         TM    DEFIND,DNAME                                                     
         BO    XIT                                                              
         MVI   ERROR,INVALID                                                    
*                                                                               
VALTY90  EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
VALDUE   EQU   *                                                                
         NI    FLAGBIT2,X'FF'-BYBILL                                            
         OI    FLAGBIT2,BYDUE                                                   
         BR    RE                                                               
*                                                                               
VALUF    EQU   *                                                                
         SR    R1,R1                                                            
         CLI   INPRMS,2                                                         
         BL    INVLDP1                                                          
         LA    R4,BLOCK                                                         
         IC    R1,INPRMS                                                        
VALUF10  EQU   *                                                                
         CLI   0(R4),2                                                          
         BE    VALUF20                                                          
         SR    R4,R4                                                            
         IC    R4,INPRMS                                                        
         SR    R4,R1                                                            
         AH    R4,=H'01'           ADJUST WHICH PARAMETER                       
         STC   R4,FNDX                                                          
         MVI   ERROR,INVALID                                                    
         B     VALUF90                                                          
VALUF20  EQU   *                                                                
         LA    R4,32(R4)                                                        
         BCT   R1,VALUF10                                                       
VALUF90  EQU   *                                                                
         BR    RE                                                               
         EJECT                                                                  
         USING COLD,R3                                                          
         USING RSCD,R5                                                          
VALDATE  NTR1                                                                   
         LA    R6,RSCSTDT                                                       
         MVI   ERROR,X'FF'                                                      
         LA    R2,COLPARAH         SCAN DATE RANGES                             
         CLI   COLPARAH+5,0        ANY INPUT?                                   
         BNE   VALDTE05                                                         
         TM    RSCDTEFG,RSCDAY                                                  
         BZ    VALDTE99                                                         
         MVC   RSCSTDT,=XL2'8000'                                               
         B     VALDTE99                                                         
*                                                                               
VALDTE05 TM    RSCDTEFG,X'FF'-RSCTODTE                                          
         BZ    VALDTE98                 ERROR                                   
         GOTO1 SCANNER,DMCB,COLPARAH,(2,BLOCK2)                                 
         LA    R4,BLOCK2                                                        
         MVC   FNDX,DMCB+4                                                      
         MVC   NPARMS,DMCB+4                                                    
         CLI   FNDX,0                                                           
         BE    VALDTE99                                                         
         CLI   FNDX,2                                                           
         BL    VALDTE08                                                         
         BH    VALDTE98                 ERROR                                   
         TM    RSCDTEFG,RSCDAY+RSCMON   ONLY DAY & MON CAN HAVE 2 PARMS         
         BZ    VALDTE98                 ERROR                                   
VALDTE08 MVI   FNDX,1                                                           
         MVI   ONEXONLY,NO                                                      
*                                                                               
VALDTE10 CLC   FNDX,NPARMS                                                      
         BH    VALDTE60                                                         
         TM    2(R4),X'80'         IS IT POSITIVE?                              
         BZ    VALDTE20                                                         
         MVC   0(2,R6),6(R4)       MOVE IN HALF WORD BINARY                     
         LA    R6,RSCENDT                                                       
         B     VALDTE50                                                         
*                                                                               
VALDTE20 SR    R1,R1                                                            
         IC    R1,0(R4)            LENGTH OF PARAMETERS                         
         BCTR  R1,0                                                             
         TM    RSCDTEFG,RSCYEAR+RSCQTR                                          
         BNZ   VALDTE40                                                         
         EXCLC R1,12(R4),=CL5'PRIOR'                                            
         BNE   VALDTE30                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALDTE98            ERROR                                        
         CLI   NPARMS,1                                                         
         BE    VALDTE98            ERROR                                        
         MVI   ONEXONLY,YES                                                     
         CLI   FNDX,2              2ND TIME IN?                                 
         BNE   *+10                                                             
         MVC   RSCENDT,RSCSTDT                                                  
         MVC   RSCSTDT,=XL2'8000'                                               
         LA    R6,RSCENDT                                                       
         B     VALDTE50                                                         
*                                                                               
VALDTE30 EXCLC R1,12(R4),=CL5'AFTER'                                            
         BNE   VALDTE40                                                         
         CLI   ONEXONLY,YES                                                     
         BE    VALDTE98            ERROR                                        
         MVI   ONEXONLY,YES                                                     
         CLI   NPARMS,1                                                         
         BE    VALDTE98            ERROR                                        
         MVC   RSCENDT,=XL2'8000'                                               
         LA    R6,RSCSTDT                                                       
         B     VALDTE50                                                         
*                                                                               
VALDTE40 AH    R1,=H'01'                                                        
         ST    R1,DMCB+4           SET UP LENGTH IN PARAM OF GOTO1              
         LA    R1,12(R4)           SET UP 1ST PARMA FOR GOTO1                   
         ST    R1,DMCB                                                          
         MVI   DMCB,C'N'           NO DECIMAL PLACES                            
         GOTO1 CASHVAL,DMCB                                                     
         CLI   DMCB,0                                                           
         BNE   VALDTE98                                                         
         MVC   0(2,R6),DMCB+6                                                   
         LA    R6,RSCENDT                                                       
*                                                                               
VALDTE50 TM    RSCDTEFG,RSCYEAR+RSCQTR                                          
         BNZ   VALDTE99                                                         
         LA    R4,32(R4)           BUMP TO 2ND PARAMETER                        
         IC    R1,FNDX                                                          
         AH    R1,=H'01'                                                        
         STC   R1,FNDX                                                          
         B     VALDTE10                                                         
*                                                                               
VALDTE60 CLI   ONEXONLY,YES                                                     
         BE    VALDTE99                                                         
         CLI   NPARMS,2                                                         
         BNE   VALDTE99                                                         
         TM    RSCDTEFG,RSCMON                                                  
         BNZ   VALDTE98                                                         
         TM    RSCSTDT,X'80'       IS IT NEGATIVE                               
         BZ    VALDTE70            IT IS POSITIVE                               
         TM    RSCENDT,X'80'       IS IT NEGATIVE?                              
         BZ    VALDTE99            IT IS POSITIVE                               
         B     VALDTE72                                                         
VALDTE70 TM    RSCENDT,X'80'                                                    
         BNZ   VALDTE80            SWAP DATES                                   
VALDTE72 CLC   RSCSTDT,RSCENDT                                                  
         BL    VALDTE99                                                         
VALDTE80 XC    RSCSTDT,RSCENDT                                                  
         XC    RSCENDT,RSCSTDT                                                  
         XC    RSCSTDT,RSCENDT                                                  
         B     VALDTE99                                                         
VALDTE98 MVI   ERROR,INVALID                                                    
VALDTE99 B     XIT                                                              
         EJECT                                                                  
*                                                                               
VALDAY   OI    FLAGBIT3,RSCDAY                                                  
         BR    RE                                                               
*                                                                               
VALPER   OI    FLAGBIT3,RSCTODTE                                                
         BR    RE                                                               
*                                                                               
VALMON   OI    FLAGBIT3,RSCMON                                                  
         BR    RE                                                               
*                                                                               
VALQTD   OI    FLAGBIT3,RSCTODTE                                                
VALQTR   OI    FLAGBIT3,RSCQTR                                                  
         BR    RE                                                               
*                                                                               
VALYTD   OI    FLAGBIT3,RSCTODTE                                                
VALYEAR  OI    FLAGBIT3,RSCYEAR                                                 
         BR    RE                                                               
*                                                                               
VALLEVD  AH    R1,=H'1'                                                         
VALLEVC  AH    R1,=H'1'                                                         
VALLEVB  AH    R1,=H'1'                                                         
         MH    R1,=Y(L'LEVA+L'LEVANAME)                                         
VALLEVA  LA    R1,LEVA(R1)         POINT TO CORRECT LEDGER LEVEL                
         CLI   0(R1),0                                                          
         BNER  RE                                                               
         MVI   ERROR,NOLVLLDG                                                   
         BR    RE                                                               
         EJECT                                                                  
VALCALC  NTR1                                                                   
         LA    R1,1                                                             
         CLI   INPRMS,4                                                         
         BL    INVLDP1                                                          
*                                                                               
         LA    R4,BLOCK                                                         
         TM    INPRMS,X'01'       MUST BE AN EVEN NUMBER OF PARAMETERS          
         BNO   VALCAL10                                                         
         B     INVLDNP                                                          
*                                                                               
VALCAL10 EQU   *                                                                
         LA    R1,1(R1)                                                         
         LA    R4,32(R4)                                                        
         TM    2(R4),X'80'              IS IT NUMERIC?                          
         BO    VALCAL20                                                         
         MVI   ERROR,NOTNUMRC                                                   
         STC   R1,FNDX                                                          
         B     XIT                                                              
*                                                                               
VALCAL20 EQU   *                                                                
         CLI   7(R4),MAXCOLS                                                    
         BNH   VALCAL30                                                         
         STC   R1,FNDX                                                          
         B     INVLDCL                                                          
*                                                                               
VALCAL30 EQU   *                                                                
         CLM   R1,1,INPRMS              WAS THAT THE LAST ONE?                  
         BE    XIT                      YES, GET OUT                            
         LA    R1,1(R1)                                                         
         LA    R4,32(R4)                                                        
         CLI   12(R4),C'+'                                                      
         BE    VALCAL10                                                         
         CLI   12(R4),C'-'                                                      
         BE    VALCAL10                                                         
         CLI   12(R4),C'/'                                                      
         BE    VALCAL10                                                         
         CLI   12(R4),C'%'                                                      
         BNE   *+12                                                             
         MVI   PCTCOL,C'Y'         THIS COLUMN IS A PERCENT                     
         B     VALCAL10                                                         
         CLI   12(R4),C'*'                                                      
         BE    VALCAL10                                                         
         STC   R1,FNDX                                                          
         B     INVLDOP                                                          
         EJECT                                                                  
*              SAVE THE CURRENT SCREEN TWA0 IN TWA3                             
         SPACE 1                                                                
SSCRN    NTR1  ,                                                                
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
SSCRN01  CLC   CSACTN,SCRACT                                                    
         BE    SSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   SSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
SSCRN03  XC    DMCB(20),DMCB                                                    
         MVC   DMCB+10(2),TWATRM   TERMINAL NO                                  
         MVI   DMCB+8,3            PAGE 3                                       
         MVI   DMCB+9,0                                                         
         LA    RF,LOGHEADH                                                      
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(RF)                         
         B     XIT                                                              
         EJECT                                                                  
*              RESTORE SAVED SCREEN                                             
         SPACE 1                                                                
RSCRN    NTR1  ,                                                                
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
RSCRN01  CLC   CSACTN,SCRACT                                                    
         BE    RSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   RSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
RSCRN03  XC    DMCB(24),DMCB                                                    
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
         MVI   DMCB+8,3            PAGE 3                                       
         MVI   DMCB+9,0                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),SCRLEN   LENGTH OF SAVE                               
         LA    RF,LOGHEADH                                                      
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TEMPSTR                           
         SPACE 1                                                                
         LA    RE,LOGHEADH         TRANSMIT SCREEN                              
         SR    R1,R1                                                            
         SPACE 1                                                                
RSCRN04  OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   RSCRN04                                                          
         B     XIT                                                              
         EJECT                                                                  
*                GET A NEW SCREEN                                               
         SPACE 1                                                                
GSCRN    NTR1  ,                                                                
         MVC   LSTCODE,HDLCODE     SAVE THE LAST CODE                           
         LA    R5,SCRNTAB          GET ENTRY FOR THIS ACTION                    
         SPACE 1                                                                
         USING SCRD,R5                                                          
GSCRN01  CLC   CSACTN,SCRACT                                                    
         BE    GSCRN03                                                          
         LA    R5,SCRLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   GSCRN01                                                          
         DC    H'0'                ACTION NOT IN SCREEN TABLE                   
         SPACE 1                                                                
GSCRN03  XC    DMCB(20),DMCB                                                    
         MVC   DMCB+4(4),=X'D9060300'                                           
         MVC   DMCB+7(1),SCRCDE    SCREEN NUMBER                                
         XR    RF,RF                                                            
         ICM   RF,3,SCRSTRT        START OF SCREEN SAVE                         
         AR    RF,RA                                                            
         GOTO1 CALLOV,DMCB,(0,(RF))                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CANT READ SCREEN                             
         SPACE 1                                                                
         LA    RE,LOGHEADH         TRANSMIT SCREEN                              
         SR    R1,R1                                                            
         SPACE 1                                                                
GSCRN04  OI    6(RE),X'80'                                                      
         OI    7(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BNE   GSCRN04                                                          
         XC    1(2,RE),1(RE)                                                    
         MVC   HDLCODE,LSTCODE     RESTORE THE CODE                             
         MVI   HDLCODEH+5,L'HDLCODE                                             
         B     XIT                                                              
         EJECT                                                                  
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
INVLDPF  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDPFM),INVLDPFM       INVALID PF KEY                
         LA    R2,LOGACTH                                                       
         B     EXIT                                                             
INVLDPFM DC    C'** ERROR ** YOU USED AN INVALID PF KEY'                        
         SPACE 1                                                                
INVLDHP  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDHPM),INVLDHPM       INVALID PF KEY                
         LA    R2,LOGACTH                                                       
         B     EXIT                                                             
INVLDHPM DC    C'** ERROR ** THE ONLY VALID KEY IS PF 12'                       
         SPACE 1                                                                
INVLDWD  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDWDM),INVLDWDM                                     
         LA    R2,LOGACTH                                                       
         B     EXIT                                                             
INVLDWDM DC    C'** ERROR ** REPORT TOO WIDE'                                   
         SPACE 1                                                                
INVLDP1  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDP1M),INVLDP1M                                     
         B     EXIT                                                             
INVLDP1M DC    C'** ERROR ** TOO FEW PARAMETERS'                                
         SPACE 1                                                                
INVLDP2  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDP2M),INVLDP2M                                     
         B     EXIT                                                             
INVLDP2M DC    C'** ERROR ** TOO MANY PARAMETERS'                               
         SPACE 1                                                                
INVLDCT  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDCTM),INVLDCTM                                     
         B     EXIT                                                             
INVLDCTM DC    C'** ERROR ** INVALIVD COLUMN TYPE'                              
         SPACE 1                                                                
INVLDRT  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDRTM),INVLDRTM                                     
         B     EXIT                                                             
INVLDRTM DC    C'** ERROR ** INVALID ROW TYPE'                                  
         SPACE 1                                                                
INVLDHT  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDHTM),INVLDHTM                                     
         B     EXIT                                                             
INVLDHTM DC    C'** ERROR ** INVALID HEADING TYPE'                              
         SPACE 1                                                                
INVLDNM  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDNMM),INVLDNMM                                     
         B     EXIT                                                             
INVLDNMM DC    C'** ERROR ** IVALID NUMBER OF DAYS'                             
         SPACE 1                                                                
INVLDNU  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDNUM),INVLDNUM                                     
         B     EXIT                                                             
INVLDNUM DC    C'** ERROR ** "N" MUST BE 1-9'                                   
         SPACE 1                                                                
INVLDNP  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDNPM),INVLDNPM                                     
         B     EXIT                                                             
INVLDNPM DC    C'** ERROR ** IVALID NUMBER OF PARAMETERS'                       
         SPACE 1                                                                
INVLDCL  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDCLM),INVLDCLM                                     
         B     EXIT                                                             
INVLDCLM DC    C'** ERROR ** IVALID COLUMN NUMBER'                              
         SPACE 1                                                                
INVLDRW  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDRWM),INVLDRWM                                     
         B     EXIT                                                             
INVLDRWM DC    C'** ERROR ** IVALID ROW NUMBER'                                 
         SPACE 1                                                                
INVLDOP  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDOPM),INVLDOPM                                     
         B     EXIT                                                             
INVLDOPM DC    C'** ERROR ** IVALID OPERERATOR'                                 
         SPACE 1                                                                
INVLDTY  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDTYM),INVLDTYM                                     
         B     EXIT                                                             
INVLDTYM DC    C'** ERROR ** HEADLINE NOT ALLOWED'                              
         SPACE 1                                                                
INVLDS1  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDS1M),INVLDS1M                                     
         B     EXIT                                                             
INVLDS1M DC    C'** ERROR ** INVALID SORT COLUMN'                               
         SPACE 1                                                                
INVLDS2  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDS2M),INVLDS2M                                     
         B     EXIT                                                             
INVLDS2M DC    C'** ERROR ** SORT SEQUENCE NUMBER'                              
         SPACE 1                                                                
INVLDS3  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDS3M),INVLDS3M                                     
         B     EXIT                                                             
INVLDS3M DC    C'** ERROR ** DUPLICATE SORT NUMBER'                             
         SPACE 1                                                                
INVLDS4  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDS4M),INVLDS4M                                     
         B     EXIT                                                             
INVLDS4M DC    C'** ERROR ** INVALID SORT SEQUENCE'                             
         SPACE 1                                                                
INVLDD1  MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD(L'INVLDD1M),INVLDD1M                                     
         B     EXIT                                                             
INVLDD1M DC    C'** ERROR ** INVALID DATE TYPE'                                 
         SPACE 1                                                                
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
EXIT     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              CONSTANTS AND TABLES                                             
*                                                                               
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
DCODE    EQU   RSRRCODE            CODE TYPE                                    
DNAME    EQU   RSRRNAME            NAME TYPE                                    
*                                                                               
*        PARAMETER TYPES                                                        
*                                                                               
TYPE1    EQU   X'80'               SPECIAL ADDRS TO PRINT                       
TYPE2    EQU   X'40'               DATE TYPE TO FILTER ON                       
TYPE3    EQU   X'20'               CODE OR NAME TYPE PARAMETERS                 
TYPE4    EQU   X'10'               DATE TYPE PARAMETERS                         
TYPE5    EQU   X'08'                                                            
TYPE6    EQU   X'04'                                                            
TYPE7    EQU   X'02'                                                            
TYPE8    EQU   X'01'                                                            
*                                                                               
ACUM     EQU   X'80'               USE AN ACCUMULATOR                           
CALC     EQU   X'40'               THIS IS A CALCULATED COLUMN                  
HIDE     EQU   X'20'               HIDE THIS COLUMN                             
BYDUE    EQU   X'08'               FILTER BY DUE DATE, ELSE INV DATE            
BYBILL   EQU   X'10'               AGE BY DAYS                                  
*                                                                               
MYMONTHS DC    CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
*                                                                               
HDL      EQU   X'01'               HEADLINE DATA                                
ROW      EQU   X'02'               ROW                                          
COL      EQU   X'04'               COLUMN                                       
PRF      EQU   X'10'               PROFILE                                      
HLP      EQU   X'80'               HELP                                         
HDLH     EQU   HDL+HLP             HEADLINE HELP                                
ROWH     EQU   ROW+HLP             ROW HELP                                     
COLH     EQU   COL+HLP             COLUMN HELP                                  
*                                                                               
PFHLP    EQU   1                   HELP                                         
PFHDL    EQU   2                   HEADS                                        
PFROW    EQU   3                   ROWS                                         
PFCOL    EQU   4                   COLUMNS                                      
PFPRF    EQU   5                   PROFILE                                      
PFINS    EQU   10                  INSERT                                       
PFDEL    EQU   11                  DELETE                                       
PFRTN    EQU   12                  RETURN                                       
*                                                                               
*              PF KEY VALIDATION TABLES                                         
*                                                                               
*              CURRENT ACTION/ POINTER TO TABLE OF VALID PF KEYS                
*                                                                               
PFTAB    DC    AL1(HDL),AL3(PFHDV-T60330)                                       
         DC    AL1(ROW),AL3(PFRWV-T60330)                                       
         DC    AL1(COL),AL3(PFCLV-T60330)                                       
         DC    AL1(PRF),AL3(PFPRV-T60330)                                       
         DC    X'FF'                                                            
*                                                                               
*              TABLES OF VALID PF KEYS                                          
*              PF KEY/ NEW ACTION/ POINTER TO ROUTINE FOR NEW ACTION            
*                                                                               
PFHDV    DC    AL1(PFHLP),AL1(HDLH),AL3(DSPHLP-T60330) HELP HEADS               
         DC    AL1(PFROW),AL1(ROW),AL3(BLDKEY-T60330)  DISPLAY ROWS             
         DC    AL1(PFCOL),AL1(COL),AL3(BLDKEY-T60330)  DISPLAY COLUMNS          
         DC    AL1(PFPRF),AL1(PRF),AL3(BLDKEY-T60330)  DISPLAY PROFILE          
         DC    X'FF'                                                            
*                                                                               
PFRWV    DC    AL1(PFHLP),AL1(ROWH),AL3(DSPHLP-T60330) HELP ROW                 
         DC    AL1(PFHDL),AL1(HDL),AL3(BLDKEY-T60330)  DISPLAY HEADS            
         DC    AL1(PFCOL),AL1(COL),AL3(BLDKEY-T60330)  DISPLAY COLUMNS          
         DC    AL1(PFPRF),AL1(PRF),AL3(BLDKEY-T60330)  DISPLAY PROFILE          
         DC    X'FF'                                                            
*                                                                               
PFCLV    DC    AL1(PFHLP),AL1(COLH),AL3(DSPHLP-T60330) HELP COLUMNS             
         DC    AL1(PFHDL),AL1(HDL),AL3(BLDKEY-T60330)  DISPLAY HEADS            
         DC    AL1(PFROW),AL1(ROW),AL3(BLDKEY-T60330)  DISPLAY ROWS             
         DC    AL1(PFPRF),AL1(PRF),AL3(BLDKEY-T60330)  DISPLAY PROFILE          
         DC    AL1(PFINS),AL1(0),AL3(BLDKEY-T60330)    INSERT A LINE            
         DC    AL1(PFDEL),AL1(0),AL3(BLDKEY-T60330)    DELETE A LINE            
         DC    X'FF'                                                            
*                                                                               
PFPRV    DC    AL1(PFHDL),AL1(HDL),AL3(BLDKEY-T60330)  DISPLAY HEADS            
         DC    AL1(PFROW),AL1(ROW),AL3(BLDKEY-T60330)  DISPLAY ROWS             
         DC    AL1(PFCOL),AL1(COL),AL3(BLDKEY-T60330)  DISPLAY COLUMNS          
         DC    X'FF'                                                            
         EJECT                                                                  
*              SCREEN TABLE (SEE SCRTD)                                         
*                                                                               
SCRNTAB  DC AL1(HDL),X'D9',AL2(LOGTABH-T603FFD),AL2(HDLWORK-LOGHEADH)           
         DC AL1(ROW),X'D5',AL2(ROWBGNH-T603FFD),AL2(ROWWORK-LOGHEADH)           
         DC AL1(COL),X'D7',AL2(COLBGNH-T603FFD),AL2(COLWORK-LOGHEADH)           
         DC AL1(PRF),X'D3',AL2(PRFBGNH-T603FFD),AL2(PRFWORK-LOGHEADH)           
         DC AL1(HDLH),X'D6',AL2(HLP08H-T603FFD),AL2(HLPWORK-LOGHEADH)           
         DC AL1(ROWH),X'D6',AL2(HLP08H-T603FFD),AL2(HLPWORK-LOGHEADH)           
         DC AL1(COLH),X'D6',AL2(HLP08H-T603FFD),AL2(HLPWORK-LOGHEADH)           
         DC X'FF'                                                               
*                                                                               
*              TABLE OF HEADLINE DATA FIELDS                                    
*              DISPLACEMENT TO FIELD/ DATA TYPE/ SEQUENCE NUMBER                
*                                                                               
HEADFLD  DC    AL3(HDLTITLH-T603FFD),AL1(RSHTITL),AL1(1)                        
         DC    AL3(HDLCNT1H-T603FFD),AL1(RSHCNTR),AL1(1)                        
         DC    AL3(HDLCNT2H-T603FFD),AL1(RSHCNTR),AL1(2)                        
         DC    AL3(HDLCNT3H-T603FFD),AL1(RSHCNTR),AL1(3)                        
         DC    AL3(HDLLH1H-T603FFD),AL1(RSHLFTH),AL1(1)                         
         DC    AL3(HDLLH2H-T603FFD),AL1(RSHLFTH),AL1(2)                         
         DC    AL3(HDLLH3H-T603FFD),AL1(RSHLFTH),AL1(3)                         
         DC    AL3(HDLRH1H-T603FFD),AL1(RSHRHTH),AL1(1)                         
         DC    AL3(HDLRH2H-T603FFD),AL1(RSHRHTH),AL1(2)                         
         DC    AL3(HDLRH3H-T603FFD),AL1(RSHRHTH),AL1(3)                         
         DC    AL3(HDLFL1H-T603FFD),AL1(RSHFTLN),AL1(1)                         
         DC    X'FF'                                                            
*                                                                               
PRMTBL   DS    0C                                                               
         DC    CL4'ADR',AL1(TYPE1),AL3(0)                                       
         DC    CL4'BILL',AL1(TYPE2),AL3(0)                                      
         DC    CL4'DUE',AL1(TYPE2),AL3(VALDUE-T60330)                           
         DC    CL4'CODE',AL1(TYPE3),AL3(VALTYPE-T60330)                         
         DC    CL4'BOTH',AL1(TYPE3),AL3(VALTYPE-T60330)                         
         DC    CL4'NAME',AL1(TYPE3),AL3(VALTYPE-T60330)                         
         DC    CL4'DAY',AL1(TYPE4),AL3(VALDAY-T60330)                           
         DC    CL4'MON',AL1(TYPE4),AL3(VALMON-T60330)                           
         DC    CL4'PER',AL1(TYPE4),AL3(VALPER-T60330)                           
         DC    CL4'QTR',AL1(TYPE4),AL3(VALQTR-T60330)                           
         DC    CL4'QTD',AL1(TYPE4),AL3(VALQTD-T60330)                           
         DC    CL4'YR',AL1(TYPE4),AL3(VALYEAR-T60330)                           
         DC    CL4'YTD',AL1(TYPE4),AL3(VALYTD-T60330)                           
NPRMTYPE EQU   (*-PRMTBL)/PRMNEXTQ                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DEFTAB   DS    0C                  SEE DEFTD                                    
*                                                                               
DEFAC    DC    AL1(DEFACX-*)                                                    
         DC    CL4'ACC'                                                         
         DC    AL1(12)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFACX-*)/L'DEFDSC)                                         
         DC    CL28'ACCOUNT CODE(12)'                                           
DEFACX   DS    0C                                                               
*                                                                               
DEFAN    DC    AL1(DEFANX-*)                                                    
         DC    CL4'ACN'                                                         
         DC    AL1(36)                                                          
         DC    AL1(COL+DNAME+DCODE)                                             
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFANX-*)/L'DEFDSC)                                         
         DC    CL28'ACCOUNT NAME(36)'                                           
DEFANX   DS    0C                                                               
*                                                                               
DEFA1C   DC    AL1(DEFA1CX-*)                                                   
         DC    CL4'A1C'                                                         
         DC    AL1(12)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVA-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVA-T60330)                                              
         DC    AL1((DEFA1CX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 1'                                                    
DEFA1CX  DS    0C                                                               
*                                                                               
DEFA1N   DC    AL1(DEFA1NX-*)                                                   
         DC    CL4'A1N'                                                         
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVA-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVA-T60330)                                              
         DC    AL1((DEFA1NX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 1'                                                    
DEFA1NX  DS    0C                                                               
*                                                                               
DEFA2C   DC    AL1(DEFA2CX-*)                                                   
         DC    CL4'A2C'                                                         
         DC    AL1(12)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVB-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVB-T60330)                                              
         DC    AL1((DEFA2CX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 2'                                                    
DEFA2CX  DS    0C                                                               
*                                                                               
DEFA2N   DC    AL1(DEFA2NX-*)                                                   
         DC    CL4'A2N'                                                         
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVB-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVB-T60330)                                              
         DC    AL1((DEFA2NX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 2'                                                    
DEFA2NX  DS    0C                                                               
*                                                                               
DEFA3C   DC    AL1(DEFA3CX-*)                                                   
         DC    CL4'A3C'                                                         
         DC    AL1(12)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVC-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVC-T60330)                                              
         DC    AL1((DEFA3CX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 3'                                                    
DEFA3CX  DS    0C                                                               
         SPACE 1                                                                
DEFA3N   DC    AL1(DEFA3NX-*)                                                   
         DC    CL4'A3N'                                                         
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVC-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVC-T60330)                                              
         DC    AL1((DEFA3NX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 3'                                                    
DEFA3NX  DS    0C                                                               
*                                                                               
DEFA4C   DC    AL1(DEFA4CX-*)                                                   
         DC    CL4'A4C'                                                         
         DC    AL1(12)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVD-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVD-T60330)                                              
         DC    AL1((DEFA4CX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 4'                                                    
DEFA4CX  DS    0C                                                               
*                                                                               
DEFA4N   DC    AL1(DEFA4NX-*)                                                   
         DC    CL4'A4N'                                                         
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(VALLEVD-T60330)                                              
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(VALLEVD-T60330)                                              
         DC    AL1((DEFA4NX-*)/L'DEFDSC)                                        
         DC    CL28'LEVEL 4'                                                    
DEFA4NX  DS    0C                                                               
*                                                                               
DEFMPGC  DC    AL1(DEFMPGCX-*)                                                  
         DC    CL4'MPGC'                                                        
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMPGCX-*)/L'DEFDSC)                                       
         DC    CL28'MEDIA PROD GRP CODE'                                        
DEFMPGCX DS    0C                                                               
*                                                                               
DEFMPGN  DC    AL1(DEFMPGNX-*)                                                  
         DC    CL4'MPGN'                                                        
         DC    AL1(36)                                                          
         DC    AL1(ROW+COL+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMPGNX-*)/L'DEFDSC)                                       
         DC    CL28'MEDIA PROD GRP NAME'                                        
DEFMPGNX DS    0C                                                               
*                                                                               
DEFBLNK  DC    AL1(DEFBLNKX-*)                                                  
         DC    CL4'BLKN'                                                        
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFBLNKX-*)/L'DEFDSC)                                       
         DC    CL28'SPACES WHERE N=1-9'                                         
DEFBLNKX DS    0C                                                               
*        SPACE 1                                                                
DEFPAGE  DC    AL1(DEFPAGEX-*)                                                  
         DC    CL4'PG'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPAGEX-*)/L'DEFDSC)                                       
         DC    CL28'PAGE NUMBER'                                                
DEFPAGEX DS    0C                                                               
*                                                                               
DEFRDTE  DC    AL1(DEFRDTEX-*)                                                  
         DC    CL4'TD'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFRDTEX-*)/L'DEFDSC)                                       
         DC    CL28'REQUEST DATE'                                               
DEFRDTEX DS    0C                                                               
*                                                                               
DEFDPAR  DC    AL1(DEFDPARX-*)                                                  
         DC    CL4'DPAR'                                                        
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFDPARX-*)/L'DEFDSC)                                       
         DC    CL28'STANDARD DATE FORMS'                                        
DEFDPARX DS    0C                                                               
*                                                                               
DEFPER   DC    AL1(DEFPERX-*)                                                   
         DC    CL4'PER'                                                         
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPERX-*)/L'DEFDSC)                                        
         DC    CL28'REQUEST PERIOD'                                             
DEFPERX  DS    0C                                                               
*                                                                               
DEFFMT   DC    AL1(DEFFMTX-*)                                                   
         DC    CL4'FMT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(HDL+DCODE)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFFMTX-*)/L'DEFDSC)                                        
         DC    CL28'FORMAT'                                                     
DEFFMTX  DS    0C                                                               
*                                                                               
DEFEDTE  DC    AL1(DEFEDTEX-*)                                                  
         DC    CL4'EDTE'                                                        
         DC    AL1(8)                                                           
         DC    AL1(HDL+COL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFEDTEX-*)/L'DEFDSC)                                       
         DC    CL28'END DATE'                                                   
DEFEDTEX DS    0C                                                               
*                                                                               
DEFAADR  DC    AL1(DEFAADRX-*)                                                  
         DC    CL4'AA'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFAADRX-*)/L'DEFDSC)                                       
         DC    CL28'AGENCY ADDRESS'                                             
DEFAADRX DS    0C                                                               
*                                                                               
DEFANME  DC    AL1(DEFANMEX-*)     AGENCY NAME                                  
         DC    CL4'AN'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFANMEX-*)/L'DEFDSC)                                       
         DC    CL28'AGENCY NAME'                                                
DEFANMEX DS    0C                                                               
*                                                                               
DEFOADR  DC    AL1(DEFOADRX-*)                                                  
         DC    CL4'OA'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFOADRX-*)/L'DEFDSC)                                       
         DC    CL28'OFFICE ADDRESS'                                             
DEFOADRX DS    0C                                                               
*                                                                               
DEFONME  DC    AL1(DEFONMEX-*)     OFFICE NAME VIA IDI RECORD                   
         DC    CL4'ON'                                                          
         DC    AL1(0)                                                           
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFONMEX-*)/L'DEFDSC)                                       
         DC    CL28'OFFICE NAME'                                                
DEFONMEX DS    0C                                                               
*                                                                               
DEFAGE   DC    AL1(DEFAGEX-*)                                                   
         DC    CL4'AGE'                                                         
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFAGEX-*)/L'DEFDSC)                                        
         DC    CL28'AGEING COLUMN'                                              
DEFAGEX  DS    0C                                                               
*                                                                               
DEFMED   DC    AL1(DEFMEDX-*)                                                   
         DC    CL4'MN'                                                          
         DC    AL1(14)                                                          
         DC    AL1(ROW+COL+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMEDX-*)/L'DEFDSC)                                        
         DC    CL28'BILL SOURCE(12) (CONTRA)'                                   
DEFMEDX  DS    0C                                                               
*                                                                               
DEFMOA   DC    AL1(DEFMOAX-*)                                                   
         DC    CL4'MOAR'                                                        
         DC    AL1(12)                                                          
         DC    AL1(HDL)                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMOAX-*)/L'DEFDSC)                                        
         DC    CL28'MONTH OF ACTIVITY'                                          
DEFMOAX  DS    0C                                                               
*                                                                               
DEFMOS   DC    AL1(DEFMOSX-*)                                                   
         DC    CL4'MOS'                                                         
         DC    AL1(8)                                                           
         DC    AL1(ROW+COL+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMOSX-*)/L'DEFDSC)                                        
         DC    CL28'MOS(MMMYY)'                                                 
DEFMOSX  DS    0C                                                               
*                                                                               
DEFCALC  DC    AL1(DEFCALCX-*)                                                  
         DC    CL4'='                                                           
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM+CALC+RSCPCT)                                            
         DC    AL1(0)                                                           
         DC    AL1(6)                                                           
         DC    AL3(VALCALC-T60330)                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCALCX-*)/L'DEFDSC)                                       
         DC    CL28'CALCULATE COLUMN(13) +-/*%'                                 
         DC    CL28'=,COLN,OP,COLN(,OP,COLN)'                                   
DEFCALCX DS    0C                                                               
         SPACE 1                                                                
DEFCD    DC    AL1(DEFCDX-*)                                                    
         DC    CL4'CD'                                                          
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCDX-*)/L'DEFDSC)                                         
         DC    CL28'CASH DISCOUNT(13)'                                          
DEFCDX   DS    0C                                                               
         SPACE 1                                                                
DEFCLIC  DC    AL1(DEFCLICX-*)                                                  
         DC    CL4'CC'                                                          
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCLICX-*)/L'DEFDSC)                                       
         DC    CL28'CLIENT CODE(3)'                                             
DEFCLICX DS    0C                                                               
         SPACE 1                                                                
DEFCLIN  DC    AL1(DEFCLINX-*)                                                  
         DC    CL4'CN'                                                          
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCLINX-*)/L'DEFDSC)                                       
         DC    CL28'CLIENT NAME(36)'                                            
DEFCLINX DS    0C                                                               
         SPACE 1                                                                
DEFPGCC  DC    AL1(DEFPGCCX-*)                                                  
         DC    CL4'PGCC'                                                        
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPGCCX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT GROUP CLI CODE(3)'                                  
DEFPGCCX DS    0C                                                               
         SPACE 1                                                                
DEFPGCN  DC    AL1(DEFPGCNX-*)                                                  
         DC    CL4'PGCN'                                                        
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPGCNX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT GROUP CLI NAME(36)'                                 
DEFPGCNX DS    0C                                                               
         SPACE 1                                                                
DEFCR    DC    AL1(DEFCRX-*)                                                    
         DC    CL4'CR'                                                          
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCRX-*)/L'DEFDSC)                                         
         DC    CL28'CREDITS(13)'                                                
DEFCRX   DS    0C                                                               
         SPACE 1                                                                
DEFCKNO  DC    AL1(DEFCKNOX-*)                                                  
         DC    CL4'CKNO'                                                        
         DC    AL1(7)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCKNOX-*)/L'DEFDSC)                                       
         DC    CL28'CHECK NUMBER(6)'                                            
DEFCKNOX DS    0C                                                               
*                                                                               
DEFCDTE  DC    AL1(DEFCDTEX-*)                                                  
         DC    CL4'CDTE'                                                        
         DC    AL1(8)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFCDTEX-*)/L'DEFDSC)                                       
         DC    CL28'CHECK DATE(8)'                                              
DEFCDTEX DS    0C                                                               
*                                                                               
DEFPDTE  DC    AL1(DEFPDTEX-*)                                                  
         DC    CL4'PDTE'                                                        
         DC    AL1(12)                                                          
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPDTEX-*)/L'DEFDSC)                                       
         DC    CL28'DEPOSIT DATE(8)'                                            
DEFPDTEX DS    0C                                                               
*                                                                               
DEFDUE   DC    AL1(DEFDUEX-*)                                                   
         DC    CL4'DDTE'                                                        
         DC    AL1(8)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFDUEX-*)/L'DEFDSC)                                        
         DC    CL28'DUE DATE(8)'                                                
DEFDUEX  DS    0C                                                               
*                                                                               
DEFDR    DC    AL1(DEFDRX-*)                                                    
         DC    CL4'DR'                                                          
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFDRX-*)/L'DEFDSC)                                         
         DC    CL28'DEBITS(13)'                                                 
DEFDRX   DS    0C                                                               
*                                                                               
DEFGRS   DC    AL1(DEFGRSX-*)                                                   
         DC    CL4'GRS'                                                         
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFGRSX-*)/L'DEFDSC)                                        
         DC    CL28'GROSS(13)'                                                  
DEFGRSX   DS    0C                                                              
*                                                                               
DEFGST   DC    AL1(DEFGSTX-*)                                                   
         DC    CL4'GST'                                                         
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFGSTX-*)/L'DEFDSC)                                        
         DC    CL28'GST(13)'                                                    
DEFGSTX   DS    0C                                                              
*                                                                               
DEFINC   DC    AL1(DEFINCX-*)                                                   
         DC    CL4'INC'                                                         
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFINCX-*)/L'DEFDSC)                                        
         DC    CL28'INCOME(13)'                                                 
DEFINCX   DS    0C                                                              
*                                                                               
DEFNET   DC    AL1(DEFNETX-*)                                                   
         DC    CL4'NET'                                                         
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFNETX-*)/L'DEFDSC)                                        
         DC    CL28'NET(13)'                                                    
DEFNETX  DS    0C                                                               
*                                                                               
DEFNAR   DC    AL1(DEFNARX-*)                                                   
         DC    CL4'NARR'                                                        
         DC    AL1(36)                                                          
         DC    AL1(COL+DCODE)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFNARX-*)/L'DEFDSC)                                        
         DC    CL28'NARRATIVE(72)'                                              
DEFNARX   DS    0C                                                              
*                                                                               
DEFJOBC  DC    AL1(DEFJOBCX-*)                                                  
         DC    CL4'JOBC'                                                        
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFJOBCX-*)/L'DEFDSC)                                       
         DC    CL28'JOB CODE(6)'                                                
DEFJOBCX DS    0C                                                               
         SPACE 1                                                                
DEFJOBN  DC    AL1(DEFJOBNX-*)                                                  
         DC    CL4'JOBN'                                                        
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFJOBNX-*)/L'DEFDSC)                                       
         DC    CL28'JOB NAME(36)'                                               
DEFJOBNX DS    0C                                                               
         SPACE 1                                                                
DEFESTC  DC    AL1(DEFESTCX-*)                                                  
         DC    CL4'ESTC'                                                        
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFESTCX-*)/L'DEFDSC)                                       
         DC    CL28'EST CODE(6)'                                                
DEFESTCX DS    0C                                                               
         SPACE 1                                                                
DEFESTN  DC    AL1(DEFESTNX-*)                                                  
         DC    CL4'ESTN'                                                        
         DC    AL1(24)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFESTNX-*)/L'DEFDSC)                                       
         DC    CL28'EST NAME(36)'                                               
DEFESTNX DS    0C                                                               
         SPACE 1                                                                
DEFF1    DC    AL1(DEFF1X-*)                                                    
         DC    CL4'F1'                                                          
         DC    AL1(2)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFF1X-*)/L'DEFDSC)                                         
         DC    CL28'ACCOUNT FILTER 1'                                           
DEFF1X   DS    0C                                                               
         SPACE 1                                                                
DEFF2    DC    AL1(DEFF2X-*)                                                    
         DC    CL4'F2'                                                          
         DC    AL1(2)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFF2X-*)/L'DEFDSC)                                         
         DC    CL28'ACCOUNT FILTER 2'                                           
DEFF2X   DS    0C                                                               
         SPACE 1                                                                
DEFF3    DC    AL1(DEFF3X-*)                                                    
         DC    CL4'F3'                                                          
         DC    AL1(2)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFF3X-*)/L'DEFDSC)                                         
         DC    CL28'ANALYSIS FILTER'                                            
DEFF3X   DS    0C                                                               
         SPACE 1                                                                
DEFF4    DC    AL1(DEFF4X-*)                                                    
         DC    CL4'F4'                                                          
         DC    AL1(2)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFF4X-*)/L'DEFDSC)                                         
         DC    CL28'SUBCOMPANY FILTER'                                          
DEFF4X   DS    0C                                                               
         SPACE 1                                                                
DEFFG    DC    AL1(DEFFGX-*)                                                    
         DC    CL4'FG'                                                          
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFFGX-*)/L'DEFDSC)                                         
         DC    CL28'FINANCIAL GROUP CODE(3)'                                    
DEFFGX   DS    0C                                                               
         SPACE 1                                                                
DEFFGN   DC    AL1(DEFFGNX-*)                                                   
         DC    CL4'FGN'                                                         
         DC    AL1(36)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFFGNX-*)/L'DEFDSC)                                        
         DC    CL28'FINANCIAL GROUP NAME(36)'                                   
DEFFGNX  DS    0C                                                               
         SPACE 1                                                                
DEFDATE  DC    AL1(DEFDATEX-*)                                                  
         DC    CL4'BDTE'                                                        
         DC    AL1(8)                                                           
         DC    AL1(ROW+COL+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFDATEX-*)/L'DEFDSC)                                       
         DC    CL28'INVOICE DATE(8)'                                            
DEFDATEX DS    0C                                                               
         SPACE 1                                                                
DEFBILL  DC    AL1(DEFBILLX-*)                                                  
         DC    CL4'BILL'                                                        
         DC    AL1(8)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFBILLX-*)/L'DEFDSC)                                       
         DC    CL28'INVOICE NUMBER(8)'                                          
DEFBILLX DS    0C                                                               
         SPACE 1                                                                
DEFSMC   DC    AL1(DEFSMCX-*)                                                   
         DC    CL4'SMC'                                                         
         DC    AL1(2)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFSMCX-*)/L'DEFDSC)                                        
         DC    CL28'SYSTEM/MEDIA CODE(2)'                                       
DEFSMCX  DS    0C                                                               
         SPACE 1                                                                
DEFMC    DC    AL1(DEFMCX-*)                                                    
         DC    CL4'MC'                                                          
         DC    AL1(8)                                                           
         DC    AL1(COL+ROW)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFMCX-*)/L'DEFDSC)                                         
         DC    CL28'MEDIA CODE(1)'                                              
DEFMCX   DS    0C                                                               
         SPACE 1                                                                
DEFSYSC  DC    AL1(DEFSYSCX-*)                                                  
         DC    CL4'SYSC'                                                        
         DC    AL1(7)                                                           
         DC    AL1(COL+ROW)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFSYSCX-*)/L'DEFDSC)                                       
         DC    CL28'SYSTEM CODE(1)'                                             
DEFSYSCX DS    0C                                                               
         SPACE 1                                                                
DEFPRDC  DC    AL1(DEFPRDCX-*)                                                  
         DC    CL4'PR'                                                          
         DC    AL1(3)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPRDCX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT CODE(3)'                                            
DEFPRDCX DS    0C                                                               
*                                                                               
DEFPRDN  DC    AL1(DEFPRDNX-*)                                                  
         DC    CL4'PN'                                                          
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPRDNX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT NAME(36)'                                           
DEFPRDNX DS    0C                                                               
*                                                                               
DEFPGPC  DC    AL1(DEFPGPCX-*)                                                  
         DC    CL4'PGPC'                                                        
         DC    AL1(3)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPGPCX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT GROUP PRD CODE(3)'                                  
DEFPGPCX DS    0C                                                               
*                                                                               
DEFPGPN  DC    AL1(DEFPGPNX-*)                                                  
         DC    CL4'PGPN'                                                        
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE1+TYPE3)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFPGPNX-*)/L'DEFDSC)                                       
         DC    CL28'PRODUCT GROUP PRD NAME(36)'                                 
DEFPGPNX DS    0C                                                               
*                                                                               
DEFJEC   DC    AL1(DEFJECX-*)                                                   
         DC    CL4'JEC'                                                         
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFJECX-*)/L'DEFDSC)                                        
         DC    CL28'EST/JOB CODE(6)'                                            
DEFJECX  DS    0C                                                               
*                                                                               
DEFJEN   DC    AL1(DEFJENX-*)                                                   
         DC    CL4'JEN'                                                         
         DC    AL1(36)                                                          
         DC    AL1(COL+ROW+DNAME+DCODE)                                         
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(2)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFJENX-*)/L'DEFDSC)                                        
         DC    CL28'EST/JOB NAME(36)'                                           
DEFJENX  DS    0C                                                               
*                                                                               
DEFAGE1  DC    AL1(DEFAGE1X-*)                                                  
         DC    CL4'AGE1'                                                        
         DC    AL1(13)                                                          
         DC    AL1(COL)                                                         
         DC    AL1(ACUM)                                                        
         DC    AL1(TYPE2+TYPE4)                                                 
         DC    AL1(3)                                                           
         DC    AL3(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFAGE1X-*)/L'DEFDSC)                                       
         DC    CL28'AGEING COLUMN'                                              
DEFAGE1X DS    0C                                                               
*                                                                               
DEFNUM2  DC    AL1(DEFNUM2X-*)                                                  
         DC    CL4'NUM2'                                                        
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFNUM2X-*)/L'DEFDSC)                                       
         DC    CL28'NUM2(6)'                                                    
DEFNUM2X DS    0C                                                               
*                                                                               
DEFTC    DC    AL1(DEFTCX-*)                                                    
         DC    CL4'TC'                                                          
         DC    AL1(13)                                                          
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFTCX-*)/L'DEFDSC)                                         
         DC    CL28'TALENT CLIENT(6)'                                           
DEFTCX   DS    0C                                                               
*                                                                               
DEFTP    DC    AL1(DEFTPX-*)                                                    
         DC    CL4'TP'                                                          
         DC    AL1(14)                                                          
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFTPX-*)/L'DEFDSC)                                         
         DC    CL28'TALENT PRODUCT(6)'                                          
DEFTPX   DS    0C                                                               
*                                                                               
DEFTJ    DC    AL1(DEFTJX-*)                                                    
         DC    CL4'TJ'                                                          
         DC    AL1(6)                                                           
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFTJX-*)/L'DEFDSC)                                         
         DC    CL28'TALENT JOB(6)'                                              
DEFTJX   DS    0C                                                               
*                                                                               
DEFTE    DC    AL1(DEFTEX-*)                                                    
         DC    CL4'TE'                                                          
         DC    AL1(10)                                                          
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1(TYPE3)                                                       
         DC    AL1(1)                                                           
         DC    AL3(0)                                                           
         DC    AL1((DEFTEX-*)/L'DEFDSC)                                         
         DC    CL28'TALENT EST'                                                 
DEFTEX   DS    0C                                                               
*                                                                               
DEFUF    DC    AL1(DEFUFX-*)                                                    
         DC    CL4'UF'                                                          
         DC    AL1(10)                                                          
         DC    AL1(COL+ROW+DCODE)                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(4)                                                           
         DC    AL3(VALUF-T60330)                                                
         DC    AL1(0)                                                           
         DC    AL1(4)                                                           
         DC    AL3(VALUF-T60330)                                                
         DC    AL1((DEFUFX-*)/L'DEFDSC)                                         
         DC    CL28'USER FIELD(10)'                                             
DEFUFX   DS    0C                                                               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR LOCAL WORKING STORAGE                                  
LWSD     DSECT                                                                  
PRELO    DS    F                                                                
ADEFTAB  DS    A                                                                
SCANNER  DS    A                                                                
LEVNUM   DS    CL1                 LEVEL ON OR WANT                             
*                                                                               
ACCTYPE  DS    CL8                 EDITING FIELD FOR ACCT LEVEL NAMES           
         ORG   ACCTYPE+5                                                        
LEVLEN   DS    CL2                 LENGTH OF LEVEL CONVERT TO EBCDIC            
*                                                                               
         ORG                                                                    
SQUISH   DS    CL28                                                             
FINDDEF  DS    CL4                 FIND DEFINITION IN TABLE                     
INPRMS   DS    CL1                 NUMBER OF PARAMETERS USED                    
NPARMS   DS    CL1                 NUMBER OF PARAMETERS USED                    
ONEXONLY DS    CL1                 FLAG FOR ONE TIME ONLY                       
MYFNDX   DS    CL1                 KEEP TRACK OF PARAMETER NUMBER               
USEDTYPE DS    XL1                 USED TYPE OF PARAMETERS                      
BYTE     DS    CL1                                                              
SIDE     DS    CL1                                                              
LINES    DS    CL1                                                              
FLAG1    DS    CL1                                                              
TWIDTH   DS    CL1                 TOTAL WIDTH OF ALL COLUMNS                   
TOTROWS  DS    CL1                 TOTAL NUMBER OF ROWS                         
TEMPCHAR DS    CL1                 HOLD CHARACTER TO CONVERT TO NUMBER          
COLARRY  DS    CL(MAXCOLS+1)       ONE BYTE ARRAY TO CHECK SORT OPTION          
COLARRY2 DS    CL(MAXCOLS+1)       ONE BYTE ARRAY, ACUM TYPE COLUMN             
MAXCOLS  EQU   12                                                               
TEMPDATE DS    CL6                                                              
FLAGBIT1 DS    CL1                 TEMP STORAGE OPT1                            
FLAGBIT2 DS    CL1                 TEMP STORAGE OPT2                            
FLAGBIT3 DS    CL1                 TEMP STORAGE DATE OPTS                       
PCTCOL   DS    CL1                 WAS THIS A PERCENT COLUMN?                   
ELCODE   DS    CL1                                                              
BLOCK2   DS    CL128                                                            
LWKEY    DS    CL42                                                             
LWIO     DS    CL2000                                                           
LWSX     DS    0C                                                               
         EJECT                                                                  
*                                                                               
*              DSECT FOR DEFINITION ENTRY                                       
*                                                                               
DEFTD    DSECT                                                                  
DEFLEN   DS    XL1                 LENGTH OF THIS ENTRY                         
DEFCDE   DS    CL4                 CODE                                         
DEFWTH   DS    XL1                 DEFAULT WIDTH                                
DEFIND   DS    XL1                 STATUS INDICATORS                            
DEFCST   DS    XL1                 DEFINE COLUMN STATUS                         
DEFCPRMS DS    XL1                 TYPE OF PRMS ALLOWED IN COL                  
DEFCPMX  DS    XL1                 MAX. NUMBER OF COL PARAMETERS                
DEFCVAL  DS    AL3                 SPECIAL COL VALIDATION ROUTINE               
DEFRPRMS DS    XL1                 TYPE OF PRMS ALLOWED IN ROW                  
DEFRPMX  DS    XL1                 MAX. NUMBER OF ROW PARAMETERS                
DEFRVAL  DS    AL3                 SPECIAL ROW VALIDATION ROUTINE               
DEFNUM   DS    XL1                 NUMBER OF DESCRIPTION LINES                  
DEFDSC   DS    CL28                DESCRIPTION                                  
DEFLNQ   EQU   *-DEFTD                                                          
         EJECT                                                                  
*              DSECT TO COVER PARAMETER TYPES                                   
         SPACE 1                                                                
PRMD     DSECT                                                                  
PRMCODE  DS    CL4                                                              
PRMTYPE  DS    AL1                                                              
PRMCHECK DS    AL3                                                              
PRMNEXTQ EQU   *-PRMD                                                           
         SPACE 1                                                                
*              DSECT TO COVER SMALL DATEKEY TABLE                               
DTETBL   DSECT                                                                  
DTETYPE  DS    CL4                 POSSIBLE PARAMETER INPUT                     
DTEMAXP  DS    AL1                 MAX NUM OF PARAMETERS                        
         ORG   DTEMAXP                                                          
DTEADDR  DS    A                   CODE TO EXECUTE IF DTETYPE MATCHED           
         SPACE 1                                                                
*              DSECT FOR A ROW LINE                                             
         SPACE 1                                                                
ROWD     DSECT                                                                  
ROWNUMH  DS    CL8                 HEADER FOR NUMBER                            
ROWNUM   DS    CL1                 ROW NUMBER                                   
ROWDATAH DS    CL8                 HEADER FOR DATA                              
ROWDATA  DS    CL12                DATA                                         
ROWTYPEH DS    CL8                 HEADER FOR ROW TYPE H OR M                   
ROWTYPE  DS    CL1                 PAGE                                         
ROWTOTLH DS    CL8                 HEADER FOR TOTAL                             
ROWTOTL  DS    CL1                 TOTAL                                        
ROWLNQ   EQU   *-ROWD                                                           
*                                                                               
*              DSECT FOR A COLUMN LINE                                          
*                                                                               
COLD     DSECT                                                                  
COLNUMH  DS    CL8                 HEADER FOR NUMBER                            
COLNUM   DS    CL2                 COLUMN NUMBER                                
COLDATAH DS    CL8                 HEADER FOR DATA                              
COLDATA  DS    CL12                DATA                                         
COLPARAH DS    CL8                 HEADER FOR DATE RANGE PARAMETERS             
COLPARA  DS    CL10                DATE RANGE PARAMETERS                        
COLWIDEH DS    CL8                 HEADER FOR WIDTH                             
COLWIDE  DS    CL2                 WIDTH                                        
COLTOTLH DS    CL8                 HEADER TO TOTAL                              
COLTOTL  DS    CL1                 TOTAL ON THIS COLUMN? (Y/N)                  
COLHIDEH DS    CL8                 HEADER FOR HIDDEN COLUMN FLAG                
COLHIDE  DS    CL1                 HIDDEN COLUMN FLAG (Y/N)                     
COLSORTH DS    CL8                 HEADER FOR SORT SEQUENCE                     
COLSORT  DS    CL2                 COL SORT SEQUENCE NUMBER                     
COLHED1H DS    CL8                 HEADER COL HEAD 1                            
COLHED1  DS    CL12                COL HEAD 1                                   
COLHED2H DS    CL8                 HEADER COL HEAD 2                            
COLHED2  DS    CL12                COL HEAD 2                                   
COLLNQ   EQU   *-COLD                                                           
         EJECT                                                                  
*                                                                               
*              DSECT FOR THE SCREEN TABLE                                       
*                                                                               
SCRD     DSECT                                                                  
SCRACT   DS    XL1                 ACTION EQUATE                                
SCRCDE   DS    XL1                 SCREEN CODE                                  
SCRSTRT  DS    XL2                 DISP.  FROM START OF TWA                     
SCRLEN   DS    XL2                 LENGTH OF SAVE                               
SCRLNQ   EQU   *-SCRD                                                           
         EJECT                                                                  
*              DSECT FOR BASE SCREEN                                            
*                                                                               
       ++INCLUDE ACLFMFFD                                                       
         EJECT                                                                  
*              DSECT FOR HEADLINE DEFINITIONS                                   
*                                                                               
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMD9D                                                       
         EJECT                                                                  
*              DSECT FOR ROW DEFINITIONS                                        
*                                                                               
         ORG   HDLBGNH                                                          
       ++INCLUDE ACLFMD5D                                                       
         EJECT                                                                  
*              DSECT FOR COLUMN DEFINITIONS                                     
*                                                                               
         ORG   HDLBGNH                                                          
       ++INCLUDE ACLFMD7D                                                       
         EJECT                                                                  
*              DSECT FOR PROFILE DEFINITIONS                                    
*                                                                               
         ORG   HDLBGNH                                                          
       ++INCLUDE ACLFMD3D                                                       
         EJECT                                                                  
*              DSECT FOR HELP SCREEN                                            
*                                                                               
         ORG   HDLBGNH                                                          
       ++INCLUDE ACLFMD6D                                                       
         ORG   LOGTABH+2400                                                     
SCEND    DS    0C                                                               
*                                                                               
*        STORAGE THAT REMAINS WHILE CONNECTED TO PROGRAM                        
*                                                                               
LEVA     DS    CL1                 LEVEL ONE LENGTH OF LEDGER                   
LEVANAME DS    CL15                LEVEL ONE DESCRIPTION                        
LEVB     DS    CL1                 LEVEL TWO LENGTH OF LEDGER                   
LEVBNAME DS    CL15                LEVEL TWO DESCRIPTION                        
LEVC     DS    CL1                 LEVEL THREE LENGTH OF LEDGER                 
LEVCNAME DS    CL15                LEVEL THREE DESCRIPTION                      
LEVD     DS    CL1                 LEVEL FOUR LENGTH OF LEDGER                  
LEVDNAME DS    CL15                LEVEL FOUR DESCRIPTION                       
         EJECT                                                                  
         ORG   T603FFD                                                          
*                                  ***** TERMINAL WORK AREA *****               
*                                                                               
TWAMXLEN EQU   6144                MAXIMUM LENGTH OF TWA RECORD                 
*                                                                               
TWATASK  DS    C         +0        PROCESSING TASK NUMBER                       
TWAOFFC  DS    C         +1        OFFICE CODE                                  
TWATRM   DS    H         +2        TERMINAL NUMBER                              
TWALEN   DS    H         +4        MESSAGE LENGTH                               
TWAACCS  DS    CL4       +6        RESTRICTED ACCESS CODE                       
TWASMI   DS    0H        +10       DISPLACEMENT TO SMI  (NON-OLAI)              
TWAUSRID DS    H         +10       CONNECT ID NUM (OLAI)                        
TWAAUTH  DS    H         +12       AUTHORIZATION CODE                           
TWAAGY   DS    H         +14       EBCDIC AGENCY CODE                           
*                                                                               
TWAUSER  DS    CL48      +16       AVAILABLE FOR USER                           
         ORG   TWAUSER                                                          
LASTSEQ  DS    CL1                 USED BY BASE (PHASE/SCREEN SEQ.)             
         DS    CL1                                                              
         DS    CL1                                                              
CSACTN   DS    CL1                 CURRENT ACTION VALUE                         
SVPFKEY  DS    CL1                 SAVED PFKEY - LAST ENTERED                   
RTNCURSE DS    A                   A(SAVED CURSOR FIELD)                        
LSTCODE  DS    CL8                 SAVE FORMAT CODE                             
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMEQU                                                       
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        DDFLDIND                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'130ACLFM30   05/01/02'                                      
         END                                                                    
