*          DATA SET PPREP0302S AT LEVEL 004 AS OF 05/01/02                      
*PHASE PP0302S,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*                                                                               
*        THIS PROGRAM WILL READ PUBS AND CHECK PUB ADDRESS OVERRIDE             
*          ELEMENTS AGAINST THE REPRECS FOR THAT PUB                            
*        IF THERE IS A REP RECORD AT THE SAME LEVEL AS A PUB                    
*          ADDRESS OVERIDE OF THE SAME "TYPE" (PAY,TRA,CON)                     
*          SELECTED INFORMATION WILL BE LISTED                                  
*                                                                               
*        QOPT1=Y     CHECK ONLY FOR OFFICE LEVEL MATCHES                        
*                                                                               
         PRINT NOGEN                                                            
PP0302   CSECT                                                                  
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   MATCNT,=P'0'        USED TO COUNT MATCHES                        
         ZAP   OFCCNT,=P'0'        USED TO COUNT MATCHES (OFFICE LVL)           
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST PUB FOR REQUEST                        
         MVC   SKEY,KEY            SAVE PPG'S KEY                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY      STARTING AGY                                 
         MVC   KEY+2(1),QMEDIA     STARTING MED                                 
         MVI   KEY+3,X'01'         AGENCY RECORD                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   KEY(64),SKEY        RESTORE PPG'S KEY                            
         XC    LASTPUB,LASTPUB                                                  
         XC    LASTREP,LASTREP                                                  
*                                                                               
REQF10   MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET PAGE NUM                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         CLC   QAGENCY,PUBKAGY     SAME AGENCY ?                                
         BNE   EXIT                NO - IGNORE                                  
         AP    INCNT,=P'1'                                                      
         LA    R2,PUBREC+33                                                     
PROCB    MVI   ELCODE,X'14'                                                     
         BAS   RE,NEXTEL           PUB HAS REP ELEMENT ?                        
         BNE   EXIT                NO - DONE WITH THIS PUB                      
*                                                                               
         USING PUBREPEL,R2                                                      
         XC    WRKREP,WRKREP                                                    
         CLI   QOPT1,C'Y'          OFFICE LEVEL ONLY WANTED ?                   
         BNE   PROCB05             NO                                           
         CLI   PUBRPOFF+1,X'FF'    DEFAULT LEVEL ?                              
         BE    PROCB               YES - BYPASS                                 
PROCB05  CLI   PUBRPOFF,X'FF'                                                   
         BL    PROCB               BYPASS CLIENT LEVEL                          
         MVC   SVOFC,PUBRPOFF      SAVE REP OFFICE                              
         OC    PUBPAREP,PUBPAREP   PAYING REP PRESENT ?                         
         BZ    PROCB10             NO - CHECK TRAFFIC                           
         MVC   WRKREP,PUBPAREP                                                  
         BAS   RE,RDREPA           REP ADDRESS RECORD THERE ?                   
         BNE   PROCB10             NO - CHECK TRAFFIC                           
*                                  YES                                          
         MVC   P+34(3),=C'PAY'                                                  
         MVI   ELCODE,X'08'        PAY ADDRESS CODE                             
         BAS   RE,TESTADR          CHECK FOR REP & AOV ADDR AT SAME LVL         
*                                                                               
PROCB10  OC    PUBTRREP,PUBTRREP   TRAFFIC REP PRESENT ?                        
         BZ    PROCB20             NO - CHECK CONTRACT                          
         MVC   WRKREP,PUBTRREP                                                  
         BAS   RE,RDREPA           REP ADDRESS RECORD THERE ?                   
         BNE   PROCB20             NO - CHECK CONTRACT                          
*                                  YES                                          
         MVC   P+34(3),=C'TRA'                                                  
         MVI   ELCODE,X'09'        TRA ADDRESS CODE                             
         BAS   RE,TESTADR          CHECK FOR REP & AOV ADDR AT SAME LVL         
*                                                                               
PROCB20  OC    PUBCNREP,PUBCNREP   CONTRACT REP PRESENT ?                       
         BZ    PROCB               NO - NEXT ELEMENT                            
         MVC   WRKREP,PUBCNREP                                                  
         BAS   RE,RDREPA           REP ADDRESS RECORD THERE ?                   
         BNE   PROCB               NO - NEXT ELEMENT                            
*                                  YES                                          
         MVC   P+34(3),=C'CON'                                                  
         MVI   ELCODE,X'0A'        CON ADDRESS CODE                             
         BAS   RE,TESTADR          CHECK FOR REP & AOV ADDR AT SAME LVL         
*                                                                               
         B     PROCB               NEXT ELEMENT                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
RDREPA   NTR1                                                                   
         CLC   WRKREP,=C'0000'                                                  
         BNH   RDREPC                                                           
         MVC   SKEY,KEY            SAVE PPG'S KEY                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY      AGY                                          
         MVC   KEY+2(1),PUBKMED    MED                                          
         MVI   KEY+3,X'11'         REPS                                         
         MVC   KEY+4(4),WRKREP                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(8),KEYSAVE        AGY/MED/RECORD/REP                         
         BE    RDREPF              HAVE REP RECORD                              
*                                                                               
RDREPC   LTR   RE,RE               NO REP RECORD                                
         B     RDREPAX             CC NOT EQUAL                                 
RDREPF   DS    0H                                                               
         CLC   KEY(8),PREPREC      SAME REPREC ?                                
         BE    RDREPT              YES                                          
         XC    PREPREC(200),PREPREC                                             
         LA    R0,PREPREC          NO - GET THE RECORD                          
         ST    R0,AREC                                                          
         GOTO1 GETREP                                                           
*                                                                               
RDREPT   CR    RE,RE               CC EQUAL                                     
RDREPAX  B     EXIT                                                             
*                                                                               
*                                                                               
TESTADR  NTR1                                                                   
         LA    R2,PUBREC+33                                                     
*                                  ELCODE SET FOR ELEMENT WANTED                
TESTLUP  BAS   RE,NEXTEL           PUB HAS ADDR ELEMENT ?                       
         BNE   TESTXIT             NO - RETURN                                  
*                                                                               
         USING PUBAOVEL,R2                                                      
*                                                                               
         CLC   PUBAOFF,SVOFC       REP AND ADDROV AT SAME LEVEL ?               
         BNE   TESTLUP             NO - LOOK FOR ANOTHER AOV ELEM               
*                                                                               
         AP    MATCNT,=P'1'        USED TO COUNT MATCHES                        
         CLI   SVOFC+1,X'FF'       "DEFAULT" LEVEL ?                            
         BE    TEST10              YES                                          
         AP    OFCCNT,=P'1'        NO - MUST BE OFFICE                          
*                            ***** PRINT ADDRESS INFO                           
TEST10   MVC   P+1(2),PUBKAGY                                                   
         GOTO1 PUBEDIT,DMCB,PUBKPUB,P+4                                         
         MVC   P+20(4),WRKREP                       REP CODE                    
         GOTO1 HEXOUT,DMCB,SVOFC,P+26,L'SVOFC,=C'N'    OFFICE CODE              
*                                                                               
         CLC   LASTREP,PREPREC     SAME REP RECORD ?                            
         BNE   TEST20              NO                                           
         XC    PREPREC+35(125),PREPREC+35    YES                                
         MVC   PREPREC+65(30),=C' ****  SAME AS ABOVE  ****    '                
*                                                                               
TEST20   MVC   LASTREP,PREPREC                                                  
         LA    R5,PREPREC                                                       
         USING PREPREC,R5                                                       
*                                                                               
         MVC   P+39(30),PREPNAME                                                
         MVC   P+76(30),PUBAONAM                                                
         BAS   RE,RPRT                                                          
         MVC   P+39(30),PREPLIN1                                                
         MVC   P+76(30),PUBAOLN1                                                
         BAS   RE,RPRT                                                          
         MVC   P+39(30),PREPLIN2                                                
         MVC   P+76(30),PUBAOLN2                                                
         BAS   RE,RPRT                                                          
         MVC   P+39(20),PREPATTN                                                
         MVC   P+61(12),PREPTEL                                                 
         MVC   P+76(20),PUBAOATN                                                
         MVC   P+98(12),PUBAOTEL                                                
         BAS   RE,RPRT                                                          
*                                                                               
TESTXIT  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
PROCX    DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE PPG'S KEY                            
         B     EXIT                                                             
*                                                                               
*                             ***  NOT USED IN THIS PROGRAM  ***                
NEXTCLT  DS    0H                                                               
         ZIC   R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(6),PUBKPUB                                                 
         MVI   KEY+3,X'21'                                                      
         XC    KEY+13(12),KEY+13                                                
*****    B     PROC20                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         XC    P,P                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1          ***  NOT USED IN THIS PROGRAM  ***                       
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)      RECORD LENGTH                                   
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   DMPRECX                                                          
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
*                                                                               
DMPRECX  BAS   RE,RPRT      SKIP BETWEEN RECORDS                                
         B     EXIT                                                             
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
MATCNT   DS    PL8                                                              
         DC    CL15'SAME LEVEL ADDR'                                            
OFCCNT   DS    PL8                                                              
         DC    CL15'AT OFFICE LEVEL'                                            
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
         BUFF  LINES=2000,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(10,A)X        
               ,COMMENT=10                                                      
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
WRKREP   DS    CL4                                                              
SVOFC    DS    CL3                                                              
LASTPUB  DS    XL6                                                              
LASTREP  DS    XL8                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
ELEM     DS    CL200                                                            
*                                                                               
BUFREC   DS    0CL68                                                            
BUFKEY   DS    0CL10                                                            
BUFTYPE  DS    CL1                                                              
BUFCLT   DS    CL3                                                              
BUFPUB   DS    CL6                                                              
BUFCOM   DS    CL10                                                             
*                                                                               
BUFNET   DS    PL8                                                              
BUFCD    DS    PL8                                                              
BUFTAX   DS    PL8                                                              
BUFBASIS DS    PL8                                                              
BUFGST   DS    PL8                                                              
BUFGSTPD DS    PL8                                                              
*                                                                               
*                                                                               
       ++INCLUDE GVALUES                                                        
*                                                                               
       ++INCLUDE PBILPROF                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREP0302S05/01/02'                                      
         END                                                                    
