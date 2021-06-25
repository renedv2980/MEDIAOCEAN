*          DATA SET COPYPRTJ   AT LEVEL 059 AS OF 08/18/00                      
*PHASE COPYPRTC                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'COPYPRTJ - COPY PRINTPAK DATA'                                  
*                                                                               
*        THIS PROGRAM WILL COPY FOLLOWING RECORDS FROM JT (PRINT 5)             
*        TO H0 (PRINT 4)                                                        
*                                                                               
*        ENTER A CONTROL CARD FOR EACH CLIENT THAT IS TO BE COPIED              
*        IF YOU WISH TO COPY DATA SET UP OR AN OFFICE - ENTER                   
*        A CARD FOR THE OFFICE AS WELL.                                         
*                                                                               
*        NOTE: IF YOU WANT TO COPY DATA FOR ALL CLIENTS IN AN OFFICE            
*              YOU MUST ENTER A CARD FOR EACH CLIENT AND ONE FOR THE            
*              OFFICE.                                                          
*                                                                               
*        THE PROGRAM WILL COPY ONLY CLIENTS (OR OFFICES) ENTERED                
*        IN THE CONTROL CARDS                                                   
*                                                                               
*        THE FOLLOWING RECORDS ARE COPIED TO THE NEW AGENCY                     
*                                                                               
*        THESE RECORDS ARE COPIED REGARDLESS OF THE CLIENT/OFFICE               
*                                                                               
*        X'11'   REPS                                                           
*                                                                               
*        THE FOLLOWING RECORDS ARE ONLY COPIED TO THE NEW AGENCY                
*        IF THEY ARE FOR THE CLIENT OR OFFICE BEING COPIED                      
*                                                                               
*        X'02'   CLIENTS           (X'10', X'11' ELEMS ARE NOT COPIED)          
*        X'03'   DIVISIONS                                                      
*        X'04'   REGIONS                                                        
*        X'05'   DISTRICTS                                                      
*        X'06'   PRODUCTS                                                       
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        FOR THIS RUN THE OUTPUT AGENCY IS HARDCODED TO H0                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                                                               
COPYPRTJ CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,COPYPRT,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,COPYPRTJ+4095                                                 
         LA    R6,1(R6)                                                         
         USING COPYPRTJ+4096,R6    R6 AS 2ND BASE REGISTER                      
*                                                                               
         USING ETABD,R8                                                         
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
* SET LOOKUP TABLE BINSRCH PARS                                                 
*                                                                               
         SR    R0,R0                                                            
         L     R1,=A(LKTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,11                                                            
         LA    R4,11                                                            
         LA    R5,200                                                           
         STM   R0,R5,LKPARS                                                     
*                                                                               
START1   DS    0H                                                               
*                                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
*                                                                               
START2   DS    0H                                                               
*                                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
*                                                                               
START3   DS    0H                                                               
*                                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(6),CARD+2                                                   
         CLC   CARD+5(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   WORK+3(3),=3X'FF'                                                
         MVC   WORK+6(3),CARD+11                                                
         CLC   CARD+11(3),=C'ALL'                                               
         BNE   *+10                                                             
         MVC   WORK+6(3),=3X'FF'                                                
         MVC   WORK+9(2),=3X'FF'                                                
         CLC   CARD+20(3),=C'ALL'                                               
         BE    START3B                                                          
         CLI   CARD+20,C' '                                                     
         BE    START3B                                                          
         PACK  DUB,CARD+20(3)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   WORK+9(2),HALF                                                   
*                                                                               
START3B  DS    0H                                                               
*                                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
*                                                                               
         GOTO1 =V(BINSRCH),LKPARS,(1,WORK)                                      
         OC    LKPARS+1(3),LKPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
         XC    X,X                                                              
         OC    LKPARS+8(4),LKPARS+8                                             
         BNZ   START12                                                          
         MVC   P(18),=C'**NO INPUT CARDS**'                                     
         BAS   RE,PRNT                                                          
         B     EOJ                                                              
*                                                                               
START12  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   REC,C'Z'            SPECIAL DDS RECORD                           
         BE    EOF                 PAST ALL REAL DATA                           
*                                                                               
* ADD CHECK FOR AGENCY BEING COPIED HERE                                        
*                                                                               
         CLC   REC(2),=C'JT'       JW THOMPSON                                  
         BH    EOF                                                              
         BNE   GET                                                              
*                                                                               
* THESE RECORD(S) GO TO CLIENT ROUTINE                                          
*                                                                               
         CLI   REC+3,X'02'         CLIENT                                       
         BE    CLT                                                              
         CLI   REC+3,X'03'         DIVISIONS                                    
         BE    CLT                                                              
         CLI   REC+3,X'04'         REGIONSS                                     
         BE    CLT                                                              
         CLI   REC+3,X'05'         DISTRICTS                                    
         BE    CLT                                                              
         CLI   REC+3,X'06'         PRODUCT                                      
         BE    CLT                                                              
*                                                                               
* THESE RECORD(S) HAVE SPECIAL ROUTINES                                         
*                                                                               
         CLI   REC+3,X'11'         REPS                                         
         BE    REP                                                              
*                                                                               
         B     GET                SKIP OTHER RECORD TYPES                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLT      DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
         LA    RF,CLTCNT                                                        
         CLI   REC+3,X'02'         CLIENTS                                      
         BE    CLT10                                                            
         LA    RF,DIVCNT                                                        
         CLI   REC+3,X'03'         DIVISIONS                                    
         BE    CLT10                                                            
         LA    RF,REGCNT                                                        
         CLI   REC+3,X'04'         REGIONS                                      
         BE    CLT10                                                            
         LA    RF,DSTCNT                                                        
         CLI   REC+3,X'05'         DISTRICTS                                    
         BE    CLT10                                                            
         LA    RF,PRDCNT                                                        
         CLI   REC+3,X'06'         PRODUCTS                                     
         BE    CLT10                                                            
*                                                                               
         DC    H'0'                UNKNOWN RECORD TYPE                          
*                                                                               
CLT10    AP    0(5,RF),=P'1'                                                    
*                                                                               
* ADD CODE HERE IF SWITCHING AGENCY/MEDIA                                       
*                                                                               
AGSW     MVC   REC(2),=C'H0'       MINDSHARE                                    
*                                                                               
         CLI   REC+3,X'02'         CLIENTS REC NEED TO CHECK FURTHER            
         BNE   CLT90                                                            
*                                                                               
         LA    R5,REC                                                           
         LA    R2,REC+33           POINT TO CLT RECORD'S FIRST ELEM             
         CLI   0(R2),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                BAD RECORD OR PROCESSING WRONG REC           
*                                                                               
CLT30    LA    R2,REC+33                                                        
         MVI   ELCODE,X'10'        CONTRACT STANDARD COMMENT ELEM               
         BAS   RE,NEXTEL                                                        
         BE    CLT50               GO DELETE THIS ELEM                          
*                                                                               
         LA    R2,REC+33                                                        
         MVI   ELCODE,X'11'        I/O STANDARD COMMENT ELEMS                   
         BAS   RE,NEXTEL                                                        
         BNE   CLT60               CLT REC CHECKING IS DONE                     
*                                                                               
CLT50    DS    0H                                                               
*                                                                               
         MVC   P+01(14),=C'(X10) CONTRACT'                                      
         LA    RE,P+15                                                          
         CLI   ELCODE,X'11'                                                     
         BNE   *+14                                                             
         MVC   P+01(09),=C'(X11) I/O'                                           
         LA    RE,P+10                                                          
*                                                                               
         MVC   00(35,RE),=C'STANDARD COMMENT ELEMENT FOR MEDIA='                
         LA    RE,35(RE)                                                        
         MVC   00(01,RE),REC+02                                                 
         LA    RE,01(RE)                                                        
         MVC   00(09,RE),=C', CLIENT='                                          
         LA    RE,09(RE)                                                        
         MVC   00(03,RE),REC+04                                                 
         LA    RE,03(RE)                                                        
         MVC   00(14,RE),=C' IS NOT COPIED'                                     
         LA    RE,14(RE)                                                        
         MVC   00(19,RE),=C' *** COMENT NUMBER='                                
         LA    RE,14(RE)                                                        
         MVC   00(06,RE),2(R2)     STANDARD COMMENT NUMBER                      
         BAS   RE,PRNT                                                          
         BAS   RE,SKIP                                                          
*                                                                               
         GOTO1 =V(RECUP),DMCB,(1,(R5)),(R2),0                                   
*                                                                               
         B     CLT30               GO BACK AND CHECK FOR MORE                   
*                                                                               
CLT60    DS    0H                  LIKE TO SEE ALL CLIENT RECS                  
*                                                                               
*NOP*    MVC   P+01(17),=C'PRINTING CLIENT# '                                   
*NOP*    EDIT  (P5,CLTCNT),(10,P+18),ALIGN=LEFT,COMMAS=YES,ZERO=BLANK           
*NOP*    BAS   RE,PRNT                                                          
*                                                                               
*NOP*    LA    R5,REC                                                           
*NOP*    MVC   HALF,REC+25                                                      
*NOP*    SR    R2,R2                                                            
*NOP*    LH    R2,HALF                                                          
*                                                                               
*NOP*    GOTO1 =V(PRNTBL),DMCB,=C'CLTREC',(R5),C'DUMP',(R2),=C'1D'              
*                                                                               
CLT90    CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP      DS    0H                  OUTPUT REPS                                  
         AP    REPCNT,=P'1'                                                     
         B     AGSW                JUST GO SWITCH AGENCYS                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SFMRECS  DS    0H                                                               
         CLC   REC+10(3),=X'FFFFFF'    ALL CLT REC?                             
         BE    SFMRX                   ALWAYS COPY                              
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+10        CLT                                      
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
SFMRX    AP    0(5,R5),=P'1'                                                    
         B     AGSW                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
SRCH     DS    0H                                                               
         MVC   X(11),WORK                                                       
SRCH2    DS    0H                                                               
         GOTO1 =V(BINSRCH),LKPARS,X                                             
*                                                                               
         CLI   LKPARS,0                                                         
         BER   R9                                                               
         CLI   X+9,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+9(2),=2X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+6,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+6(3),=3X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+3,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+3(3),=3X'FF'                                                   
         B     SRCH2                                                            
*                                                                               
SRCH6    DS    0H                                                               
         CLI   LKPARS,0                                                         
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUT      DS    0H                                                               
         MVI   BYTE,X'80'                                                       
         TM    REC+27,X'20'        TEST DELETED                                 
         BNZ   *+8                                                              
         MVI   BYTE,0                                                           
         MVC   REC+27(1),BYTE                                                   
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         B     EOJ                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDT      DS    0H                                                               
*                                                                               
         MVI   P+35,C'G'                                                        
         EDIT  (B4,ETBUYS),(7,P+19),COMMAS=YES                                  
         EDIT  (B4,ETBILLS),(7,P+27),COMMAS=YES                                 
         L     R0,ETBUYGRS                                                      
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYGRS                                                      
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILGRS                                                      
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         L     R0,ETBUYAC                                                       
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYAC                                                       
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILAC                                                       
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         L     R0,ETBUYCD                                                       
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYCD                                                       
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILCD                                                       
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTPK    DS    0H                                                               
*                                                                               
         EDIT  (P8,0(R8)),(7,P+19),COMMAS=YES                                   
         EDIT  (P8,8(R8)),(7,P+27),COMMAS=YES                                   
         MVI   P+35,C'G'                                                        
         ZAP   MYDUB,16(8,R8)      ORD GRS                                      
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,40(8,R8)      PAY GROSS                                    
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,64(8,R8)      BILL GROSS                                   
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         ZAP   MYDUB,24(8,R8)      ORD AC                                       
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,48(8,R8)      PAY AC                                       
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,72(8,R8)      BILL AC                                      
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         ZAP   MYDUB,32(8,R8)      ORD CD                                       
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,56(8,R8)      PAY CD                                       
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,80(8,R8)      BILL CD                                      
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         BR    R9                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         MVC   HALF,REC+25                                                      
         SR    R2,R2                                                            
         LH    R2,HALF                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'REC',(R5),C'DUMP',(R2),=C'1D'                 
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'KEY',(R5),C'DUMP',(R2),=C'1D'                 
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLC   REC+25(2),=X'80FF'   SEE IF DIRECTORY ONLY (DELETED)             
         BE    GETRDO                                                           
         CLC   REC+25(2),=X'00FF'   SEE IF DIRECTORY ONLY                       
         BE    GETRDO                                                           
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         B     GETRX                                                            
*                                                                               
GETRDO   DS    0H               FOR DIRECTORY ONLY RECS                         
*                               NO NEED FOR END OF REC ZERO                     
GETRX    AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,DMPREC                                                        
         BAS   RE,SKIP                                                          
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
CLTCNT   DC    PL5'0',CL20'CLIENTS'                                             
PRDCNT   DC    PL5'0',CL20'PRODUCTS'                                            
DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                           
REGCNT   DC    PL5'0',CL20'REGIONS'                                             
DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                           
REPCNT   DC    PL5'0',CL20'REPS'                                                
*                                                                               
* REST OF COUNTERS SHOULD ALL BE ZERO IN THIS COPYPRT RUN                       
*                                                                               
ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                           
BUCCNT   DC    PL5'0',CL20'ESTIMATE BUCKETS'                                    
CONCNT   DC    PL5'0',CL20'CONTRACTS'                                           
BUDCNT   DC    PL5'0',CL20'BUDGETS'                                             
FSICNT   DC    PL5'0',CL20'FSI RECORDS'                                         
ISSCNT   DC    PL5'0',CL20'ISSUE DATE RECORDS'                                  
CURCNT   DC    PL5'0',CL20'CU RECORDS'                                          
SPCCNT   DC    PL5'0',CL20'SPACE RECORDS'                                       
AORCNT   DC    PL5'0',CL20'AOR RECORDS'                                         
BLFCNT   DC    PL5'0',CL20'BILLING FORMULA RECS'                                
COMMCNT  DC    PL5'0',CL20'COMMENTS'                                            
NVTCNT   DC    PL5'0',CL20'NV TEXT'                                             
JOBCNT   DC    PL5'0',CL20'JOB RECORDS'                                         
PBLCNT   DC    PL5'0',CL20'PUB LISTS'                                           
*                                                                               
* OTHER COUNTERS ADDED HERE WILL AUTOMATICALLY PRINT AT EOJ                     
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PCONREC                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PESTREC                                                        
*                                                                               
ETABD    DSECT                                                                  
ETAGY    DS    CL2                                                              
ETMED    DS    CL1                                                              
ETCLT    DS    CL3                                                              
ETPRD    DS    CL3                                                              
ETEST    DS    XL2                                                              
ETESTSW  DS    CL1                                                              
ETBUYS   DS    F                                                                
ETBILLS  DS    F                                                                
ETBUYGRS DS    F                                                                
ETBUYAC  DS    F                                                                
ETBUYCD  DS    F                                                                
ETPAYGRS DS    F                                                                
ETPAYAC  DS    F                                                                
ETPAYCD  DS    F                                                                
ETBILGRS DS    F                                                                
ETBILAC  DS    F                                                                
ETBILCD  DS    F                                                                
ETABEL   EQU   *-ETABD                                                          
*                                                                               
LKTAB    CSECT                                                                  
         DS    200CL11                                                          
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059COPYPRTJ  08/18/00'                                      
         END                                                                    
