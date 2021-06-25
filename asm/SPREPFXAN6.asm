*          DATA SET SPREPFXAN6 AT LEVEL 011 AS OF 01/19/99                      
*PHASE SPFX026                                                                  
         TITLE 'SPFX02 - FIND BAD COKE INCH RECS'                               
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
DMXIT    XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
*                                                                               
INCH     XC    INCHKEY,INCHKEY                                                  
         MVC   INCHKEY(3),=X'0E04B1'        COKE INCH RECS                      
         MVC   KEYSAVE,INCHKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',INCHKEY,INCHKEY,0             
         B     INCH05                                                           
INCHSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',INCHKEY,INCHKEY,0             
INCH05   CLC   INCHKEY(2),KEYSAVE                                               
         BH    INCHX                                                            
         CLI   INCHKEY+2,X'B2'                                                  
         BH    INCHX                                                            
*                                                                               
         LA    R3,INCHKEY                                                       
         USING MSRKEY,R3                                                        
         MVC   MSRDA,MSRDDA                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MSRDA,ADBUY,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ADBUY                                                         
         MVI   ELCODE,X'13'        INCH STATUS                                  
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         BNE   INCHSEQ                                                          
         CLC   =C'981108',5(R6)    FIND RECS RUN AFTER 11/08/98                 
         BNL   INCHSEQ             THEN SKIP                                    
         CLC   =C'990106',5(R6)    FIND RECS RUN BEFORE 1/07/99                 
         BH    INCHSEQ             THEN SKIP                                    
*                                                                               
         MVC   PTYPE,=C'CD'                                                     
         MVC   PAGY,=C'CK'                                                      
         MVI   PMED,C'T'                                                        
         CLI   MSRKAM,X'B1'        TV                                           
         BE    INCH110                                                          
         MVI   PMED,C'R'                                                        
         CLI   MSRKAM,X'B2'        RADIO                                        
         BE    INCH110                                                          
         DC    H'0'                                                             
*                                                                               
INCH110  GOTO1 CLUNPK,DMCB,MSRKCLT,PCLT                                         
*                                                                               
         LA    R2,PRDTAB                                                        
INCH120  CLC   0(2,R2),=X'FFFF'                                                 
         BE    INCH130                                                          
         CLC   0(1,R2),MSRKPRD                                                  
         BE    INCH140                                                          
         LA    R2,4(R2)                                                         
         B     INCH120                                                          
INCH130  MVC   PPRD,TEMP                                                        
         B     INCH150                                                          
INCH140  MVC   PPRD,1(R2)                                                       
*                                                                               
INCH150  GOTO1 MSUNPK,DMCB,MSRKMKT,PMKT,PSTA                                    
         EDIT  (B1,MSRKEST),(3,PEST),FILL=0                                     
         XC    MSRKMOS,=X'FFFF'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,MSRKMOS),(9,TEMP)                                 
         LA    R2,DATESTAB                                                      
INCH160  CLI   0(R2),X'FF'                                                      
         BE    INCHSEQ                                                          
         CLC   TEMP(6),0(R2)                                                    
         BE    INCH170                                                          
         LA    R2,18(R2)                                                        
         B     INCH160                                                          
INCH170  MVC   PDATES,6(R2)                                                     
*                                                                               
         GOTO1 REPORT                                                           
         B     INCHSEQ                                                          
*                                                                               
INCHX    GOTO1 AENDREQ                                                          
         DROP  R3                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,42,ELCODE                                                     
         EJECT                                                                  
DATESTAB DC    C'JUL/98',C'980629980726'                                        
         DC    C'AUG/98',C'980727980830'                                        
         DC    C'SEP/98',C'980831980927'                                        
         DC    C'OCT/98',C'980928981025'                                        
         DC    C'NOV/98',C'981026981129'                                        
         DC    C'DEC/98',C'981130981227'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
PRDTAB   DC    X'01',C'BQ '                                                     
         DC    X'02',C'CD '                                                     
         DC    X'1E',C'CF '                                                     
         DC    X'03',C'CL '                                                     
         DC    X'1C',C'CO '                                                     
         DC    X'1D',C'CT '                                                     
         DC    X'04',C'CY '                                                     
         DC    X'05',C'DC '                                                     
         DC    X'06',C'DM '                                                     
         DC    X'07',C'DS '                                                     
         DC    X'08',C'FN '                                                     
         DC    X'09',C'FR '                                                     
         DC    X'0A',C'FT '                                                     
         DC    X'0B',C'MJ '                                                     
         DC    X'0C',C'MM '                                                     
         DC    X'0D',C'MY '                                                     
         DC    X'1B',C'NC '                                                     
         DC    X'0E',C'NT '                                                     
         DC    X'0F',C'PA '                                                     
         DC    X'10',C'PB '                                                     
         DC    X'11',C'PC '                                                     
         DC    X'12',C'PD '                                                     
         DC    X'13',C'PP '                                                     
         DC    X'14',C'PR '                                                     
         DC    X'15',C'PRC'                                                     
         DC    X'16',C'PRD'                                                     
         DC    X'17',C'PRS'                                                     
         DC    X'18',C'PS '                                                     
         DC    X'19',C'PW '                                                     
         DC    X'1F',C'SG '                                                     
         DC    X'1A',C'SP '                                                     
         DC    X'FF',C'POL'                                                     
         DC    X'FFFF'                                                          
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    CL80                                                             
TABADDR  DS    A                                                                
TABCOUNT DS    F            MARKT/STAT                                          
BAGYMED  DS    CL1                                                              
MSRDA    DS    XL4                                                              
INCHKEY  DS    CL50                                                             
*                                                                               
       ++INCLUDE SPGENMSR                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PTYPE    DS    CL2                                                              
PAGY     DS    CL2                                                              
PMED     DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL3                                                              
PPRD     DS    CL3                                                              
PMKT     DS    CL4                                                              
PSTA     DS    CL5                                                              
PEST     DS    CL3                                                              
         DS    CL11                                                             
PDATES   DS    CL12                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPFXAN601/19/99'                                      
         END                                                                    
