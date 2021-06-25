*          DATA SET ACREPXG02  AT LEVEL 062 AS OF 03/04/19                      
*PHASE ACXG02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'CHANGE OFFICE CODE - IN KEY AND TRANSACTION ELEMENT'            
         PRINT NOGEN                                                            
ACXG02   CSECT                                                                  
         NMOD1 0,**ACXG**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXGD,RC                                                         
                                                                                
***********************************************************************         
* QOPT 4 = Y DUMP SOME ADDREC AND PUTRECS                             *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
RQF00    CLI   MODE,REQFRST                                                     
         BNE   PAC00                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
*                                                                               
         L     R3,ADCMPEL                                                       
         USING CPYELD,R3                                                        
         MVC   STAT4,CPYSTAT4                                                   
         DROP  R3                                                               
                                                                                
XIT      XIT1  1                                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT - FIND TRANSACTIONS                                 *         
***********************************************************************         
                                                                                
PAC00    CLI   MODE,PROCACC                                                     
         BNE   RNL00                                                            
         XC    OFFICE,OFFICE                                                    
         L     RF,ADACC                                                         
         MVC   AKEY,0(RF)                                                       
         LA    R2,AKEY                                                          
         USING ACTRECD,R2                                                       
         CLC   ACTKUNT(2),=C'SS'                                                
         BE    *+14                                                             
         CLC   ACTKUNT(2),=C'SZ'                                                
         BNE   XIT                                                              
*                                                                               
PAC03    GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAC05    GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,AKEY,ADIR                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAC07    CLC   ADIR(L'ACTKCULA),AKEY                                            
         BNE   XIT                 END OF ACCOUNT                               
         LA    R2,ADIR                                                          
         USING TRNRECD,R2          FIND OFA / CAC /  TRN                        
         TM    TRNKSTAT,TRNSDELT   TEST DELETED                                 
         BO    PAC05                                                            
         CLC   TRNKOFF,=C'ZD'                                                   
         BNE   PAC05                                                            
*                                                                               
PAC11    MVI   DMPTRN,C'N'                                                      
         MVC   AKEY,ADIR                                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ADA,AIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO                                                           
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ        TEST TRANSACTION                             
         BNE   *+14                                                             
         MVC   RECORD,=C'TRNREC'                                                
         B     PAC13                                                            
         CLI   0(R3),CACELQ        TEST CONTRA                                  
         BNE   *+14                                                             
         MVC   RECORD,=C'CHDREC'                                                
         B     PAC13                                                            
*                                                                               
         SR    R0,R0                                                            
         LA    R3,TRNRFST                                                       
PAC12    CLI   0(R3),ABLELQ                                                     
         BE    PAC12A                                                           
         CLI   0(R3),BUKELQ                                                     
         BE    PAC12B                                                           
         CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PAC12                                                            
PAC12A   MVC   RECORD,=C'OFAREC'                                                
         B     PAC13                                                            
PAC12B   MVC   RECORD,=C'BUKREC'                                                
*                                                                               
PAC13    L     RE,AIO              MOVE A TO B                                  
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN                                                     
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,BIO              SET NEW OFFICE CODE                          
         MVC   TRNKOFF,=C'DA'                                                   
         MVC   BKEY,TRNKEY                                                      
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   RECORD,C'T'                                                      
         BNE   PAC15                                                            
         MVC   TRNOFFC,=C'DA'                                                   
*                                                                               
PAC15    GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,BKEY,BDIR                     
         CLI   8(R1),X'10'         TEST RECORD NOT FOUND                        
         BE    PAC16                                                            
         CLI   8(R1),X'02'         TEST RECORD DELETED                          
         BE    PAC17                                                            
         CLI   8(R1),X'00'         TEST RECORD IS ON FILE                       
         BE    PAC17                                                            
         DC    H'0'                IO ERROR                                     
*                                                                               
PAC16    CLC   RECORD,=C'TRNREC'   TEST TRANSACTION                             
         BNE   PAC16N                                                           
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ADIR,ADIR                             
         CLI   8(R1),0             READ ORIGINAL RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ADA,AIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDA,ADA             USE SAME DISK ADDRESS                        
         AP    CHARCD,=P'1'                                                     
         MVC   ACTION,PUTREC                                                    
         L     R2,BIO                                                           
         BAS   RE,PRNT                                                          
         BAS   RE,DMPR                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PAC16X                                                           
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,BDA,BIO,DMWORK    PUT FROM B          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDIR,ADIR            OLD KEY + STATUS                            
         L     R2,BIO                                                           
         MVC   BDIR(L'TRNKEY),0(R2)  NEW KEY                                    
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,BDIR,BDIR                              
         CLI   8(R1),0             ADD NEW POINTER                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PAC16X                                                           
*                                                                               
PAC16N   AP    ADDRCD,=P'1'                                                     
         MVC   ACTION,ADDREC                                                    
         L     R2,BIO                                                           
         BAS   RE,PRNT                                                          
         BAS   RE,DMPR                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PAC16X                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0             ADD RECORD IN IOA                            
         BE    *+6                                                              
         DC    H'0'                                                             
PAC16X   B     PAC23               DELETE ADIR                                  
*                                                                               
PAC17    DS    0H                                                               
         CLC   RECORD,=C'TRNREC'   HANDLE DUPLICATE TRANSACTIONS                
         BNE   PAC18                                                            
         SR    R0,R0                                                            
         IC    R0,TRNKSBR                                                       
         AH    R0,=H'1'                                                         
         CH    R0,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                TOO MANT WITH SAME KEY                       
         STC   R0,TRNKSBR                                                       
         STC   R0,TRNSUB                                                        
         MVC   BKEY,TRNKEY                                                      
         B     PAC15               TRY AGAIN                                    
*                                                                               
PAC18    CLI   RECORD,C'O'         HANDLE DUPLICATE OFFACC RECORDS              
         BNE   PAC21                                                            
         CLI   8(R1),X'02'         TEST OFFACC DELETED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDBAL           ADD BALANCE ELEMENTS  A TO B                 
         AP    CHARCD,=P'1'                                                     
         MVC   ACTION,PUTREC                                                    
         L     R2,BIO                                                           
         BAS   RE,PRNT                                                          
         BAS   RE,DMPR                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PAC18X                                                           
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
PAC18X   B     PAC23               DELETE ADIR                                  
*                                                                               
PAC21    CLI   RECORD,C'C'         HANDLE DUPLICATE CONTRA                      
         BE    *+14                                                             
         CLI   RECORD,C'B'         HANDLE DUPLICATE BUCKET                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   8(R1),X'02'         TEST CONTRA DELETED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RECORD,C'B'         MERGE BUCKETS                                
         BNE   PAC23                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,IOA              OLD RECORD                                   
         LA    R4,CACRFST-CACRECD(RE) POINT TO FIRST ELEMENT                    
PAC22    CLI   0(R4),0             EOR                                          
         BE    PAC22G                                                           
         CLI   0(R4),BUKELQ        ARE WE AT A BUCKET?                          
         BE    PAC22B                                                           
PAC22A   SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PAC22                                                            
*                                                                               
PAC22B   SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AHI   R1,-1                                                            
         MVC   ELEMENT(0),0(R4)                                                 
         EX    R1,*-6                                                           
*                                                                               
         LA    R1,IOB              NEW RECORD                                   
         LA    RE,CACRFST-CACRECD(R1) POINT TO FIRST ELEMENT                    
PAC22C   CLI   0(RE),0             EOR                                          
         BE    PAC22F                                                           
         CLC   0(BUKDR-BUKELD,RE),ELEMENT                                       
         BE    PAC22E                                                           
PAC22D   SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     PAC22C                                                           
*                                                                               
NEW      USING BUKELD,RE                                                        
OLD      USING BUKELD,R4                                                        
PAC22E   AP    NEW.BUKDR,OLD.BUKDR                                              
         AP    NEW.BUKCR,OLD.BUKCR                                              
         B     PAC22A                                                           
         DROP  NEW,OLD                                                          
*                                                                               
PAC22F   GOTO1 HELLO,DMCB,(C'P',ACCMST),IOB,ELEMENT,0                           
         B     PAC22A                                                           
*                                                                               
PAC22G   AP    CHARCD,=P'1'                                                     
         MVC   ACTION,PUTREC                                                    
         L     R2,BIO                                                           
         BAS   RE,PRNT                                                          
         BAS   RE,DMPR                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    PAC23                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,BDA,BIO,DMWORK    PUT FROM B          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAC23    GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,ADIR,ADIR                     
         CLI   8(R1),0             READ ORIGINAL RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ADIR             DELETE ORIGINAL POINTER                      
         OI    TRNKSTAT,TRNSDELT                                                
         AP    DELRCD,=P'1'                                                     
         MVC   ACTION,DELREC                                                    
         LA    R2,ADIR                                                          
         BAS   RE,PRNT                                                          
         BAS   RE,DMPDIR                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    PAC23X                                                           
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,ADIR,ADIR                              
         CLI   8(R1),0             READ ORIGINAL RECORD                         
         BE    *+6                                                              
         DC    H'0'                                                             
PAC23X   B     PAC05                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD BALANCE ELEMENTS                                                *         
***********************************************************************         
                                                                                
ADDBAL   NTR1  ,                                                                
         L     R2,AIO                                                           
         LA    R2,ACTRFST-ACTRECD(R2)                                           
         SR    R0,R0                                                            
ADDB3    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                EXPECTED BALANCE ELEMENT                     
         CLI   0(R2),ABLELQ                                                     
         BE    ADDB5                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     ADDB3                                                            
*                                                                               
ADDB5    L     R3,BIO                                                           
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         SR    R0,R0                                                            
ADDB7    CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                EXPECTED BALANCE ELEMENT                     
         CLI   0(R3),ABLELQ                                                     
         BE    ADDB9                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     ADDB7                                                            
*                                                                               
         USING ABLELD,R3                                                        
ADDB9    AP    ABLFRWD,ABLFRWD-ABLELD(L'ABLFRWD,R2)                             
         AP    ABLDR,ABLDR-ABLELD(L'ABLDR,R2)                                   
         AP    ABLCR,ABLCR-ABLELD(L'ABLCR,R2)                                   
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK OFFICE ACCOUNT ACCOUNT RECORD                                 *         
***********************************************************************         
                                                                                
CHKOFA   NTR1  ,                                                                
         TM    STAT4,CPYSOFF2      TEST NEW OFFICE                              
         BNO   XIT                                                              
         CLI   QOPT3,C'Y'                                                       
         BNE   XIT                                                              
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         CLC   TRNKOFF,OFFICE      TEST SAME OFFICE                             
         BE    XIT                                                              
         MVC   OFFICE,TRNKOFF                                                   
         LA    R3,BKEY             BUILD KEY FOR OFFICE ACCOUNT                 
         USING OFARECD,R3                                                       
         MVC   BKEY,SPACES                                                      
         MVC   OFAKCULA,TRNKCULA   READ FOR ACCOUNT OFFICE                      
         MVC   OFAKOFF,TRNKOFF                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,BKEY,BDIR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY,BDIR           TEST RECORD FOUND                            
         BE    XIT                                                              
         BAS   RE,NEWOFA           ADD NEW OFFICE/ACCOUNT                       
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW OFFICE/ACCOUNT RECORD                                     *         
***********************************************************************         
                                                                                
NEWOFA   NTR1  ,                                                                
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING OFARECD,R3                                                       
         MVC   OFAKEY,BKEY         KEY OF CONTRA                                
         MVC   OFARLEN,=Y(OFARFST-OFAKEY)                                       
         MVC   OFAKLMOS,=X'FFFF'                                                
         LA    R5,ELEMENT                                                       
         USING ABLELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 45 ELEMENT                             
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),OFARECD,ABLEL,0                         
*                                                                               
         MVC   RECORD,=C'OFAREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         L     R2,BIO                                                           
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET CONTRA NAME                                                     *         
***********************************************************************         
                                                                                
GETNAM   NTR1  ,                                                                
         MVC   CONNAM,SPACES       CLEAR NAME                                   
         XC    CONNAML,CONNAML     LENGTH                                       
         MVC   OLDNAM,SPACES       AND OLD NAME                                 
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         CLC   TRNKCCPY,TRNKCPY    TEST REAL CONTRA                             
         BNE   XIT                                                              
         LA    R3,CKEY             BUILD KEY FOR ACCOUNT RECORD                 
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,TRNKCULC                                                
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,CKEY,CDIR                     
         CLI   8(R1),0                                                          
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,CDA,CIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,CIO                                                           
         LA    R5,ACTRFST                                                       
         USING NAMELD,R5                                                        
         SR    R0,R0                                                            
*                                                                               
GETNAM5  CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    GETNAM7                                                          
         IC    R0,NAMLN                                                         
         AR    R5,R0                                                            
         B     GETNAM5                                                          
*                                                                               
GETNAM7  SR    R1,R1               EXTRACT THE NAME                             
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         MVC   CONNAM(0),NAMEREC                                                
         EX    R1,*-6                                                           
         LA    R1,1(R1)           SAVE LENGTH                                   
         STC   R1,CONNAML                                                       
         B     XIT                                                              
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA HEADER RECORD                                          *         
***********************************************************************         
                                                                                
CHKCHD   NTR1  ,                                                                
         LA    R2,ADIR                                                          
         USING TRNRECD,R2                                                       
         LA    R3,BKEY             BUILD KEY OF CONTRA HEADER                   
         USING CHDRECD,R3                                                       
         XC    BKEY,BKEY                                                        
         MVC   CHDKCULA,TRNKCULA   READ FOR CONTRA HEADER                       
         MVC   CHDKOFF,SPACES                                                   
         MVC   CHDKCULC,TRNKCULC                                                
         MVC   CHDKSPCS,SPACES                                                  
         MVC   CHDKBTYP,SPACES                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,BKEY,BDIR                     
         BAS   RE,CHDALL                                                        
         TM    STAT4,CPYSOFF2      TEST NEW OFFICE                              
         BNO   XIT                                                              
         MVC   CHDKOFF,TRNKOFF     BUILD OFFICE CONTRA                          
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,BKEY,BDIR                     
         BAS   RE,CHDALL                                                        
         B     XIT                                                              
*                                                                               
CHDALL   LR    R0,RE                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY(CHDKBTYP-CHDRECD),BDIR     TEST RECORD FOUND                
         BNE   CHDALL3                                                          
       OC  BDIR+(CHDKNULL-CHDRECD)(L'CHDKNULL),BDIR+(CHDKNULL-CHDRECD)          
         BNZ   CHDALL3                                                          
         BAS   RE,CHKNAM           CHECK CONTRA NAME                            
         B     CHDALLX                                                          
*                                                                               
CHDALL3  BAS   RE,NEWCHD           ADD NEW CONTRA HEADER                        
CHDALLX  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA HEADER RECORD                                      *         
***********************************************************************         
                                                                                
NEWCHD   NTR1  ,                                                                
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING CHDRECD,R3                                                       
         MVC   CHDKEY,BKEY         KEY OF CONTRA                                
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY)                                       
         BAS   RE,NEWCACL          BUILD NEW CACEL ELEMENT                      
*                                                                               
         MVC   RECORD,=C'CHDREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         L     R2,BIO                                                           
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,BKEY,BDIR GET DA              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA NAME ELEMENT                                       *         
***********************************************************************         
                                                                                
         USING CHDRECD,R3                                                       
NEWCACL  NTR1  ,                                                                
         LA    R5,ELEMENT                                                       
         USING CACELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 43 ELEMENT                             
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,CHDKCULC     CODE                                         
         MVC   CACNAME,CONNAM      NAME                                         
         LA    R1,CACLN1Q                                                       
         SR    R0,R0                                                            
         IC    R0,CONNAML          LENGTH OF NAME                               
         AR    R1,R0                                                            
         STC   R1,CACLN                                                         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),CHDRECD,CACEL,0                         
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA BUCKET RECORD                                          *         
***********************************************************************         
                                                                                
CHKCAC   NTR1  ,                                                                
         LA    R2,ADIR                                                          
         USING TRNRECD,R2                                                       
         LA    R3,BKEY             BUILD KEY OF CONTRA HEADER                   
         USING CACRECD,R3                                                       
         MVC   BKEY,SPACES                                                      
         MVC   CACKCULA,TRNKCULA   READ FOR CONTRA HEADER                       
         MVC   CACKCULC,TRNKCULC                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,BKEY,BDIR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BKEY,BDIR           TEST RECORD FOUND                            
         BE    XIT                                                              
         BAS   RE,NEWCAC           ADD NEW CONTRA BUCKET                        
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW CONTRA BUCKET RECORD                                      *         
***********************************************************************         
                                                                                
NEWCAC   NTR1  ,                                                                
         CLI   QOPT2,C'Y'                                                       
         BNE   XIT                                                              
         L     R0,BIO                                                           
         LA    R1,L'IOB                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BIO                                    
*                                                                               
         L     R3,BIO                                                           
         USING CACRECD,R3                                                       
         MVC   CACKEY,BKEY         KEY OF CONTRA                                
         MVC   CACRLEN,=Y(CACRFST-CACKEY)                                       
         LA    R5,ELEMENT                                                       
         USING BUKELD,R5                                                        
         XC    ELEMENT,ELEMENT     BUILD 45 ELEMENT                             
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,=X'9701'     DUMMY MONTH                                  
         ZAP   BUKDR,=P'0'                                                      
         ZAP   BUKCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),CACRECD,BUKEL,0                         
*                                                                               
         MVC   RECORD,=C'CACREC'                                                
         MVC   ACTION,ADDREC                                                    
*                                                                               
         L     R2,BIO                                                           
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    ADDRCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK CONTRA NAME                                                   *         
***********************************************************************         
                                                                                
CHKNAM   NTR1  ,                                                                
         CLI   QOPT1,C'Y'          OPTION TO FIX NAMES                          
         BNE   XIT                                                              
         CLC   CONNAM,SPACES       TEST NEW NAME                                
         BE    XIT                 CAN'T FIX IT                                 
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,BDA,BIO,DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,BIO                                                           
         USING CHDRECD,R3                                                       
         LA    R5,CHDRFST          FIND OLD CONTRA NAME                         
         USING CACELD,R5                                                        
         CLI   CACEL,CACELQ        TEST CONTRA NAME ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1               SAVE OLD NAME                                
         IC    R1,CACLN                                                         
         SH    R1,=Y(CACLN1Q+1)                                                 
         BM    CHKNAM5                                                          
         MVC   OLDNAM(0),CACNAME                                                
         EX    R1,*-6                                                           
         CLC   OLDNAM,CONNAM       TEST NAME CHANGE                             
         BE    XIT                                                              
         MVI   CACEL,X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',CHDRECD),0                       
*                                                                               
CHKNAM5  BAS   RE,NEWCACL          ADD NEW CONTRA NAME ELEMENT                  
         MVC   RECORD,=C'CHDREC'                                                
         MVC   ACTION,=C'PUTREC'                                                
*                                                                               
         L     R2,BIO                                                           
         BAS   RE,PRNT             PRINT RECORD                                 
         BAS   RE,DMPR             DUMP RECORD                                  
*                                                                               
         AP    CHARCD,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,BDA,BIO,DMWORK                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
PRNT     NTR1  ,                                                                
         LA    R5,P                                                             
         USING PLD,R5                                                           
         USING CHDRECD,R2                                                       
         MVC   PLREC,RECORD        RECORD                                       
         MVC   PLACT,ACTION        ACTION                                       
         MVC   PLACC,CHDKULA       ACCOUNT                                      
         MVC   PLOFF,CHDKOFF       OFFICE                                       
         MVC   PLCON,CHDKULC       CONTRA                                       
         MVC   PLOLD,OLDNAM        OLD NAME                                     
         MVC   PLNEW,CONNAM        NEW NAME                                     
         GOTO1 ACREPORT                                                         
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         LA    R5,RECCNT                                                        
*                                                                               
RNL3     MVC   P+1(16),4(R5)                                                    
         EDIT  (P4,0(R5)),(9,P+20),COMMAS=YES                                   
         GOTO1 ACREPORT                                                         
         LA    R5,L'RECCNT(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   RNL3                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP ROUTINES                                                       *         
***********************************************************************         
                                                                                
DMPR     NTR1  ,                                                                
         CLI   QOPT4,C'Y'          OPTION TO DUMP SOME RECORDS                  
         BNE   XIT                                                              
         CP    DMPCNT,DMPMAX                                                    
         BNL   XIT                                                              
*                                                                               
         SR    R5,R5                                                            
         L     R3,AIO                                                           
         ICM   R5,3,ACCRLEN-ACCRECD(R3)                                         
         GOTO1 HEXOUT,DMCB,ADA,TRNSACTA+17,4,0                                  
         MVC   P+8(25),TRNSACTA                                                 
         L     R3,BIO                                                           
         ICM   R5,3,ACCRLEN-ACCRECD(R3)                                         
         GOTO1 HEXOUT,DMCB,BDA,TRNSACTB+17,4,0                                  
         MVC   P+40(25),TRNSACTB                                                
         GOTO1 ACREPORT                                                         
*                                                                               
         CLI   DMPTRN,C'N'         HAS TRANSACTION BEEN DUMPED                  
         BNE   DMPR3                                                            
         MVI   DMPTRN,C'Y'         SET TRANSACTION HAS BEEN DUMPED              
*                                                                               
         LA    R0,L'RECORD                                                      
         GOTO1 PRNTBL,DMCB,((R0),RECORD),(R3),C'DUMP',(R5),=C'2D'               
*                                                                               
DMPR3    AP    DMPCNT,=P'1'                                                     
         SR    R5,R5                                                            
         ICM   R5,3,ACCRLEN-ACCRECD(R2)                                         
         LA    R0,L'ACTION                                                      
         GOTO1 PRNTBL,DMCB,((R0),ACTION),(R2),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
*                                                                               
DMPDIR   NTR1  ,                                                                
         CLI   QOPT4,C'Y'          OPTION TO DUMP SOME RECORDS                  
         BNE   XIT                                                              
         CP    DMPCNT,DMPMAX                                                    
         BNL   XIT                                                              
*                                                                               
         LA    R0,L'ACCDIR                                                      
         LA    R5,54                                                            
         GOTO1 PRNTBL,DMCB,((R0),ACCDIR),(R2),C'DUMP',(R5),=C'2D'               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
ADACTBL  DC    A(ACTBL)                                                         
AIO      DC    A(IOA)                                                           
BIO      DC    A(IOB)                                                           
CIO      DC    A(IOC)                                                           
*                                                                               
DMPCNT   DC    PL2'0'                                                           
DMPMAX   DC    PL2'200'                                                         
*                                                                               
TRNSACTA DC    CL25'TRANSACTION A DA=00000000'                                  
TRNSACTB DC    CL25'TRANSACTION B DA=00000000'                                  
*                                                                               
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
DELREC   DC    CL8'DELREC'                                                      
*                                                                               
RECCNT   DS    0CL20                                                            
CHARCD   DC    PL4'0',CL16'RECORDS CHANGED'                                     
ADDRCD   DC    PL4'0',CL16'RECORDS ADDED'                                       
DELRCD   DC    PL4'0',CL16'RECORDS DELETED'                                     
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACTBL    DS    0CL14                                                            
ACCEND   DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* IO AREAS                                                            *         
***********************************************************************         
                                                                                
IOA      DS    XL2000                                                           
IOB      DS    XL2000                                                           
IOC      DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
ACXGD    DSECT                                                                  
AKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ADIR     DS    CL(ACCKDA-ACCKEY)   TRANSACTION DIRECTORY RECORD                 
ADA      DS    XL(L'ACCKDA)        DISK ADDRESS                                 
*                                                                               
BKEY     DS    CL(L'ACCKEY)                                                     
BDIR     DS    CL(ACCKDA-ACCKEY)                                                
BDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
CKEY     DS    CL(L'ACCKEY)                                                     
CDIR     DS    CL(ACCKDA-ACCKEY)                                                
CDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
FIND     DS    CL1                 A=FOUND ACCOUNT, C=FOUND CONTRA              
OFFICE   DS    CL2                 OFFICE CODE                                  
CONNAM   DS    CL36                CONTRA NAME                                  
CONNAML  DS    XL1                 LENGTH OF NAME                               
OLDNAM   DS    CL36                OLD NAME                                     
*                                                                               
STAT4    DS    XL1                 COMPANY STATUS BYTE 4                        
DMPTRN   DS    CL1                 TRANSACTION DUMP SWITCH                      
*                                                                               
RECORD   DS    CL6                                                              
ACTION   DS    CL6                                                              
*                                                                               
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
         DS    XL1                                                              
PLREC    DS    CL6                 RECORD                                       
         DS    CL1                                                              
PLACT    DS    CL6                 ACTION                                       
         DS    CL1                                                              
PLACC    DS    CL14                ACCOUNT CODE                                 
         DS    CL3                                                              
PLOFF    DS    CL2                 OFFICE                                       
         DS    CL3                                                              
PLCON    DS    CL14                CONTRA ACCOUNT                               
         DS    CL1                                                              
PLOLD    DS    CL36                OLD NAME                                     
         DS    CL1                                                              
PLNEW    DS    CL36                NEW NAME                                     
         ORG   PLD+L'P                                                          
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062ACREPXG02 03/04/19'                                      
         END                                                                    
