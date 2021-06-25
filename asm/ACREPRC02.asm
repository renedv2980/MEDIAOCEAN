*          DATA SET ACREPRC02  AT LEVEL 024 AS OF 05/01/02                      
*PHASE ACRC02A                                                                  
*INCLUDE ACDISOPT                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE POPTCONV                                                               
         TITLE 'PRODUCTION RULES OPTION CONVERSION'                             
ACRC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACRC**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACRCD,RC                                                         
*              OPTIONS                                                          
***********************************************************************         
* OPTION1     D=DELETE OLD ELMENTS (X'3C', X'42')                     *         
* OPTION2     Y= DUMP OUTPUT RECORDS(RCVTAPE)                         *         
* OPTION3     Y= DUMP INPUT RECORDS BEFORE AND AFTER                  *         
* OPTION4     Y= DUMP OPTION RECORDS BEFORE AND AFTER IO              *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACC10                                                            
         MVC   DISOPT(VTYPLNQ),VTYPES                                           
         L     RE,ADCOMFAC                                                      
         USING COMFACSD,RE                                                      
         MVC   HELLO,CHELLO                                                     
         DROP  RE                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         LA    R2,BXHOOK                                                        
         ST    R2,HEADHOOK                                                      
         B     XIT                                                              
         EJECT                                                                  
ACC10    CLI   MODE,REQFRST                                                     
         BNE   ACC30                                                            
         ZAP   OUTRCD,=P'0'                                                     
         ZAP   ADDOPT,=P'0'                                                     
         ZAP   SAMEOPT,=P'0'                                                    
         ZAP   PUTOPT,=P'0'                                                     
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   EVERY,=P'1'                                                      
         ZAP   PDUMP,=P'0'                                                      
         ZAP   MAXDUMP,=P'50'                                                   
*                                                                               
         ZAP   OPTCNT,=P'0'                                                     
         ZAP   EVERYOPT,=P'5'                                                   
         ZAP   OPTDUMP,=P'0'                                                    
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         L     R3,ADCOMP                                                        
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    0(50,R4),0(R4)      SET-UP OPTIONS KEY                           
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL(1),0(R3)        COMPANY                                  
         MVC   ACOPCUL+1(2),=C'SJ'                                              
*                                                                               
* FOR RULES ONLY CONVERT, DELETE THE COMPANY ELEMENT                            
*                                                                               
         BAS   RE,FIXEL                                                         
*                                  GET COMPANY OPTIONS/RATES                    
         GOTO1 POPTCONV,DMCB,(C'C',(R3)),(R4),PUTHOOK,ADCOMFAC                  
*                                  GET COMPANY OPTIONS/RATES                    
ACC15    BAS   RE,RESEL                                                         
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC30    CLI   MODE,PROCLEVA                                                    
         BNE   ACC40                                                            
         L     R3,ADHEIRA          SETUP KEY AT CLIENT LEVEL                    
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPCLI(50),ACOPCLI                                              
         MVC   ACOPCLI,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPCLI(0),3(R3)                                                 
*                                                                               
         BAS   RE,FIXEL                                                         
         GOTO1 POPTCONV,DMCB,(C'C',(R3)),(R4),PUTHOOK,ADCOMFAC                  
         BAS   RE,RESEL                                                         
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC40    CLI   MODE,PROCLEVB                                                    
         BNE   ACC50                                                            
         L     R3,ADHEIRB          SETUP PRODUCT CODE                           
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPPRO(50),ACOPPRO                                              
         MVC   ACOPPRO,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVB                                                      
         ZIC   RF,ACHRLEVA                                                      
         LA    RE,3(RF,R3)         RE TO START OF PRODUCT FIELD                 
         SR    R1,RF               R1 FOR LENGTH OF PRODUCT                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPPRO(0),0(RE)                                                 
         BAS   RE,FIXEL                                                         
*                                                                               
         GOTO1 POPTCONV,DMCB,(C'P',(R3)),(R4),PUTHOOK,ADCOMFAC                  
         BAS   RE,RESEL                                                         
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC50    CLI   MODE,PROCLEVC                                                    
         BNE   ACC60                                                            
         L     R3,ADHEIRC          SETUP JOB KEY                                
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         XC    ACOPJOB(50),ACOPJOB                                              
         MVC   ACOPJOB,SPACES                                                   
         L     R5,ADLDGHIR                                                      
         USING ACHEIRD,R5                                                       
         ZIC   R1,ACHRLEVC                                                      
         ZIC   RF,ACHRLEVB                                                      
         LA    RE,3(RF,R3)         RE TO JOB FIELD                              
         SR    R1,RF               R1 TO LENGTH OF JOB JOB                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOPJOB(0),0(RE)                                                 
*                                                                               
         BAS   RE,FIXEL                                                         
*                                                                               
         GOTO1 POPTCONV,DMCB,(C'J',(R3)),(R4),PUTHOOK,ADCOMFAC                  
         BAS   RE,RESEL                                                         
         BAS   RE,DELETE           DELETE OLD ELEMENTS                          
         B     XIT                                                              
         EJECT                                                                  
ACC60    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         SPACE 1                                                                
ACC63    GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,4(R1)                                                      
         BZ    ACC70               EOF                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         LA    R0,IO-4                                                          
         LR    RF,R1                                                            
         LR    RE,R3               MOVE RECORD TO IO                            
         MVCL  R0,RE                                                            
         BAS   RE,PRNTIT                                                        
         BAS   RE,UPDATE                                                        
         B     ACC63               AND GET NEXT SORT RECORD                     
*                                                                               
ACC70    GOTO1 ACREPORT                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         XC    HEADHOOK,HEADHOOK   REMOVE HOOK ROUTINE                          
         MVI   BOXREQ,C'C'         CLOSE UP THE BOX                             
         GOTO1 ACREPORT                                                         
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
*                                                                               
         MVC   P+1(15),=CL15'RECORDS ADDED'                                     
         EDIT  ADDOPT,(12,P+17),COMMAS=YES                                      
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(22),=CL22'OPTION KEY DUPLICATES'                             
         EDIT  SAMEOPT,(12,P+24),COMMAS=YES                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P+1(15),=CL15'RECORDS CHANGED'                                   
         EDIT  PUTOPT,(12,P+17),COMMAS=YES                                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         MVC   P+2(1),ACOPOG       OFFICE GROUP                                 
         MVC   P+6(2),ACOPOFC      OFFICE                                       
         MVC   P+9(6),ACOPCLI      CLIENT                                       
         MVC   P+16(6),ACOPPRO     PRODUCT                                      
         MVC   P+24(6),ACOPJOB     JOB                                          
         CLI   ACOPMG,X'FF'                                                     
         BE    *+10                                                             
         MVC   P+33(1),ACOPMG      MEDIA GROUP                                  
         MVC   P+37(1),ACOPMED     MEDIA                                        
         CLI   ACOPWG,X'FF'                                                     
         BE    *+10                                                             
         MVC   P+41(1),ACOPWG      WORK GROUP                                   
         MVC   P+45(2),ACOPWORK    WORK CODE                                    
         LA    R2,P+88-8                                                        
         LA    R6,IO                                                            
         USING ACOPD,R6                                                         
         MVI   ELCODE,X'A4'                                                     
         BAS   RE,GETEL                                                         
PRNT15   BNE   PRNT20                                                           
*                                                                               
         XC    DOBLOCK(DOBLOCKX-DOBLOCK),DOBLOCK                                
         ST    R6,DOAOPTEL                                                      
         ST    R2,DOAFLDH                                                       
         MVC   DOACOM,ADCOMFAC                                                  
         GOTO1 DISOPT,DMCB,DOBLOCK                                              
*                                                                               
         L     R5,DOAOPTEN                                                      
         USING OPTBD,R5                                                         
         MVC   P+50(L'OPTBSHRT),OPTBSHRT                                        
         LA    RE,P+50+L'OPTBSHRT                                               
         MVI   0(RE),C'-'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(L'OPTBDESC,RE),OPTBDESC                                        
*                                                                               
         GOTO1 ACREPORT                                                         
         BAS   RE,NEXTEL                                                        
         B     PRNT15                                                           
*                                                                               
PRNT20   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE THE OPTIONS RECORD                                      
*                                                                               
UPDATE   NTR1  ,                                                                
         LA    R4,IO               R4=A(OPTIONS RECORD)                         
         USING ACOPKEY,R4                                                       
         MVC   KEY(L'ACOPKEY),ACOPKEY                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',KEY,IO2                          
         CLC   ACOPKEY,IO2         TEST IF OPTION KEY FOUND                     
         BE    UPDATE6             YES                                          
*                                                                               
UPDATE2  BAS   RE,DMPOPT           HANDLE DUMPING OF OPTIONS RECORDS            
         CLI   RCWRITE,C'N'        TEST IF WRITE=NO SPECIFIED                   
         BE    UPDATE4             YES                                          
         GOTO1 DATAMGR,DMCB,DMADD,=C'ACCOUNT',IO,IO                             
*                                                                               
UPDATE4  BAS   RE,DMPADD                                                        
         AP    ADDOPT,=P'1'                                                     
         B     UPDATEX                                                          
*                                                                               
UPDATE6  LA    R6,ACRECORD-ACKEYD(R4) R6=A(1ST ELEM)                            
         USING ACOPD,R6                                                         
         BAS   RE,DMPOPT                                                        
         AP    SAMEOPT,=P'1'       INCREMENT COUNT OF SAME KEYS                 
         GOTO1 GETL,DUB,('ACOPELQ',IO2),(1,ACOPNUM)                             
         CLI   DMCB+12,0           TEST IF ELEMENT FOUND                        
         BE    UPDATEX             YES-NO NEED TO UPDATE FILE                   
*                                                                               
         GOTO1 ADDL,DUB,IO2,ACOPD                                               
         CLI   RCWRITE,C'N'                                                     
         BE    UPDATE8                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',IO2,IO2                           
*                                                                               
UPDATE8  BAS   RE,DMPPUT                                                        
         AP    PUTOPT,=P'1'                                                     
*                                                                               
UPDATEX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              DELETE THE OLD ELEMENTS                                          
*                                                                               
         DROP  R4                                                               
         USING ACKEYD,R3                                                        
DELETE   NTR1                                                                   
         CLI   QOPT1,C'D'          TEST FOR DELETE OPTION                       
         BNE   DELX                                                             
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R3),IO2                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   AFTSW,C'N'                                                       
         BAS   RE,DMPBFOR          GET A DUMP BEFORE                            
         GOTO1 DELL,DMCB,(X'3C',IO2),0    EXTRA PROFILE                         
         GOTO1 DELL,DMCB,(X'42',IO2),0    AND RULES                             
DEL03    CLI   MODE,REQFRST                                                     
         BNE   DEL05                                                            
         GOTO1 GETL,DMCB,(X'10',IO2),0    COMPANY ELEMENT                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         L     R6,DMCB+12                                                       
         USING ACCOMPD,R6                                                       
         OI    ACMPSTA4,X'20'      TURN ON CONVERTED BIT                        
DEL05    CLI   RCWRITE,C'N'                                                     
         BE    DEL07                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCOUNT',IO2,IO2                           
DEL07    BAS   RE,DMPAFTR            GET DUMP AFTER                             
         MVC   IO2(50),SPACES                                                   
         MVC   IO2(32),KEY                                                      
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',IO2,IO2    RE-READ KEY           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO RENAME ELEMENT CODES TO CONVERT ONLY RULES ELEMENTS            
* AT ENTRY, R3=A(RECORD)                                                        
*                                                                               
FIXEL    NTR1  ,                                                                
         MVI   ELCODE,0                                                         
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     FIXEL2                                                           
*                                                                               
FIXEL1   BAS   RE,NEXTEL                                                        
*                                                                               
FIXEL2   BNE   FIXELX                                                           
         CLI   0(R6),ACMPELQ                                                    
         BNE   *+8                                                              
         MVI   0(R6),X'F0'                                                      
         CLI   0(R6),ACPRELQ                                                    
         BNE   *+8                                                              
         MVI   0(R6),X'F1'                                                      
         CLI   0(R6),ACXPELQ                                                    
         BNE   *+8                                                              
         MVI   0(R6),X'F2'                                                      
         B     FIXEL1                                                           
*                                                                               
FIXELX   B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE RENAMED ELEMENTS                                       
* AT ENTRY, R3=A(RECORD)                                                        
*                                                                               
RESEL    NTR1  ,                                                                
         MVI   ELCODE,0                                                         
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     RESEL2                                                           
*                                                                               
RESEL1   BAS   RE,NEXTEL                                                        
*                                                                               
RESEL2   BNE   RESELX                                                           
         CLI   0(R6),X'F0'                                                      
         BNE   *+8                                                              
         MVI   0(R6),ACMPELQ                                                    
         CLI   0(R6),X'F1'                                                      
         BNE   *+8                                                              
         MVI   0(R6),ACPRELQ                                                    
         CLI   0(R6),X'F2'                                                      
         BNE   *+8                                                              
         MVI   0(R6),ACXPELQ                                                    
         B     RESEL1                                                           
*                                                                               
RESELX   B     XIT                                                              
         EJECT                                                                  
DMPBFOR  NTR1                                                                   
         CLI   QOPT3,C'Y'                                                       
         BNE   XIT                                                              
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         XR    R8,R8                                                            
         LA    R3,IO2                                                           
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,=C'BEFORE',(R3),C'DUMP',(R8),=C'2D'                  
         MVI   AFTSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DMPAFTR  NTR1                                                                   
         CLI   AFTSW,C'Y'                                                       
         BNE   XIT                                                              
         XR    R8,R8                                                            
         LA    R3,IO2                                                           
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,=C'AFTER',(R3),C'DUMP',(R8),=C'2D'                   
         MVI   AFTSW,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
DMPRCV   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'RCVTAPE'                                                   
         XR    R8,R8                                                            
         ICM   R8,3,RCVHEAD                                                     
         LA    R3,RCVHEAD                                                       
         GOTO1 PRNTBL,DMCB,=C'RCVTAPE',(R3),C'DUMP',(R8),=C'2D'                 
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO DUMP THE OPTIONS RECORD                                        
*                                                                               
DMPOPT   NTR1  ,                                                                
         MVI   OPTDMPSW,C'N'                                                    
         CLI   QOPT4,C'Y'                                                       
         BNE   DMPOPTX                                                          
         AP    OPTCNT,=P'1'                                                     
         ZAP   DUB,OPTCNT                                                       
         DP    DUB,EVERYOPT                                                     
         CP    DUB+4(4),=P'0'                                                   
         BNE   DMPOPTX                                                          
         AP    OPTDUMP,=P'1'                                                    
         CP    OPTDUMP,MAXDUMP                                                  
         BH    DMPOPTX                                                          
*                                                                               
         MVI   OPTDMPSW,C'Y'                                                    
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         LH    R3,ACLENGTH                                                      
         GOTO1 PRNTBL,DMCB,=C'OPTION',IO,C'DUMP',(R3),=C'2D'                    
*                                                                               
DMPOPTX  B     XIT                                                              
         SPACE 1                                                                
* SUB-ROUTINE TO DUMP THE ADDED OPTIONS RECORD                                  
*                                                                               
DMPADD   NTR1  ,                                                                
         CLI   OPTDMPSW,C'Y'       TEST TO DUMP OPTION                          
         BNE   XIT                                                              
         LA    R4,IO                                                            
         USING ACOPKEY,R4                                                       
         LH    R3,ACLENGTH                                                      
         GOTO1 PRNTBL,DMCB,=C'ADD',IO,C'DUMP',(R3),=C'2D'                       
         B     XIT                                                              
         SPACE 1                                                                
* SUB-ROUTINE TO DUMP PUT RECORD                                                
*                                                                               
DMPPUT   NTR1  ,                                                                
         CLI   OPTDMPSW,C'Y'                                                    
         BNE   XIT                                                              
         LA    R4,IO2                                                           
         USING ACOPKEY,R4                                                       
         LH    R3,ACLENGTH                                                      
         GOTO1 PRNTBL,DMCB,=C'PUT',IO2,C'DUMP',(R3),=C'2D'                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* HOOK ROUTINE PROVIDED TO POPTCONV TO WRITE RECORDS TO SORT FILE               
*                                                                               
PUTHOOK  NTR1  ,                                                                
         LA    RC,SPACEND          RE-ESTABLISH RC                              
         LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         LH    R5,ACLENGTH                                                      
         LA    R5,4(R5)                  ADD ON 4 TO RECORD LENGTH              
         SLL   R5,16               MOVE TO HIGH ORDER HALFWORD                  
         ST    R5,IO-4                                                          
         GOTO1 SORTER,DMCB,=C'PUT',IO-4,0                                       
         B     XIT                                                              
         EJECT                                                                  
*              BOX ROUTINES (HOOK)                                              
         SPACE 2                                                                
BXHOOK   NTR1  ,                                                                
         LA    RC,SPACEND          RE-ESTABLISH RC                              
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+7,C'T'        SET ROWS                                     
         MVI   MYROW+9,C'M'                                                     
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'          SET LH MARGIN                                
         MVI   MYCOL+4,C'C'                                                     
         MVI   MYCOL+8,C'C'                                                     
         MVI   MYCOL+15,C'C'                                                    
         MVI   MYCOL+23,C'C'                                                    
         MVI   MYCOL+31,C'C'                                                    
         MVI   MYCOL+34,C'C'                                                    
         MVI   MYCOL+40,C'C'                                                    
         MVI   MYCOL+43,C'C'                                                    
         MVI   MYCOL+48,C'C'                                                    
         MVI   MYCOL+87,C'C'                                                    
         MVI   MYCOL+104,C'R'                                                   
         SPACE 1                                                                
         MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         EJECT                                                                  
VTYPES   DS    0F                                                               
         DC    V(ACDISOPT)                                                      
         DC    V(PRNTBL)                                                        
         DC    V(SORTER)                                                        
         DC    V(POPTCONV)                                                      
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,42,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1000'                                  
         SPACE 2                                                                
*              DTF FOR OUTPUT TAPE                                              
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,RECFM=VB,DSORG=PS,MACRF=(PM),            X        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
ACRCD    DSECT                                                                  
DISOPT   DS    V                                                                
PRNTBL   DS    V                                                                
SORTER   DS    V                                                                
POPTCONV DS    V                                                                
HELLO    DS    V                                                                
*                                                                               
       ++INCLUDE ACDOBLOCK                                                      
*                                                                               
OUTRCD   DS    PL6                                                              
ADDOPT   DS    PL6                                                              
SAMEOPT  DS    PL6                                                              
PUTOPT   DS    PL6                                                              
DUMPCNT  DS    PL4                                                              
EVERY    DS    PL4                                                              
PDUMP    DS    PL4                                                              
MAXDUMP  DS    PL4                                                              
*                                                                               
OPTDUMP  DS    PL4                                                              
EVERYOPT DS    PL4                                                              
OPTCNT   DS    PL4                                                              
*                                                                               
ELCODE   DS    CL1                                                              
TODAY2   DS    CL2                                                              
AFTSW    DS    CL1                                                              
OPTDMPSW DS    CL1                                                              
ADBOX    DS    A                                                                
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
         DS    0D                                                               
OUTREC   DS    1028C                                                            
         ORG   OUTREC                                                           
RCVHEAD  DS    28C                                                              
IO       DS    1000C                                                            
IO2      DS    1000C                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACOPTTABD                                                                    
*  DDBIGBOX                                                                     
*  DDCOMFACS                                                                    
*  DDREPXTRAD                                                                   
*  DDREPMASTD                                                                   
*  DDBOXEQUS                                                                    
*  DDREMOTED                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACOPTTABD                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPRC02 05/01/02'                                      
         END                                                                    
