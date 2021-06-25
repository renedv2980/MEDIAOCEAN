*          DATA SET SECONVACM  AT LEVEL 213 AS OF 05/22/02                      
*PHASE SECACM                                                                   
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SECACM - ACCESS RECORD CONVERT FOR MEDIA PROGRAMS'              
SECACM   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CONV**,RA,R8,WORK=A(WORKC),CLEAR=YES               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(23),=C'CONTROL FILE CONVERSION'                            
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING SAASKEY,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SAASKEY,SAASKEY                      
         B     CONV15A                                                          
*                                                                               
CONV14   MVC   SAASKEY(L'SAASKEY),IOKEY                                         
         CLI   SQFLAG,0                                                         
         BE    CONV15                                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SAASKEY,SAASKEY                      
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
CONV15   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SAASKEY,SAASKEY                      
*                                                                               
CONV15A  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV300                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SAASKEY),SAASKEY                                         
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(1),IO                                                          
*                                                                               
CONV16   CLI   SAASTYP,SAASTYPQ                                                 
         BNE   CONV200                                                          
         CLI   SAASSUB,SAASSUBQ                                                 
         BNE   CONV200                                                          
*        CLI   SAASOVS,X'04'                                                    
*        BE    CONVMED                                                          
         CLI   SAASOVS,X'06'                                                    
         BE    CONVMED                                                          
*        CLI   SAASOVS,X'05'                                                    
*        BNE   CONV200                                                          
*        CLI   SAASPGM,X'12'       CONVERT ALL LEVEL CRAFT RECORDS              
*        BE    CONV17                                                           
         B     CONV200                                                          
*                                                                               
CONVMED  EQU   *                                                                
         CLI   SAASPGM,X'1D'       CONVERT ALL LEVEL COST RECORDS               
         BE    CONV17                                                           
*        CLI   SAASPGM,X'02'       CONVERT ALL LEVEL BUY RECORDS                
*        BE    CONV17                                                           
*        CLI   SAASPGM,X'15'       CONVERT ALL LEVEL DEAL RECORDS               
*        BE    CONV17                                                           
*        B     CONV200                                                          
*        CLI   SAASPGM,X'1D'       CONVERT ALL LEVEL STATS RECORDS              
*        BE    CONV17                                                           
*        B     CONV200                                                          
*        CLI   SAASPGM,X'0C'       CONVERT ALL LEVEL APROG RECORDS              
*        BE    CONV17                                                           
*        B     CONV200                                                          
*        CLI   SAASPGM,X'14'       CONVERT ALL LEVEL REQ RECORDS                
*        BE    CONV17                                                           
*        B     CONV200                                                          
*        CLI   SAASPGM,X'03'       CONVERT ALL LEVEL FILE RECORDS               
*        BE    CONV17                                                           
*        CLI   SAASPGM,X'13'       CONVERT ALL LEVEL FLIST RECORDS              
*        BE    CONV17                                                           
*        CLI   SAASPGM,X'18'       CONVERT ALL LEVEL DENQ RECORDS               
*        BE    CONV17                                                           
*        B     CONV200                                                          
*        CLI   SAASPGM,X'0A'       CONVERT ALL LEVEL CONTRAC RECORDS            
*        BE    CONV17                                                           
         B     CONV200                                                          
*        OC    SAASUID,SAASUID                                                  
*        BNZ   CONV200                                                          
*        OC    SAASAGN,SAASAGN                                                  
*        BNZ   CONV200                                                          
*                                                                               
CONV17   MVC   AGYALPH,SAASAGY                                                  
         BAS   RE,CHKAGY                                                        
         BNE   CONV18                                                           
*        CLI   COUNTRY,3           GERMAN ONLY                                  
*        BNE   CONV18                                                           
*        CLI   COUNTRY,0           UK ONLY                                      
*        BE    *+12                                                             
*        CLI   COUNTRY,1                                                        
*        BNE   CONV18                                                           
*        CLI   SAASPGM,X'02'       CONVERT ALL LEVEL BUY RECORDS                
*        BE    CBUY                                                             
*        CLI   SAASPGM,X'03'       CONVERT ALL LEVEL FILE RECORDS               
*        BE    CFILE                                                            
*        CLI   SAASPGM,X'13'       CONVERT ALL LEVEL FLIST RECORDS              
*        BE    CFLDEL                                                           
         CLI   SAASPGM,X'1D'       CONVERT ALL LEVEL COST RECORDS               
         BE    CCOST                                                            
*        CLI   SAASPGM,X'18'       CONVERT ALL LEVEL DENQ RECORDS               
*        BE    CDENQ                                                            
*        BE    CFLTAR                                                           
*        BE    CFLIST                                                           
*        CLI   SAASPGM,X'0E'       CONVERT ALL LEVEL SCHEDULE RECORDS           
*        BE    CSCHED                                                           
*        CLI   SAASPGM,X'1D'       CONVERT ALL LEVEL STATS RECORDS              
*        BE    CONV17A                                                          
*        BE    CSTATS                                                           
*        CLI   SAASPGM,X'0A'       CONVERT ALL LEVEL CONTRAC RECORDS            
*        BE    CCONTRA                                                          
*        CLI   SAASPGM,X'0C'       CONVERT ALL LEVEL APROG RECORDS              
*        BE    CAPROG                                                           
*        CLI   SAASPGM,X'14'       CONVERT ALL LEVEL REQ RECORDS                
*        BE    CREQ                                                             
*        CLI   SAASPGM,X'15'       CONVERT ALL LEVEL DEAL RECORDS               
*        BE    CDEAL                                                            
*        CLI   SAASPGM,X'12'       CONVERT ALL LEVEL CRAFT RECORDS              
*        BE    CCRAFT                                                           
         DC    H'0'                                                             
CONV17A  CLI   COUNTRY,3           GERMAN ONLY                                  
         BE    CSTATG                                                           
         CLI   COUNTRY,0           UK ONLY                                      
         BE    CSTATE                                                           
         CLI   COUNTRY,1                                                        
         BE    CSTATE                                                           
         B     CONV18                                                           
*                                                                               
CONV18   MVC   P+2(2),AGYALPH                                                   
         MVC   P+4(40),=CL40'NON UK AGENCY'                                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CBUY     LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CBUY010  CLI   0(R3),0                                                          
         BE    CBUY100                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CBUY020                                                          
         CLI   SAMIXRCD,X'09'                                                   
         BE    CBUY030                                                          
CBUY020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CBUY010                                                          
*                                                                               
CBUY030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
*        CLI   SAMIXLN,X'13'                TST                                 
*        BNL   *+8                          TST                                 
*        MVI   SAMIXLN,X'13'                TST                                 
*        NI    SAMIXEL+8,X'FF'-X'18'        TST                                 
         CLI   SAMIXLN,X'11'                ADV                                 
         BNL   *+8                          ADV                                 
         MVI   SAMIXLN,X'11'                ADV                                 
         NI    SAMIXEL+8,X'FF'-X'03'        ADV                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS BUY/GEN ELEMENT CHANGED'                    
         GOTO1 VPRINTER                                                         
         B     CBUY100                                                          
*                                                                               
CBUY040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA0A09'      ADV                                 
         OI    SAMIXEL+9,X'04'              ADV                                 
*        MVC   SAMIXEL(3),=XL3'BA1309'      TST                                 
*        OI    SAMIXEL+18,X'80'             TST                                 
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(23),=C'ACCESS BUY/GEN ELEMENT ADDED'                         
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CBUY100  LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CBUY110  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CBUY120                                                          
         CLI   SAMIXRCD,X'06'                                                   
         BE    CBUY130                                                          
CBUY120  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CBUY110                                                          
*                                                                               
CBUY130  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
*        CLI   SAMIXLN,X'0D'                TST                                 
*        BNL   *+8                          TST                                 
*        MVI   SAMIXLN,X'0D'                TST                                 
*        NI    SAMIXEL+8,X'FF'-X'04'        TST                                 
         CLI   SAMIXLN,X'0D'                ADV                                 
         BNL   *+8                          ADV                                 
         MVI   SAMIXLN,X'0D'                ADV                                 
         NI    SAMIXEL+9,X'FF'-X'80'        ADV                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS BUY/BUY ELEMENT CHANGED'                    
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CSTATE   EQU   *                                                                
*                                                                               
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+4(40),=CL40'UK AGENCY'                                         
         GOTO1 VPRINTER                                                         
*                                                                               
CSMED    EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSME010  CLI   0(R3),0                                                          
         BE    CSME040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSME020                                                          
         CLI   SAMIXRCD,X'05'                                                   
         BE    CSME030                                                          
CSME020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSME010                                                          
*                                                                               
CSME030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'17'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'17'                                                    
         OI    SAMIXEL+22,X'08'                                                 
*ADV*    CLI   SAMIXLN,X'18'                                                    
*ADV*    BNL   *+8                                                              
*ADV*    MVI   SAMIXLN,X'18'                                                    
*ADV*    OI    SAMIXEL+23,X'08'                                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED TYPE CHANGED'                      
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSME040  EQU   *                                                                
         DC    H'0'                                                             
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA1305'                                          
         OI    SAMIXEL+16,X'10'                                                 
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED ELEMENT ADDED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CSTATG   EQU   *                                                                
*                                                                               
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+4(40),=CL40'GERMAN AGENCY'                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSMG010  CLI   0(R3),0                                                          
         BE    CSMG040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSMG020                                                          
         CLI   SAMIXRCD,X'05'                                                   
         BE    CSMG030                                                          
CSMG020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSMG010                                                          
*                                                                               
CSMG030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'17'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'17'                                                    
         OI    SAMIXEL+22,X'04'                                                 
*ADV*    CLI   SAMIXLN,X'18'                                                    
*ADV*    BNL   *+8                                                              
*ADV*    MVI   SAMIXLN,X'18'                                                    
*ADV*    OI    SAMIXEL+23,X'04'                                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED STATION CHANGED'                   
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSMG040  EQU   *                                                                
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA1305'                                          
         OI    SAMIXEL+16,X'10'                                                 
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED ELEMENT ADDED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CSTATS   EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSTA010  CLI   0(R3),0                                                          
         BE    CSTA040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSTA020                                                          
         CLI   SAMIXRCD,X'05'                                                   
         BE    CSTA030                                                          
CSTA020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSTA010                                                          
*                                                                               
CSTA030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
*TST*    CLI   SAMIXLN,X'17'                                                    
*TST*    BNL   *+8                                                              
*TST*    MVI   SAMIXLN,X'17'                                                    
*TST*    OI    SAMIXEL+22,X'02'                                                 
         CLI   SAMIXLN,X'19'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'19'                                                    
         OI    SAMIXEL+24,X'40'                                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED TYPE CHANGED'                      
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSTA040  EQU   *                                                                
         B     CONV200                                                          
         DC    H'0'                                                             
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA1305'                                          
         OI    SAMIXEL+16,X'10'                                                 
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/MED ELEMENT ADDED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CDEAL    EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CDEA010  CLI   0(R3),0                                                          
         BE    CDEA040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CDEA020                                                          
         CLI   SAMIXRCD,X'12'                                                   
         BE    CDEA030                                                          
CDEA020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CDEA010                                                          
*                                                                               
CDEA030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'06'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'06'                                                    
         OI    SAMIXEL+5,X'80'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS DEAL/MED ELEMENT CHANGED'                   
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CDEA040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA0612'                                          
         OI    SAMIXEL+5,X'80'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS DEAL/MED ELEMENT ADDED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CCOST    EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CCOS010  CLI   0(R3),0                                                          
         BE    CCOS040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CCOS020                                                          
         CLI   SAMIXRCD,X'09'                                                   
         BE    CCOS030                                                          
CCOS020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CCOS010                                                          
*                                                                               
CCOS030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'07'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'07'                                                    
         OI    SAMIXEL+5,X'10'                                                  
         OI    SAMIXEL+6,X'20'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS COST/ACC ELEMENT CHANGED'                   
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CCOS040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(3),=XL3'BA0709'                                          
         OI    SAMIXEL+5,X'80'                                                  
         OI    SAMIXEL+5,X'10'                                                  
         OI    SAMIXEL+6,X'20'                                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS COST/ACC ELEMENT ADDED'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CAPROG   EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CAPR010  CLI   0(R3),0                                                          
         BE    CAPR040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CAPR020                                                          
         CLI   SAMIXRCD,X'A9'                                                   
         BE    CAPR030                                                          
CAPR020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CAPR010                                                          
*                                                                               
CAPR030  EQU   *                                                                
         CLI   SAMIXLN,X'06'                                                    
         BL    CAPR040                                                          
         TM    SAMIXEL+5,X'80'                                                  
         BZ    CAPR040                                                          
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS APROG ELEMENT OK'                           
         GOTO1 VPRINTER                                                         
         B     CAPR100                                                          
*                                                                               
CAPR040  EQU   *                                                                
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS APROG ELEMENT NOT OK'                       
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CAPR100  EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CAPR110  CLI   0(R3),0                                                          
         BE    CAPR140                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CAPR120                                                          
         CLI   SAMIXRCD,X'AA'                                                   
         BE    CAPR130                                                          
CAPR120  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CAPR110                                                          
*                                                                               
CAPR130  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'06'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'06'                                                    
         OI    SAMIXEL+5,X'80'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS APROG ELEMENT CHANGED'                      
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CAPR140  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(6),=XL6'BA06AA000080'                                    
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS APROG ELEMENT ADDED'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CDENQ    EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CDEN010  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CDEN020                                                          
         CLI   SAMIXRCD,X'A3'                                                   
         BE    CDEN030                                                          
CDEN020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CDEN010                                                          
*                                                                               
CDEN030  EQU   *                                                                
         CLI   SAMIXLN,X'06'                                                    
         BL    CDEN040                                                          
         TM    SAMIXEL+5,X'80'                                                  
         BZ    CDEN040                                                          
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS QTRHOUR ELEMENT OK'                         
         GOTO1 VPRINTER                                                         
         B     CDEN100                                                          
*                                                                               
CDEN040  EQU   *                                                                
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS QTRHOUR ELEMENT NOT OK'                     
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CDEN100  EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CDEN110  CLI   0(R3),0                                                          
         BE    CDEN140                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CDEN120                                                          
         CLI   SAMIXRCD,X'AB'                                                   
         BE    CDEN130                                                          
CDEN120  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CDEN110                                                          
*                                                                               
CDEN130  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'06'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'06'                                                    
         OI    SAMIXEL+5,X'80'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS MIN ELEMENT CHANGED'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CDEN140  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(6),=XL6'BA06AB000080'                                    
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS MIN ELEMENT ADDED'                          
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CCRAFT   EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CCRA010  CLI   0(R3),0                                                          
         BE    CCRA040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CCRA020                                                          
         CLI   SAMIXRCD,X'B5'                                                   
         BE    CCRA030                                                          
CCRA020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CCRA010                                                          
*                                                                               
CCRA030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'06'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'06'                                                    
         OI    SAMIXEL+5,X'80'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS CRAFT ELEMENT CHANGED'                      
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CCRA040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(6),=XL6'BA06B5000080'                                    
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS CRAFT ELEMENT ADDED'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CREQ     EQU   *                                                                
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CREQ010  CLI   0(R3),0                                                          
         BE    CREQ040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CREQ020                                                          
         CLI   SAMIXRCD,X'01'                                                   
         BE    CREQ030                                                          
CREQ020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CREQ010                                                          
*                                                                               
CREQ030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'0D'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'0D'                                                    
         OI    SAMIXEL+6,X'08'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS REQ ELEMENT CHANGED'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CREQ040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   SAMIXEL(7),=XL7'BA0D0100000108'                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS APROG ELEMENT ADDED'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CSSHED   LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSSH010  CLI   0(R3),0                                                          
         BE    CSFOTH                                                           
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSSH020                                                          
         CLI   SAMIXRCD,X'05'                                                   
         BE    CSSH030                                                          
CSSH020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSSH010                                                          
*                                                                               
CSSH030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
*        CLI   SAMIXLN,X'13'                TST                                 
*        BNL   *+8                          TST                                 
*        MVI   SAMIXLN,X'13'                TST                                 
*        OI    SAMIXEL+17,X'3F'             TST                                 
*        OI    SAMIXEL+18,X'80'             TST                                 
         CLI   SAMIXLN,X'13'                ADV                                 
         BNL   *+8                          ADV                                 
         MVI   SAMIXLN,X'13'                ADV                                 
         OI    SAMIXEL+15,X'01'             ADV                                 
         OI    SAMIXEL+16,X'E0'             ADV                                 
         OI    SAMIXEL+17,X'03'             ADV                                 
         OI    SAMIXEL+18,X'80'             ADV                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS TO MED/STAT ELEMENT CHANGED'                
         GOTO1 VPRINTER                                                         
         B     CSFOTH                                                           
*                                                                               
CSSH040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
*        MVC   SAMIXEL(13),=XL13'BA0D010000F71C75FC'                            
*        GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
*        CLI   12(R1),0                                                         
*        BE    *+6                                                              
*        DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS TO MED/STAT ELEMENT NULL'                   
         GOTO1 VPRINTER                                                         
         B     CSFOTH                                                           
         DROP  R3                                                               
*                                                                               
CSFOTH   EQU   *                                                                
         B     CONV200                                                          
         LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSFO010  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSFO020                                                          
         CLI   SAMIXRCD,X'14'                                                   
         BE    CSFO030                                                          
CSFO020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSFO010                                                          
*                                                                               
CSFO030  EQU   *                                                                
*        CLI   SAMIXLN,X'0B'                TST                                 
*        BL    CSFO050                                                          
*        TM    SAMIXEL+10,X'07'             TST                                 
*        BNZ   *+12                                                             
*        TM    SAMIXEL+11,X'FC'             TST                                 
*        BZ    CSFO050                                                          
         CLI   SAMIXLN,X'10'                ADV                                 
         BL    CSFO050                                                          
         TM    SAMIXEL+15,X'38'             ADV                                 
         BNZ   *+12                                                             
         TM    SAMIXEL+17,X'FC'             ADV                                 
         BZ    CSFO050                                                          
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
*        CLI   SAMIXLN,X'0E'                TST                                 
*        BNL   *+8                          TST                                 
*        MVI   SAMIXLN,X'0E'                TST                                 
*        OI    SAMIXEL+11,X'FC'             TST                                 
         CLI   SAMIXLN,X'12'                ADV                                 
         BNL   *+8                          ADV                                 
         MVI   SAMIXLN,X'12'                ADV                                 
         OI    SAMIXEL+17,X'FC'             ADV                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/FOTH ELEMENT CHANGED'                  
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSFO050  MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS STAT/FOTH ELEMENT NOT CHANGED'              
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSFO040  LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
*        MVC   SAMIXEL(13),=XL13'BA0D010000F71C75FC'                            
*        GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
*        CLI   12(R1),0                                                         
*        BE    *+6                                                              
*        DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(23),=C'ACCESS REQ ELEMENT NULL'                              
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CFLIST   LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CFLI010  CLI   0(R3),0                                                          
         BE    CFLI040                                                          
*        BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CFLI020                                                          
         CLI   SAMIXRCD,X'3A'                                                   
         BE    CFLI030                                                          
CFLI020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLI010                                                          
*                                                                               
CFLI030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'07'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'07'                                                    
*TST*    OI    SAMIXEL+5,X'09'                                                  
*TST*    OI    SAMIXEL+6,X'D0'                                                  
         OI    SAMIXEL+5,X'09'                                                  
         OI    SAMIXEL+6,X'98'                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS FLIST ELEMENT CHANGED'                      
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CFLI040  EQU   *                                                                
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
*TST*    MVC   SAMIXEL(7),=XL7'BA073A000009D0'                                  
         MVC   SAMIXEL(7),=XL7'BA073A00000998'                                  
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(25),=C'ACCESS FLIST ELEMENT NULL'                            
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CFLTAR   LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CFLT010  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CFLT020                                                          
         CLI   SAMIXRCD,X'24'                                                   
         BE    CFLT030                                                          
CFLT020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLT010                                                          
*                                                                               
CFLT030  EQU   *                                                                
         CLI   SAMIXLN,X'06'                                                    
         BL    CONV200                                                          
         TM    SAMIXEL+5,X'01'                                                  
         BNZ   CFLIST                                                           
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CFLDEL   LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CFLD010  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CFLD020                                                          
         CLI   SAMIXRCD,X'12'                                                   
         BE    CFLD030                                                          
CFLD020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFLD010                                                          
*                                                                               
CFLD030  EQU   *                                                                
         CLI   SAMIXLN,X'06'                                                    
         BL    CONV200                                                          
         TM    SAMIXEL+5,X'01'                                                  
         BNZ   CFLIST                                                           
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CSCHED   LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CSCH010  CLI   0(R3),0                                                          
         BE    CSCH040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CSCH020                                                          
         CLI   SAMIXRCD,X'0A'                                                   
         BE    CSCH030                                                          
CSCH020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CSCH010                                                          
*                                                                               
CSCH030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'0D'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'0D'                                                    
         OI    SAMIXEL+12,X'01'                                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS MED/SCHED ELEMENT CHANGED'                  
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CSCH040  EQU   *                                                                
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(25),=C'ACCESS MED/SCHED ELEMENT NULL'                        
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CFILE    LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CFIL010  CLI   0(R3),0                                                          
         BE    CFIL040                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CFIL020                                                          
         CLI   SAMIXRCD,X'4E'                                                   
         BE    CFIL030                                                          
CFIL020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CFIL010                                                          
*                                                                               
CFIL030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS FILE ELEMENT REMOVED'                       
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
*                                                                               
CFIL040  EQU   *                                                                
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(25),=C'ACCESS FILE ELEMENT NULL'                             
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
CCONTRA  LA    R3,SAASDATA                                                      
         SR    R0,R0                                                            
         USING SAMIXEL,R3                                                       
CCON010  CLI   0(R3),0                                                          
         BE    CONV200                                                          
         CLI   SAMIXEL,SAMIXELQ                                                 
         BNE   CCON020                                                          
         CLI   SAMIXRCD,X'01'                                                   
         BE    CCON030                                                          
CCON020  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CCON010                                                          
*                                                                               
CCON030  EQU   *                                                                
         XC    WORK,WORK                                                        
         ZIC   RF,SAMIXLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SAMIXEL                                                  
         MVC   DUB(1),SAMIXRCD                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         CLI   SAMIXLN,X'07'                                                    
         BNL   *+8                                                              
         MVI   SAMIXLN,X'07'                                                    
         NI    SAMIXEL+6,X'FF'-X'80'                                            
*        TM    SAMIXEL+5,X'01'              TST                                 
*        BZ    *+8                          TST                                 
*        OI    SAMIXEL+6,X'80'              TST                                 
*        NI    SAMIXEL+5,X'FF'-X'01'        TST                                 
         TM    SAMIXEL+5,X'08'              ADV                                 
         BZ    *+8                          ADV                                 
         OI    SAMIXEL+6,X'80'              ADV                                 
         NI    SAMIXEL+5,X'FF'-X'08'        ADV                                 
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+2(2),AGYALPH                                                   
         MVC   P+6(40),=CL40'ACCESS CONTRAC ELEMENT CHANGED'                    
         GOTO1 VPRINTER                                                         
         B     CONV200                                                          
         DROP  R3                                                               
*                                                                               
         USING SAASKEY,R2                                                       
CONV200  SR    RE,RE                                                            
         ICM   RE,3,SAASLEN                                                     
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     CONV14                                                           
*                                                                               
CONV300  CLOSE (TAPEOUT)                                                        
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* ADJUST ACCESS RECORD SYSTEM PROGRAM SECURITY                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5REC,R2                                                        
ADSECURE NTR1                                                                   
         MVC   P(2),CT5KALPH                                                    
         MVC   P+4(16),=C'SECURITY PRESENT'                                     
         LA    R3,CT5DATA                                                       
         USING CTSYSD,R3                                                        
ASEC010  CLI   CTSYSEL,0                                                        
         BE    ASECX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   ASEC020                                                          
         CLI   CTSYSNUM,X'06'                                                   
         BNE   ASEC020                                                          
         CLI   CTSYSLEN,X'10'                                                   
         BE    ASEC030                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BE    ASEC040                                                          
ASEC020  SR    R0,R0                                                            
         IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     ASEC010                                                          
*                                                                               
ASEC030  XC    WORK,WORK                                                        
         ZIC   RF,CTSYSLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTSYSEL                                                  
         MVC   DUB(1),CTSYSNUM                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         MVI   CTSYSLEN,X'18'                                                   
         MVC   CTSYSPGM(8),=XL8'01100020A0000000'                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+4(16),=C'SECURITY ADDED  '                                     
         B     ASEC020                                                          
*                                                                               
ASEC040  EQU   *                                                                
         OC    CTSYSPGM(8),=XL8'0100000000000000'                               
         MVC   P+4(16),=C'SECURITY CHANGED'                                     
         B     ASEC020                                                          
*                                                                               
ASECX    GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK AGENCY ALPHA COUNTRY                                          *         
***********************************************************************         
CHKAGY   NTR1  ,                                                                
         MVI   COUNTRY,0                                                        
         LA    RF,AGYTAB                                                        
CAGY002  CLI   0(RF),0                                                          
         BE    CAGY010                                                          
         CLC   0(2,RF),AGYALPH                                                  
         BE    CAGYNO                                                           
         LA    RF,2(RF)                                                         
         B     CAGY002                                                          
*                                                                               
CAGY010  LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH(2),AGYALPH                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5KEY,CT5KEY                        
         BNE   CAGYOK                                                           
* ??     DC    H'00'                                                            
         LA    R1,CT5DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
CAGY020  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R1),CTAGDELQ                                                   
         BE    CAGY030                                                          
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     CAGY020                                                          
*                                                                               
         USING CTAGDEL,R1                                                       
CAGY030  MVC   COUNTRY,CTAGDCTY                                                 
         B     CAGYOK                                                           
*                                                                               
CAGYNO   B     NO                                                               
*                                                                               
CAGYOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK OCONTROL MED/REQ RULE                                         *         
***********************************************************************         
         USING SAASKEY,R2                                                       
CHKOCON  NTR1  ,                                                                
*                                                                               
         LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
*                                                                               
         USING SAOCREC,R4                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,SAASAGY                                                  
         MVC   SAOCOVS,SAASOVS                                                  
         MVC   SAOCPGM,SAASPGM                                                  
         MVC   SAOCUID,SAASUID                                                  
         MVC   SAOCAGN,SAASAGN                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SAOCKEY,SAOCKEY                      
         BNE   COCOOK                                                           
* ??     DC    H'00'                                                            
         LA    R1,SAOCDATA         EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
COCO020  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R1),SAOCTELQ                                                   
         BE    COCO030                                                          
         IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     COCO020                                                          
*                                                                               
         USING SAOCTEL,R1                                                       
COCO030  CLI   SAOCTLN,X'04'                                                    
         BE    COCONO                                                           
         B     COCOOK                                                           
*                                                                               
COCONO   B     NO                                                               
*                                                                               
COCOOK   B     YES                                                              
         DROP  R4                                                               
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
ENGFCW   DC    XL23'0000000000000000000000077F8AA7E4073BF03C000420'             
ENGFCR   DC    XL23'0000000000000000000000077F8AA7E4073BF03C000420'             
GERFCW   DC    XL23'0000000000000000000000077F8AA7E4073FF03C0007E0'             
GERFCR   DC    XL23'0000000000000000000000077F8AA7E4073FF03C0007E0'             
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
* AGENCY DEFAULT EXCEPTIONS                                                     
*                                                                               
AGYTAB   DS    0D                                                               
AGYTABX  DC    X'00'                                                            
*                                                                               
BATTAB   DC    AL1(065)                                                         
         DC    AL1(066)                                                         
         DC    AL1(067)                                                         
         DC    AL1(068)                                                         
         DC    AL1(069)                                                         
         DC    AL1(070)                                                         
         DC    AL1(071)                                                         
         DC    AL1(072)                                                         
         DC    AL1(075)                                                         
         DC    AL1(078)                                                         
         DC    AL1(081)                                                         
         DC    AL1(082)                                                         
         DC    AL1(083)                                                         
         DC    AL1(085)                                                         
         DC    AL1(086)                                                         
         DC    AL1(087)                                                         
         DC    AL1(090)                                                         
         DC    AL1(091)                                                         
         DC    AL1(094)                                                         
         DC    AL1(097)                                                         
         DC    AL1(098)                                                         
         DC    AL1(100)                                                         
         DC    AL1(101)                                                         
         DC    AL1(103)                                                         
         DC    AL1(107)                                                         
         DC    AL1(108)                                                         
         DC    AL1(109)                                                         
         DC    AL1(110)                                                         
         DC    AL1(113)                                                         
         DC    AL1(122)                                                         
         DC    AL1(123)                                                         
* ??     DC    AL1(134)            NOT BT70                                     
         DC    AL1(135)                                                         
         DC    AL1(139)                                                         
BATTABX  DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
SQFLAG   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
PROGNAM  DS    CL7                                                              
AGYALPH  DS    CL2                                                              
COUNTRY  DS    CL1                                                              
PACCVAL  DS    XL2                                                              
PACCADR  DS    A                                                                
PACCSAV  DS    XL2                                                              
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    F                                                                
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'213SECONVACM 05/22/02'                                      
         END                                                                    
