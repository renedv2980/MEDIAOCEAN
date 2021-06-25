*          DATA SET CTCOPYREC  AT LEVEL 020 AS OF 01/06/17                      
*PHASE CTCOPYA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATTIM                                                                 
***********************************************************************         
* COPY CONTROL RECORDS FROM ONE AGENCY TO ANOTHER                               
* AGYIN COPIED TO AGYOUT                                                        
***********************************************************************         
         TITLE 'CTFOPY - COPY CONTROL FILE RECORDS'                             
CTCOPY   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CCOP**,RA,WORK=A(WORKC),CLEAR=YES                  
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
                                                                                
***********************************************************************         
* MAIN CONTROL CODE                                                             
***********************************************************************         
MAIN     BRAS  RE,INIT             INTIALISATION                                
         BRAS  RE,COPYWRTR         COPY WRITER  RECORDS                         
         BRAS  RE,COPYDRVR         COPY DRIVER  RECORDS                         
         BRAS  RE,COPYPROF         COPY PROFILE RECORDS                         
         BRAS  RE,DONE             FINISHED                                     
         XBASE                                                                  
                                                                                
***********************************************************************         
* GENERAL INITIALISATION                                                        
***********************************************************************         
INIT     NTR1  ,                                                                
         MVC   P,SPACES                                                         
         BAS   RE,VALCARD          VALIDATE CARD DECK                           
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
         CLI   RCWRITE,YES         WRITE=Y?                                     
         BNE   INIT010             NO                                           
         MVI   FLISTCTF,C'U'       OPEN CTFILE FOR UPDATE                       
         MVI   FLISTCTR,C'U'       OPEN CTRCVR FOR UPDATE                       
*                                                                               
         CLI   RECOVERY,NO         RECOVERY=Y?                                  
         BNE   INIT010             YES                                          
         MVI   FLISTCTR,C'N'       NO NEED TO OPEN RECOVERY FOR UPDATE          
         MVI   SSOSTAT2,SSOSNRCV   NO RECOVERY NEEDED                           
*                                                                               
INIT010  GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLIST,IO                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         USING CT5REC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    ACCESS RECORD                                
         MVC   CT5KALPH,AGYOUT     NEW AGENCY                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5KEY,IO                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         LA    R2,IO                                                            
         LA    R3,CT5DATA          POINT TO ELEMENTS                            
INIT012  CLI   0(R3),0             EOR?                                         
         JE    *+2                 NOT GOOD                                     
         CLI   0(R3),X'02'         USER ID NUMBER ELEMENT                       
         BE    INIT015                                                          
         LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     INIT012                                                          
*                                                                               
INIT015  MVC   ORIGIN#,2(R3)       SAVE ORIGIN ID NUMBER                        
         CLI   RCWRITE,YES         ENQUEUE CONTROL IF WRITING                   
         BNE   INITX                                                            
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                        
         TM    8(R1),X'04'                                                      
         JNO   *+2                                                              
*                                                                               
INITX    J     XITOK                                                            
                                                                                
***********************************************************************         
* CLOSE AND FINISH                                                              
***********************************************************************         
DONE     NTR1  ,                                                                
*                                                                               
         CLOSE (TAPEOUT)                                                        
*                                                                               
         CLI   RCWRITE,YES         ENQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                        
*                                                                               
DONE010  GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,FLIST,IO                            
*                                                                               
         MVC   P(4),=C'DONE'                                                    
         EDIT  (P8,RECCOUNT),(10,P+10),ALIGN=LEFT,ZERO=NOBLANK                  
         MVC   P+21(10),=CL10'RECORDS'                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         CP    DUPCNT,=PL1'0'                                                   
         JE    XITOK                                                            
         EDIT  (P8,DUPCNT),(10,P+10),ALIGN=LEFT,ZERO=NOBLANK                    
         MVC   P+21(30),=CL30'DUPLICATE KEYS ON ADD'                            
         BRAS  RE,PRNT                                                          
         J     XITOK                                                            
                                                                                
***********************************************************************         
* READ CTFILE PROFILE RECORDS AND PUT                                           
***********************************************************************         
COPYPROF CLI   PROFILE,NO                                                       
         BER   RE                  DON'T BOTHER                                 
*                                                                               
         NTR1  ,                                                                
         MVC   P(18),=C'READ USER PROFILES'                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         XC    SVIOKEY,SVIOKEY                                                  
         LA    R2,IO                                                            
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
         B     CPYU30                                                           
CPYU10   MVC   CTUKEY,SVIOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTUKEY,CTUKEY                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
CPYU20   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                        
CPYU30   CLI   8(R1),0                                                          
         BE    CPYU40                                                           
         TM    8(R1),X'80'                                                      
         BO    CPYU90                                                           
         DC    H'0'                                                             
*                                                                               
CPYU40   MVC   SVIOKEY,CTUKEY                                                   
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD                          
         BNE   XITNO               NO: FINISHED                                 
         CLC   CTUKAGY,AGYIN       MATCH ON AGENCY IN                           
         BNE   CPYU20              NO: READ SEQUENTIAL                          
*                                                                               
         CLI   PROFILE,C'O'        OFFICE LIST COPY ONLY?                       
         BNE   CPYU50              NO: SKIP OFFICE LIST CHECK                   
         CLI   CTUKPROG,C'$'       OFFICE LIST                                  
         BE    CPYU50              YES: COPY THIS ONE                           
         CLI   CTUKPROG,C' '       OFFICE LISTS CAN HAVE A SPACE HERE           
         BH    CPYU20              NO: THEN GET NEXT                            
         CLI   CTUKPROG+1,C'$'     OFFICE LIST                                  
         BNE   CPYU20              NO: THEN GET NEXT                            
*                                                                               
CPYU50   AP    COUNTU,=PL1'1'                                                   
*                                                                               
         MVC   P(5),=CL5'COPY'                                                  
         MVC   P+5(25),CTUKEY                                                   
         GOTO1 VHEXOUT,DMCB,CTUKEY,P+35,L'CTUKEY                                
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   CTUKAGY,AGYOUT      REPLACE AGENCY WITH NEW AGENCY               
*                                                                               
         CLI   RCWRITE,YES                                                      
         BNE   CPYU60                                                           
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,CTUKEY,CTUKEY                         
         CLI   8(R1),0                                                          
         JE    CPYU60                                                           
         TM    8(R1),X'20'         DUPLICATE KEY ON ADD                         
         JNO   *+2                                                              
         AP    DUPCNT,=PL1'1'                                                   
         B     CPYU10                                                           
*                                                                               
CPYU60   MVC   P(5),=CL5'ADD'                                                   
         MVC   P+5(25),CTUKEY                                                   
         GOTO1 VHEXOUT,DMCB,CTUKEY,P+35,L'CTUKEY                                
         BRAS  RE,PRNT                                                          
         BRAS  RE,PUTOUT                                                        
         B     CPYU10              NEXT                                         
*                                                                               
CPYU90   MVC   P(30),=CL30'END OF USR PROFILE RECORDS'                          
         BRAS  RE,PRNT                                                          
         B     XITOK                                                            
                                                                                
***********************************************************************         
* READ CTFILE WRITER RECORDS AND PUT                                            
***********************************************************************         
COPYWRTR CLI   WRITER,YES                                                       
         BNER  RE                  DON'T BOTHER                                 
*                                                                               
         NTR1  ,                                                                
         MVC   P(19),=C'READ WRITER RECORDS'                                    
         BRAS  RE,PRNT                                                          
*                                                                               
         XC    SVIOKEY,SVIOKEY                                                  
         LA    R2,IO                                                            
         USING CT01RECD,R2                                                      
         XC    CT01KEY,CT01KEY                                                  
         MVI   CT01TYPE,CT01TYPQ   X'01' WRITER RECORDS                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CT01KEY,CT01KEY                      
         B     CPYW30                                                           
CPYW10   MVC   CT01KEY,SVIOKEY                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT01KEY,CT01KEY                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
CPYW20   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CT01KEY,CT01KEY                      
CPYW30   CLI   8(R1),0                                                          
         BE    CPYW40                                                           
         TM    8(R1),X'80'                                                      
         BO    CPYW90                                                           
         DC    H'0'                                                             
*                                                                               
CPYW40   MVC   SVIOKEY,CT01KEY                                                  
         CLI   CT01TYPE,CT01TYPQ   USER PROFILE RECORD                          
         BNE   CPYW90              NO: FINISHED                                 
         CLC   CT01AGID,AGYIN      MATCH ON AGENCY IN                           
         BNE   CPYW20              NO: READ SEQUENTIAL                          
         AP    COUNT01,=PL1'1'                                                  
*                                                                               
         MVC   P(5),=CL5'COPY'                                                  
         MVC   P+5(25),CT01KEY                                                  
         GOTO1 VHEXOUT,DMCB,CT01KEY,P+35,L'CT01KEY                              
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   CT01AGID,AGYOUT     REPLACE AGENCY WITH NEW AGENCY               
         LA    R3,CT01DATA         POINT TO ELEMENTS                            
CPYW42   CLI   0(R3),0             EOR                                          
         BE    CPYW50                                                           
         USING CT01ORGD,R3                                                      
         CLI   0(R3),CT01UCDQ      X'05' ORIGIN ID ELEMENT                      
         BNE   *+10                                                             
         MVC   CT01UORG,ORIGIN#    REPLACE WITH NEW COMPANY ORIGIN              
         USING CT01OTHD,R3                                                      
         CLI   0(R3),CT01OCDQ      X'20' OTHER DATA ELEMENT                     
         BNE   CPYW48                                                           
         CLI   1(R3),CT01OLNQ      MUST BE THIS LENGTH                          
         BNE   *+10                                                             
         MVC   CT01ORIG,ORIGIN#    REPLACE WITH NEW COMPANY ORIGIN              
         USING CTLOUTD,R3                                                       
         CLI   1(R3),CTLOLENQ      X'20' ELEMENT TOO                            
         BNE   *+8                                                              
         MVI   0(R3),X'FF'         DELETE ELEMENT HAS DEST OVERRIDE             
*                                                                               
CPYW48   LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CPYW42                                                           
*                                                                               
CPYW50   CLI   RCWRITE,YES                                                      
         BNE   CPYW60                                                           
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,CT01KEY,CT01KEY                       
         CLI   8(R1),0                                                          
         JE    CPYW60                                                           
         TM    8(R1),X'20'         DUPLICATE KEY ON ADD                         
         JNO   *+2                                                              
         AP    DUPCNT,=PL1'1'                                                   
         B     CPYW10                                                           
*                                                                               
CPYW60   MVC   P(5),=CL5'ADD'                                                   
         MVC   P+5(25),CT01KEY                                                  
         GOTO1 VHEXOUT,DMCB,CT01KEY,P+35,L'CT01KEY                              
         BRAS  RE,PRNT                                                          
         BRAS  RE,PUTOUT                                                        
         B     CPYW10              NEXT                                         
*                                                                               
CPYW90   MVC   P(25),=CL25'END OF WRITER RECORDS'                               
         BRAS  RE,PRNT                                                          
         B     XITOK                                                            
                                                                                
***********************************************************************         
* READ CTFILE DRIVER RECORDS AND PUT                                            
***********************************************************************         
COPYDRVR CLI   DRIVER,YES                                                       
         BNER  RE                  DON'T BOTHER                                 
*                                                                               
         NTR1  ,                                                                
         MVC   P(19),=C'READ DRIVER RECORDS'                                    
         BRAS  RE,PRNT                                                          
*                                                                               
         XC    SVIOKEY,SVIOKEY                                                  
         LA    R2,IO                                                            
         USING CT02REC,R2                                                       
         XC    CT02KEY,CT02KEY                                                  
         MVI   CT02KTYP,CT02KTYQ   X'02' DRIVER RECORDS                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CT02KEY,CT02KEY                      
         B     CPYD30                                                           
CPYD10   MVC   CT02KEY,SVIOKEY                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT02KEY,CT02KEY                      
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
CPYD20   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CT02KEY,CT02KEY                      
CPYD30   CLI   8(R1),0                                                          
         BE    CPYD40                                                           
         TM    8(R1),X'80'                                                      
         BO    CPYD90                                                           
         DC    H'0'                                                             
*                                                                               
CPYD40   MVC   SVIOKEY,CT02KEY                                                  
         CLI   CT02KTYP,CT02KTYQ   USER PROFILE RECORD                          
         BNE   CPYD90              NO: FINISHED                                 
         CLC   CT02KAGY,AGYIN      MATCH ON AGENCY IN                           
         BNE   CPYD20              NO: READ SEQUENTIAL                          
         AP    COUNT01,=PL1'1'                                                  
*                                                                               
         MVC   P(5),=CL5'COPY'                                                  
         MVC   P+5(25),CT02KEY                                                  
         GOTO1 VHEXOUT,DMCB,CT02KEY,P+35,L'CT02KEY                              
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   CT02KAGY,AGYOUT     REPLACE AGENCY WITH NEW AGENCY               
*                                                                               
*        LA    R3,CT02DATA         POINT TO ELEMENTS                            
*PYD42   CLI   0(R3),0             EOR                                          
*        BE    CPYD50                                                           
*                                                                               
*        ELEMENT SPECIFIC CODE                                                  
*                                                                               
*PYD48   LLC   RF,1(R3)                                                         
*        AR    R3,RF                                                            
*        B     CPYD42                                                           
*                                                                               
CPYD50   CLI   RCWRITE,YES                                                      
         BNE   CPYD60                                                           
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,CT02KEY,CT02KEY                       
         CLI   8(R1),0                                                          
         JE    CPYD60                                                           
         TM    8(R1),X'20'         DUPLICATE KEY ON ADD                         
         JNO   *+2                                                              
         AP    DUPCNT,=PL1'1'                                                   
         B     CPYD10                                                           
*                                                                               
CPYD60   MVC   P(5),=CL5'ADD'                                                   
         MVC   P+5(25),CT02KEY                                                  
         GOTO1 VHEXOUT,DMCB,CT02KEY,P+35,L'CT02KEY                              
         BRAS  RE,PRNT                                                          
         BRAS  RE,PUTOUT                                                        
         B     CPYD10              NEXT                                         
*                                                                               
CPYD90   MVC   P(25),=CL25'END OF DRIVER RECORDS'                               
         BRAS  RE,PRNT                                                          
         B     XITOK                                                            
                                                                                
***********************************************************************         
* PUT RECORDS TO OUTPUT FILE                                                    
***********************************************************************         
PUTOUT   NTR1                                                                   
         AP    RECCOUNT,=PL1'1'                                                 
         SR    RE,RE                                                            
         ICM   RE,3,IO+L'CTUKEY     GET RECORD LENGTH                           
         LA    RE,4(RE)             ADD OUTPUT RECORD LENGTH                    
         SLL   RE,16                SHIFT AND STICK                             
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     XITOK                                                            
                                                                                
***********************************************************************         
* PRINT OUT WHAT IS IN P AND CLEAR P                                            
***********************************************************************         
PRNT     NTR1                                                                   
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         J     XITOK                                                            
***********************************************************************         
* VALIDATE JCL INPUT CARDS                                                      
***********************************************************************         
VALCARD  NTR1                                                                   
*                                                                               
         LA    R3,CARD                                                          
VC010    GOTO1 VCARDS,DMCB,(R3),=C'RE00' VALIDATE CARDS                         
         CLC   =C'/*',0(R3)                                                     
         BE    VC100                                                            
         CLC   =C'XX',0(R3)                                                     
         BE    VC100                                                            
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER                  PRINT PARAMETER CARD                   
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   VC020                                                            
         L     RF,=V(DDSIO)              OVERRIDE DDSIO NAME                    
         MVC   0(8,RF),CARD+6                                                   
         B     VC010                                                            
*                                                                               
VC020    CLC   =C'DSPACE=',CARD                                                 
         BNE   VC030                                                            
         LARL  RF,SSB                    SET DSPACE ID IN SSB                   
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     VC010                                                            
*                                                                               
VC030    CLC   =C'AGYIN=',CARD                                                  
         BNE   VC040                                                            
         MVC   AGYIN,CARD+6              INPUT AGENCY                           
         B     VC010                                                            
*                                                                               
VC040    CLC   =C'AGYOUT=',CARD                                                 
         BNE   VC050                                                            
         MVC   AGYOUT,CARD+7             OUTPUT AGENCY                          
         B     VC010                                                            
*                                                                               
VC050    CLC   =C'WRITE=N',CARD          DEFULT VALUE IS NO                     
         BE    VC010                                                            
         CLC   =C'WRITE=Y',CARD                                                 
         BNE   VC055                                                            
         MVI   RCWRITE,YES               WRITE Y/N                              
         B     VC010                                                            
*                                                                               
VC055    CLC   =C'RECOVERY=Y',CARD       DEFULT VALUE IS YES                    
         BE    VC010                                                            
         CLC   =C'RECOVERY=N',CARD                                              
         BNE   VC060                                                            
         MVI   RECOVERY,NO               WRITE Y/N                              
         B     VC010                                                            
*                                                                               
VC060    CLC   =C'PROFILE=',CARD         PROFILES (DEFAULT IS YES)              
         BNE   VC062                                                            
         MVC   PROFILE,CARD+8                                                   
         CLI   CARD+8,C'O'               OFFICE LIST ONLY                       
         BE    VC010                                                            
         CLI   CARD+8,NO                                                        
         BE    VC010                                                            
         CLI   CARD+8,YES                                                       
         JNE   *+2                                                              
         B     VC010                                                            
*                                                                               
VC062    CLC   =C'WRITER=Y',CARD         DEFAULT IS NO                          
         BNE   VC064                                                            
         MVI   WRITER,YES                COPY WRITER RECORDS                    
         B     VC010                                                            
*                                                                               
VC064    CLC   =C'DRIVER=Y',CARD         DEFAULT IS NO                          
         BNE   VC010                                                            
         MVI   DRIVER,YES                COPY DRIVER RECORDS                    
         B     VC010                                                            
*                                                                               
VCERRX   MVC   P(40),=CL40'**ERROR** INVALID CONTROL CARD'                      
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*                                        END OF CARDS                           
VC100    GOTO1 VPRINTER                                                         
         CLC   AGYIN,SPACES              CARD IS REQUIRED                       
         BNH   VCERRX                                                           
         CLC   AGYOUT,SPACES             CARD IS REQUIRED                       
         BNH   VCERRX                                                           
         J     XITOK                                                            
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
XITOK    SR    RC,RC               RETURN CC EQUAL                              
XITNO    LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
                                                                                
***********************************************************************         
* CONSTANTS AND LITERALS                                                        
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
         DC    0D                                                               
         DC    CL16'**AGENCY INFO**'                                            
AGYIN    DC    CL2'  '             INPUT AGENCY                                 
AGYOUT   DC    CL2'  '             OUTPUT AGENCY                                
ORIGIN#  DC    XL2'0000'           ORIGIN NUMBER OF OUTPUT AGENCY               
*                                                                               
RCWRITE  DC    AL1(NO)             WRITE TO FILE Y/N                            
RECOVERY DC    AL1(YES)            WRITE TO RECOVERY Y/N                        
PROFILE  DC    AL1(YES)            PROFILE RECORDS/O-OFFICE LIST ONLY           
WRITER   DC    AL1(NO)             WRITER RECORDS                               
DRIVER   DC    AL1(NO)             DRIVER RECORDS                               
*                                                                               
RECCOUNT DC    PL8'0'                                                           
DUPCNT   DC    PL8'0'                                                           
COUNTU   DC    PL8'0'              USER PROFILE RECORDS                         
COUNT01  DC    PL8'0'              WRITER RECORDS                               
FFILL    DC    32X'FF'                                                          
         LTORG                                                                  
*                                                                               
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VCARDS   DC    V(CARDS)                                                         
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            +        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
*                                                                               
FLIST    DS    0CL8                                                             
FLISTCTF DC    CL8'NCTFILE '                                                    
FLISTCTR DC    CL8'NCTRCVR '                                                    
         DC    C'X'                                                             
*                                                                               
DMOPEN   DC    C'DMOPEN '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMADD    DC    C'DMADD  '                                                       
*                                                                               
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* CTGENPGREC                                                                    
*        PRINT OFF                                                              
*      ++INCLUDE CTGENPGREC                                                     
*        PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
ASSB     DS    A                                                                
CARD     DS    CL84                                                             
MODE     DS    CL1                                                              
RETCODE  DS    XL1                                                              
SQFLAG   DS    XL1                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
*                                                                               
ACTAB    DS    A                                                                
ACTABX   DS    A                                                                
*                                                                               
TODAY    DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAYC   DS    XL2                 AS ABOVE 2S COMPLEMENTED                     
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
MVSTIME  DS    F                   IBM TIME BINARY 100THS SECS.                 
MVSDATE  DS    F                   IBM DATE JULIAN                              
*                                                                               
WORK     DS    XL256                                                            
*                                                                               
SVIOKEY  DS    XL(L'CTUKEY)                                                     
IOKEY    DS    XL(L'CTUKEY)                                                     
*                                                                               
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
*                                                                               
IOL2     DS    XL4                                                              
IO2      DS    2000X                                                            
*                                                                               
WORKX    DS    0D                                                               
                                                                                
***********************************************************************         
* WORKING STORAGE POOL **                                                       
***********************************************************************         
WORKC    CSECT                                                                  
         DS    (64*1024)X                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTCOPYREC 01/06/17'                                      
         END                                                                    
