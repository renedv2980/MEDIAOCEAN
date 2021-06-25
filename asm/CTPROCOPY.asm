*          DATA SET CTPROCOPY  AT LEVEL 001 AS OF 10/13/16                      
*PHASE CTPCOPA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
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
         TITLE 'CTPCOP - COPY CONTROL FILE RECORDS'                             
CTPCOP   CSECT                                                                  
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
         BRAS  RE,COPYRECS         READ CTFILE AND PUT RECS                     
         BRAS  RE,DONE             FINISHED                                     
         XBASE                                                                  
                                                                                
***********************************************************************         
* GENERAL INITIALISATION                                                        
***********************************************************************         
INIT     NTR1  ,                                                                
*                                                                               
         BAS   RE,VALCARD          VALIDATE CARD DECK                           
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
         CLI   RCWRITE,C'Y'        WRITE=Y?                                     
         BNE   INIT010             NO                                           
         MVI   FLISTCTF,C'U'       OPEN CTFILE FOR UPDATE                       
         MVI   FLISTCTR,C'U'       OPEN CTRCVR FOR UPDATE                       
*                                                                               
INIT010  GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLIST,IO                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   INIT020                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'E',=C'CTRL')                        
         TM    8(R1),X'04'                                                      
         JNO   *+2                                                              
*                                                                               
INIT020  ZAP   COUNT1,=PL1'0'                                                   
         ZAP   COUNT2,=PL1'0'                                                   
         ZAP   DUPCNT,=PL1'0'                                                   
*                                                                               
INITX    J     EXIT                                                             
                                                                                
***********************************************************************         
* CLOSE AND FINISH                                                              
***********************************************************************         
DONE     NTR1  ,                                                                
*                                                                               
         CLOSE (TAPEOUT)                                                        
*                                                                               
         CLI   RCWRITE,C'Y'        ENQUEUE CONTROL IF WRITING                   
         BNE   DONE010                                                          
         GOTO1 VDATAMGR,DMCB,=C'ENQCTL ',(C'D',=C'CTRL')                        
*                                                                               
DONE010  GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,FLIST,IO                            
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(4),=C'DONE'                                                    
         EDIT  (P8,COUNT1),(10,P+10),ALIGN=LEFT,ZERO=NOBLANK                    
         MVC   P+21(10),=CL10'RECORDS'                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         CP   DUPCNT,=PL1'0'                                                    
         JE   EXIT                                                              
         EDIT  (P8,DUPCNT),(10,P+10),ALIGN=LEFT,ZERO=NOBLANK                    
         MVC   P+21(30),=CL30'DUPLICATE KEYS ON ADD'                            
         GOTO1 VPRINTER                                                         
         J     EXIT                                                             
                                                                                
***********************************************************************         
* READ CTFILE RECORDS AND PUT                                                   
***********************************************************************         
COPYRECS NTR1  ,                                                                
*                                                                               
         MVC   P(11),=C'READ CTFILE'                                            
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
*                                                                               
         LA    R2,IO                                                            
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,CTUKEY,CTUKEY                        
         B     RC030                                                            
RC010    MVC   CTUKEY,IOKEY                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTUKEY,CTUKEY                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
RC020    GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,CTUKEY,CTUKEY                        
RC030    CLI   8(R1),0                                                          
         BE    RC040                                                            
         TM    8(R1),X'80'                                                      
         BO    RC200                                                            
         DC    H'0'                                                             
*                                                                               
RC040    MVC   IOKEY,CTUKEY                                                     
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ    USER PROFILE RECORD                          
         BNE   RCNOX               NO: FINISHED                                 
         CLC   CTUKAGY,AGYIN       MATCH ON AGENCY IN                           
         BNE   RC020               NO: READ SEQUENTIAL                          
*                                                                               
         CLI   OLONLY,C'Y'         OFFICE LIST COPY ONLY?                       
         BNE   RC050               NO: SKIP OFFICE LIST CHECK                   
         CLI   CTUKPROG,C'$'       OFFICE LIST                                  
         BE    RC050               YES: COPY THIS ONE                           
         CLI   CTUKPROG,C' '       OFFICE LISTS CAN HAVE A SPACE HERE           
         BH    RC020               NO: THEN GET NEXT                            
         CLI   CTUKPROG+1,C'$'     OFFICE LIST                                  
         BNE   RC020               NO: THEN GET NEXT                            
*                                                                               
RC050    AP    COUNT2,=PL1'1'                                                   
*                                                                               
         MVC   P(5),=CL5'COPY'                                                  
         MVC   P+5(25),CTUKEY                                                   
         GOTO1 VHEXOUT,DMCB,CTUKEY,P+35,L'CTUKEY                                
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   CTUKAGY,AGYOUT      REPLACE AGENCY WITH NEW AGENCY               
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   RC060                                                            
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,CTUKEY,CTUKEY                         
         CLI   8(R1),0                                                          
         JE    RC060                                                            
         TM    8(R1),X'20'         DUPLICATE KEY ON ADD                         
         JNO   *+2                                                              
         AP    DUPCNT,=PL1'1'                                                   
         B     RC010                                                            
*                                                                               
RC060    MVC   P(5),=CL5'ADD'                                                   
         MVC   P+5(25),CTUKEY                                                   
         GOTO1 VHEXOUT,DMCB,CTUKEY,P+35,L'CTUKEY                                
         GOTO1 VPRINTER                                                         
         BAS   RE,PUTOUT                                                        
         B     RC010                                                            
*                                                                               
RC200    MVC   P,SPACES                                                         
         MVC   P(20),=CL20'END OF FILE'                                         
         GOTO1 VPRINTER                                                         
*                                                                               
RCNOX    B     NO                                                               
RCOKX    B     YES                                                              
                                                                                
***********************************************************************         
* PUT RECORDS TO OUTPUT FILE                                                    
***********************************************************************         
PUTOUT   NTR1                                                                   
*                                                                               
         AP    COUNT1,=PL1'1'                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,IO+L'CTUKEY     GET RECORD LENGTH                           
         LA    RE,4(RE)             ADD OUTPUT RECORD LENGTH                    
         SLL   RE,16                SHIFT AND STICK                             
         STCM  RE,15,IOL                                                        
         PUT   TAPEOUT,IOL                                                      
         B     POOK                                                             
*                                                                               
PONO     B     NO                                                               
POOK     B     YES                                                              
                                                                                
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
         LA    RF,SSB                    SET DSPACE ID IN SSB                   
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
VC050    CLC   =C'WRITE=',CARD                                                  
         BNE   VC060                                                            
         MVC   RCWRITE,CARD+6             WRITE Y/N                             
         B     VC010                                                            
*                                                                               
VC060    CLC   =C'OFFLIST=',CARD                                                
         BNE   VC070                                                            
         MVC   OLONLY,CARD+8             OFFIST LIST ONLY Y/N                   
         B     VC010                                                            
*                                                                               
VC070    B     VCERRX                                                           
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
         J     EXIT                                                             
                                                                                
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* CONSTANTS AND LITERALS                                                        
***********************************************************************         
AGYIN    DC    CL2'  '             INPUT AGENCY                                 
AGYOUT   DC    CL2'  '             OUTPUT AGENCY                                
RCWRITE  DC    C'N'                WRITE TO FILE Y/N                            
OLONLY   DC    C'N'                OFFICE LISTS ONLY                            
*                                                                               
FFILL    DC    32X'FF'                                                          
         LTORG                                                                  
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
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
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
COUNT1   DS    PL8                                                              
COUNT2   DS    PL8                                                              
DUPCNT   DS    PL8                                                              
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
SAVEKEY  DS    XL(L'CTUKEY)                                                     
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
**PAN#1  DC    CL21'001CTPROCOPY 10/13/16'                                      
         END                                                                    
