*          DATA SET SPREPFXSC0 AT LEVEL 084 AS OF 04/19/99                      
*PHASE SPFX02S                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SPFX02 - COPY ADD DATE IN SNV RECS'                             
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
                                                                                
************************  REQFRST  **********************************           
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
         XC    COUNT2,COUNT2                                                    
         LA    R7,0                REPORT LINE COUNTER                          
         LA    R8,200              MAXIMUM OUTPUT                               
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         BAS   RE,MNINIT           ITIALIZE MINIO                               
*                                                                               
         LA    R2,XKEY                                                          
         USING SNVKEYD,R2                                                       
*                                                                               
         MVI   SNVKTYPE,SNVKTYPQ   X'0E'                                        
         MVI   SNVKSUB,SNVKSUBQ    X'03'                                        
         MVC   XKEYSAVE,XKEY       BACKING UP A COPY OF THE KEY                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF15                                                           
*                                                                               
REQF10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
REQF15   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,XKEY,P+2,2                                           
         CLC   XKEY(2),XKEYSAVE                                                 
         BNE   REQFX                                                            
         MVC   SVKEY,XKEY          SAVE THE KEY FOR REQ CALLS                   
         MVC   MINMKEY,XKEY                                                     
*                                                                               
         CLI   SNVKMINK,X'FF'      ONLY DEAL WITH MINIO MASTERS                 
         BNE   REQF10                                                           
         DROP  R2                                                               
***********************************************************************         
*        GET THE HEADER ELEMENT                                       *         
***********************************************************************         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'10'                                                    
         MVI   MINFILTL,1                                                       
         XC    MELEM,MELEM                                                      
         LA    R3,MELEM                                                         
         USING SNVHDELD,R3                                                      
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
***********************************************************************         
*    "MINDEL" SHORT HEADER --> PLUG IN DATE --> "MINADD" NEW HEADER   *         
***********************************************************************         
         CLI   SNVHDLEN,SNVHDLN2   IS IT THE LONG VERSION OF THE ELEM?          
         BE    REQF20                                                           
*                                                                               
         MVC   XMELEM,MELEM        DELETE THE SHORTER ELEMENT                   
         GOTO1 VMINIO,DMCB,('MINDEL',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   COUNT3,COUNT                                                     
         MVC   COUNT4,COUNT2                                                    
*                                                                               
         L     RE,COUNT            COUNT HOW MANY RECORDS NEEDS CHANGE          
         LA    RE,1(RE)                                                         
         ST    RE,COUNT                                                         
*                                                                               
         LA    R1,MBUFFR                                                        
         USING SNVKEYD,R1                                                       
         LH    RE,SNVRLEN          RECORD LENGTH                                
         DROP  R1                                                               
         A     RE,COUNT2                                                        
         ST    RE,COUNT2           COUNT HOW MANY EXTRA BYTES NEEDED            
         DROP  R3                                                               
*                                                                               
         XC    MINEKEY,MINEKEY     PULL THE DATE FROM THE F1 ELEMENT            
         MVI   MINEKEY,X'F1'                                                    
         XC    MELEM,MELEM                                                      
         LA    R3,MELEM                                                         
         USING ACTVD,R3                                                         
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0                                                         
         BE    REQF16              DISREGARD IF NO 'F1' ELEMENT                 
         MVC   COUNT,COUNT3                                                     
         MVC   COUNT2,COUNT4                                                    
         B     REQF50                                                           
*                                                                               
REQF16   GOTO1 DATCON,DMCB,(3,ACTVADDT),(2,XHALF)                               
         CLC   XHALF,=X'C421'      ONLY INVOICE AFTER JAN01/98                  
         BNL   REQF17              ELSE GO ON TO NEXT REC                       
         MVC   COUNT,COUNT3                                                     
         MVC   COUNT2,COUNT4                                                    
         B     REQF50                                                           
*                                                                               
REQF17   XC    MELEM,MELEM                                                      
         MVC   MELEM,XMELEM        COPY THE SAVED ELEMENT                       
         LA    R3,MELEM                                                         
         USING SNVHDELD,R3                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    X'10' ELCODE                                 
         MVI   SNVHDLEN,SNVHDLN2   CHANGE THE LENGTH                            
         MVC   SNVHDCDT,XHALF      THE DATE FROM ACTIVITY ELEMENT               
         GOTO1 VMINIO,DMCB,('MINADD',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CR    R7,R8                                                            
         BH    REQF18                                                           
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'10'                                                    
         LA    R3,MELEM                                                         
         USING SNVHDELD,R3                                                      
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,SNVHDCDT,P+14,2         CURRENT TWO BYTES            
         GOTO1 DATCON,DMCB,(2,SNVHDCDT),(8,P+22)   PRINTABLE DATE               
         GOTO1 HEXOUT,DMCB,SVKEY,P+51,15                                        
         MVC   P+45(3),=C'ADD'                                                  
         GOTO1 REPORT                                                           
         LA    R7,1(R7)                                                         
*                                                                               
REQF18   GOTO1 VMINIO,DMCB,('MINCLS',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     REQF50                                                           
*                                                                               
***********************************************************************         
* CHECK TO SEE IF THE DATE IS ALREADY FILLED IN, ELSE GET X'F1' ELEM  *         
***********************************************************************         
REQF20   MVC   XHALF1,SNVHDCDT                                                  
*                                                                               
         OC    SNVHDCDT,SNVHDCDT   IS THE DATE ALREADY FILLED IN?               
         BZ    REQF30                                                           
*                                                                               
REQF25   B     REQF50              DATE ALREADY FILLED, NEXT RECORD             
         DROP  R3                                                               
*                                                                               
REQF30   XC    MINEKEY,MINEKEY     NOT FILLED, GET DATE                         
         MVI   MINEKEY,X'F1'                                                    
         XC    MELEM,MELEM                                                      
         LA    R3,MELEM                                                         
         USING ACTVD,R3                                                         
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0            CAN'T FIND 'F1' -> LEAVE NULLS               
         BNE   REQF33              SKIP MINWRT,MINCLS                           
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(2,XHALF)                               
         B     REQF32                                                           
         DROP  R3                                                               
*                                                                               
REQF32   XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'10'       READ AGAIN THE HEADER ELEMENT                
*                                                                               
         XC    MELEM,MELEM                                                      
         LA    R3,MELEM                                                         
         USING SNVHDELD,R3                                                      
         GOTO1 VMINIO,DMCB,('MINHI',MINBLKD)                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SNVHDCDT,XHALF      STORE THE DATE RETRIEVED                     
*                                                                               
         GOTO1 VMINIO,DMCB,('MINWRT',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VMINIO,DMCB,('MINCLS',MINBLKD)                                   
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF33   CR    R7,R8                                                            
         BH    REQF50                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,XHALF1,P+8,2            ORIGINAL TWO BYTES           
         GOTO1 HEXOUT,DMCB,SNVHDCDT,P+14,2         CURRENT TWO BYTES            
         GOTO1 DATCON,DMCB,(2,SNVHDCDT),(8,P+22)   PRINTABLE DATE               
         GOTO1 HEXOUT,DMCB,SVKEY,P+51,15                                        
         MVC   P+45(4),=C'FILL'                                                 
         GOTO1 REPORT                                                           
         LA    R7,1(R7)                                                         
*                                                                               
REQF50   MVC   XKEY,SVKEY                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF10              LOOP BACK FOR THE SEQUENTIAL CALL            
*                                                                               
REQFX    MVC   P(45),=C'NUMBER OF RECORDS CHANGED TO LONGER ELEMENT: '          
         EDIT  COUNT,(10,P+48),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         MVC   P(45),=C'TOTAL AMOUNT OF SPACE NEEDED FOR THIS FILE : '          
         EDIT  COUNT2,(12,P+48),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
***********************************************************************         
* THIS ROUTINE INITIALIZES MINIO VARIABLES                            *         
***********************************************************************         
*                                                                               
MNINIT   NTR1                                                                   
         GOTO1 LOADER,DMCB,=CL8'T00A74',0,0                                     
         MVC   VMINIO,DMCB+4       SET VMINIO ADDRESS                           
*                                                                               
         LA    R0,MINBLOCK         CLEAR MINBLOCK                               
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,RECUP      A(RECUP)                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'XSPFIL'     FILE NAME                                
         MVC   MINDIR,=CL8'XSPDIR'     DIR NAME                                 
         MVI   MINFKLEN,L'SNVKEY   KEY LENGTH                                   
         MVI   MINEKLEN,L'SNVKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'SNVKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,L'SNVDSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(3975) MAXIMUM RECORD LENGTH                        
         LA    RF,MBUFFR           A(MINIO BUFFER)                              
         ST    RF,MINBUFF                                                       
         MVI   MINNBUF,2           USE ONE BUFFER                               
*                                                                               
         LA    R1,MRECTAB          A(AREA FOR RECORD TABLE)                     
         ST    R1,MINRTAB                                                       
         MVC   MINRTABL,=Y(MRECTABL)  LENGTH OF RECORD TABLE                    
*                                                                               
         LA    RE,MELEM            A(AREA FOR ELEM OR CLUSTER)                  
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'MELEM)   MAX LENGTH OF ELEM OF CLUSTER             
         XC    0(L'MELEM,RE),0(RE)   CLEAR MINELEM AREA                         
         MVC   MINWRITE,RCWRITE                                                 
         MVI   MINDELSW,C'N'       BYPASS DELETES                               
*                                                                               
MINITX   B     EXIT                                                             
         EJECT                                                                  
**************************  REQL  ***********************************           
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
CHANGED  DS    X                                                                
COUNT    DS    F                                                                
COUNT2   DS    F                                                                
COUNT3   DS    F                                                                
COUNT4   DS    F                                                                
TEMP     DS    CL80                                                             
INVDA    DS    XL4                                                              
INVKEY   DS    CL50                                                             
INVKEYSV DS    CL50                                                             
*                                                                               
XHALF    DS    XL2                                                              
XHALF1   DS    XL2                                                              
XHALF2   DS    XL2                                                              
*                                                                               
VMINIO   DS    A                                                                
*                                                                               
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SVKEY    DS    XL(L'XKEY)                                                       
XMELEM   DS    XL256               BACKUP MINIO ELEMENT                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*MELEM*'                                                     
MELEM    DS    XL256               MINIO ELEMENT                                
         DS    0D                                                               
         DC    CL8'*MBLOCK*'                                                    
MINBLOCK DS    CL(MINBLKL)                                                      
*                                                                               
MRECTABL EQU   2000                                                             
         DS    0D                                                               
         DC    CL8'*MRTAB*'                                                     
MRECTAB  DS    XL(MRECTABL)                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*MBUFF*'                                                     
MBUFFR   DS    XL(4000*2)          ROOM FOR 2 BUFFERS                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084SPREPFXSC004/19/99'                                      
         END                                                                    
